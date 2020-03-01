{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- | This module uses the <https://hackage.haskell.org/package/immortal immortal>
library to build a pool of worker threads that process a queue of tasks
asynchronously.

-}
module Control.Immortal.Queue
    ( -- * Config
      ImmortalQueue(..)
      -- * Run
    , processImmortalQueue
    , QueueId
      -- * Stop
    , closeImmortalQueue
    , killImmortalQueue
    )
where

import           Control.Concurrent             ( MVar
                                                , newEmptyMVar
                                                , takeMVar
                                                , putMVar
                                                , threadDelay
                                                , readMVar
                                                , tryPutMVar
                                                , tryTakeMVar
                                                )
import           Control.Concurrent.Async       ( Async
                                                , async
                                                , wait
                                                , race
                                                , cancel
                                                )
import           Control.Exception              ( Exception )
import           Control.Immortal               ( Thread )
import           Control.Monad                  ( (>=>)
                                                , forever
                                                , void
                                                )
import           Numeric.Natural                ( Natural )

import qualified Control.Immortal              as Immortal


-- | The configuration data required for initializing a worker pool.
data ImmortalQueue a =
    ImmortalQueue
        { qThreadCount :: Natural
        -- ^ Number of worker threads to run.
        , qPollWorkerTime :: Int
        -- ^ Wait time in milliseconds for polling for a free worker.
        , qPop :: IO a
        -- ^ An blocking action to pop the next item off of the queue.
        , qPush :: a -> IO ()
        -- ^ An action to put an item on the queue. Used during shutdown if
        -- we've popped an item but haven't assigned it to a worker yet.
        , qHandler :: a -> IO ()
        -- ^ The handler to perform a queued task.
        , qFailure :: forall e. Exception e => a -> e -> IO ()
        -- ^ An error handler for when a thread encounters an unhandled
        -- exception.
        }

-- | Start a management thread that creates the queue-processing worker
-- threads & return a QueueId that can be used to stop the workers.
processImmortalQueue :: forall a . ImmortalQueue a -> IO QueueId
processImmortalQueue queue = do
    shutdown   <- newEmptyMVar
    asyncQueue <- async $ do
        threads          <- mapM (const makeWorker) [1 .. qThreadCount queue]
        nextAction       <- newEmptyMVar
        asyncQueuePopper <- async $ popQueue nextAction threads
        cleanClose       <- takeMVar shutdown
        cancel asyncQueuePopper
        if cleanClose
            then do
                mapM_ (Immortal.mortalize . wdThread) threads
                mapM_ (flip putMVar () . wdCloseMVar) threads
            else mapM_ (Immortal.stop . wdThread) threads
        mapM_ (Immortal.wait . wdThread) threads
        tryTakeMVar nextAction >>= \case
            Nothing     -> return ()
            Just action -> qPush queue action
    return QueueId { qiCloseCleanly = shutdown, qiAsyncQueue = asyncQueue }
  where
    -- Create the communication MVars for a worker & then start the
    -- Immortal thread.
    makeWorker :: IO (WorkerData a)
    makeWorker = do
        closeMVar <- newEmptyMVar
        inputMVar <- newEmptyMVar
        let makeData thread = WorkerData { wdCloseMVar = closeMVar
                                         , wdInputMVar = inputMVar
                                         , wdThread    = thread
                                         }
        makeData <$> Immortal.create (processAction . makeData)

    -- Wait for an input action or the close signal, then process the input
    -- or close by returning.
    processAction :: WorkerData a -> IO ()
    processAction workerData = do
        actionOrClose <- race (takeMVar $ wdCloseMVar workerData)
                              (readMVar $ wdInputMVar workerData)
        case actionOrClose of
            Left () -> return ()
            Right action ->
                let flushInput = void $ takeMVar $ wdInputMVar workerData
                    finalize   = errorHandler action >=> const flushInput
                in  Immortal.onFinish finalize $ qHandler queue action

    -- Ignore successful results & run the error handler on any exceptions.
    errorHandler :: Exception e => a -> Either e () -> IO ()
    errorHandler action = either (qFailure queue action) (const $ return ())

    -- Pop an item from the queue, put it in the enqueued action MVar,
    -- assign it to a worker, & then remove it as the currently enqueued
    -- action.
    popQueue :: MVar a -> [WorkerData a] -> IO ()
    popQueue actionMVar workers = forever $ do
        nextAction <- qPop queue
        putMVar actionMVar nextAction
        assignWork workers nextAction
        void $ takeMVar actionMVar

    -- Assign the action to the first free worker, retrying until a worker
    -- is available.
    assignWork :: [WorkerData a] -> a -> IO ()
    assignWork workers action = do
        assigned <- assignToFreeWorker action workers
        if assigned
            then return ()
            else
                threadDelay (1000 * qPollWorkerTime queue)
                    >> assignWork workers action

    -- Iterate through the workers, trying to assign the action & returning
    -- whether assigning was successful or not.
    assignToFreeWorker :: a -> [WorkerData a] -> IO Bool
    assignToFreeWorker action = \case
        []          -> return False
        worker : ws -> do
            successfulPut <- tryPutMVar (wdInputMVar worker) action
            if successfulPut then return True else assignToFreeWorker action ws


data WorkerData a =
    WorkerData
        { wdCloseMVar :: MVar ()
        -- ^ Signal to shut down the worker
        , wdInputMVar :: MVar a
        -- ^ The action to process
        , wdThread :: Thread
        -- ^ The "Immortal" thread for the worker
        }


-- | An identifier created by a queue manager that can be used to stop the
-- worker processes.
data QueueId =
    QueueId
        { qiCloseCleanly :: MVar Bool
        , qiAsyncQueue :: Async ()
        }


-- | Cleanly close the worker pool, allowing them to complete their
-- actions.
closeImmortalQueue :: QueueId -> IO ()
closeImmortalQueue queueId =
    putMVar (qiCloseCleanly queueId) True >> wait (qiAsyncQueue queueId)

-- | Uncleanly close the worker pool, aborting current actions.
killImmortalQueue :: QueueId -> IO ()
killImmortalQueue queueId =
    putMVar (qiCloseCleanly queueId) False >> wait (qiAsyncQueue queueId)