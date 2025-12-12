{-# LANGUAGE LambdaCase #-}
module MockQueue where

import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Immortal.Queue
import           Data.Maybe


data Task
    = Log Integer Int
    -- ^ Always succeeds with Integer after Int milliseconds
    | Fail String
    -- ^ Always fails
    | PopFail String
    -- ^ Throws error in `qPop`

data PopFailException
    = PopE String
    deriving (Show, Eq)
instance Exception PopFailException

queueConfig :: TVar ([Integer], [String], [PopFailException]) -> TQueue Task -> ImmortalQueue Task
queueConfig output q = ImmortalQueue { qThreadCount = 2
                                     , qPollWorkerTime = 200
                                     , qPop = popQueue
                                     , qPush = atomically . writeTQueue q
                                     , qHandler = performTask
                                     , qFailure = handleError
                                     , qPopFailure = atomically . addPopFailure
                                     }
  where
    performTask = \case
        Log i t   -> threadDelay (1000 * t) >> atomically (addSuccess i)
        Fail _    -> error "failed"
        PopFail _ -> return ()

    handleError t _ = atomically $ case t of
        Log _ _   -> return ()
        Fail s    -> addFailure s
        PopFail _ -> return ()

    popQueue = do
        task <- atomically (readTQueue q)
        case task of
            PopFail errMsg ->
                throw $ PopE errMsg
            _ ->
                return task

    addSuccess i    = modifyTVar output $ \(s, f, p) -> (s ++ [i], f, p)
    addFailure m    = modifyTVar output $ \(s, f, p) -> (s, f ++ [m], p)
    addPopFailure e = modifyTVar output $ \(s, f, p) -> (s, f, p ++ maybeToList (fromException e))


-- | Run pool that processes all the given tasks, splitting successes and
-- failures.
runPool :: [Task] -> IO ([Integer], [String], [PopFailException])
runPool = runPool_ True Nothing


-- | Run a pool that processes the given tasks. `cleanClose` indicates if the
-- pool should be closed cleanly and `waitTime` will wait the specified
-- time before closing/killing or wait until the queue is empty if Nothing.
runPool_ :: Bool -> Maybe Int -> [Task] -> IO ([Integer], [String], [PopFailException])
runPool_ cleanClose waitTime tasks = do
    output  <- newTVarIO ([], [], [])
    tqueue  <- newTQueueIO
    workers <- processImmortalQueue $ queueConfig output tqueue
    atomically $ mapM_ (writeTQueue tqueue) tasks
    maybe (waitEmpty tqueue) (threadDelay . (* 1000)) waitTime
    if cleanClose then closeImmortalQueue workers else killImmortalQueue workers
    readTVarIO output


-- | Wait until the given queue is empty
waitEmpty :: TQueue a -> IO ()
waitEmpty q = do
    isEmpty <- atomically $ isEmptyTQueue q
    if isEmpty then return () else threadDelay 1000000 >> waitEmpty q
