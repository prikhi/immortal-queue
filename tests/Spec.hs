import           Test.Tasty
import           Test.Tasty.HUnit

import           MockQueue


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup
    "Immortal Queue Tests"
    [ testCase "All Succeed"                      allSuccess
    , testCase "Error Handling"                   errorHandling
    , testCase "Close Waits For Workers"          closeWaits
    , testCase "Kill Stops Workers Immediately"   killExitsEarly
    , testCase "Pop Failure Continues Processing" popFailureContinues
    ]
  where
    allSuccess :: Assertion
    allSuccess = do
        (successes, failures, popFailures) <- runPool
            [Log 1 100, Log 2 200, Log 3 300, Log 4 400, Log 5 500]
        successes @?= [1, 2, 3, 4, 5]
        failures @?= []
        popFailures @?= []

    errorHandling :: Assertion
    errorHandling = do
        (successes, failures, popFailures) <- runPool
            [Log 1 0, Fail "hello", Log 2 0, Fail "world", Fail "9001"]
        successes @?= [1, 2]
        failures @?= ["hello", "world", "9001"]
        popFailures @?= []

    closeWaits :: Assertion
    closeWaits = do
        (successes, failures, popFailures) <- runPool_ True
                                                       (Just 500)
                                                       [Log 1 0, Log 2 200, Log 3 2000]
        successes @?= [1, 2, 3]
        failures @?= []
        popFailures @?= []

    killExitsEarly :: Assertion
    killExitsEarly = do
        (successes, failures, popFailures) <- runPool_ False
                                                       (Just 500)
                                                       [Log 1 0, Log 2 200, Log 3 3000]
        successes @?= [1, 2]
        failures @?= []
        popFailures @?= []

    popFailureContinues :: Assertion
    popFailureContinues = do
        (successes, failures, popFailures) <- runPool_
            True
            (Just 800)
            [Log 1 10, PopFail "died", Log 2 70, Log 3 100, PopFail "again", Log 4 200, Log 5 300, Log 6 400]
        successes @?= [1, 2, 3, 4, 5, 6]
        failures @?= []
        popFailures @?= [PopE "died", PopE "again"]
