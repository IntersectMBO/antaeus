{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Helpers.Test (
    runTest,
    assert,
    failure,
    success
) where

import CardanoTestnet qualified as TN
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef)
import Data.Maybe (isNothing)
import Data.Time.Clock.POSIX qualified as Time
import GHC.IORef (atomicModifyIORef')
import GHC.Stack (HasCallStack, callStack, prettyCallStack, withFrozenCallStack)
import Hedgehog (MonadTest)
import Hedgehog qualified as H
import Helpers.TestData (TestInfo (..), TestParams (..))
import Helpers.TestResults (TestResult (..))
import Helpers.Testnet qualified as TN
import Text.Printf (printf)

runTest :: (MonadIO m, MonadTest m) =>
  TestInfo ->
  IORef [TestResult] ->
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m ()
runTest testInfo resultsRef networkOptions testParams = do
  liftIO $ putStrLn $ "\nRunning: " ++ testName testInfo
  t <- liftIO Time.getPOSIXTime
  mError <- test testInfo networkOptions testParams
  t2 <- liftIO Time.getPOSIXTime
  let diff = realToFrac $ t2 - t :: Double
  case mError of
    Nothing -> liftIO $ putStrLn $ "Result: Pass\nDuration: " ++ printf "%.2f" diff ++ "s"
    Just e  -> liftIO $ putStrLn $ "Result: Fail\nDuration: " ++ printf "%.2f" diff ++ "s" ++ "\nFailure message: " ++ e
  let result = TestResult
        { resultTestInfo = testInfo
        , resultSuccessful = isNothing mError
        , resultFailure = mError
        , resultTime = diff
        }
  liftIO $ atomicModifyIORef' resultsRef (\results -> (result : results, ()))

success :: MonadTest m => m (Maybe String)
success = return Nothing

failure :: (MonadTest m) => String -> m (Maybe String)
failure s = do
    let message = s ++ "\n\n" ++ prettyCallStack callStack
    return $ Just message

assert :: (MonadTest m, HasCallStack) => String -> Bool -> m (Maybe String)
assert s b = do
  ok <- withFrozenCallStack $ H.eval b
  if ok then
    success
  else do
    let message = "assert failed: " ++ s ++ "\n\n" ++ prettyCallStack callStack
    return $ Just message
