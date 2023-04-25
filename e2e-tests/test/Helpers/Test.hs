{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Helpers.Test (
    TestParams(..),
    runTest,
    runTestWithPosixTime,

    assert,
    failure,
    success
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import CardanoTestnet qualified as TN
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef)
import Data.Maybe (fromMaybe, isNothing)
import Data.Time.Clock.POSIX qualified as Time
import GHC.IORef (atomicModifyIORef')
import GHC.Stack (HasCallStack, callStack, prettyCallStack, withFrozenCallStack)
import Hedgehog (MonadTest)
import Hedgehog qualified as H
import Helpers.TestResults (TestInfo (..), TestResult (..))
import Helpers.Testnet qualified as TN
import Text.Printf (printf)

data TestParams = TestParams {
    localNodeConnectInfo :: C.LocalNodeConnectInfo C.CardanoMode,
    pparams              :: C.ProtocolParameters,
    networkId            :: C.NetworkId,
    tempAbsPath          :: FilePath
}

runTestGeneric :: MonadIO m =>
  TestInfo ->
  (Either TN.LocalNodeOptions TN.TestnetOptions -> TestParams -> Time.POSIXTime -> m (Maybe String)) ->
  IORef [TestResult] ->
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  Maybe Time.POSIXTime ->
  m ()
runTestGeneric testInfo test resultsRef networkOptions testParams preTestnetTime = do
  liftIO $ putStrLn $ "\nRunning: " ++ testName testInfo
  t <- liftIO Time.getPOSIXTime
  mError <- test networkOptions testParams (fromMaybe t preTestnetTime)
  t2 <- liftIO Time.getPOSIXTime
  let diff = realToFrac $ t2 - t :: Double
  case mError of
    Nothing -> liftIO $ putStrLn $ "Result: Pass\nDuration: " ++ printf "%.2f" diff ++ "s"
    Just e  -> liftIO $ putStrLn $ "Result: Fail\nDuration: " ++ printf "%.2f" diff ++ "s" ++ "\nFailure message: " ++ e
  let result = TestResult
        { resultTestInfo = testInfo
        , resultSuccessful = isNothing mError -- TODO: support test failure
        , resultFailure = mError
        , resultTime = diff
        }
  liftIO $ atomicModifyIORef' resultsRef (\results -> (result : results, ()))

runTest :: MonadIO m =>
  TestInfo ->
  (Either TN.LocalNodeOptions TN.TestnetOptions -> TestParams -> m (Maybe String)) ->
  IORef [TestResult] ->
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m ()
runTest testInfo test resultsRef networkOptions testParams =
  runTestGeneric testInfo (\opts params _ -> test opts params) resultsRef networkOptions testParams Nothing

runTestWithPosixTime :: MonadIO m =>
  TestInfo ->
  (Either TN.LocalNodeOptions TN.TestnetOptions -> TestParams -> Time.POSIXTime -> m (Maybe String)) ->
  IORef [TestResult] ->
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  Time.POSIXTime ->
  m ()
runTestWithPosixTime testInfo test resultsRef networkOptions testParams preTestnetTime =
    runTestGeneric testInfo test resultsRef networkOptions testParams (Just preTestnetTime)

success :: MonadTest m => m (Maybe String)
success = return Nothing

failure :: (MonadTest m) => String -> m (Maybe String)
failure s = do
    let message = s ++ "\n\n" ++ prettyCallStack callStack
    --let src = prettyCallStack callStack --getCaller callStack
    --withFrozenCallStack $ annotateShow s
    return $ Just message

assert :: (MonadTest m, HasCallStack) => String -> Bool -> m (Maybe String)
assert s b = do
  ok <- withFrozenCallStack $ H.eval b
  if ok then
    success
  else do
    let message = "assert failed: " ++ s ++ "\n\n" ++ prettyCallStack callStack
    return $ Just message
