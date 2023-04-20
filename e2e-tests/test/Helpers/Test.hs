{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Helpers.Test (
    TestParams(..),
    runTest,
    runTestWithPosixTime
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import CardanoTestnet qualified as TN
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef)
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX qualified as Time
import GHC.IORef (atomicModifyIORef')
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
  (Either TN.LocalNodeOptions TN.TestnetOptions -> TestParams -> Time.POSIXTime -> m ()) ->
  IORef [TestResult] ->
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  Maybe Time.POSIXTime ->
  m ()
runTestGeneric testInfo test resultsRef networkOptions testParams preTestnetTime = do
  liftIO $ putStrLn $ "\nRunning: " ++ testName testInfo
  t <- liftIO Time.getPOSIXTime
  test networkOptions testParams (fromMaybe t preTestnetTime)
  t2 <- liftIO Time.getPOSIXTime
  let diff = realToFrac $ t2 - t :: Double
  liftIO $ putStrLn $ "Pass\nDuration: " ++ printf "%.2f" diff ++ "s"
  let result = TestResult
        { resultTestInfo = testInfo
        , resultSuccessful = True -- TODO: support test failure
        , resultTime = diff
        }
  liftIO $ atomicModifyIORef' resultsRef (\results -> (result : results, ()))

runTest :: MonadIO m =>
  TestInfo ->
  (Either TN.LocalNodeOptions TN.TestnetOptions -> TestParams -> m ()) ->
  IORef [TestResult] ->
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m ()
runTest testInfo test resultsRef networkOptions testParams =
  runTestGeneric testInfo (\opts params _ -> test opts params) resultsRef networkOptions testParams Nothing

runTestWithPosixTime :: MonadIO m =>
  TestInfo ->
  (Either TN.LocalNodeOptions TN.TestnetOptions -> TestParams -> Time.POSIXTime -> m ()) ->
  IORef [TestResult] ->
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  Time.POSIXTime ->
  m ()
runTestWithPosixTime testInfo test resultsRef networkOptions testParams preTestnetTime =
    runTestGeneric testInfo test resultsRef networkOptions testParams (Just preTestnetTime)
