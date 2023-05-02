module Helpers.Test (
    TestParams(..),
    runTest,
    runTestWithPosixTime
) where

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import qualified CardanoTestnet as TN
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe)
import qualified Data.Time.Clock.POSIX as Time
import qualified Helpers.Testnet as TN
import Text.Printf (printf)

data TestParams = TestParams {
    localNodeConnectInfo :: C.LocalNodeConnectInfo C.CardanoMode,
    pparams              :: C.ProtocolParameters,
    networkId            :: C.NetworkId,
    tempAbsPath          :: FilePath
}

runTestGeneric :: MonadIO m =>
  String ->
  (Either TN.LocalNodeOptions TN.TestnetOptions -> TestParams -> Time.POSIXTime -> m ()) ->
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  Maybe Time.POSIXTime ->
  m ()
runTestGeneric testName test networkOptions testParams preTestnetTime = do
  liftIO $ putStrLn $ "\nRunning: " ++ testName
  t <- liftIO Time.getPOSIXTime
  test networkOptions testParams (fromMaybe t preTestnetTime)
  t2 <- liftIO Time.getPOSIXTime
  let diff = realToFrac $ t2 - t :: Double
  liftIO $ putStrLn $ "Pass\nDuration: " ++ printf "%.2f" diff ++ "s"

runTest :: MonadIO m =>
  String ->
  (Either TN.LocalNodeOptions TN.TestnetOptions -> TestParams -> m ()) ->
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  m ()
runTest testName test networkOptions testParams =
    runTestGeneric testName (\opts params _ -> test opts params) networkOptions testParams Nothing

runTestWithPosixTime :: MonadIO m =>
  String ->
  (Either TN.LocalNodeOptions TN.TestnetOptions -> TestParams -> Time.POSIXTime -> m ()) ->
  Either TN.LocalNodeOptions TN.TestnetOptions ->
  TestParams ->
  Time.POSIXTime ->
  m ()
runTestWithPosixTime testName test networkOptions testParams preTestnetTime =
    runTestGeneric testName test networkOptions testParams (Just preTestnetTime)

