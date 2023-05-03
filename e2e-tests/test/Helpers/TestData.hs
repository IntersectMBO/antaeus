{-# LANGUAGE RankNTypes #-}
module Helpers.TestData where
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import CardanoTestnet qualified as TN
import Control.Monad.IO.Class (MonadIO)
import Data.Time.Clock.POSIX (POSIXTime)
import Hedgehog (MonadTest)
import Helpers.Testnet qualified as TN

type TestFunction = forall m. (MonadIO m, MonadTest m) =>
                Either TN.LocalNodeOptions TN.TestnetOptions ->
                TestParams ->
                m (Maybe String)

data TestInfo = TestInfo {
    testName        :: String,
    testDescription :: String,
    test            :: TestFunction
}

instance Show TestInfo where
  show (TestInfo name description _) =
    "TestInfo { testName = " ++ show name ++
    ", testDescription = " ++ show description ++ " }"

data TestParams = TestParams {
    localNodeConnectInfo :: C.LocalNodeConnectInfo C.CardanoMode,
    pparams              :: C.ProtocolParameters,
    networkId            :: C.NetworkId,
    tempAbsPath          :: FilePath,
    mTime                :: Maybe POSIXTime
}
