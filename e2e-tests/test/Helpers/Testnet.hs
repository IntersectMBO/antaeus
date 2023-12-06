{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Helpers.Testnet where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromJust)
import Hedgehog (MonadTest)
import Hedgehog.Extras.Stock (waitSecondsForProcess)
import Hedgehog.Extras.Stock.IO.Network.Sprocket qualified as IO
import Hedgehog.Extras.Test qualified as HE
import Hedgehog.Extras.Test.Base qualified as H
import Helpers.Common (
  makeAddress,
  makeAddressWithStake,
  toConwayEraOnwards,
  toMaryEraOnwards,
  toShelleyBasedEra,
 )
import Helpers.Utils (maybeReadAs)
import System.Directory qualified as IO
import System.FilePath ((</>))
import System.Posix.Signals (sigKILL, signalProcess)

import Cardano.Api.Ledger (
  Credential (KeyHashObj),
  EraCrypto,
  KeyHash,
  KeyRole (StakePool),
  StandardCrypto,
  Voter (StakePoolVoter),
 )
import Cardano.Testnet qualified as CTN
import Control.Lens ((&), (.~))
import Hedgehog qualified as H
import Helpers.Error (TimedOut (ProcessExitTimedOut))
import Helpers.Query qualified as Q
import Prettyprinter (Doc)
import System.Process (cleanupProcess)
import System.Process.Internals (
  PHANDLE,
  ProcessHandle__ (ClosedHandle, OpenExtHandle, OpenHandle),
  withProcessHandle,
 )
import Testnet.Runtime qualified as CTN

data TestEnvironmentOptions era
  = TestnetOptions
      { testnetEra :: C.CardanoEra era
      , testnetProtocolVersion :: Int
      , testnetCardanoOptions :: CTN.CardanoTestnetOptions
      }
  | LocalNodeOptions
      { localNodeEra :: C.CardanoEra era
      , localNodeProtocolVersion :: Int
      , localNodeEnvDir :: FilePath -- path to directory containing 'utxo-keys' and 'ipc' directories
      , localNodeTestnetMagic :: Int
      }
  deriving (Show)

defAlonzoTestnetOptions :: TestEnvironmentOptions C.AlonzoEra
defAlonzoTestnetOptions =
  TestnetOptions
    { testnetEra = C.AlonzoEra
    , testnetProtocolVersion = 6
    , testnetCardanoOptions =
        CTN.cardanoDefaultTestnetOptions
          { CTN.cardanoNodeEra = C.AnyCardanoEra C.AlonzoEra
          , CTN.cardanoProtocolVersion = 6
          , CTN.cardanoSlotLength = 0.1
          , CTN.cardanoEpochLength = 10_000 -- higher value so that txs can have higher upper bound validity range
          }
    }

defBabbageTestnetOptions :: Int -> TestEnvironmentOptions C.BabbageEra
defBabbageTestnetOptions protocolVersion =
  TestnetOptions
    { testnetEra = C.BabbageEra
    , testnetProtocolVersion = protocolVersion
    , testnetCardanoOptions =
        CTN.cardanoDefaultTestnetOptions
          { CTN.cardanoNodeEra = C.AnyCardanoEra C.BabbageEra
          , CTN.cardanoProtocolVersion = protocolVersion
          , CTN.cardanoSlotLength = 0.1
          , CTN.cardanoEpochLength = 10_000 -- higher value so that txs can have higher upper bound validity range
          }
    }

defConwayTestnetOptions :: TestEnvironmentOptions C.ConwayEra
defConwayTestnetOptions =
  TestnetOptions
    { testnetEra = C.ConwayEra
    , testnetProtocolVersion = 9
    , testnetCardanoOptions =
        CTN.cardanoDefaultTestnetOptions
          { CTN.cardanoNodeEra = C.AnyCardanoEra C.ConwayEra
          , CTN.cardanoProtocolVersion = 9
          , CTN.cardanoSlotLength = 0.1
          , CTN.cardanoEpochLength = 10_000 -- higher value so that txs can have higher upper bound validity range
          }
    }

shortEpochConwayTestnetOptions :: TestEnvironmentOptions C.ConwayEra
shortEpochConwayTestnetOptions =
  defConwayTestnetOptions
    { testnetCardanoOptions =
        (testnetCardanoOptions defConwayTestnetOptions)
          { CTN.cardanoActiveSlotsCoeff = 0.5 -- adjusted from default due to short epoch length
          , CTN.cardanoEpochLength = 200 -- 20 second epoch for testing outcome of governance actions
          }
    }

localNodeOptionsPreview :: TestEnvironmentOptions C.BabbageEra
localNodeOptionsPreview =
  LocalNodeOptions
    { localNodeEra = C.BabbageEra
    , localNodeProtocolVersion = 8
    , localNodeEnvDir = "/tmp/preview"
    , localNodeTestnetMagic = 2
    }

testnetOptionsAlonzo6 :: TestEnvironmentOptions C.AlonzoEra
testnetOptionsAlonzo6 = defAlonzoTestnetOptions

testnetOptionsBabbage7 :: TestEnvironmentOptions C.BabbageEra
testnetOptionsBabbage7 = defBabbageTestnetOptions 7

testnetOptionsBabbage8 :: TestEnvironmentOptions C.BabbageEra
testnetOptionsBabbage8 = defBabbageTestnetOptions 8

testnetOptionsConway9 :: TestEnvironmentOptions C.ConwayEra
testnetOptionsConway9 = defConwayTestnetOptions

testnetOptionsConway9Governance :: TestEnvironmentOptions C.ConwayEra
testnetOptionsConway9Governance = shortEpochConwayTestnetOptions

eraFromOptions :: TestEnvironmentOptions era -> C.CardanoEra era
eraFromOptions options = case options of
  TestnetOptions era _ _ -> era
  LocalNodeOptions era _ _ _ -> era

eraFromOptionsM :: (MonadTest m) => TestEnvironmentOptions era -> m (C.CardanoEra era)
eraFromOptionsM = return . eraFromOptions

pvFromOptions :: (MonadTest m) => TestEnvironmentOptions era -> m Int
pvFromOptions (TestnetOptions _ pv _) = pure pv
pvFromOptions (LocalNodeOptions _ pv _ _) = pure pv

-- | Get path to where cardano-testnet files are
getProjectBase :: (MonadIO m, MonadTest m) => m String
getProjectBase = liftIO . IO.canonicalizePath =<< HE.getProjectBase

-- | Start a testnet with provided testnet options (including era and protocol version)
startTestnet
  :: TestEnvironmentOptions era
  -> FilePath
  -> H.Integration
      ( C.LocalNodeConnectInfo
      , C.LedgerProtocolParameters era
      , C.NetworkId
      , Maybe [CTN.PoolNode]
      )
startTestnet LocalNodeOptions{} _ = error "LocalNodeOptions not supported"
startTestnet TestnetOptions{..} tempAbsBasePath = do
  conf :: CTN.Conf <-
    HE.noteShowM $
      CTN.mkConf tempAbsBasePath
  tn <- CTN.cardanoTestnet testnetCardanoOptions conf
  -- needed to avoid duplication of directory in filepath
  let tmpAbsBasePath' = CTN.makeTmpBaseAbsPath $ CTN.tempAbsPath conf

  -- Boilerplate codecs used for protocol serialisation. The number of epochSlots is specific
  -- to each blockchain instance. This value is used by cardano mainnet/testnet and only applies
  -- to the Byron era.
  socketPathAbs <- getPoolSocketPathAbs tmpAbsBasePath' tn
  let epochSlots = C.EpochSlots 21_600
      localNodeConnectInfo =
        C.LocalNodeConnectInfo
          { C.localConsensusModeParams = C.CardanoModeParams epochSlots
          , C.localNodeNetworkId = getNetworkId tn
          , C.localNodeSocketPath = socketPathAbs
          }
      networkId = getNetworkId tn
  pparams <- Q.getProtocolParams testnetEra localNodeConnectInfo
  pure (localNodeConnectInfo, pparams, networkId, Just $ CTN.poolNodes tn)

cleanupTestnet :: (MonadIO m) => Maybe [CTN.PoolNode] -> m [Either TimedOut ()]
cleanupTestnet mPoolNodes =
  case mPoolNodes of
    Just poolNodes -> do
      forM_ poolNodes $ \(CTN.PoolNode poolRuntime _) -> do
        -- graceful SIGTERM all nodes
        liftIO $
          cleanupProcess
            (Just (CTN.nodeStdinHandle poolRuntime), Nothing, Nothing, CTN.nodeProcessHandle poolRuntime)
      forM poolNodes $ \node ->
        -- kill signal for any node unix handles still open
        killUnixHandle $ CTN.nodeProcessHandle $ CTN.poolRuntime node
    _ ->
      return []
  where
    killUnixHandle ph = liftIO $ withProcessHandle ph $ \case
      OpenHandle pid -> do
        signalProcess sigKILL pid -- send kill signal if handle still open
        eTimeOut <- waitSecondsForProcess 60 ph -- wait 60s for process to exit
        case eTimeOut of
          Left _ -> return $ Left $ ProcessExitTimedOut 60 pid
          Right _ -> return $ Right ()
      OpenExtHandle _ _ -> return $ Right () -- do nothing on Windows
      ClosedHandle _ -> return $ Right () -- do nothing if already closed

connectToLocalNode
  :: TestEnvironmentOptions era
  -> FilePath
  -> H.Integration
      ( C.LocalNodeConnectInfo
      , C.LedgerProtocolParameters era
      , C.NetworkId
      , Maybe [CTN.PoolNode]
      )
connectToLocalNode TestnetOptions{} _ = error "TestnetOptions not supported"
connectToLocalNode LocalNodeOptions{..} tempAbsPath = do
  HE.createDirectoryIfMissing (tempAbsPath </> "utxo-keys")
  HE.createDirectoryIfMissing (tempAbsPath </> "sockets")

  HE.createFileLink (localNodeEnvDir </> "test.skey") (tempAbsPath </> "utxo-keys/utxo1.skey")
  HE.createFileLink (localNodeEnvDir </> "test.vkey") (tempAbsPath </> "utxo-keys/utxo1.vkey")
  HE.createFileLink (localNodeEnvDir </> "ipc/node.socket") (tempAbsPath </> "sockets/node.socket")

  let socketPathAbs = C.File $ tempAbsPath </> "sockets/node.socket"
      networkId = C.Testnet $ C.NetworkMagic $ fromIntegral localNodeTestnetMagic

  -- Boilerplate codecs used for protocol serialisation. The number of epochSlots is specific
  -- to each blockchain instance. This value is used by cardano mainnet/testnet and only applies
  -- to the Byron era.
  let epochSlots = C.EpochSlots 21600
      localNodeConnectInfo =
        C.LocalNodeConnectInfo
          { C.localConsensusModeParams = C.CardanoModeParams epochSlots
          , C.localNodeNetworkId = networkId
          , C.localNodeSocketPath = socketPathAbs
          }
  pparams <- Q.getProtocolParams localNodeEra localNodeConnectInfo
  pure (localNodeConnectInfo, pparams, networkId, Nothing)

{- | Start testnet with cardano-testnet or use local node that's already
  connected to a public testnet
-}
setupTestEnvironment
  :: TestEnvironmentOptions era
  -> FilePath
  -> H.Integration
      ( C.LocalNodeConnectInfo
      , C.LedgerProtocolParameters era
      , C.NetworkId
      , Maybe [CTN.PoolNode]
      )
setupTestEnvironment testnetOptions@TestnetOptions{..} tempAbsPath = do
  liftIO $
    putStrLn $
      "\nStarting local testnet in "
        ++ show testnetEra
        ++ " PV"
        ++ show testnetProtocolVersion
        ++ "..."
  startTestnet testnetOptions tempAbsPath
setupTestEnvironment localNodeOptions@LocalNodeOptions{} tempAbsPath = do
  liftIO $ putStrLn "\nConnecting to local node..."
  connectToLocalNode localNodeOptions tempAbsPath

-- | Network ID of the testnet
getNetworkId :: CTN.TestnetRuntime -> C.NetworkId
getNetworkId tn = C.Testnet $ C.NetworkMagic $ fromIntegral (CTN.testnetMagic tn)

-- | Path to a pool node's unix socket
getPoolSocketPathAbs :: (MonadTest m, MonadIO m) => FilePath -> CTN.TestnetRuntime -> m C.SocketPath
getPoolSocketPathAbs tempAbsPath tn = do
  socketPath <- IO.sprocketArgumentName <$> H.headM (CTN.poolSprockets tn)
  fp <- liftIO $ IO.canonicalizePath $ tempAbsPath </> socketPath
  H.annotate fp
  return $ C.File fp

{- | Signing key and address for wallet 1
  Handles two key types: GenesisUTxOKey and PaymentKey
-}
w1All
  :: (MonadIO m, MonadTest m)
  => FilePath
  -> C.NetworkId
  -> m (C.SigningKey C.PaymentKey, C.VerificationKey C.PaymentKey, C.Address C.ShelleyAddr)
w1All tempAbsPath networkId = do
  let w1VKeyFile = C.File $ tempAbsPath </> "utxo-keys/utxo1.vkey"
      w1SKeyFile = C.File $ tempAbsPath </> "utxo-keys/utxo1.skey"
  -- GenesisUTxOKey comes from cardano-testnet
  mGenesisVKey :: Maybe (C.VerificationKey C.GenesisUTxOKey) <-
    maybeReadAs (C.AsVerificationKey C.AsGenesisUTxOKey) w1VKeyFile
  mGenesisSKey :: Maybe (C.SigningKey C.GenesisUTxOKey) <-
    maybeReadAs (C.AsSigningKey C.AsGenesisUTxOKey) w1SKeyFile
  -- PaymentKey comes from cardano-cli (the likely type for a locally created wallet)
  mPaymentVKey :: Maybe (C.VerificationKey C.PaymentKey) <-
    maybeReadAs (C.AsVerificationKey C.AsPaymentKey) w1VKeyFile
  mPaymentSKey :: Maybe (C.SigningKey C.PaymentKey) <-
    maybeReadAs (C.AsSigningKey C.AsPaymentKey) w1SKeyFile

  let
    vKey :: C.VerificationKey C.PaymentKey = maybe (fromJust mPaymentVKey) C.castVerificationKey mGenesisVKey
    sKey :: C.SigningKey C.PaymentKey = maybe (fromJust mPaymentSKey) C.castSigningKey mGenesisSKey
    address = makeAddress (Left vKey) networkId

  return (sKey, vKey, address)

w1
  :: (MonadIO m, MonadTest m)
  => -- => Either (LocalNodeOptions era) (TestnetOptions era)
  FilePath
  -> C.NetworkId
  -> m (C.SigningKey C.PaymentKey, C.Address C.ShelleyAddr)
w1 tempAbsPath networkId = (\(sKey, _, address) -> (sKey, address)) <$> w1All tempAbsPath networkId

pool1
  :: (MonadIO m, MonadTest m)
  => FilePath
  -> m (C.SigningKey C.StakePoolKey, C.Hash C.StakeKey)
pool1 tempAbsPath = (\(sKey, _, _, stakeKeyHash, _, _) -> (sKey, stakeKeyHash)) <$> pool1All tempAbsPath

pool1Voter
  :: (MonadIO m, MonadTest m)
  => C.ConwayEraOnwards era
  -> FilePath
  -> m (Voter (EraCrypto (C.ShelleyLedgerEra era)))
pool1Voter ceo tempAbsPath = do
  (_, _, _, _, _, pool1StakePoolKeyHash) <- pool1All tempAbsPath
  return $ StakePoolVoter $ C.conwayEraOnwardsConstraints ceo pool1StakePoolKeyHash

pool1All
  :: (MonadIO m, MonadTest m)
  => FilePath
  -> m
      ( C.SigningKey C.StakePoolKey
      , C.VerificationKey C.StakePoolKey
      , C.Hash C.StakePoolKey
      , C.Hash C.StakeKey
      , C.Hash C.VrfKey
      , KeyHash 'StakePool StandardCrypto
      )
pool1All tempAbsPath = do
  let pool1SKeyFile = C.File $ tempAbsPath </> "pools/cold1.skey"
  mPool1SKey :: Maybe (C.SigningKey C.StakePoolKey) <-
    maybeReadAs (C.AsSigningKey C.AsStakePoolKey) pool1SKeyFile
  let pool1SKey = fromJust mPool1SKey

  let pool1VerificationKeyFile = C.File $ tempAbsPath </> "pools/cold1.vkey"
  mPool1VKey :: Maybe (C.VerificationKey C.StakePoolKey) <-
    maybeReadAs (C.AsVerificationKey C.AsStakePoolKey) pool1VerificationKeyFile
  let pool1VKey = fromJust mPool1VKey
      pool1VKeyHash = C.verificationKeyHash pool1VKey

  let pool1StakingRewardsFile = C.File $ tempAbsPath </> "pools/staking-reward1.vkey"
  mPool1StakingRewards :: Maybe (C.VerificationKey C.StakeKey) <-
    maybeReadAs (C.AsVerificationKey C.AsStakeKey) pool1StakingRewardsFile
  let pool1StakeKeyHash = C.verificationKeyHash (fromJust mPool1StakingRewards)

  let pool1VrfKeyFile = C.File $ tempAbsPath </> "pools/vrf1.vkey"
  mPool1VrfKey :: Maybe (C.VerificationKey C.VrfKey) <-
    maybeReadAs (C.AsVerificationKey C.AsVrfKey) pool1VrfKeyFile
  let pool1VrfKeyHash = C.verificationKeyHash (fromJust mPool1VrfKey)
      C.StakePoolKeyHash pool1StakePoolKeyHash = C.verificationKeyHash pool1VKey

  return
    (pool1SKey, pool1VKey, pool1VKeyHash, pool1StakeKeyHash, pool1VrfKeyHash, pool1StakePoolKeyHash)
