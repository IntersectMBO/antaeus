{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module CardanoTestnet
  ( TestnetOptions(..)
  , defaultTestnetOptions

  , Era(..)
  , TestnetRuntime (..)
  , PaymentKeyPair(..)
  , testnet
  ) where

import Prelude

import Control.Monad
import Data.Aeson (encode, object, toJSON, (.=))
import Data.HashMap.Lazy qualified as HM
import Data.List qualified as L
import Data.Time.Clock qualified as DTC
import Hedgehog.Extras.Stock.Aeson qualified as J
import Hedgehog.Extras.Stock.OS qualified as OS
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H
import System.FilePath.Posix ((</>))
import System.Info qualified as OS

import Control.Monad.Catch (MonadCatch)
import Data.Aeson.Types (Value)
import Data.Time.Clock (UTCTime)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog.Internal.Property (MonadTest)

import Cardano.Api qualified as C
import Cardano.Testnet qualified as CTN
import Control.Monad.IO.Class (MonadIO)
import Hedgehog.Extras.Stock (showUTCTimeSeconds)
import System.FilePath qualified as FP
import System.Random qualified as IO
import Testnet.Util.Assert qualified as H
import Testnet.Util.Process (execCli_)
import Testnet.Util.Runtime (Delegator (..), NodeLoggingFormat (..), PaymentKeyPair (..), PoolNode (PoolNode),
                             PoolNodeKeys (..), StakingKeyPair (..), TestnetRuntime (..), startNode)

---- cardano-testnet/src/Testnet/Options.hs (Testnet.Options)

data Era = Alonzo | Babbage | Conway deriving (Eq, Show)

data TestnetOptions = TestnetOptions
  { era               :: C.AnyCardanoEra
  , protocolVersion   :: Int
  , numSpoNodes       :: Int
  , slotDuration      :: Int
  , slotLength        :: Double
  , activeSlotsCoeff  :: Double
  , securityParam     :: Int
  , totalBalance      :: Int
  , nodeLoggingFormat :: NodeLoggingFormat
  } deriving (Eq, Show)

defaultTestnetOptions :: TestnetOptions
defaultTestnetOptions = TestnetOptions
  { era = C.AnyCardanoEra C.BabbageEra
  , protocolVersion = 8
  , numSpoNodes = 3
  , slotDuration = 1000
  , slotLength = 0.2
  , activeSlotsCoeff = 0.1 -- higher value (e.g. 0.9) prevents long waits for slot leader but could be the cause of more rollbacks/forks
  , securityParam = 10
  , totalBalance = 10020000000
  , nodeLoggingFormat = NodeLoggingFormatAsJson
  }

---- from cardano-testnet/src/Testnet/Commands/Genesis.hs (Testnet.Commands.Genesis)

createByronGenesis
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => Int
  -> UTCTime
  -> TestnetOptions
  -> String
  -> String
  -> m ()
createByronGenesis testnetMagic startTime testnetOptions pParamFp genOutputDir =
  withFrozenCallStack $ execCli_
    [ "byron", "genesis", "genesis"
    , "--protocol-magic", show testnetMagic
    , "--start-time", showUTCTimeSeconds startTime
    , "--k", show (securityParam testnetOptions)
    , "--n-poor-addresses", "0"
    , "--n-delegate-addresses", show @Int (numSpoNodes testnetOptions)
    , "--total-balance", show @Int (totalBalance testnetOptions)
    , "--delegate-share", "1"
    , "--avvm-entry-count", "0"
    , "--avvm-entry-balance", "0"
    , "--protocol-parameters-file", pParamFp
    , "--genesis-output-dir", genOutputDir
    ]

defaultByronGenesisJsonValue :: Value
defaultByronGenesisJsonValue =
  object
    [ "heavyDelThd" .= toJSON @String "300000000000"
    , "maxBlockSize" .= toJSON @String "2000000"
    , "maxTxSize" .= toJSON @String "4096"
    , "maxHeaderSize" .= toJSON @String "2000000"
    , "maxProposalSize" .= toJSON @String "700"
    , "mpcThd" .= toJSON @String "20000000000000"
    , "scriptVersion" .= toJSON @Int 0
    , "slotDuration" .= toJSON @String "1000"
    , "softforkRule" .= object
      [ "initThd" .= toJSON @String "900000000000000"
      , "minThd" .= toJSON @String "600000000000000"
      , "thdDecrement" .= toJSON @String "50000000000000"
      ]
    , "txFeePolicy" .= object
      [ "multiplier" .= toJSON @String "43946000000"
      , "summand" .= toJSON @String "155381000000000"
      ]
    , "unlockStakeEpoch" .= toJSON @String "18446744073709551615"
    , "updateImplicit" .= toJSON @String "10000"
    , "updateProposalThd" .= toJSON @String "100000000000000"
    , "updateVoteThd" .= toJSON @String "1000000000000"
    ]

----


{- HLINT ignore "Redundant flip" -}

-- | For an unknown reason, CLI commands are a lot slower on Windows than on Linux and
-- MacOS.  We need to allow a lot more time to set up a testnet.
startTimeOffsetSeconds :: DTC.NominalDiffTime
startTimeOffsetSeconds = if OS.isWin32 then 90 else 15

testnet :: TestnetOptions -> CTN.Conf -> FilePath -> H.Integration TestnetRuntime
testnet testnetOptions CTN.Conf{..} projectBasePath = do
  H.createDirectoryIfMissing (tempAbsPath </> "logs")

  H.lbsWriteFile (tempAbsPath </> "byron.genesis.spec.json")
    . encode $ defaultByronGenesisJsonValue

  void $ H.note OS.os
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime startTimeOffsetSeconds currentTime

  createByronGenesis
    testnetMagic
    startTime
    testnetOptions
    (tempAbsPath </> "byron.genesis.spec.json")
    (tempAbsPath </> "byron-gen-command")


  -- Because in Babbage the overlay schedule and decentralization parameter
  -- are deprecated, we must use the "create-staked" cli command to create
  -- SPOs in the ShelleyGenesis

  alonzoBabbageTestGenesisJsonSourceFile <- H.noteShow $ projectBasePath </> "scripts/babbage/alonzo-babbage-test-genesis.json"
  alonzoBabbageTestGenesisJsonTargetFile <- H.noteShow $ tempAbsPath </> "genesis.alonzo.spec.json"
  H.copyFile alonzoBabbageTestGenesisJsonSourceFile alonzoBabbageTestGenesisJsonTargetFile

  conwayBabbageTestGenesisJsonSourceFile <- H.noteShow $ projectBasePath </> "scripts/babbage/conway-babbage-test-genesis.json"
  conwayBabbageTestGenesisJsonTargetFile <- H.noteShow $ tempAbsPath </> "genesis.conway.spec.json"
  H.copyFile conwayBabbageTestGenesisJsonSourceFile conwayBabbageTestGenesisJsonTargetFile

  configurationFile <- H.noteShow $ tempAbsPath </> "configuration.yaml"

  H.readFile configurationTemplate >>= H.writeFile configurationFile

  H.rewriteYamlFile (tempAbsPath </> "configuration.yaml") . J.rewriteObject
    $ HM.delete "GenesisFile"
    . HM.insert "Protocol" (toJSON @String "Cardano")
    . HM.insert "PBftSignatureThreshold" (toJSON @Double 0.6)
    . HM.insert "minSeverity" (toJSON @String "Debug")
    . HM.insert "ByronGenesisFile" (toJSON @String "genesis/byron/genesis.json")
    . HM.insert "ShelleyGenesisFile" (toJSON @String "genesis/shelley/genesis.json")
    . HM.insert "AlonzoGenesisFile" (toJSON @String "genesis/shelley/genesis.alonzo.json")
    . HM.insert "ConwayGenesisFile" (toJSON @String "genesis/shelley/genesis.conway.json")
    . HM.insert "RequiresNetworkMagic" (toJSON @String "RequiresMagic")
    . HM.insert "LastKnownBlockVersion-Major" (toJSON @Int 6)
    . HM.insert "LastKnownBlockVersion-Minor" (toJSON @Int 0)
    . HM.insert "TestShelleyHardForkAtEpoch" (toJSON @Int 0)
    . HM.insert "TestAllegraHardForkAtEpoch" (toJSON @Int 0)
    . HM.insert "TestMaryHardForkAtEpoch" (toJSON @Int 0)
    . HM.insert "TestAlonzoHardForkAtEpoch" (toJSON @Int 0)
    . HM.insert "TestBabbageHardForkAtEpoch" (toJSON @Int 0)
    . HM.insert "ExperimentalHardForksEnabled" (toJSON True)
    . flip HM.alter "setupScribes"
        ( fmap
          . J.rewriteArrayElements
            . J.rewriteObject
              . HM.insert "scFormat"
                $ case nodeLoggingFormat testnetOptions of
                    NodeLoggingFormatAsJson -> "ScJson"
                    NodeLoggingFormatAsText -> "ScText")

  let numPoolNodes = 3 :: Int

  execCli_
    [ "genesis", "create-staked"
    , "--genesis-dir", tempAbsPath
    , "--testnet-magic", show @Int testnetMagic
    , "--gen-pools", show @Int 3
    , "--supply", "1000000000000"
    , "--supply-delegated", "1000000000000"
    , "--gen-stake-delegs", "3"
    , "--gen-utxo-keys", "3"
    ]

  poolKeys <- H.noteShow $ flip fmap [1..numPoolNodes] $ \n ->
    PoolNodeKeys
      { poolNodeKeysColdVkey = tempAbsPath </> "pools" </> "cold" <> show n <> ".vkey"
      , poolNodeKeysColdSkey = tempAbsPath </> "pools" </> "cold" <> show n <> ".skey"
      , poolNodeKeysVrfVkey = tempAbsPath </> "node-spo" <> show n </> "vrf.vkey"
      , poolNodeKeysVrfSkey = tempAbsPath </> "node-spo" <> show n </> "vrf.skey"
      , poolNodeKeysStakingVkey = tempAbsPath </> "pools" </> "staking-reward" <> show n <> ".vkey"
      , poolNodeKeysStakingSkey = tempAbsPath </> "pools" </> "staking-reward" <> show n <> ".skey"
      }

  wallets <- forM [1..3] $ \idx -> do
    pure $ PaymentKeyPair
      { paymentSKey = tempAbsPath </> "utxo-keys/utxo" <> show @Int idx <> ".skey"
      , paymentVKey = tempAbsPath </> "utxo-keys/utxo" <> show @Int idx <> ".vkey"
      }

  delegators <- forM [1..3] $ \idx -> do
    pure $ Delegator
      { paymentKeyPair = PaymentKeyPair
        { paymentSKey = tempAbsPath </> "stake-delegator-keys/payment" <> show @Int idx <> ".skey"
        , paymentVKey = tempAbsPath </> "stake-delegator-keys/payment" <> show @Int idx <> ".vkey"
        }
      , stakingKeyPair = StakingKeyPair
        { stakingSKey = tempAbsPath </> "stake-delegator-keys/staking" <> show @Int idx <> ".skey"
        , stakingVKey = tempAbsPath </> "stake-delegator-keys/staking" <> show @Int idx <> ".vkey"
        }
      }

  let spoNodes :: [String] = ("node-spo" <>) . show <$> [1 .. numSpoNodes testnetOptions]

  -- Create the node directories

  forM_ spoNodes $ \node -> do
    H.createDirectoryIfMissing (tempAbsPath </> node)

  -- Here we move all of the keys etc generated by create-staked
  -- for the nodes to use

  -- Move all genesis related files

  H.createDirectoryIfMissing $ tempAbsPath </> "genesis/byron"
  H.createDirectoryIfMissing $ tempAbsPath </> "genesis/shelley"

  files <- H.listDirectory tempAbsPath
  forM_ files $ \file -> do
    H.note file

  H.renameFile (tempAbsPath </> "byron-gen-command/genesis.json") (tempAbsPath </> "genesis/byron/genesis.json")
  H.renameFile (tempAbsPath </> "genesis.alonzo.json") (tempAbsPath </> "genesis/shelley/genesis.alonzo.json")
  H.renameFile (tempAbsPath </> "genesis.conway.json") (tempAbsPath </> "genesis/shelley/genesis.conway.json")
  H.renameFile (tempAbsPath </> "genesis.json") (tempAbsPath </> "genesis/shelley/genesis.json")

  H.rewriteJsonFile (tempAbsPath </> "genesis/byron/genesis.json") $ J.rewriteObject
    $ flip HM.adjust "protocolConsts"
      ( J.rewriteObject ( HM.insert "protocolMagic" (toJSON @Int testnetMagic)))

  H.rewriteJsonFile (tempAbsPath </> "genesis/shelley/genesis.json") $ J.rewriteObject
    ( HM.insert "slotLength"             (toJSON @Double 0.1)
    . HM.insert "activeSlotsCoeff"       (toJSON @Double 0.1)
    . HM.insert "securityParam"          (toJSON @Int 10)     -- TODO: USE config parameter
    . HM.insert "epochLength"            (toJSON @Int 500)
    . HM.insert "maxLovelaceSupply"      (toJSON @Int 1000000000000)
    . HM.insert "minFeeA"                (toJSON @Int 44)
    . HM.insert "minFeeB"                (toJSON @Int 155381)
    . HM.insert "minUTxOValue"           (toJSON @Int 1000000)
    . HM.insert "decentralisationParam"  (toJSON @Double 0.7)
    . flip HM.adjust "protocolParams"
      ( J.rewriteObject
        ( flip HM.adjust "protocolVersion"
          ( J.rewriteObject ( HM.insert "major" (toJSON @Int $ protocolVersion testnetOptions)))
        )
      )
    . HM.insert "rho"                    (toJSON @Double 0.1)
    . HM.insert "tau"                    (toJSON @Double 0.1)
    . HM.insert "updateQuorum"           (toJSON @Int 2)
    )

  H.renameFile (tempAbsPath </> "pools/vrf1.skey") (tempAbsPath </> "node-spo1/vrf.skey")
  H.renameFile (tempAbsPath </> "pools/vrf2.skey") (tempAbsPath </> "node-spo2/vrf.skey")
  H.renameFile (tempAbsPath </> "pools/vrf3.skey") (tempAbsPath </> "node-spo3/vrf.skey")

  H.renameFile (tempAbsPath </> "pools/opcert1.cert") (tempAbsPath </> "node-spo1/opcert.cert")
  H.renameFile (tempAbsPath </> "pools/opcert2.cert") (tempAbsPath </> "node-spo2/opcert.cert")
  H.renameFile (tempAbsPath </> "pools/opcert3.cert") (tempAbsPath </> "node-spo3/opcert.cert")

  H.renameFile (tempAbsPath </> "pools/kes1.skey") (tempAbsPath </> "node-spo1/kes.skey")
  H.renameFile (tempAbsPath </> "pools/kes2.skey") (tempAbsPath </> "node-spo2/kes.skey")
  H.renameFile (tempAbsPath </> "pools/kes3.skey") (tempAbsPath </> "node-spo3/kes.skey")

  -- Byron related

  H.renameFile (tempAbsPath </> "byron-gen-command/delegate-keys.000.key") (tempAbsPath </> "node-spo1/byron-delegate.key")
  H.renameFile (tempAbsPath </> "byron-gen-command/delegate-keys.001.key") (tempAbsPath </> "node-spo2/byron-delegate.key")
  H.renameFile (tempAbsPath </> "byron-gen-command/delegate-keys.002.key") (tempAbsPath </> "node-spo3/byron-delegate.key")

  H.renameFile (tempAbsPath </> "byron-gen-command/delegation-cert.000.json") (tempAbsPath </> "node-spo1/byron-delegation.cert")
  H.renameFile (tempAbsPath </> "byron-gen-command/delegation-cert.001.json") (tempAbsPath </> "node-spo2/byron-delegation.cert")
  H.renameFile (tempAbsPath </> "byron-gen-command/delegation-cert.002.json") (tempAbsPath </> "node-spo3/byron-delegation.cert")

  H.writeFile (tempAbsPath </> "node-spo1/port") "3001"
  H.writeFile (tempAbsPath </> "node-spo2/port") "3002"
  H.writeFile (tempAbsPath </> "node-spo3/port") "3003"


  -- Make topology files
  -- TODO generalise this over the N BFT nodes and pool nodes

  H.lbsWriteFile (tempAbsPath </> "node-spo1/topology.json") $ encode $
    object
    [ "Producers" .= toJSON
      [ object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int 3002
        , "valency" .= toJSON @Int 1
        ]
      , object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int 3003
        , "valency" .= toJSON @Int 1
        ]
      ]
    ]

  H.lbsWriteFile (tempAbsPath </> "node-spo2/topology.json") $ encode $
    object
    [ "Producers" .= toJSON
      [ object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int 3001
        , "valency" .= toJSON @Int 1
        ]
      , object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int 3003
        , "valency" .= toJSON @Int 1
        ]
      ]
    ]

  H.lbsWriteFile (tempAbsPath </> "node-spo3/topology.json") $ encode $
    object
    [ "Producers" .= toJSON
      [ object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int 3001
        , "valency" .= toJSON @Int 1
        ]
      , object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int 3002
        , "valency" .= toJSON @Int 1
        ]
      ]
    ]

  poolNodes <- forM (L.zip spoNodes poolKeys) $ \(node,key) -> do
    runtime <- startNode tempBaseAbsPath tempAbsPath logDir socketDir node
        [ "run"
        , "--config", tempAbsPath </> "configuration.yaml"
        , "--topology", tempAbsPath </> node </> "topology.json"
        , "--database-path", tempAbsPath </> node </> "db"
        , "--shelley-kes-key", tempAbsPath </> node </> "kes.skey"
        , "--shelley-vrf-key", tempAbsPath </> node </> "vrf.skey"
        , "--byron-delegation-certificate", tempAbsPath </> node </> "byron-delegation.cert"
        , "--byron-signing-key", tempAbsPath </> node </> "byron-delegate.key"
        , "--shelley-operational-certificate", tempAbsPath </> node </> "opcert.cert"
        ]
    return $ PoolNode runtime key

  now <- H.noteShowIO DTC.getCurrentTime
  deadline <- H.noteShow $ DTC.addUTCTime 90 now

  forM_ spoNodes $ \node -> do
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    H.assertChainExtended deadline (nodeLoggingFormat testnetOptions) nodeStdoutFile

  H.noteShowIO_ DTC.getCurrentTime

  forM_ wallets $ \wallet -> do
    H.cat $ paymentSKey wallet
    H.cat $ paymentVKey wallet

  return TestnetRuntime
    { configurationFile
    , shelleyGenesisFile = tempAbsPath </> "genesis/shelley/genesis.json"
    , testnetMagic
    , poolNodes
    , wallets = wallets
    , bftNodes = []
    , delegators = delegators
    }
