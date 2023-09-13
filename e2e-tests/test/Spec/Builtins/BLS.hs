{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use if" #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Spec.Builtins.BLS where

import Cardano.Api qualified as C
import Data.Map qualified as Map

import Control.Monad.IO.Class (MonadIO)
import Hedgehog (MonadTest)
import Helpers.Query qualified as Q
import Helpers.Test (assert)
import Helpers.TestData (TestInfo (..), TestParams (..))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import PlutusScripts.BLS qualified as PS
import PlutusScripts.BLS.Vrf.Common qualified as VRF

verifyBlsFunctionsTestInfo =
  TestInfo
    { testName = "verifyBlsFunctionsTestInfo"
    , testDescription =
        "Uses multiple scripts to cover all builtin BLS functions."
          ++ "Only available in Plutus V3 and beyond."
    , test = verifyBlsFunctionsTest
    }
verifyBlsFunctionsTest
  :: (MonadIO m, MonadTest m)
  => Either TN.LocalNodeOptions TN.TestnetOptions
  -> TestParams
  -> m (Maybe String)
verifyBlsFunctionsTest networkOptions TestParams{..} = do
  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions
  (w1SKey, _, w1Address) <- TN.w1 networkOptions tempAbsPath networkId

  -- build a transaction

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  -- produce vrf proof for use in minting
  let vrfProofWithOutput = VRF.generateVrfProofWithOutput VRF.vrfPrivKey VRF.vrfMessage

  let
    tokenValues =
      C.valueFromList
        [ -- (PS.verifyBlsSimpleAssetIdV3, 1)
          (PS.verifyBlsVrfAssetIdV3, 2)
          -- , (PS.verifyBlsGroth16AssetIdV3, 3)
          -- , (PS.verifyBlsSigG1AssetIdV3, 4)
          -- , (PS.verifyBlsSigG2AssetIdV3, 5)
          -- , (PS.verifyBlsAggregateSigSingleKeyG1AssetIdV3, 6)
          -- , (PS.verifyBlsAggregateSigMultiKeyG1AssetIdV3, 7)
          -- , (PS.verifyBlsSchnorrG1AssetIdV3, 8)
          -- , (PS.verifyBlsSchnorrG2AssetIdV3, 9)
        ]
    txOut = Tx.txOut era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address
    mintWitnesses =
      Map.fromList
        [ -- PS.verifyBlsSimpleMintWitnessV3 era
          PS.verifyBlsVrfMintWitnessV3 era vrfProofWithOutput
          -- , PS.verifyBlsGroth16MintWitnessV3 era
          -- , PS.verifyBlsSigG1MintWitnessV3 era
          -- , PS.verifyBlsSigG2MintWitnessV3 era
          -- , PS.verifyBlsAggregateSigSingleKeyG1MintWitnessV3 era
          -- , PS.verifyBlsAggregateSigMultiKeyG2MintWitnessV3 era
          -- , PS.verifyBlsSchnorrG1MintWitnessV3 era
          -- , PS.verifyBlsSchnorrG2MintWitnessV3 era
        ]
    collateral = Tx.txInsCollateral era [txIn]
    txBodyContent =
      (Tx.emptyTxBodyContent era pparams)
        { C.txIns = Tx.pubkeyTxIns [txIn]
        , C.txInsCollateral = collateral
        , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
        , C.txOuts = [txOut]
        }

  -- Build and submit transaction
  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w1Address w1SKey
  Tx.submitTx era localNodeConnectInfo signedTx
  let expectedTxIn = Tx.txIn (Tx.txId signedTx) 0

  -- Query for txo and assert it contains newly minting tokens to prove successful use of BLS builtins
  resultTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "TN.getTxOutAtAddress"
  txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
  assert "txOut has tokens" txOutHasTokenValue
