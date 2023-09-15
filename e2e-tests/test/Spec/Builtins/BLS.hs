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

  -- produce vrf proof for use in minting
  let vrfProofWithOutput = VRF.generateVrfProofWithOutput VRF.vrfPrivKey VRF.vrfMessage

  -- due to the max tx size constraint we must split use of BLS builtins across two transactions

  -- build and submit tx1

  tx1In <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    tx1TokenValues =
      C.valueFromList
        [ (PS.verifyBlsSimpleAssetIdV3, 1)
        , (PS.verifyBlsVrfAssetIdV3, 2)
        , (PS.verifyBlsGroth16AssetIdV3, 3)
        , (PS.verifyBlsSigG1AssetIdV3, 4)
        , (PS.verifyBlsSigG2AssetIdV3, 5)
        ]
    tx1Out = Tx.txOut era (C.lovelaceToValue 3_100_000 <> tx1TokenValues) w1Address
    tx1MintWitnesses =
      Map.fromList
        [ PS.verifyBlsSimpleMintWitnessV3 era
        , PS.verifyBlsVrfMintWitnessV3 era vrfProofWithOutput
        , PS.verifyBlsGroth16MintWitnessV3 era
        , PS.verifyBlsSigG1MintWitnessV3 era
        , PS.verifyBlsSigG2MintWitnessV3 era
        ]
    tx1Collateral = Tx.txInsCollateral era [tx1In]
    tx1BodyContent =
      (Tx.emptyTxBodyContent era pparams)
        { C.txIns = Tx.pubkeyTxIns [tx1In]
        , C.txInsCollateral = tx1Collateral
        , C.txMintValue = Tx.txMintValue era tx1TokenValues tx1MintWitnesses
        , C.txOuts = [tx1Out]
        }

  signedTx1 <- Tx.buildTx era localNodeConnectInfo tx1BodyContent w1Address w1SKey
  Tx.submitTx era localNodeConnectInfo signedTx1
  let expectedTxIn1 = Tx.txIn (Tx.txId signedTx1) 0

  -- Query for txo and whether it contains newly minting tokens to prove successful use of BLS builtins
  resultTxOut1 <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn1 "TN.getTxOutAtAddress"
  tx1OutHasTokenValue <- Q.txOutHasValue resultTxOut1 tx1TokenValues

  -- build and submit tx1

  tx2In <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    tx2TokenValues =
      C.valueFromList
        [ (PS.verifyBlsAggregateSigSingleKeyG1AssetIdV3, 6)
        , (PS.verifyBlsAggregateSigMultiKeyG1AssetIdV3, 7)
        , (PS.verifyBlsSchnorrG1AssetIdV3, 8)
        , (PS.verifyBlsSchnorrG2AssetIdV3, 9)
        ]
    tx2Out = Tx.txOut era (C.lovelaceToValue 3_200_000 <> tx2TokenValues) w1Address
    tx2MintWitnesses =
      Map.fromList
        [ PS.verifyBlsAggregateSigSingleKeyG1MintWitnessV3 era
        , PS.verifyBlsAggregateSigMultiKeyG2MintWitnessV3 era
        , PS.verifyBlsSchnorrG1MintWitnessV3 era
        , PS.verifyBlsSchnorrG2MintWitnessV3 era
        ]
    tx2Collateral = Tx.txInsCollateral era [tx2In]
    tx2BodyContent =
      (Tx.emptyTxBodyContent era pparams)
        { C.txIns = Tx.pubkeyTxIns [tx2In]
        , C.txInsCollateral = tx2Collateral
        , C.txMintValue = Tx.txMintValue era tx2TokenValues tx2MintWitnesses
        , C.txOuts = [tx2Out]
        }

  signedTx2 <- Tx.buildTx era localNodeConnectInfo tx2BodyContent w1Address w1SKey
  Tx.submitTx era localNodeConnectInfo signedTx2
  let expectedTxIn2 = Tx.txIn (Tx.txId signedTx2) 0

  -- Query for txo and whether it contains newly minting tokens to prove successful use of BLS builtins
  resultTxOut2 <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn2 "TN.getTxOutAtAddress"
  tx2OutHasTokenValue <- Q.txOutHasValue resultTxOut2 tx2TokenValues

  -- assert both transaction outputs contain expected token values mitned by the use of all BLS builtins
  assert "txOuts have tokens" (tx1OutHasTokenValue && tx2OutHasTokenValue)
