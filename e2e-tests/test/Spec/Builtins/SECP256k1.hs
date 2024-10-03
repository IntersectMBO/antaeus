{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use if" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Spec.Builtins.SECP256k1 where

import Cardano.Api qualified as C
import Control.Monad.IO.Class (MonadIO)
import Data.Map qualified as Map
import Hedgehog (MonadTest)
import Hedgehog.Internal.Property (annotate)
import Helpers.Common (toShelleyBasedEra)
import Helpers.Query qualified as Q
import Helpers.Test (assert)
import Helpers.TestData (TestInfo (..), TestParams (..))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import Helpers.Utils qualified as U
import PlutusLedgerApi.Common qualified as C
import PlutusScripts.SECP256k1.V_1_0 qualified as PS_1_0
import PlutusScripts.SECP256k1.V_1_1 qualified as PS_1_1

verifySchnorrAndEcdsaTestInfo =
  TestInfo
    { testName = "verifySchnorrAndEcdsaTest"
    , testDescription =
        "SECP256k1 builtin verify functions verifySchnorrSecp256k1Signature and "
          ++ "verifyEcdsaSecp256k1Signature can only be used to mint in Babbage era protocol version 8 "
          ++ "and beyond."
    , test = verifySchnorrAndEcdsaTest
    }

verifySchnorrAndEcdsaTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
verifySchnorrAndEcdsaTest networkOptions testParams = do
  let TestParams
        { localNodeConnectInfo = conn
        , pparams
        , networkId
        , tempAbsPath
        } = testParams
  era <- TN.eraFromOptionsM networkOptions
  pv <- TN.pvFromOptions networkOptions
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction

  txIn <- Q.adaOnlyTxInAtAddress era conn w1Address

  let (tokenValues, mintWitnesses, plutusVersion) = case era of
        C.AlonzoEra ->
          ( C.valueFromList
              [ (PS_1_0.verifySchnorrAssetIdV1, 4)
              , (PS_1_0.verifyEcdsaAssetIdV1, 2)
              ]
          , Map.fromList
              [ PS_1_0.verifySchnorrMintWitnessV1 sbe
              , PS_1_0.verifyEcdsaMintWitnessV1 sbe
              ]
          , show C.PlutusV1
          )
        C.BabbageEra ->
          ( C.valueFromList
              [ (PS_1_0.verifySchnorrAssetIdV2, 4)
              , (PS_1_0.verifyEcdsaAssetIdV2, 2)
              ]
          , Map.fromList
              [ PS_1_0.verifySchnorrMintWitnessV2 sbe
              , PS_1_0.verifyEcdsaMintWitnessV2 sbe
              ]
          , show C.PlutusV2
          )
        C.ConwayEra ->
          ( C.valueFromList
              [ (PS_1_0.verifySchnorrAssetIdV2, 4)
              , (PS_1_1.verifySchnorrAssetIdV3, 3)
              , -- ECDSA
                (PS_1_0.verifyEcdsaAssetIdV2, 2)
              , (PS_1_1.verifyEcdsaAssetIdV3, 5)
              ]
          , Map.fromList
              [ PS_1_0.verifySchnorrMintWitnessV2 sbe
              , PS_1_1.verifySchnorrMintWitnessV3 sbe
              , -- ECDSA
                PS_1_0.verifyEcdsaMintWitnessV2 sbe
              , PS_1_1.verifyEcdsaMintWitnessV3 sbe
              ]
          , show C.PlutusV3
          )
      txOut = Tx.txOut era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address
      collateral = Tx.txInsCollateral era [txIn]
      txBodyContent =
        (Tx.emptyTxBodyContent sbe pparams)
          { C.txIns = Tx.pubkeyTxIns [txIn]
          , C.txInsCollateral = collateral
          , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
          , C.txOuts = [txOut]
          }

  case pv < 8 of
    True -> do
      -- Assert that "forbidden" error occurs when attempting to use either SECP256k1 builtin
      eitherTx <-
        Tx.buildTxWithError
          era
          conn
          txBodyContent
          w1Address
          Nothing
          [C.WitnessPaymentKey w1SKey]
      annotate $ show eitherTx
      let expErrorSchnorr =
            "Builtin function VerifySchnorrSecp256k1Signature is not available in language "
              ++ plutusVersion
              ++ " at and protocol version "
              ++ show pv
          expErrorEcdsa =
            "Builtin function VerifyEcdsaSecp256k1Signature is not available in language "
              ++ plutusVersion
              ++ " at and protocol version "
              ++ show pv
      a1 <-
        assert expErrorSchnorr $
          Tx.isTxBodyScriptExecutionError expErrorSchnorr eitherTx
      a2 <-
        assert expErrorEcdsa $
          Tx.isTxBodyScriptExecutionError expErrorEcdsa eitherTx
      U.concatMaybes [a1, a2]
    False -> do
      -- Build and submit transaction
      signedTx <- Tx.buildTx era conn txBodyContent w1Address w1SKey
      Tx.submitTx sbe conn signedTx
      let expectedTxIn = Tx.txIn (Tx.txId signedTx) 0

      -- Query for txo and assert it contains newly minting tokens to prove successful use of SECP256k1 builtins
      resultTxOut <-
        Q.getTxOutAtAddress era conn w1Address expectedTxIn "TN.getTxOutAtAddress"
      txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
      assert "txOut has tokens" txOutHasTokenValue
