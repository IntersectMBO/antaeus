{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Spec.Builtins.Bitwise where

import Cardano.Api qualified as C
import Control.Monad.IO.Class (MonadIO)
import Data.Map qualified as Map
import Hedgehog (MonadTest)
import Helpers.Common (toShelleyBasedEra)
import Helpers.Query qualified as Q
import Helpers.Test (assert)
import Helpers.TestData (TestInfo (..), TestParams (..))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import PlutusScripts.Bitwise.V_1_1 qualified as PS_1_1

verifyBitwiseFunctionsTestInfo =
  TestInfo
    { testName = "verifyBitwiseFunctionsTest"
    , testDescription =
        "Use byteStringToInteger and integerToByteString to verify correct conversions"
    , test = verifyBitwiseFunctionsTest
    }
verifyBitwiseFunctionsTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
verifyBitwiseFunctionsTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let (tokenValues, mintWitnesses) = case era of
        C.AlonzoEra ->
          error "Alonzo era not supported"
        C.BabbageEra ->
          error "Babbage era not supported"
        C.ConwayEra ->
          ( C.valueFromList
              [ (PS_1_1.byteStringToIntegerAssetIdV3, 1)
              , (PS_1_1.integerToByteStringAssetIdV3, 2)
              , (PS_1_1.byteStringToIntegerAndBackAssetIdV3, 3)
              ]
          , Map.fromList
              [ PS_1_1.byteStringToIntegerMintWitnessV3 sbe -- TODO: insert params
              , PS_1_1.integerToByteStringMintWitnessV3 sbe -- TODO: insert params
              , PS_1_1.byteStringToIntegerAndBackMintWitnessV3 sbe "abcd" -- TOOD: also empty bs
              ]
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

  -- Build and submit transaction
  signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w1Address w1SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let expectedTxIn = Tx.txIn (Tx.txId signedTx) 0

  -- Query for txo and assert it contains newly minting tokens to prove successful use of SECP256k1 builtins
  resultTxOut <-
    Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "TN.getTxOutAtAddress"
  txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
  assert "txOut has tokens" txOutHasTokenValue

integerToByteStringBitwiseNegativeIntegerErrorTestInfo =
  TestInfo
    { testName = "integerToByteStringBitwiseNegativeIntegerErrorTest"
    , testDescription =
        "Check scripts evaluation errors when using a negative Integer"
          ++ " with integerToByteString bitwise builtin"
    , test = integerToByteStringBitwiseNegativeIntegerError
    }
integerToByteStringBitwiseNegativeIntegerErrorTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
integerToByteStringBitwiseNegativeIntegerErrorTest
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    let negIntegerParam = IntegerToByteStringParams False 0 (-1) ""
        expError = "some error"
    checkIntegerToByteStringError networkOptions negIntegerParam expError

integerToByteStringBitwiseSizeArgumentGreaterThan8192ErrorTestInfo =
  TestInfo
    { testName = "integerToByteStringBitwiseSizeArgumentGreaterThan8192ErrorTest"
    , testDescription =
        "Check scripts evaluation errors when using argument size greater than 8192"
          ++ " with integerToByteString bitwise builtin"
    , test = integerToByteStringBitwiseSizeArgumentGreaterThan8192ErrorTest
    }
integerToByteStringBitwiseSizeArgumentGreaterThan8192ErrorTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
integerToByteStringBitwiseSizeArgumentGreaterThan8192ErrorTest
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    let negIntegerParam = IntegerToByteStringParams False 8193 1234 ""
        expError = "some error"
    checkIntegerToByteStringError networkOptions negIntegerParam expError

integerToByteStringBitwiseInvalidConversionErrorTestInfo =
  TestInfo
    { testName = "integerToByteStringBitwiseInvalidConversionErrorTest"
    , testDescription =
        "Check scripts evaluation errors when using an Integer that cannot be converted to ByteString"
          ++ " with integerToByteString bitwise builtin"
    , test = integerToByteStringBitwiseInvalidConversionErrorTest
    }
integerToByteStringBitwiseInvalidConversionErrorTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
integerToByteStringBitwiseInvalidConversionErrorTest
  networkOptions
  TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    let negIntegerParam = IntegerToByteStringParams False 0 123 ""
        expError = "some error"
    checkIntegerToByteStringError networkOptions negIntegerParam expError

checkIntegerToByteStringError
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> IntegerToByteStringParams
  -> String -- expected error
  -> m (Maybe String)
checkIntegerToByteStringError networkOptions redeemer expError = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- build a transaction

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let negIntegerParam = IntegerToByteStringParams False - 1 ""
      (tokenValues, mintWitnesses) = case era of
        C.AlonzoEra ->
          error "Alonzo era not supported"
        C.BabbageEra ->
          error "Babbage era not supported"
        C.ConwayEra ->
          ( C.valueFromList [(PS_1_1.integerToByteStringAssetIdV3, 1)]
          , Map.fromList [PS_1_1.integerToByteStringMintWitnessV3 sbe emptyIntegerParam]
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

  -- Build and submit transaction
  eitherTx <-
    Tx.buildTxWithError
      era
      localNodeConnectInfo
      txBodyContent
      w1Address
      Nothing
      [C.WitnessPaymentKey w1SKey]
  annotate $ show eitherTx
  assert expError $ Tx.isTxBodyScriptExecutionError expError eitherTx
