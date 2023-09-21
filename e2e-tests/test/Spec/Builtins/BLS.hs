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

import Cardano.Api.Shelley qualified as C
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor ((<&>))
import Hedgehog (MonadTest)
import Helpers.Query qualified as Q
import Helpers.Test (assert)
import Helpers.TestData (TestInfo (..), TestParams (..))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import PlutusScripts.BLS qualified as PS

txIxToQuantity :: C.TxIx -> C.Quantity
txIxToQuantity (C.TxIx wordValue) = fromIntegral wordValue

buildAndSubmit
  :: (MonadIO m, MonadTest m, C.IsCardanoEra era)
  => C.CardanoEra era
  -> C.LocalNodeConnectInfo C.CardanoMode
  -> C.LedgerProtocolParameters era
  -> C.TxIn
  -> C.TxInsCollateral era
  -> C.Address C.ShelleyAddr
  -> C.SigningKey C.PaymentKey
  -> C.AssetId
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
  -> m (C.Value, C.TxIn)
buildAndSubmit era lnci pparams txIn@(C.TxIn _id ix) collateral address skey assetId mintWitness = do
  let
    -- use unique asset quantity for each script to assist debugging
    assetQuantity = txIxToQuantity ix + 1
    txTokenValue = C.valueFromList [(assetId, assetQuantity)]
    txMintWitness = Map.fromList [mintWitness]
    -- use unique lovelace value for each script to assist debugging
    outputLovelaceValue = C.lovelaceToValue $ 3_000_000 + (C.quantityToLovelace assetQuantity * 100_000)
    txOut = Tx.txOut era (outputLovelaceValue <> txTokenValue) address
    txBodyContent =
      (Tx.emptyTxBodyContent pparams)
        { C.txIns = Tx.pubkeyTxIns [txIn]
        , C.txInsCollateral = collateral
        , C.txMintValue = Tx.txMintValue era txTokenValue txMintWitness
        , C.txOuts = [txOut]
        }
  signedTx <- Tx.buildTx era lnci txBodyContent address skey
  Tx.submitTx era lnci signedTx
  return (txTokenValue, Tx.txIn (Tx.txId signedTx) 0)

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
  => Either (TN.LocalNodeOptions era) (TN.TestnetOptions era)
  -> TestParams era
  -> m (Maybe String)
verifyBlsFunctionsTest networkOptions TestParams{..} = do
  era <- TN.eraFromOptions networkOptions
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
  let numberOfBlsScripts = 9

  -- produce an input for each script to run in its own transaction

  tx1In <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
  let
    tx1Outs = replicate numberOfBlsScripts (Tx.txOut era (C.lovelaceToValue 10_000_000) w1Address)
    tx1BodyContent =
      (Tx.emptyTxBodyContent pparams)
        { C.txIns = Tx.pubkeyTxIns [tx1In]
        , C.txOuts = tx1Outs
        }
  signedTx1 <- Tx.buildTx era localNodeConnectInfo tx1BodyContent w1Address w1SKey
  Tx.submitTx era localNodeConnectInfo signedTx1
  let expectedTxIns = [0 .. numberOfBlsScripts - 1] <&> Tx.txIn (Tx.txId signedTx1)
  Q.waitForTxInAtAddress
    era
    localNodeConnectInfo
    w1Address
    (head expectedTxIns)
    "waitForTxInAtAddress"

  -- build and submit a transaction for each BLS script
  let
    tx2Collateral = Tx.txInsCollateral era [Tx.txIn (Tx.txId signedTx1) numberOfBlsScripts]
    alles =
      [ (1, PS.verifyBlsSimpleAssetIdV3, PS.verifyBlsSimpleMintWitnessV3 era)
      , (2, PS.verifyBlsVrfAssetIdV3, PS.verifyBlsVrfMintWitnessV3 era)
      , (3, PS.verifyBlsGroth16AssetIdV3, PS.verifyBlsGroth16MintWitnessV3 era)
      , (4, PS.verifyBlsSigG1AssetIdV3, PS.verifyBlsSigG1MintWitnessV3 era)
      , (5, PS.verifyBlsSigG2AssetIdV3, PS.verifyBlsSigG2MintWitnessV3 era)
      ,
        ( 6
        , PS.verifyBlsAggregateSigSingleKeyG1AssetIdV3
        , PS.verifyBlsAggregateSigSingleKeyG1MintWitnessV3 era
        )
      ,
        ( 7
        , PS.verifyBlsAggregateSigMultiKeyG2AssetIdV3
        , PS.verifyBlsAggregateSigMultiKeyG2MintWitnessV3 era
        )
      , (8, PS.verifyBlsSchnorrG1AssetIdV3, PS.verifyBlsSchnorrG1MintWitnessV3 era)
      , (9, PS.verifyBlsSchnorrG2AssetIdV3, PS.verifyBlsSchnorrG2MintWitnessV3 era)
      ]
  tokenValuesAndTxIns <- forM alles $ \(assetCount, assetId, mintWitness) ->
    buildAndSubmit
      era
      localNodeConnectInfo
      pparams
      (expectedTxIns !! (assetCount - 1))
      tx2Collateral
      w1Address
      w1SKey
      assetId
      mintWitness

  -- check that all scripts have minted the expected tokens
  resultTxOuts <- forM tokenValuesAndTxIns $ \(tokenValue, txIn) -> do
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address txIn "TN.getTxOutAtAddress"
    Q.txOutHasValue resultTxOut tokenValue

  assert "all txOuts have expected bls tokens" (and resultTxOuts)
