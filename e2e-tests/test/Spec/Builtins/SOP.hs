{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Spec.Builtins.SOP where

import Cardano.Api qualified as C
import Control.Monad.IO.Class (MonadIO)
import Data.Map qualified as Map
import GHC.IsList (fromList)
import Hedgehog (MonadTest)
import Helpers.Common (toShelleyBasedEra)
import Helpers.Query qualified as Q
import Helpers.Test (assert)
import Helpers.TestData (TestInfo (..), TestParams (..))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import PlutusScripts.SOP.Common qualified as SOP
import PlutusScripts.SOP.V_1_1 qualified as SOP_1_1

verifySopTestInfo =
  TestInfo
    { testName = "verifySopTest"
    , testDescription =
        "Sums-of-products optimization can be used in Plutus V3 scripts to mint."
    , test = verifySopTest
    }

verifySopTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
verifySopTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
  let sbe = toShelleyBasedEra era

  -- Only Plutus V3 supports natively compiled SOPs. Therefore, run only in Conway+
  case era of
    C.ConwayEra -> do
      txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

      let
        tokenValues = fromList [(SOP_1_1.checkSopAssetIdV3, 5)]
        mintWitnesses = Map.fromList [SOP_1_1.checkSopMintWitnessV3 sbe SOP.sopRedeemer3]
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

      -- Query for txo and assert it contains newly minting tokens to prove successful use of SOP
      resultTxOut <-
        Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "TN.getTxOutAtAddress"
      txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
      assert "txOut has SOP tokens" txOutHasTokenValue
    _ ->
      assert "SOP feature is only applicable starting from Conway era" True
