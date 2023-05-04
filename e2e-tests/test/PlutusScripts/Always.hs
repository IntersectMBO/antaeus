{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- Not using all CardanoEra

module PlutusScripts.Always (
    alwaysSucceedPolicyScriptV1
  , alwaysSucceedPolicyScriptV2
  , alwaysSucceedAssetIdV1
  , alwaysSucceedAssetIdV2
  , alwaysSucceedMintWitnessV1
  , alwaysSucceedMintWitnessV1'
  , alwaysSucceedMintWitnessV2
  , alwaysSucceedMintWitnessV2'
  , alwaysSucceedPolicyTxInfoRedeemerV2

  , alwaysSucceedSpendScriptV1
  , alwaysSucceedSpendScriptV2
  , alwaysSucceedSpendScriptHashV1
  , alwaysSucceedSpendScriptHashV2
  , alwaysSucceedSpendWitnessV1
  , alwaysSucceedSpendWitnessV2

  , alwaysFailsPolicyScriptV1
  , alwaysFailsPolicyScriptV2
  , alwaysFailsAssetIdV1
  , alwaysFailsAssetIdV2
  , alwaysFailsMintWitnessV1
  , alwaysFailsMintWitnessV1'
  , alwaysFailsMintWitnessV2
  , alwaysFailsMintWitnessV2'
  , alwaysFailsPolicyTxInfoRedeemerV2
  ) where

import Cardano.Api qualified as C
import Plutus.V1.Ledger.Api (MintingPolicy, Redeemer, ScriptPurpose (Minting), Validator, mkMintingPolicyScript,
                             mkValidatorScript)
import Plutus.V1.Ledger.Api qualified as BI
import Plutus.V2.Ledger.Api qualified as PlutusV2 (Map)
import PlutusScripts.Helpers (asRedeemer, fromPolicyId, mintScriptWitness, mintScriptWitness', plutusL1, plutusL2,
                              policyIdV1, policyIdV2, policyScript, spendScriptWitness, toScriptData, validatorScript)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Prelude qualified as P

-- AlwaysSucceeds minting policy --

alwaysSucceedPolicy :: MintingPolicy
alwaysSucceedPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| \_ _ -> () ||])

alwaysSucceedPolicyScriptV1 :: C.PlutusScript C.PlutusScriptV1
alwaysSucceedPolicyScriptV1 = policyScript alwaysSucceedPolicy

alwaysSucceedPolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
alwaysSucceedPolicyScriptV2 = policyScript alwaysSucceedPolicy

alwaysSucceedPolicyIdV2 :: C.PolicyId
alwaysSucceedPolicyIdV2 = policyIdV2 alwaysSucceedPolicy

alwaysSucceedAssetIdV1 :: C.AssetId
alwaysSucceedAssetIdV1 = C.AssetId (policyIdV1 alwaysSucceedPolicy)  ""

alwaysSucceedAssetIdV2 :: C.AssetId
alwaysSucceedAssetIdV2 = C.AssetId alwaysSucceedPolicyIdV2 ""

alwaysSucceedPolicyTxInfoRedeemerV2 :: PlutusV2.Map ScriptPurpose Redeemer
alwaysSucceedPolicyTxInfoRedeemerV2 = AMap.singleton
  (Minting $ fromPolicyId alwaysSucceedPolicyIdV2)
  (asRedeemer $ BI.toBuiltinData ())

-- | Witness token mint for including in txbody's txMintValue
-- Use Nothing to include script in witness, else provide TxIn to reference script
alwaysSucceedMintWitnessV1 :: C.CardanoEra era
  -> Maybe C.TxIn -- maybe reference input
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysSucceedMintWitnessV1 era Nothing =
    (policyIdV1 alwaysSucceedPolicy,
     mintScriptWitness era plutusL1 (Left alwaysSucceedPolicyScriptV1) (toScriptData ()))
alwaysSucceedMintWitnessV1 era (Just refTxIn) =
    (policyIdV1 alwaysSucceedPolicy,
     mintScriptWitness era plutusL1 (Right refTxIn) (toScriptData ()))

alwaysSucceedMintWitnessV1' :: C.CardanoEra era
  -> C.ExecutionUnits
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysSucceedMintWitnessV1'  era exunits =
  (policyIdV1 alwaysSucceedPolicy,
   mintScriptWitness' era plutusL1 (Left alwaysSucceedPolicyScriptV1) (toScriptData ()) exunits)

-- | Witness token mint for including in txbody's txMintValue
-- Use Nothing to include script in witness, else provide TxIn to reference script
alwaysSucceedMintWitnessV2 :: C.CardanoEra era
  -> Maybe C.TxIn -- maybe reference input
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysSucceedMintWitnessV2 era Nothing =
    (policyIdV2 alwaysSucceedPolicy,
     mintScriptWitness era plutusL2 (Left alwaysSucceedPolicyScriptV2) (toScriptData ()))
alwaysSucceedMintWitnessV2 era (Just refTxIn) =
    (policyIdV2 alwaysSucceedPolicy,
     mintScriptWitness era plutusL2 (Right refTxIn) (toScriptData ()))

alwaysSucceedMintWitnessV2' :: C.CardanoEra era
  -> C.ExecutionUnits
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysSucceedMintWitnessV2'  era exunits =
  (policyIdV2 alwaysSucceedPolicy,
   mintScriptWitness' era plutusL2 (Left alwaysSucceedPolicyScriptV2) (toScriptData ()) exunits)

-- AlwaysSucceeds validator --

alwaysSucceedSpend :: Validator
alwaysSucceedSpend = mkValidatorScript $$(PlutusTx.compile [|| \_ _ _ -> () ||])

alwaysSucceedSpendScriptV1 :: C.PlutusScript C.PlutusScriptV1
alwaysSucceedSpendScriptV1 = validatorScript alwaysSucceedSpend

alwaysSucceedSpendScriptV2 :: C.PlutusScript C.PlutusScriptV2
alwaysSucceedSpendScriptV2 = validatorScript alwaysSucceedSpend

alwaysSucceedSpendScriptHashV1 :: C.ScriptHash
alwaysSucceedSpendScriptHashV1 = C.hashScript $ C.PlutusScript C.PlutusScriptV1 alwaysSucceedSpendScriptV1

alwaysSucceedSpendScriptHashV2 :: C.ScriptHash
alwaysSucceedSpendScriptHashV2 = C.hashScript $ C.PlutusScript C.PlutusScriptV2 alwaysSucceedSpendScriptV2

alwaysSucceedSpendWitnessV1 :: C.CardanoEra era
  -> Maybe C.TxIn
  -> Maybe C.ScriptData
  -> C.Witness C.WitCtxTxIn era
alwaysSucceedSpendWitnessV1 era mRefScript mDatum =
    C.ScriptWitness C.ScriptWitnessForSpending $ spendScriptWitness era plutusL1
      (maybe (Left alwaysSucceedSpendScriptV1) (\refScript -> Right refScript) mRefScript) -- script or reference script
      (maybe C.InlineScriptDatum (\datum -> C.ScriptDatumForTxIn datum) mDatum) -- inline datum or datum value
      (toScriptData ()) -- redeemer

alwaysSucceedSpendWitnessV2 :: C.CardanoEra era
  -> Maybe C.TxIn
  -> Maybe C.ScriptData
  -> C.Witness C.WitCtxTxIn era
alwaysSucceedSpendWitnessV2 era mRefScript mDatum =
    C.ScriptWitness C.ScriptWitnessForSpending $ spendScriptWitness era plutusL2
      (maybe (Left alwaysSucceedSpendScriptV2) (\refScript -> Right refScript) mRefScript) -- script or reference script
      (maybe C.InlineScriptDatum (\datum -> C.ScriptDatumForTxIn datum) mDatum) -- inline datum or datum value
      (toScriptData ()) -- redeemer

-- AlwaysFails minting policy --

alwaysFailsPolicy :: MintingPolicy
alwaysFailsPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| \_ _ -> P.error () ||])

alwaysFailsPolicyScriptV1 :: C.PlutusScript C.PlutusScriptV1
alwaysFailsPolicyScriptV1 = policyScript alwaysFailsPolicy

alwaysFailsPolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
alwaysFailsPolicyScriptV2 = policyScript alwaysFailsPolicy

alwaysFailsPolicyIdV2 :: C.PolicyId
alwaysFailsPolicyIdV2 = policyIdV2 alwaysFailsPolicy

alwaysFailsAssetIdV1 :: C.AssetId
alwaysFailsAssetIdV1 = C.AssetId (policyIdV1 alwaysFailsPolicy)  ""

alwaysFailsAssetIdV2 :: C.AssetId
alwaysFailsAssetIdV2 = C.AssetId alwaysFailsPolicyIdV2 ""

alwaysFailsPolicyTxInfoRedeemerV2 :: PlutusV2.Map ScriptPurpose Redeemer
alwaysFailsPolicyTxInfoRedeemerV2 = AMap.singleton
  (Minting $ fromPolicyId alwaysFailsPolicyIdV2)
  (asRedeemer $ BI.toBuiltinData ())

-- | Witness token mint for including in txbody's txMintValue
-- Use Nothing to include script in witness, else provide TxIn to reference script
alwaysFailsMintWitnessV1 :: C.CardanoEra era
  -> Maybe C.TxIn -- maybe reference input
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysFailsMintWitnessV1 era Nothing =
    (policyIdV1 alwaysFailsPolicy,
     mintScriptWitness era plutusL1 (Left alwaysFailsPolicyScriptV1) (toScriptData ()))
alwaysFailsMintWitnessV1 era (Just refTxIn) =
    (policyIdV1 alwaysFailsPolicy,
     mintScriptWitness era plutusL1 (Right refTxIn) (toScriptData ()))

alwaysFailsMintWitnessV1' :: C.CardanoEra era
  -> C.ExecutionUnits
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysFailsMintWitnessV1'  era exunits =
  (policyIdV1 alwaysFailsPolicy,
   mintScriptWitness' era plutusL1 (Left alwaysFailsPolicyScriptV1) (toScriptData ()) exunits)

-- | Witness token mint for including in txbody's txMintValue
-- Use Nothing to include script in witness, else provide TxIn to reference script
alwaysFailsMintWitnessV2 :: C.CardanoEra era
  -> Maybe C.TxIn -- maybe reference input
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysFailsMintWitnessV2 era Nothing =
    (policyIdV2 alwaysFailsPolicy,
     mintScriptWitness era plutusL2 (Left alwaysFailsPolicyScriptV2) (toScriptData ()))
alwaysFailsMintWitnessV2 era (Just refTxIn) =
    (policyIdV2 alwaysFailsPolicy,
     mintScriptWitness era plutusL2 (Right refTxIn) (toScriptData ()))

alwaysFailsMintWitnessV2' :: C.CardanoEra era
  -> C.ExecutionUnits
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysFailsMintWitnessV2'  era exunits =
  (policyIdV2 alwaysFailsPolicy,
   mintScriptWitness' era plutusL2 (Left alwaysFailsPolicyScriptV2) (toScriptData ()) exunits)
