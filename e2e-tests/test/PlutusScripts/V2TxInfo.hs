{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module PlutusScripts.V2TxInfo (
  txInfoInputs,
  txInfoOutputs,
  txInfoFee,
  txInfoMint,
  txInfoSigs,
  txInfoData,
  checkV2TxInfoScriptV2,
  checkV2TxInfoAssetIdV2,
  checkV2TxInfoRedeemer,
  checkV2TxInfoMintWitnessV2,
) where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley qualified as C
import Debug.Trace qualified as Debug
import Helpers.Common (toShelleyBasedEra)
import Helpers.ScriptUtils (mkUntypedMintingPolicy)
import Helpers.TypeConverters (
  coinToLovelace,
  fromCardanoPaymentKeyHash,
  fromCardanoScriptData,
  fromCardanoTxOutToPV2TxInfoTxOut,
  fromCardanoTxOutToPV2TxInfoTxOut',
  fromCardanoValue,
 )
import PlutusCore.Pretty (Render (render), prettyPlcClassicSimple)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V1.Interval qualified as Interval
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 qualified as V2
import PlutusLedgerApi.V2.Contexts (ownCurrencySymbol)
import PlutusScripts.Helpers (mintScriptWitness', plutusL2, policyIdV2, toScriptData)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Builtins qualified as P
import PlutusTx.Prelude qualified as P

data V2TxInfo = V2TxInfo
  { expTxInfoInputs :: [V2.TxInInfo]
  -- ^ Transaction inputs; cannot be an empty list
  , expTxInfoReferenceInputs :: [V2.TxInInfo]
  -- ^ Added in V2: Transaction reference inputs
  , expTxInfoOutputs :: [V2.TxOut]
  -- ^ Transaction outputs
  , expTxInfoFee :: V2.Value
  -- ^ The fee paid by this transaction.
  , expTxInfoMint :: V2.Value
  -- ^ The 'Value' minted by this transaction.
  , expTxInfoDCert :: [V2.DCert]
  -- ^ Digests of certificates included in this transaction
  , expTxInfoWdrl :: V2.Map V2.StakingCredential Integer
  -- ^ Withdrawals
  , expTxInfoValidRange :: V2.POSIXTimeRange
  -- ^ The valid range for the transaction.
  , expTxInfoSignatories :: [V2.PubKeyHash]
  -- ^ Signatures provided with the transaction, attested that they all signed the tx
  , expTxInfoRedeemers :: V2.Map V2.ScriptPurpose V2.Redeemer
  -- ^ Added in V2: a table of redeemers attached to the transaction
  , expTxInfoData :: V2.Map V2.DatumHash V2.Datum
  -- ^ The lookup table of datums attached to the transaction
  -- , expTxInfoId          :: V2.TxId
  -- ^ Hash of the pending transaction body (i.e. transaction excluding witnesses).
  -- Cannot be verified onchain.
  }
PlutusTx.unstableMakeIsData ''V2TxInfo

checkV2TxInfoRedeemer
  :: [V2.TxInInfo]
  -> [V2.TxInInfo]
  -> [V2.TxOut]
  -> V2.Value
  -> V2.Value
  -> [V2.DCert]
  -> V2.Map V2.StakingCredential Integer
  -> V2.POSIXTimeRange
  -> [V2.PubKeyHash]
  -> V2.Map V2.ScriptPurpose V2.Redeemer
  -> V2.Map V2.DatumHash V2.Datum
  -> C.HashableScriptData
checkV2TxInfoRedeemer
  expIns
  expRefIns
  expOuts
  expFee
  expMint
  expDCert
  expWdrl
  expRange
  expSigs
  expReds
  expData =
    toScriptData $
      V2TxInfo
        expIns
        expRefIns
        expOuts
        expFee
        expMint
        expDCert
        expWdrl
        expRange
        expSigs
        expReds
        expData

txInfoInputs
  :: C.CardanoEra era
  -> (C.TxIn, C.TxOut C.CtxUTxO era)
  -> V2.TxInInfo
txInfoInputs era (txIn, txOut) = do
  V2.TxInInfo
    { V2.txInInfoOutRef = fromCardanoTxIn txIn
    , V2.txInInfoResolved =
        fromCardanoTxOutToPV2TxInfoTxOut' (toShelleyBasedEra era) txOut
    }
  where
    fromCardanoTxIn :: C.TxIn -> V2.TxOutRef
    fromCardanoTxIn (C.TxIn txId (C.TxIx txIx)) =
      V2.TxOutRef (fromCardanoTxId txId) (toInteger txIx)

    fromCardanoTxId :: C.TxId -> V2.TxId
    fromCardanoTxId = V2.TxId . P.toBuiltin . C.serialiseToRawBytes

txInfoOutputs :: C.CardanoEra era -> [C.TxOut C.CtxTx era] -> [V2.TxOut]
txInfoOutputs era = map (fromCardanoTxOutToPV2TxInfoTxOut (toShelleyBasedEra era))

txInfoFee :: L.Coin -> V2.Value
txInfoFee = Value.lovelaceValue . coinToLovelace

txInfoMint :: C.Value -> V2.Value
txInfoMint = fromCardanoValue

txInfoSigs :: [C.VerificationKey C.PaymentKey] -> [V2.PubKeyHash]
txInfoSigs = map (fromCardanoPaymentKeyHash . C.verificationKeyHash)

txInfoData
  :: [C.HashableScriptData]
  -> V2.Map V2.DatumHash V2.Datum
txInfoData =
  V2.unsafeFromList
    . map
      ( \datum ->
          ( V2.DatumHash $
              V2.toBuiltin $
                C.serialiseToRawBytes $
                  C.hashScriptDataBytes datum
          , V2.Datum $ fromCardanoScriptData datum
          )
      )

-- minting policy --

{-# INLINEABLE mkCheckV2TxInfo #-}
mkCheckV2TxInfo :: V2TxInfo -> V2.ScriptContext -> Bool
mkCheckV2TxInfo V2TxInfo{..} ctx =
  P.traceIfFalse "unexpected txInfoInputs" checkTxInfoInputs
    P.&& P.traceIfFalse "unexpected txInfoReferenceInputs" checkTxInfoReferenceInputs
    P.&& P.traceIfFalse "unexpected txInfoOutputs" checkTxInfoOutputs
    P.&& P.traceIfFalse "unexpected txInfoFee" checkTxInfoFee
    P.&& P.traceIfFalse "unexpected txInfoMint" checkTxInfoMint
    P.&& P.traceIfFalse "unexpected txInfoDCert" checkTxInfoDCert
    P.&& P.traceIfFalse "unexpected txInfoWdrl" checkTxInfoWdrl
    P.&& P.traceIfFalse "provided range doesn't contain txInfoValidRange" checkTxInfoValidRange
    P.&& P.traceIfFalse "unexpected txInfoSignatories" checkTxInfoSignatories
    P.&& P.traceIfFalse "unexpected txInfoRedeemers" checkTxInfoRedeemers
    P.&& P.traceIfFalse "unexpected txInfoData" checkTxInfoData
    P.&& P.traceIfFalse "txInfoId isn't the expected TxId length" checkTxInfoId
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

    checkTxInfoInputs =
      expTxInfoInputs P.== V2.txInfoInputs info
    checkTxInfoReferenceInputs =
      expTxInfoReferenceInputs P.== V2.txInfoReferenceInputs info
    checkTxInfoOutputs =
      expTxInfoOutputs P.== V2.txInfoOutputs info
    checkTxInfoFee =
      expTxInfoFee P.== V2.txInfoFee info
    checkTxInfoMint =
      expTxInfoMint P.== V2.txInfoMint info
    checkTxInfoDCert =
      expTxInfoDCert P.== V2.txInfoDCert info
    checkTxInfoWdrl =
      P.toList expTxInfoWdrl P.== P.toList (V2.txInfoWdrl info)
    checkTxInfoValidRange =
      expTxInfoValidRange `Interval.contains` V2.txInfoValidRange info
    checkTxInfoSignatories =
      expTxInfoSignatories P.== V2.txInfoSignatories info
    checkTxInfoRedeemers = do
      let ownScriptPurpose = V2.Minting (ownCurrencySymbol ctx)
          withoutOwnRedeemer = AMap.delete ownScriptPurpose (V2.txInfoRedeemers info)
      P.toList expTxInfoRedeemers P.== P.toList withoutOwnRedeemer
    -- \^ cannot check own redeemer so only check other script's redeemer
    checkTxInfoData = P.toList expTxInfoData P.== P.toList (V2.txInfoData info)
    checkTxInfoId =
      P.equalsInteger
        32
        (P.lengthOfByteString P.$ V2.getTxId P.$ V2.txInfoId info)

checkV2TxInfoV2 :: SerialisedScript
checkV2TxInfoV2 = Debug.trace "" typedMintingPolicy
  where
    debug :: String =
      render . prettyPlcClassicSimple $
        V2.uncheckedDeserialiseUPLC typedMintingPolicy
    typedMintingPolicy =
      serialiseCompiledCode $$(PlutusTx.compile [||untypedMintingPolicy||])
    untypedMintingPolicy =
      mkUntypedMintingPolicy @V2.ScriptContext mkCheckV2TxInfo

checkV2TxInfoScriptV2 :: C.PlutusScript C.PlutusScriptV2
checkV2TxInfoScriptV2 = C.PlutusScriptSerialised checkV2TxInfoV2

checkV2TxInfoAssetIdV2 :: C.AssetId
checkV2TxInfoAssetIdV2 = C.AssetId (policyIdV2 checkV2TxInfoV2) "V2TxInfo"

checkV2TxInfoMintWitnessV2
  :: C.ShelleyBasedEra era
  -> C.HashableScriptData
  -> C.ExecutionUnits
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
checkV2TxInfoMintWitnessV2 sbe redeemer exunits =
  ( policyIdV2 checkV2TxInfoV2
  , mintScriptWitness' sbe plutusL2 (Left checkV2TxInfoScriptV2) redeemer exunits
  )
