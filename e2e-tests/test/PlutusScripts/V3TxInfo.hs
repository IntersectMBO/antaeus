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

module PlutusScripts.V3TxInfo where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Coin qualified as L
import Helpers.TypeConverters (
  coinToLovelace,
  fromCardanoPaymentKeyHash,
  fromCardanoScriptData,
  fromCardanoTxInV3,
  fromCardanoTxOutToPV3TxInfoTxOut,
  fromCardanoTxOutToPV3TxInfoTxOut',
  fromCardanoValue,
 )
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V1.Interval qualified as P
import PlutusLedgerApi.V1.Value qualified as PlutusV1
import PlutusLedgerApi.V3 qualified as PlutusV3
import PlutusScripts.Helpers (mintScriptWitness', plutusL3, policyIdV3, toScriptData)
import PlutusTx qualified
import PlutusTx.Builtins qualified as P
import PlutusTx.Prelude qualified as P

data V3TxInfo = V3TxInfo
  { expTxInfoInputs :: [PlutusV3.TxInInfo]
  -- ^ Transaction inputs; cannot be an empty list
  , expTxInfoReferenceInputs :: [PlutusV3.TxInInfo]
  -- ^ Added in V3: Transaction reference inputs
  , expTxInfoOutputs :: [PlutusV3.TxOut]
  -- ^ Transaction outputs
  , expTxInfoFee :: PlutusV3.Lovelace
  -- ^ The fee paid by this transaction.
  , expTxInfoMint :: PlutusV3.Value
  -- ^ The 'Value' minted by this transaction (no zero ada from V3).
  , expTxInfoTxCert :: [PlutusV3.TxCert]
  -- ^ Digests of certificates included in this transaction
  , expTxInfoWdrl :: PlutusV3.Map PlutusV3.Credential PlutusV3.Lovelace
  -- ^ Withdrawals
  , expTxInfoValidRange :: PlutusV3.POSIXTimeRange
  -- ^ The valid range for the transaction.
  , expTxInfoSignatories :: [PlutusV3.PubKeyHash]
  -- ^ Signatures provided with the transaction, attested that they all signed the tx
  , expTxInfoRedeemers :: PlutusV3.Map PlutusV3.ScriptPurpose PlutusV3.Redeemer
  -- ^ Added in V3: a table of redeemers attached to the transaction
  , expTxInfoData :: PlutusV3.Map PlutusV3.DatumHash PlutusV3.Datum
  -- ^ The lookup table of datums attached to the transaction
  -- , expTxInfoId          :: PlutusV3.TxId
  -- ^ Hash of the pending transaction body (i.e. transaction excluding witnesses). Cannot be verified onchain.
  , txInfoVotingProcedures
      :: PlutusV3.Map PlutusV3.Voter (PlutusV3.Map PlutusV3.GovernanceActionId PlutusV3.Vote)
  -- ^ This is Map with all of the votes that were included in the transaction
  , txInfoProposalProcedures :: [PlutusV3.ProposalProcedure]
  -- ^ This is a list with Proposals that will be turned into GovernanceActions, that everyone
  -- can vote on
  , txInfoCurrentTreasuryAmount :: Maybe PlutusV3.Value
  -- ^ Optional amount for the current treasury. If included it will be checked to be equal
  -- the current amount in the treasury.
  , txInfoTreasuryDonation :: Maybe PlutusV3.Value
  -- ^ Optional amount for donating to the current treasury. If included, specified amount
  -- will go into the treasury.
  }
PlutusTx.unstableMakeIsData ''V3TxInfo

checkV3TxInfoRedeemer
  :: [PlutusV3.TxInInfo]
  -> [PlutusV3.TxInInfo]
  -> [PlutusV3.TxOut]
  -> PlutusV3.Lovelace
  -> PlutusV3.Value
  -> [PlutusV3.TxCert]
  -> PlutusV3.Map PlutusV3.Credential PlutusV3.Lovelace
  -> PlutusV3.POSIXTimeRange
  -> [PlutusV3.PubKeyHash]
  -> PlutusV3.Map PlutusV3.ScriptPurpose PlutusV3.Redeemer
  -> PlutusV3.Map PlutusV3.DatumHash PlutusV3.Datum
  -> PlutusV3.Map PlutusV3.Voter (PlutusV3.Map PlutusV3.GovernanceActionId PlutusV3.Vote)
  -> [PlutusV3.ProposalProcedure]
  -> Maybe PlutusV3.Value
  -> Maybe PlutusV3.Value
  -> C.HashableScriptData
checkV3TxInfoRedeemer
  expIns
  expRefIns
  expOuts
  expFee
  expMint
  expTxCert
  expWdrl
  expRange
  expSigs
  expReds
  expData
  expVPs
  expPPs
  expTA
  expTD =
    toScriptData $
      V3TxInfo
        expIns
        expRefIns
        expOuts
        expFee
        expMint
        expTxCert
        expWdrl
        expRange
        expSigs
        expReds
        expData
        expVPs
        expPPs
        expTA
        expTD

txInfoInputs :: C.ShelleyBasedEra era -> (C.TxIn, C.TxOut C.CtxUTxO era) -> PlutusV3.TxInInfo
txInfoInputs sbe (txIn, txOut) = do
  PlutusV3.TxInInfo
    { PlutusV3.txInInfoOutRef = fromCardanoTxInV3 txIn
    , PlutusV3.txInInfoResolved = fromCardanoTxOutToPV3TxInfoTxOut' sbe txOut
    }

txInfoOutputs :: C.ShelleyBasedEra era -> [C.TxOut C.CtxTx era] -> [PlutusV3.TxOut]
txInfoOutputs sbe = map (fromCardanoTxOutToPV3TxInfoTxOut sbe)

txInfoFee :: L.Coin -> PlutusV3.Value
txInfoFee = PlutusV1.lovelaceValue . coinToLovelace

txInfoMint :: C.Value -> PlutusV3.Value
txInfoMint = fromCardanoValue

txInfoSigs :: [C.VerificationKey C.PaymentKey] -> [PlutusV3.PubKeyHash]
txInfoSigs = map (fromCardanoPaymentKeyHash . C.verificationKeyHash)

txInfoData :: [C.HashableScriptData] -> PlutusV3.Map PlutusV3.DatumHash PlutusV3.Datum
txInfoData =
  PlutusV3.unsafeFromList
    . map
      ( \datum ->
          ( PlutusV3.DatumHash $
              PlutusV3.toBuiltin $
                C.serialiseToRawBytes $
                  C.hashScriptDataBytes datum
          , PlutusV3.Datum $ fromCardanoScriptData datum
          )
      )

-- minting policy --

{-# INLINEABLE mkCheckV3TxInfo #-}
mkCheckV3TxInfo :: V3TxInfo -> PlutusV3.ScriptContext -> Bool
mkCheckV3TxInfo V3TxInfo{..} ctx =
  P.traceIfFalse "unexpected txInfoInputs" checkTxInfoInputs
    P.&& P.traceIfFalse "unexpected txInfoReferenceInputs" checkTxInfoReferenceInputs
    P.&& P.traceIfFalse "unexpected txInfoOutputs" checkTxInfoOutputs
    P.&& P.traceIfFalse "unexpected txInfoFee" checkTxInfoFee
    P.&& P.traceIfFalse "unexpected txInfoMint" checkTxInfoMint
    P.&& P.traceIfFalse "unexpected txInfoTxCert" checkTxInfoTxCert
    P.&& P.traceIfFalse "unexpected txInfoWdrl" checkTxInfoWdrl
    P.&& P.traceIfFalse "provided range doesn't contain txInfoValidRange" checkTxInfoValidRange
    P.&& P.traceIfFalse "unexpected txInfoSignatories" checkTxInfoSignatories
    P.&& P.traceIfFalse "unexpected txInfoRedeemers" checkTxInfoRedeemers
    P.&& P.traceIfFalse "unexpected txInfoData" checkTxInfoData
    P.&& P.traceIfFalse "txInfoId isn't the expected TxId length" checkTxInfoId
  where
    info :: PlutusV3.TxInfo
    info = PlutusV3.scriptContextTxInfo ctx

    checkTxInfoInputs = expTxInfoInputs P.== PlutusV3.txInfoInputs info
    checkTxInfoReferenceInputs = expTxInfoReferenceInputs P.== PlutusV3.txInfoReferenceInputs info
    checkTxInfoOutputs = expTxInfoOutputs P.== PlutusV3.txInfoOutputs info
    checkTxInfoFee = expTxInfoFee P.== PlutusV3.txInfoFee info
    checkTxInfoMint = expTxInfoMint P.== PlutusV3.txInfoMint info
    checkTxInfoTxCert = expTxInfoTxCert P.== PlutusV3.txInfoTxCerts info
    checkTxInfoWdrl = P.toList expTxInfoWdrl P.== P.toList (PlutusV3.txInfoWdrl info)
    checkTxInfoValidRange = expTxInfoValidRange `P.contains` PlutusV3.txInfoValidRange info
    checkTxInfoSignatories = expTxInfoSignatories P.== PlutusV3.txInfoSignatories info
    checkTxInfoRedeemers = False -- do -- TODO: uncomment section when ownCurrencySymbol etc is implemented for V3
    -- let ownScriptPurpose = PlutusV3.Minting (ownCurrencySymbol ctx)
    --    withoutOwnRedeemer = AMap.delete ownScriptPurpose (PlutusV3.txInfoRedeemers info)
    -- expTxInfoRedeemers P.== withoutOwnRedeemer -- cannot check own redeemer so only check other script's redeemer
    checkTxInfoData = P.toList expTxInfoData P.== P.toList (PlutusV3.txInfoData info)
    checkTxInfoId = P.equalsInteger 32 (P.lengthOfByteString P.$ PlutusV3.getTxId P.$ PlutusV3.txInfoId info)

checkV3TxInfoV3 :: SerialisedScript
checkV3TxInfoV3 = serialiseCompiledCode $$(PlutusTx.compile [||mkCheckV3TxInfo||])

checkV3TxInfoScriptV3 :: C.PlutusScript C.PlutusScriptV3
checkV3TxInfoScriptV3 = C.PlutusScriptSerialised checkV3TxInfoV3

checkV3TxInfoAssetIdV3 :: C.AssetId
checkV3TxInfoAssetIdV3 = C.AssetId (policyIdV3 checkV3TxInfoV3) "V3TxInfo"

checkV3TxInfoMintWitnessV3
  :: C.ShelleyBasedEra era
  -> C.HashableScriptData
  -> C.ExecutionUnits
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
checkV3TxInfoMintWitnessV3 sbe redeemer exunits =
  ( policyIdV3 checkV3TxInfoV3
  , mintScriptWitness' sbe plutusL3 (Left checkV3TxInfoScriptV3) redeemer exunits
  )
