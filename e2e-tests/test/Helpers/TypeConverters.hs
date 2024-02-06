{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Helpers.TypeConverters where

import Cardano.Api.Byron qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Chain.Common (addrToBase58)
import Cardano.Ledger.Conway.Governance qualified as L
import Cardano.Ledger.Crypto qualified as L
import Cardano.Ledger.Shelley.API qualified as L
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Address (Address (Address))
import PlutusLedgerApi.V1.Credential (
  Credential (PubKeyCredential, ScriptCredential),
  StakingCredential (StakingHash),
 )
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 qualified as PV2
import PlutusLedgerApi.V3 qualified as PV3
import PlutusTx.Prelude qualified as PlutusTx

fromCardanoPaymentKeyHash :: C.Hash C.PaymentKey -> PV1.PubKeyHash
fromCardanoPaymentKeyHash = PV1.PubKeyHash . PlutusTx.toBuiltin . C.serialiseToRawBytes

fromCardanoStakeKeyHash :: C.Hash C.StakeKey -> PV1.PubKeyHash
fromCardanoStakeKeyHash = PV1.PubKeyHash . PlutusTx.toBuiltin . C.serialiseToRawBytes

fromCardanoScriptData :: C.HashableScriptData -> PV1.BuiltinData
fromCardanoScriptData = PV1.dataToBuiltinData . C.toPlutusData . C.getScriptData

fromCardanoScriptHash :: C.ScriptHash -> PV1.ScriptHash
fromCardanoScriptHash scriptHash = PV1.ScriptHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes scriptHash

fromCardanoTxIn :: C.TxIn -> PV1.TxOutRef
fromCardanoTxIn (C.TxIn txId (C.TxIx txIx)) = PV1.TxOutRef (fromCardanoTxId txId) (toInteger txIx)

cardanoAddressCredential :: C.AddressInEra era -> Credential
cardanoAddressCredential (C.AddressInEra C.ByronAddressInAnyEra (C.ByronAddress address)) =
  PubKeyCredential $
    PV1.PubKeyHash $
      PlutusTx.toBuiltin $
        addrToBase58 address
cardanoAddressCredential (C.AddressInEra _ (C.ShelleyAddress _ paymentCredential _)) =
  case C.fromShelleyPaymentCredential paymentCredential of
    C.PaymentCredentialByKey paymentKeyHash ->
      PubKeyCredential $
        PV1.PubKeyHash $
          PlutusTx.toBuiltin $
            C.serialiseToRawBytes paymentKeyHash
    C.PaymentCredentialByScript scriptHash ->
      ScriptCredential $ fromCardanoScriptHash scriptHash

cardanoStakingCredential :: C.AddressInEra era -> Maybe StakingCredential
cardanoStakingCredential (C.AddressInEra C.ByronAddressInAnyEra _) = Nothing
cardanoStakingCredential (C.AddressInEra _ (C.ShelleyAddress _ _ stakeAddressReference)) =
  case C.fromShelleyStakeReference stakeAddressReference of
    C.NoStakeAddress -> Nothing
    (C.StakeAddressByValue stakeCredential) ->
      Just (StakingHash $ fromCardanoStakeCredential stakeCredential)
    C.StakeAddressByPointer{} -> Nothing -- Not supported
  where
    fromCardanoStakeCredential :: C.StakeCredential -> Credential
    fromCardanoStakeCredential (C.StakeCredentialByKey stakeKeyHash) =
      PubKeyCredential $
        PV1.PubKeyHash $
          PlutusTx.toBuiltin $
            C.serialiseToRawBytes stakeKeyHash
    fromCardanoStakeCredential (C.StakeCredentialByScript scriptHash) =
      ScriptCredential $ fromCardanoScriptHash scriptHash

toPlutusAddress :: C.AddressInEra era -> Address
toPlutusAddress address = Address (cardanoAddressCredential address) (cardanoStakingCredential address)

fromCardanoAddressInEra :: C.AddressInEra era -> Address
fromCardanoAddressInEra = toPlutusAddress

fromCardanoTxOutValue :: C.ShelleyBasedEra era -> C.TxOutValue era -> C.Value
fromCardanoTxOutValue _ (C.TxOutValueByron l) = C.lovelaceToValue l
fromCardanoTxOutValue era (C.TxOutValueShelleyBased _ v) = C.fromLedgerValue era v

-- fromCardanoTxOutValue (C.TxOutAdaOnly _ 0) = mempty
-- fromCardanoTxOutValue (C.TxOutAdaOnly _ lovelace) = C.lovelaceToValue lovelace
-- fromCardanoTxOutValue (C.TxOutValue _ value) = value

fromCardanoTxOutDatum :: C.TxOutDatum C.CtxTx era -> PV2.OutputDatum
fromCardanoTxOutDatum C.TxOutDatumNone =
  PV2.NoOutputDatum
fromCardanoTxOutDatum (C.TxOutDatumHash _ h) =
  PV2.OutputDatumHash $ PV2.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes h)
fromCardanoTxOutDatum (C.TxOutDatumInTx _ d) =
  PV2.OutputDatumHash $
    PV2.DatumHash $
      PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptDataBytes d))
fromCardanoTxOutDatum (C.TxOutDatumInline _ d) =
  PV2.OutputDatum $ PV2.Datum $ fromCardanoScriptData d

fromCardanoTxOutDatum' :: C.TxOutDatum C.CtxUTxO era -> PV2.OutputDatum
fromCardanoTxOutDatum' C.TxOutDatumNone =
  PV2.NoOutputDatum
fromCardanoTxOutDatum' (C.TxOutDatumHash _ h) =
  PV2.OutputDatumHash $ PV2.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes h)
fromCardanoTxOutDatum' (C.TxOutDatumInline _ d) =
  PV2.OutputDatum $ PV2.Datum $ fromCardanoScriptData d

fromCardanoTxOutDatumHash :: C.TxOutDatum C.CtxTx era -> Maybe PV1.DatumHash
fromCardanoTxOutDatumHash C.TxOutDatumNone = Nothing
fromCardanoTxOutDatumHash (C.TxOutDatumHash _ h) =
  Just $ PV1.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes h)
fromCardanoTxOutDatumHash (C.TxOutDatumInTx _ d) =
  Just $ PV1.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptDataBytes d))
fromCardanoTxOutDatumHash (C.TxOutDatumInline _ d) =
  Just $ PV1.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptDataBytes d))

fromCardanoTxOutDatumHash' :: C.TxOutDatum C.CtxUTxO era -> Maybe PV1.DatumHash
fromCardanoTxOutDatumHash' C.TxOutDatumNone = Nothing
fromCardanoTxOutDatumHash' (C.TxOutDatumHash _ h) =
  Just $ PV1.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes h)
fromCardanoTxOutDatumHash' (C.TxOutDatumInline _ d) =
  Just $ PV1.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptDataBytes d))

fromCardanoTxOutToPV1TxInfoTxOut :: C.ShelleyBasedEra era -> C.TxOut C.CtxTx era -> PV1.TxOut
fromCardanoTxOutToPV1TxInfoTxOut _ (C.TxOut _ _ C.TxOutDatumInline{} _) =
  error "V1 TxOut doesn't support inline datum"
fromCardanoTxOutToPV1TxInfoTxOut _ (C.TxOut _ _ _ C.ReferenceScript{}) =
  error "V1 TxOut doesn't support reference scripts"
fromCardanoTxOutToPV1TxInfoTxOut sbe (C.TxOut addr value dh _) = do
  PV1.TxOut
    (fromCardanoAddressInEra addr)
    (fromCardanoValue $ fromCardanoTxOutValue sbe value)
    (fromCardanoTxOutDatumHash dh)

fromCardanoTxOutToPV1TxInfoTxOut' :: C.ShelleyBasedEra era -> C.TxOut C.CtxUTxO era -> PV1.TxOut
fromCardanoTxOutToPV1TxInfoTxOut' _ (C.TxOut _ _ C.TxOutDatumInline{} _) =
  error "V1 TxOut doesn't support inline datum"
fromCardanoTxOutToPV1TxInfoTxOut' _ (C.TxOut _ _ _ C.ReferenceScript{}) =
  error "V1 TxOut doesn't support reference scripts"
fromCardanoTxOutToPV1TxInfoTxOut' sbe (C.TxOut addr value dh _) = do
  PV1.TxOut
    (fromCardanoAddressInEra addr)
    (fromCardanoValue $ fromCardanoTxOutValue sbe value)
    (fromCardanoTxOutDatumHash' dh)

fromCardanoTxOutToPV2TxInfoTxOut :: C.ShelleyBasedEra era -> C.TxOut C.CtxTx era -> PV2.TxOut
fromCardanoTxOutToPV2TxInfoTxOut sbe (C.TxOut addr value datum refScript) =
  PV2.TxOut
    (fromCardanoAddressInEra addr)
    (fromCardanoValue $ fromCardanoTxOutValue sbe value)
    (fromCardanoTxOutDatum datum)
    (refScriptToScriptHash refScript)

fromCardanoTxOutToPV2TxInfoTxOut' :: C.ShelleyBasedEra era -> C.TxOut C.CtxUTxO era -> PV2.TxOut
fromCardanoTxOutToPV2TxInfoTxOut' sbe (C.TxOut addr value datum refScript) =
  PV2.TxOut
    (fromCardanoAddressInEra addr)
    (fromCardanoValue $ fromCardanoTxOutValue sbe value)
    (fromCardanoTxOutDatum' datum)
    (refScriptToScriptHash refScript)

fromCardanoTxOutToPV3TxInfoTxOut :: C.ShelleyBasedEra era -> C.TxOut C.CtxTx era -> PV3.TxOut
fromCardanoTxOutToPV3TxInfoTxOut = fromCardanoTxOutToPV2TxInfoTxOut

fromCardanoTxOutToPV3TxInfoTxOut' :: C.ShelleyBasedEra era -> C.TxOut C.CtxUTxO era -> PV3.TxOut
fromCardanoTxOutToPV3TxInfoTxOut' = fromCardanoTxOutToPV2TxInfoTxOut'

refScriptToScriptHash :: C.ReferenceScript era -> Maybe PV2.ScriptHash
refScriptToScriptHash C.ReferenceScriptNone = Nothing
refScriptToScriptHash (C.ReferenceScript _ (C.ScriptInAnyLang _ s)) =
  let (PV2.ScriptHash h) = fromCardanoScriptHash $ C.hashScript s
   in Just $ PV2.ScriptHash h

fromCardanoTxId :: C.TxId -> PV1.TxId
fromCardanoTxId txId = PV1.TxId $ PlutusTx.toBuiltin $ C.serialiseToRawBytes txId

fromCardanoPolicyId :: C.PolicyId -> PV1.CurrencySymbol
fromCardanoPolicyId (C.PolicyId scriptHash) = PV2.CurrencySymbol $ PlutusTx.toBuiltin (C.serialiseToRawBytes scriptHash)

fromCardanoAssetName :: C.AssetName -> Value.TokenName
fromCardanoAssetName (C.AssetName bs) = Value.TokenName $ PlutusTx.toBuiltin bs

fromCardanoAssetId :: C.AssetId -> Value.AssetClass
fromCardanoAssetId C.AdaAssetId = Value.assetClass PV1.adaSymbol PV1.adaToken
fromCardanoAssetId (C.AssetId policyId assetName) =
  Value.assetClass (fromCardanoPolicyId policyId) (fromCardanoAssetName assetName)

fromCardanoValue :: C.Value -> Value.Value
fromCardanoValue (C.valueToList -> list) =
  foldMap fromSingleton list
  where
    fromSingleton (fromCardanoAssetId -> assetClass, C.Quantity quantity) =
      Value.assetClassValue assetClass quantity

fromCardanoLovelace :: C.Lovelace -> PV3.Lovelace
fromCardanoLovelace (C.Lovelace l) = PV3.Lovelace l

fromCardanoProposal
  -- :: forall era
  --  . (L.EraCrypto era ~ L.StandardCrypto)
  :: C.ShelleyBasedEra era
  -> C.Proposal era
  -> PV3.ProposalProcedure
fromCardanoProposal sbe (C.Proposal ledgerPP) =
  C.shelleyBasedEraConstraints sbe $
    PV3.ProposalProcedure
      { PV3.ppDeposit =
          -- fromCardanoValue $ C.lovelaceToValue $ C.fromShelleyLovelace (L.pProcDeposit ledgerPP)
          fromCardanoLovelace $ C.fromShelleyLovelace (L.pProcDeposit ledgerPP)
      , PV3.ppReturnAddr = fromLedgerStakingCredential $ L.getRwdCred $ L.pProcReturnAddr ledgerPP
      , PV3.ppGovernanceAction = fromLedgerGovernanceAction $ L.pProcGovAction ledgerPP
      -- The optional anchor is omitted.
      }
  where
    fromLedgerStakingCredential :: L.Credential 'L.Staking L.StandardCrypto -> PV3.Credential
    fromLedgerStakingCredential (L.KeyHashObj kh) = PV3.PubKeyCredential (fromCardanoStakeKeyHash $ C.StakeKeyHash kh)
    fromLedgerStakingCredential (L.ScriptHashObj sh) = PV3.ScriptCredential (fromCardanoScriptHash $ C.ScriptHash sh)

    fromLedgerGovernanceAction :: L.GovAction era -> PV3.GovernanceAction
    fromLedgerGovernanceAction = undefined -- TODO once ledger implements PlutusV3 TxInfo

-- fromLedgerStakingCredential :: C.StakeCredential -> PV3.Credential
-- fromLedgerStakingCredential (C.StakeCredentialByKey kh) = PV3.PubKeyCredential (fromCardanoStakeKeyHash kh)
-- fromLedgerStakingCredential (L.StakeCredentialByScript sh) = PV3.ScriptCredential (fromCardanoScriptHash sh)

-- unused?
-- fromCardanoProposal'
--   :: (C.Lovelace, C.Hash C.StakeKey, C.GovernanceAction era) -> PV3.ProposalProcedure
-- fromCardanoProposal' (deposit, returnAddr, govAction) =
--   PV3.ProposalProcedure
--     { PV3.ppDeposit = fromCardanoValue $ C.lovelaceToValue deposit
--     , PV3.ppReturnAddr = PV3.PubKeyCredential $ fromCardanoStakeKeyHash returnAddr
--     , PV3.ppGovernanceAction = undefined -- TODO once ledger implements PlutusV3 TxInfo
--     }
