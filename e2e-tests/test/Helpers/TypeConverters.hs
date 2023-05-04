{-# LANGUAGE ViewPatterns #-}
module Helpers.TypeConverters where

import Cardano.Api.Byron qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Chain.Common (addrToBase58)
import Plutus.V1.Ledger.Address (Address (Address))
import Plutus.V1.Ledger.Api qualified as PV1
import Plutus.V1.Ledger.Credential (Credential (PubKeyCredential, ScriptCredential), StakingCredential (StakingHash))
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx.Prelude qualified as PlutusTx

fromCardanoPaymentKeyHash :: C.Hash C.PaymentKey -> PV1.PubKeyHash
fromCardanoPaymentKeyHash paymentKeyHash = PV1.PubKeyHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes paymentKeyHash

fromCardanoScriptData :: C.ScriptData -> PV1.BuiltinData
fromCardanoScriptData = PV1.dataToBuiltinData . C.toPlutusData

fromCardanoScriptHash :: C.ScriptHash -> PV1.ValidatorHash
fromCardanoScriptHash scriptHash = PV1.ValidatorHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes scriptHash

fromCardanoTxIn :: C.TxIn -> PV1.TxOutRef
fromCardanoTxIn (C.TxIn txId (C.TxIx txIx)) = PV1.TxOutRef (fromCardanoTxId txId) (toInteger txIx)

scriptToValidatorHash :: C.ScriptHash -> PV1.ValidatorHash
scriptToValidatorHash = PV1.ValidatorHash . PlutusTx.toBuiltin . C.serialiseToRawBytes

cardanoAddressCredential :: C.AddressInEra era -> Credential
cardanoAddressCredential (C.AddressInEra C.ByronAddressInAnyEra (C.ByronAddress address))
  = PubKeyCredential
  $ PV1.PubKeyHash
  $ PlutusTx.toBuiltin
  $ addrToBase58 address
cardanoAddressCredential (C.AddressInEra _ (C.ShelleyAddress _ paymentCredential _))
  = case C.fromShelleyPaymentCredential paymentCredential of
      C.PaymentCredentialByKey paymentKeyHash ->
          PubKeyCredential
          $ PV1.PubKeyHash
          $ PlutusTx.toBuiltin
          $ C.serialiseToRawBytes paymentKeyHash
      C.PaymentCredentialByScript scriptHash ->
          ScriptCredential $ scriptToValidatorHash scriptHash

cardanoStakingCredential :: C.AddressInEra era -> Maybe StakingCredential
cardanoStakingCredential (C.AddressInEra C.ByronAddressInAnyEra _) = Nothing
cardanoStakingCredential (C.AddressInEra _ (C.ShelleyAddress _ _ stakeAddressReference))
  = case C.fromShelleyStakeReference stakeAddressReference of
         C.NoStakeAddress -> Nothing
         (C.StakeAddressByValue stakeCredential) ->
             Just (StakingHash $ fromCardanoStakeCredential stakeCredential)
         C.StakeAddressByPointer{} -> Nothing -- Not supported
  where
    fromCardanoStakeCredential :: C.StakeCredential -> Credential
    fromCardanoStakeCredential (C.StakeCredentialByKey stakeKeyHash)
      = PubKeyCredential
      $ PV1.PubKeyHash
      $ PlutusTx.toBuiltin
      $ C.serialiseToRawBytes stakeKeyHash
    fromCardanoStakeCredential (C.StakeCredentialByScript scriptHash) = ScriptCredential (scriptToValidatorHash scriptHash)

toPlutusAddress :: C.AddressInEra era -> Address
toPlutusAddress address = Address (cardanoAddressCredential address) (cardanoStakingCredential address)

fromCardanoAddressInEra :: C.AddressInEra era -> Address
fromCardanoAddressInEra = toPlutusAddress

fromCardanoTxOutValue :: C.TxOutValue era -> C.Value
fromCardanoTxOutValue (C.TxOutAdaOnly _ 0)        = mempty
fromCardanoTxOutValue (C.TxOutAdaOnly _ lovelace) = C.lovelaceToValue lovelace
fromCardanoTxOutValue (C.TxOutValue _ value)      = value

fromCardanoTxOutDatum :: C.TxOutDatum C.CtxTx era -> PV2.OutputDatum
fromCardanoTxOutDatum C.TxOutDatumNone       =
    PV2.NoOutputDatum
fromCardanoTxOutDatum (C.TxOutDatumHash _ h) =
    PV2.OutputDatumHash $ PV2.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes h)
fromCardanoTxOutDatum (C.TxOutDatumInTx _ d) =
    PV2.OutputDatumHash $ PV2.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptData d))
fromCardanoTxOutDatum (C.TxOutDatumInline _ d) =
    PV2.OutputDatum $ PV2.Datum $ fromCardanoScriptData d

fromCardanoTxOutDatum' :: C.TxOutDatum C.CtxUTxO era -> PV2.OutputDatum
fromCardanoTxOutDatum' C.TxOutDatumNone       =
    PV2.NoOutputDatum
fromCardanoTxOutDatum' (C.TxOutDatumHash _ h) =
    PV2.OutputDatumHash $ PV2.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes h)
fromCardanoTxOutDatum' (C.TxOutDatumInline _ d) =
    PV2.OutputDatum $ PV2.Datum $ fromCardanoScriptData d

fromCardanoTxOutDatumHash :: C.TxOutDatum C.CtxTx era -> Maybe PV1.DatumHash
fromCardanoTxOutDatumHash C.TxOutDatumNone       = Nothing
fromCardanoTxOutDatumHash (C.TxOutDatumHash _ h) =
    Just $ PV1.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes h)
fromCardanoTxOutDatumHash (C.TxOutDatumInTx _ d) =
    Just $ PV1.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptData d))
fromCardanoTxOutDatumHash (C.TxOutDatumInline _ d) =
    Just $ PV1.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptData d))

fromCardanoTxOutDatumHash' :: C.TxOutDatum C.CtxUTxO era -> Maybe PV1.DatumHash
fromCardanoTxOutDatumHash' C.TxOutDatumNone       = Nothing
fromCardanoTxOutDatumHash' (C.TxOutDatumHash _ h) =
    Just $ PV1.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes h)
fromCardanoTxOutDatumHash' (C.TxOutDatumInline _ d) =
    Just $ PV1.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptData d))

fromCardanoTxOutToPV1TxInfoTxOut :: C.TxOut C.CtxTx era -> PV1.TxOut
fromCardanoTxOutToPV1TxInfoTxOut (C.TxOut _ _ C.TxOutDatumInline{} _) =
  error "V1 TxOut doesn't support inline datum"
fromCardanoTxOutToPV1TxInfoTxOut (C.TxOut _ _ _ C.ReferenceScript{}) =
  error "V1 TxOut doesn't support reference scripts"
fromCardanoTxOutToPV1TxInfoTxOut (C.TxOut addr value dh _) = do
  PV1.TxOut
    (fromCardanoAddressInEra addr)
    (fromCardanoValue $ fromCardanoTxOutValue value)
    (fromCardanoTxOutDatumHash dh)

fromCardanoTxOutToPV1TxInfoTxOut' :: C.TxOut C.CtxUTxO era -> PV1.TxOut
fromCardanoTxOutToPV1TxInfoTxOut' (C.TxOut _ _ C.TxOutDatumInline{} _) =
  error "V1 TxOut doesn't support inline datum"
fromCardanoTxOutToPV1TxInfoTxOut' (C.TxOut _ _ _ C.ReferenceScript{}) =
  error "V1 TxOut doesn't support reference scripts"
fromCardanoTxOutToPV1TxInfoTxOut' (C.TxOut addr value dh _) = do
  PV1.TxOut
    (fromCardanoAddressInEra addr)
    (fromCardanoValue $ fromCardanoTxOutValue value)
    (fromCardanoTxOutDatumHash' dh)

fromCardanoTxOutToPV2TxInfoTxOut :: C.TxOut C.CtxTx era -> PV2.TxOut
fromCardanoTxOutToPV2TxInfoTxOut (C.TxOut addr value datum refScript) =
    PV2.TxOut
    (fromCardanoAddressInEra addr)
    (fromCardanoValue $ fromCardanoTxOutValue value)
    (fromCardanoTxOutDatum datum)
    (refScriptToScriptHash refScript)

fromCardanoTxOutToPV2TxInfoTxOut' :: C.TxOut C.CtxUTxO era -> PV2.TxOut
fromCardanoTxOutToPV2TxInfoTxOut' (C.TxOut addr value datum refScript) =
    PV2.TxOut
    (fromCardanoAddressInEra addr)
    (fromCardanoValue $ fromCardanoTxOutValue value)
    (fromCardanoTxOutDatum' datum)
    (refScriptToScriptHash refScript)

refScriptToScriptHash :: C.ReferenceScript era -> Maybe PV2.ScriptHash
refScriptToScriptHash C.ReferenceScriptNone = Nothing
refScriptToScriptHash (C.ReferenceScript _ (C.ScriptInAnyLang _ s)) =
    let (PV2.ValidatorHash h) = fromCardanoScriptHash $ C.hashScript s
     in Just $ PV2.ScriptHash h

fromCardanoTxId :: C.TxId -> PV1.TxId
fromCardanoTxId txId = PV1.TxId $ PlutusTx.toBuiltin $ C.serialiseToRawBytes txId

fromCardanoPolicyId :: C.PolicyId -> PV1.MintingPolicyHash
fromCardanoPolicyId (C.PolicyId scriptHash) = PV2.MintingPolicyHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes scriptHash)

fromCardanoAssetName :: C.AssetName -> Value.TokenName
fromCardanoAssetName (C.AssetName bs) = Value.TokenName $ PlutusTx.toBuiltin bs

fromCardanoAssetId :: C.AssetId -> Value.AssetClass
fromCardanoAssetId C.AdaAssetId = Value.assetClass PV1.adaSymbol PV1.adaToken
fromCardanoAssetId (C.AssetId policyId assetName) =
    Value.assetClass
        (Value.mpsSymbol . fromCardanoPolicyId $ policyId)
        (fromCardanoAssetName assetName)

fromCardanoValue :: C.Value -> Value.Value
fromCardanoValue (C.valueToList -> list) =
    foldMap fromSingleton list
  where
    fromSingleton (fromCardanoAssetId -> assetClass, C.Quantity quantity) =
        Value.assetClassValue assetClass quantity
