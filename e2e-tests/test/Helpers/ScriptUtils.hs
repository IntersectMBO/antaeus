{-# LANGUAGE OverloadedStrings #-}

-- subset of utilities from plutus-script-utils
module Helpers.ScriptUtils where

import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V2 qualified as PV2
import PlutusTx (UnsafeFromData)
import PlutusTx.Prelude qualified as P

type UntypedValidator = PV1.BuiltinData -> PV1.BuiltinData -> PV1.BuiltinData -> ()
type UntypedMintingPolicy = PV1.BuiltinData -> PV1.BuiltinData -> ()
type UntypedStakeValidator = PV1.BuiltinData -> PV1.BuiltinData -> ()

{-# INLINEABLE tracedUnsafeFrom #-}
tracedUnsafeFrom :: forall a. (UnsafeFromData a) => P.BuiltinString -> PV1.BuiltinData -> a
tracedUnsafeFrom label d = P.trace label $ PV1.unsafeFromBuiltinData d

class (PV1.UnsafeFromData sc) => IsScriptContext sc where
  {-# INLINEABLE mkUntypedValidator #-}
  mkUntypedValidator
    :: (PV1.UnsafeFromData d, PV1.UnsafeFromData r)
    => (d -> r -> sc -> Bool)
    -> UntypedValidator
  -- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
  mkUntypedValidator f d r p =
    check $
      f
        (tracedUnsafeFrom "Data decoded successfully" d)
        (tracedUnsafeFrom "Redeemer decoded successfully" r)
        (tracedUnsafeFrom "Script context decoded successfully" p)

  {-# INLINEABLE mkUntypedStakeValidator #-}
  mkUntypedStakeValidator
    :: (PV1.UnsafeFromData r)
    => (r -> sc -> Bool)
    -> UntypedStakeValidator
  mkUntypedStakeValidator f r p =
    check $
      f
        (tracedUnsafeFrom "Redeemer decoded successfully" r)
        (tracedUnsafeFrom "Script context decoded successfully" p)

  {-# INLINEABLE mkUntypedMintingPolicy #-}
  mkUntypedMintingPolicy
    :: (UnsafeFromData r) => (r -> sc -> Bool) -> UntypedMintingPolicy
  -- We can use unsafeFromBuiltinData here as we would fail immediately anyway
  -- if parsing failed
  mkUntypedMintingPolicy f r sc =
    check $
      f
        (tracedUnsafeFrom "Redeemer decoded successfully" r)
        (tracedUnsafeFrom "Script context decoded successfully" sc)

check :: Bool -> ()
check True = ()
check False = P.error ()

instance IsScriptContext PV1.ScriptContext
instance IsScriptContext PV2.ScriptContext
