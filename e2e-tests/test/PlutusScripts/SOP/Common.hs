{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module PlutusScripts.SOP.Common where

import Cardano.Api qualified as C
import PlutusScripts.Helpers (
  toScriptData,
 )
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

data SOPRedeemer
  = Sum1 Integer
  | Sum2 Integer Integer
  | Sum3 Integer Integer Integer

PlutusTx.unstableMakeIsData ''SOPRedeemer
PlutusTx.makeLift ''SOPRedeemer

{-# INLINEABLE mkSopPolicyV3 #-}
mkSopPolicyV3 :: SOPRedeemer -> P.BuiltinUnit
mkSopPolicyV3 redeemer =
  case redeemer of
    Sum1 a ->
      P.check (a P.== 1)
    Sum2 a b ->
      P.check (a P.== 1 P.&& b P.== 2)
    Sum3 a b c ->
      P.check (a P.== 1 P.&& b P.== 2 P.&& c P.== 3)

sopAssetName :: C.AssetName
sopAssetName = C.AssetName "sop"

sopRedeemer1 :: C.HashableScriptData
sopRedeemer1 = toScriptData (Sum1 1)

sopRedeemer2 :: C.HashableScriptData
sopRedeemer2 = toScriptData (Sum2 1 2)

sopRedeemer3 :: C.HashableScriptData
sopRedeemer3 = toScriptData (Sum3 1 2 3)

sopRedeemerFail :: C.HashableScriptData
sopRedeemerFail = toScriptData (Sum3 1 2 4)
