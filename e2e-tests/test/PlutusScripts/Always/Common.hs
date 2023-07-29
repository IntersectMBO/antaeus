module PlutusScripts.Always.Common where

import PlutusLedgerApi.V1 (BuiltinData)
import PlutusTx.Prelude qualified as P

-- AlwaysSucceeds minting policy --

mkAlwaysSucceedPolicy :: BuiltinData -> BuiltinData -> ()
mkAlwaysSucceedPolicy _datum _sc = ()

-- AlwaysSucceeds validator --

mkAlwaysSucceedSpend :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkAlwaysSucceedSpend _datum _redeemer _sc = ()

-- AlwaysFails minting policy --

mkAlwaysFailsPolicy :: BuiltinData -> BuiltinData -> ()
mkAlwaysFailsPolicy _datum _sc = P.error ()
