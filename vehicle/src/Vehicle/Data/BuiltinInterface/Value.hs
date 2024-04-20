module Vehicle.Data.BuiltinInterface.Value where

import Vehicle.Data.BuiltinInterface
import Vehicle.Data.NormalisedExpr
import Vehicle.Syntax.Builtin
import Prelude hiding (pi)

--------------------------------------------------------------------------------
-- WHNFValue constructors patterns

pattern VConstructor :: (HasStandardData builtin) => BuiltinConstructor -> Spine strategy builtin -> Value strategy builtin
pattern VConstructor c spine <- VBuiltin (getBuiltinConstructor -> Just c) spine
  where
    VConstructor c spine = VBuiltin (mkBuiltinConstructor c) spine

--------------------------------------------------------------------------------
-- WHNFValue Function patterns

pattern VBuiltinFunction :: (HasStandardData builtin) => BuiltinFunction -> Spine strategy builtin -> Value strategy builtin
pattern VBuiltinFunction f args <- VBuiltin (getBuiltinFunction -> Just f) args
  where
    VBuiltinFunction f args = VBuiltin (mkBuiltinFunction f) args
