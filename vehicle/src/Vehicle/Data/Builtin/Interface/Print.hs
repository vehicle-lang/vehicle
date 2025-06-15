module Vehicle.Data.Builtin.Interface.Print where

import Data.Maybe (isJust)
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Code.Expr (Arg, Expr (..), mapBuiltins, normAppList, pattern App)
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Conversion

class ConvertableBuiltin builtin1 builtin2 where
  convertBuiltin :: Provenance -> builtin1 -> Expr builtin2

instance ConvertableBuiltin builtin builtin where
  convertBuiltin = Builtin

instance ConvertableBuiltin BuiltinType Builtin where
  convertBuiltin p = Builtin p . BuiltinType

instance ConvertableBuiltin TypeClassOp Builtin where
  convertBuiltin p = Builtin p . TypeClassOp

instance ConvertableBuiltin BuiltinConstructor Builtin where
  convertBuiltin p = Builtin p . BuiltinConstructor

instance ConvertableBuiltin BuiltinFunction Builtin where
  convertBuiltin p = Builtin p . BuiltinFunction

instance ConvertableBuiltin DerivedFunction Builtin where
  convertBuiltin p = Builtin p . DerivedFunction

instance ConvertableBuiltin ComparisonOp Builtin where
  convertBuiltin p = convertBuiltin p . CompareTC

convertExprBuiltins ::
  forall builtin1 builtin2.
  (ConvertableBuiltin builtin1 builtin2) =>
  Expr builtin1 ->
  Expr builtin2
convertExprBuiltins = mapBuiltins $ \p b args ->
  normAppList (convertBuiltin p b) args

--------------------------------------------------------------------------------
-- Printing

class (Show builtin, Pretty builtin, ConvertableBuiltin builtin Builtin) => PrintableBuiltin builtin where
  -- | Convert expressions with the builtin back to expressions with the standard
  -- builtin type. Used for printing.
  coercionArgs :: builtin -> Maybe ([Arg builtin] -> Expr builtin)

  isDerivedBuiltin :: builtin -> Maybe Identifier

isCoercionExpr :: (PrintableBuiltin builtin) => Expr builtin -> Bool
isCoercionExpr = \case
  Builtin _ b -> isJust $ coercionArgs b
  App (Builtin _ b) _ -> isJust $ coercionArgs b
  _ -> False

-- | Use to convert builtins for printing that have no representation in the
-- standard `Builtin` type.
cheatConvertBuiltin :: Provenance -> Doc a -> Expr builtin
cheatConvertBuiltin p b = FreeVar p $ stdlibIdentifier $ layoutAsText b
