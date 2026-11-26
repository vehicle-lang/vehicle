module Vehicle.Data.Builtin.Interface.Print where

import Data.Maybe (isJust)
import Vehicle.Data.AST.Expr.Scoped (Arg, Expr (..), mapBuiltins, normAppList, pattern App)
import Vehicle.Data.Builtin.Standard.Core
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

  -- | An explanation, specific to the type class, of why there is no instance
  -- of the type class for the provided arguments, if there is a more helpful
  -- one than the generic explanation. This allows the most common mistakes
  -- (e.g. a vector literal with the wrong number of elements) to be explained
  -- in terms of what the user wrote, rather than in terms of the instances.
  explainInstanceFailure ::
    -- | Prints an expression for the user.
    (Expr builtin -> Doc a) ->
    -- | The overloaded expression, as written by the user.
    Doc a ->
    -- | The type class.
    Either Identifier builtin ->
    -- | The arguments of the type class, which are fully normalised.
    [Arg builtin] ->
    Maybe (Doc a)

isCoercionExpr :: (PrintableBuiltin builtin) => Expr builtin -> Bool
isCoercionExpr = \case
  Builtin _ b -> isJust $ coercionArgs b
  App (Builtin _ b) _ -> isJust $ coercionArgs b
  _ -> False

-- | Use to convert builtins for printing that have no representation in the
-- standard `Builtin` type.
cheatConvertBuiltin :: Provenance -> Doc a -> Expr builtin
cheatConvertBuiltin p b = FreeVar p $ stdlibIdentifier $ layoutAsText b
