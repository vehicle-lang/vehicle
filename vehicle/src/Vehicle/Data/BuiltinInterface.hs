module Vehicle.Data.BuiltinInterface where

import Vehicle.Data.DeBruijn
import Vehicle.Prelude
import Vehicle.Syntax.AST
import Vehicle.Syntax.Builtin
import Prelude hiding (pi)

--------------------------------------------------------------------------------
-- Interface to standard builtins
--------------------------------------------------------------------------------

-- At various points in the compiler, we have different sets of builtins (e.g.
-- first time we type-check we use the standard set of builtins + type +
-- type classes, but when checking polarity and linearity information we
-- subsitute out all the types and type-classes for new types.)
--
-- The interfaces defined in this file allow us to abstract over the exact set
-- of builtins being used, and therefore allows us to define operations
-- (e.g. normalisation) once, rather than once for each builtin type.

--------------------------------------------------------------------------------
-- HasStandardData

-- | Indicates that this set of builtins has the standard builtin constructors
-- and functions.
class (Show builtin) => HasStandardData builtin where
  mkBuiltinConstructor :: BuiltinConstructor -> builtin
  getBuiltinConstructor :: builtin -> Maybe BuiltinConstructor

  mkBuiltinFunction :: BuiltinFunction -> builtin
  getBuiltinFunction :: builtin -> Maybe BuiltinFunction

  isTypeClassOp :: builtin -> Bool

instance HasStandardData Builtin where
  mkBuiltinFunction = BuiltinFunction
  getBuiltinFunction = \case
    BuiltinFunction c -> Just c
    _ -> Nothing

  mkBuiltinConstructor = BuiltinConstructor
  getBuiltinConstructor = \case
    BuiltinConstructor c -> Just c
    _ -> Nothing

  isTypeClassOp = \case
    TypeClassOp {} -> True
    _ -> False

--------------------------------------------------------------------------------
-- HasStandardTypes

-- | Indicates that this set of builtins has the standard set of types.
class HasStandardTypes builtin where
  mkBuiltinType :: BuiltinType -> builtin
  getBuiltinType :: builtin -> Maybe BuiltinType

  mkNatInDomainConstraint :: builtin

instance HasStandardTypes Builtin where
  mkBuiltinType = BuiltinType
  getBuiltinType = \case
    BuiltinType c -> Just c
    _ -> Nothing

  mkNatInDomainConstraint = NatInDomainConstraint

--------------------------------------------------------------------------------
-- HasStandardBuiltins

-- | Indicates that this set of builtins has the standard set of constructors,
-- functions and types.
class HasStandardTypeClasses builtin where
  mkBuiltinTypeClass :: TypeClass -> builtin

instance HasStandardTypeClasses Builtin where
  mkBuiltinTypeClass = TypeClass

--------------------------------------------------------------------------------
-- HasStandardBuiltins

-- | Indicates that this set of builtins has the standard set of constructors,
-- functions and types.
type HasStandardBuiltins builtin =
  ( HasStandardTypes builtin,
    HasStandardData builtin
  )

--------------------------------------------------------------------------------
-- Printing builtins

class (Show builtin, Eq builtin) => PrintableBuiltin builtin where
  -- | Convert expressions with the builtin back to expressions with the standard
  -- builtin type. Used for printing.
  convertBuiltin ::
    Provenance ->
    builtin ->
    Expr var Builtin

  isCoercion ::
    builtin ->
    Bool

instance PrintableBuiltin Builtin where
  convertBuiltin = Builtin

  isCoercion = \case
    BuiltinFunction FromNat {} -> True
    BuiltinFunction FromRat {} -> True
    TypeClassOp FromNatTC {} -> True
    TypeClassOp FromRatTC {} -> True
    TypeClassOp FromVecTC {} -> True
    _ -> False

-- | Use to convert builtins for printing that have no representation in the
-- standard `Builtin` type.
cheatConvertBuiltin :: Provenance -> Doc a -> Expr var builtin
cheatConvertBuiltin p b = FreeVar p $ Identifier StdLib (layoutAsText b)

--------------------------------------------------------------------------------
-- Typable builtin

class (PrintableBuiltin builtin) => TypableBuiltin builtin where
  -- | Construct a type for the builtin
  typeBuiltin ::
    Provenance -> builtin -> Type Ix builtin
