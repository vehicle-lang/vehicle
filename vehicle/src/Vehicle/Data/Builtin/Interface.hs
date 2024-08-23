module Vehicle.Data.Builtin.Interface where

import Vehicle.Data.DeBruijn
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
-- Converting builtins

class ConvertableBuiltin builtin1 builtin2 where
  convertBuiltin ::
    Provenance ->
    builtin1 ->
    Expr var builtin2

instance ConvertableBuiltin builtin builtin where
  convertBuiltin = Builtin

--------------------------------------------------------------------------------
-- Printing builtins

class (Show builtin, ConvertableBuiltin builtin Builtin) => PrintableBuiltin builtin where
  -- | Convert expressions with the builtin back to expressions with the standard
  -- builtin type. Used for printing.
  isCoercion :: builtin -> Bool

--------------------------------------------------------------------------------
-- Typable builtin

class (PrintableBuiltin builtin) => TypableBuiltin builtin where
  -- | Construct a type for the builtin
  typeBuiltin :: Provenance -> builtin -> Type Ix builtin

--------------------------------------------------------------------------------
-- Interface to content of standard builtins
--------------------------------------------------------------------------------
-- In these classes we need to separate out the types from the literals, as
-- various sets of builtins may have the literals but not the types (e.g.
-- `LinearityBuiltin`)
--------------------------------------------------------------------------------
-- HasBool

class BuiltinHasBoolLiterals builtin where
  mkBoolBuiltinLit :: Bool -> builtin
  getBoolBuiltinLit :: builtin -> Maybe Bool

--------------------------------------------------------------------------------
-- HasIndex

class BuiltinHasIndexLiterals builtin where
  mkIndexBuiltinLit :: Int -> builtin
  getIndexBuiltinLit :: builtin -> Maybe Int

--------------------------------------------------------------------------------
-- HasNat

class BuiltinHasNatLiterals builtin where
  mkNatBuiltinLit :: Int -> builtin
  getNatBuiltinLit :: builtin -> Maybe Int

--------------------------------------------------------------------------------
-- HasRat

class BuiltinHasRatLiterals builtin where
  mkRatBuiltinLit :: Rational -> builtin
  getRatBuiltinLit :: builtin -> Maybe Rational

class (BuiltinHasRatLiterals builtin) => HasRatTypeBuiltin builtin where
  mkRatBuiltinType :: builtin
  isRatBuiltinType :: builtin -> Bool

--------------------------------------------------------------------------------
-- HasList

class BuiltinHasListLiterals builtin where
  mkBuiltinNil :: builtin
  isBuiltinNil :: builtin -> Bool

  mkBuiltinCons :: builtin
  isBuiltinCons :: builtin -> Bool

--------------------------------------------------------------------------------
-- HasVector

class BuiltinHasVecLiterals builtin where
  mkVecBuiltinLit :: Int -> builtin
  getVecBuiltinLit :: builtin -> Maybe Int

class (BuiltinHasVecLiterals builtin) => HasVecTypeBuiltin builtin where
  mkVecBuiltinType :: builtin
  isVecBuiltinType :: builtin -> Bool

--------------------------------------------------------------------------------
-- BuiltinHasStandardData

-- | Indicates that this set of builtins has the standard builtin constructors
-- and functions.
class BuiltinHasStandardData builtin where
  mkBuiltinConstructor :: BuiltinConstructor -> builtin
  getBuiltinConstructor :: builtin -> Maybe BuiltinConstructor

  mkBuiltinFunction :: BuiltinFunction -> builtin
  getBuiltinFunction :: builtin -> Maybe BuiltinFunction

  getBuiltinTypeClassOp :: builtin -> Maybe TypeClassOp

  isTypeClassOp :: builtin -> Bool
  isTypeClassOp b = case getBuiltinTypeClassOp b of
    Just {} -> True
    Nothing -> False

--------------------------------------------------------------------------------
-- BuiltinHasStandardTypes

-- | Indicates that this set of builtins has the standard set of types.
class BuiltinHasStandardTypes builtin where
  mkBuiltinType :: BuiltinType -> builtin
  getBuiltinType :: builtin -> Maybe BuiltinType

  mkNatInDomainConstraint :: builtin

--------------------------------------------------------------------------------
-- HasStandardBuiltins

-- | Indicates that this set of builtins has the standard set of constructors,
-- functions and types.
class BuiltinHasStandardTypeClasses builtin where
  mkBuiltinTypeClass :: TypeClass -> builtin

--------------------------------------------------------------------------------
-- HasStandardBuiltins

-- | Indicates that this set of builtins has the standard set of constructors,
-- functions and types.
type HasStandardBuiltins builtin =
  ( BuiltinHasStandardTypes builtin,
    BuiltinHasStandardData builtin
  )
