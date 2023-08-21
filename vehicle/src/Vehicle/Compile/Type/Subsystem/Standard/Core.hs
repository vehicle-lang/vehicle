{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Subsystem.Standard.Core
  ( module Vehicle.Compile.Type.Subsystem.Standard.Core,
    module Syntax,
  )
where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Vehicle.Compile.Prelude
import Vehicle.Expr.BuiltinInterface
import Vehicle.Syntax.Builtin as Syntax

-----------------------------------------------------------------------------
-- Definition

newtype StandardTypingBuiltin
  = StandardBuiltin Builtin
  deriving (Show, Eq, Ord, Generic)

instance Hashable StandardTypingBuiltin

instance Pretty StandardTypingBuiltin where
  pretty = \case
    StandardBuiltin b -> pretty b

-- ResourceConstraint b -> pretty b

instance HasStandardData StandardTypingBuiltin where
  mkBuiltinConstructor = StandardBuiltin . mkBuiltinConstructor

  getBuiltinConstructor = \case
    StandardBuiltin b -> getBuiltinConstructor b

  -- _ -> Nothing

  mkBuiltinFunction = StandardBuiltin . mkBuiltinFunction

  getBuiltinFunction = \case
    StandardBuiltin b -> getBuiltinFunction b

  -- _ -> Nothing

  isTypeClassOp = \case
    StandardBuiltin b -> isTypeClassOp b

-- _ -> False

instance HasStandardTypes StandardTypingBuiltin where
  mkBuiltinType t = StandardBuiltin (mkBuiltinType t)

  getBuiltinType = \case
    StandardBuiltin (BuiltinType t) -> Just t
    _ -> Nothing

  mkNatInDomainConstraint = StandardBuiltin mkNatInDomainConstraint

instance HasStandardTypeClasses StandardTypingBuiltin where
  mkBuiltinTypeClass t = StandardBuiltin (mkBuiltinTypeClass t)

instance PrintableBuiltin StandardTypingBuiltin where
  isCoercion = \case
    StandardBuiltin b -> isCoercion b

  -- ResourceConstraint {} -> False

  convertBuiltin p = \case
    StandardBuiltin b -> Builtin p b

-- b@ResourceConstraint {} -> cheatConvertBuiltin p b
