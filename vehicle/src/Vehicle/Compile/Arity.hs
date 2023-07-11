module Vehicle.Compile.Arity where

import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Compile.Type.Subsystem.Standard.Type
import Vehicle.Expr.Normalised

type Arity = Int

arityFromVType :: VType builtin -> Arity
arityFromVType = \case
  VPi _ r -> 1 + arityFromVType r
  _ -> 0

builtinArity :: StandardBuiltin -> Arity
builtinArity = unsafeArityFromType . typeStandardBuiltin mempty
  where
    -- This is only safe because typing a builtin is guaranteed to
    -- return a normalised type.
    unsafeArityFromType :: StandardType -> Arity
    unsafeArityFromType = \case
      Pi _ _ r -> 1 + unsafeArityFromType r
      _ -> 0

lamArity :: Expr var builtin -> Arity
lamArity = \case
  Lam _ _ body -> 1 + lamArity body
  _ -> 0

vlamArity :: Value builtin -> Arity
vlamArity = \case
  VLam _ _ body -> 1 + lamArity body
  _ -> 0
