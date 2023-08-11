module Vehicle.Compile.Arity where

import Vehicle.Compile.Prelude
import Vehicle.Expr.Normalised

type Arity = Int

arityFromVType :: VType builtin -> Arity
arityFromVType = \case
  VPi _ r -> 1 + arityFromVType r
  _ -> 0

-- | This is only safe when the type is known to be in normalised type.
explicitArityFromType :: Type var builtin -> Arity
explicitArityFromType = \case
  Pi _ binder r
    | isExplicit binder -> 1 + explicitArityFromType r
    | otherwise -> explicitArityFromType r
  _ -> 0

lamArity :: Expr var builtin -> Arity
lamArity = \case
  Lam _ _ body -> 1 + lamArity body
  _ -> 0
