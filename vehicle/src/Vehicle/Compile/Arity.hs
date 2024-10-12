module Vehicle.Compile.Arity where

import Vehicle.Data.Code.Expr
import Vehicle.Data.Code.Value
import Vehicle.Prelude (isExplicit)

type Arity = Int

class HasArity a where
  arityOf :: a -> Arity

arityFromVType :: Value closure builtin -> Arity
arityFromVType = \case
  VPi _ r -> 1 + arityFromVType r
  _ -> 0

-- | This is only safe when the type is known to be in normalised type.
explicitArityFromType :: Type builtin -> Arity
explicitArityFromType = \case
  Pi _ binder r
    | isExplicit binder -> 1 + explicitArityFromType r
    | otherwise -> explicitArityFromType r
  _ -> 0

lamArity :: Expr builtin -> Arity
lamArity = \case
  Lam _ _ body -> 1 + lamArity body
  _ -> 0
