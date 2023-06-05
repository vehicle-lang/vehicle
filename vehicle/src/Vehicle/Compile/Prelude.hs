module Vehicle.Compile.Prelude
  ( module X,
    module Vehicle.Compile.Prelude,
  )
where

import Control.Monad.Identity (Identity (..))
import Data.List.NonEmpty qualified as NonEmpty
import Vehicle.Compile.Prelude.Contexts as X
import Vehicle.Compile.Prelude.Utils as X
import Vehicle.Prelude as X
import Vehicle.Resource as X
import Vehicle.Syntax.AST as X

--------------------------------------------------------------------------------
-- Type synonyms

type DeclProvenance = (Identifier, Provenance)

--------------------------------------------------------------------------------
-- Other

data Contextualised object context = WithContext
  { objectIn :: object,
    contextOf :: context
  }
  deriving (Show)

type family WithContext a

class HasType expr typ | expr -> typ where
  typeOf :: expr -> typ

instance HasType (GenericBinder expr) expr where
  typeOf = binderType

instance HasType (GenericDecl expr) expr where
  typeOf = \case
    DefAbstract _ _ _ t -> t
    DefFunction _ _ _ t _ -> t

mapObject :: (a -> b) -> Contextualised a ctx -> Contextualised b ctx
mapObject f WithContext {..} = WithContext {objectIn = f objectIn, ..}

-------------------------------------------------------------------------------
-- Utilities for traversing auxiliary arguments.

-- | Function for updating an auxiliary argument (which may be missing)
type BuiltinUpdate m binder var builtin1 builtin2 =
  Provenance -> Provenance -> builtin1 -> [Arg var builtin2] -> m (Expr var builtin2)

-- | Traverses all the auxiliary type arguments in the provided element,
-- applying the provided update function when it finds them (or a space
-- where they should be).
traverseBuiltinsM ::
  (Monad m) =>
  BuiltinUpdate m binder var builtin1 builtin2 ->
  Expr var builtin1 ->
  m (Expr var builtin2)
traverseBuiltinsM f expr = case expr of
  Builtin p b -> f p p b []
  App p1 (Builtin p2 b) args -> do
    args' <- traverse (traverseBuiltinsArg f) args
    f p1 p2 b (NonEmpty.toList args')
  Ann p e t -> Ann p <$> traverseBuiltinsM f e <*> traverseBuiltinsM f t
  App p fun args -> App p <$> traverseBuiltinsM f fun <*> traverse (traverseBuiltinsArg f) args
  Pi p binder res -> Pi p <$> traverseBuiltinsBinder f binder <*> traverseBuiltinsM f res
  Let p bound binder body -> Let p <$> traverseBuiltinsM f bound <*> traverseBuiltinsBinder f binder <*> traverseBuiltinsM f body
  Lam p binder body -> Lam p <$> traverseBuiltinsBinder f binder <*> traverseBuiltinsM f body
  Universe p u -> return $ Universe p u
  FreeVar p v -> return $ FreeVar p v
  BoundVar p v -> return $ BoundVar p v
  Hole p n -> return $ Hole p n
  Meta p m -> return $ Meta p m

traverseBuiltinsArg :: (Monad m) => BuiltinUpdate m binder var builtin1 builtin2 -> Arg var builtin1 -> m (Arg var builtin2)
traverseBuiltinsArg f = traverse (traverseBuiltinsM f)

traverseBuiltinsBinder :: (Monad m) => BuiltinUpdate m binder var builtin1 builtin2 -> Binder var builtin1 -> m (Binder var builtin2)
traverseBuiltinsBinder f = traverse (traverseBuiltinsM f)

traverseBuiltins ::
  (Provenance -> Provenance -> builtin1 -> [Arg var builtin2] -> Expr var builtin2) ->
  Expr var builtin1 ->
  Expr var builtin2
traverseBuiltins f e = runIdentity (traverseBuiltinsM (\p1 p2 b args -> return $ f p1 p2 b args) e)
