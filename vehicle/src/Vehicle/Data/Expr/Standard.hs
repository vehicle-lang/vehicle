module Vehicle.Data.Expr.Standard where

import Control.Monad.Identity (Identity (..))
import Control.Monad.Writer (MonadWriter (..), execWriter)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Prettyprinter (Doc)
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.DeBruijn (Ix (..), substDBInto)
import Vehicle.Prelude (layoutAsText)
import Vehicle.Syntax.AST as X

-----------------------------------------------------------------------------
-- Traversing builtins

-- | Function for updating a builtin application
type BuiltinUpdate m var builtin1 builtin2 =
  Provenance -> builtin1 -> [Arg var builtin2] -> m (Expr var builtin2)

-- | Traverses all the auxiliary type arguments in the provided element,
-- applying the provided update function when it finds them (or a space
-- where they should be).
traverseBuiltinsM ::
  (Monad m) =>
  BuiltinUpdate m var builtin1 builtin2 ->
  Expr var builtin1 ->
  m (Expr var builtin2)
traverseBuiltinsM f expr = case expr of
  Builtin p b -> f p b []
  App (Builtin p b) args -> do
    args' <- traverse (traverseBuiltinsArg f) args
    f p b (NonEmpty.toList args')
  App fun args -> App <$> traverseBuiltinsM f fun <*> traverse (traverseBuiltinsArg f) args
  Pi p binder res -> Pi p <$> traverseBuiltinsBinder f binder <*> traverseBuiltinsM f res
  Let p bound binder body -> Let p <$> traverseBuiltinsM f bound <*> traverseBuiltinsBinder f binder <*> traverseBuiltinsM f body
  Lam p binder body -> Lam p <$> traverseBuiltinsBinder f binder <*> traverseBuiltinsM f body
  Universe p u -> return $ Universe p u
  FreeVar p v -> return $ FreeVar p v
  BoundVar p v -> return $ BoundVar p v
  Hole p n -> return $ Hole p n
  Meta p m -> return $ Meta p m

traverseBuiltinsArg :: (Monad m) => BuiltinUpdate m var builtin1 builtin2 -> Arg var builtin1 -> m (Arg var builtin2)
traverseBuiltinsArg f = traverse (traverseBuiltinsM f)

traverseBuiltinsBinder :: (Monad m) => BuiltinUpdate m var builtin1 builtin2 -> Binder var builtin1 -> m (Binder var builtin2)
traverseBuiltinsBinder f = traverse (traverseBuiltinsM f)

mapBuiltins ::
  (Provenance -> builtin1 -> [Arg var builtin2] -> Expr var builtin2) ->
  Expr var builtin1 ->
  Expr var builtin2
mapBuiltins f e = runIdentity (traverseBuiltinsM (\p b args -> return $ f p b args) e)

-----------------------------------------------------------------------------
-- Traversing free variables

-- | Function for updating a free variable application
type FreeVarUpdate m var builtin =
  (Expr var builtin -> m (Expr var builtin)) ->
  Provenance ->
  Identifier ->
  [Arg var builtin] ->
  m (Expr var builtin)

-- | Traverses all the free variables in the provided element,
-- applying the provided update function when it finds them (or a space
-- where they should be).
traverseFreeVarsM ::
  forall m var builtin.
  (Monad m) =>
  (Binder var builtin -> m (Expr var builtin) -> m (Expr var builtin)) ->
  FreeVarUpdate m var builtin ->
  Expr var builtin ->
  m (Expr var builtin)
traverseFreeVarsM underBinder processFreeVar = go
  where
    go :: Expr var builtin -> m (Expr var builtin)
    go expr = case expr of
      FreeVar p ident -> do
        processFreeVar go p ident mempty
      App (FreeVar p ident) args -> do
        processFreeVar go p ident (NonEmpty.toList args)
      App fun args -> do
        fun' <- go fun
        args' <- traverse (traverse go) args
        return $ App fun' args'
      BoundVar {} -> return expr
      Universe {} -> return expr
      Meta {} -> return expr
      Hole {} -> return expr
      Builtin {} -> return expr
      Pi p binder res -> do
        binder' <- traverse go binder
        res' <- underBinder binder' (go res)
        return $ Pi p binder' res'
      Lam p binder body -> do
        binder' <- traverse go binder
        body' <- underBinder binder' (go body)
        return $ Lam p binder' body'
      Let p bound binder body -> do
        bound' <- go bound
        binder' <- traverse go binder
        body' <- underBinder binder' (go body)
        return $ Let p bound' binder' body'

freeVarsIn :: Expr var builtin -> Set Identifier
freeVarsIn =
  execWriter
    . traverseFreeVarsM
      (const id)
      ( \recGo p i args -> do
          args' <- traverse (traverse recGo) args
          tell $ Set.singleton i
          return $ normAppList (FreeVar p i) args'
      )

substArgs :: Expr Ix builtin -> [Arg Ix builtin] -> Expr Ix builtin
substArgs (Lam _ _ body) (arg : args) = do
  substArgs (argExpr arg `substDBInto` body) args
substArgs e args = normAppList e args

-----------------------------------------------------------------------------
-- Builtins

-- | Use to convert builtins for printing that have no representation in the
-- standard `Builtin` type.
cheatConvertBuiltin :: Provenance -> Doc a -> Expr var builtin
cheatConvertBuiltin p b = FreeVar p $ Identifier StdLib (layoutAsText b)

convertExprBuiltins ::
  forall builtin1 builtin2 var.
  (ConvertableBuiltin builtin1 builtin2, Show var) =>
  Expr var builtin1 ->
  Expr var builtin2
convertExprBuiltins = mapBuiltins $ \p b args ->
  normAppList (convertBuiltin p b) args
