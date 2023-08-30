module Vehicle.Compile.Prelude
  ( module X,
    module Vehicle.Compile.Prelude,
    Ix (..),
    Lv (..),
  )
where

import Control.Monad.Identity (Identity (..))
import Control.Monad.Writer (MonadWriter (..), execWriter)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Vehicle.Compile.Context.Bound.Core as X
import Vehicle.Compile.Context.Free.Core as X
import Vehicle.Compile.Prelude.Utils as X
import Vehicle.Data.DeBruijn (Ix (..), Lv (..), substDBInto)
import Vehicle.Prelude as X
import Vehicle.Prelude.Logging as X
import Vehicle.Resource as X
import Vehicle.Syntax.AST as X
import Vehicle.Syntax.Builtin (Builtin)

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

mapObject :: (a -> b) -> Contextualised a ctx -> Contextualised b ctx
mapObject f WithContext {..} = WithContext {objectIn = f objectIn, ..}

-------------------------------------------------------------------------------
-- Utilities for traversing auxiliary arguments.

-- | Function for updating a builtin application
type BuiltinUpdate m var builtin1 builtin2 =
  Provenance -> Provenance -> builtin1 -> [Arg var builtin2] -> m (Expr var builtin2)

-- | Traverses all the auxiliary type arguments in the provided element,
-- applying the provided update function when it finds them (or a space
-- where they should be).
traverseBuiltinsM ::
  (Monad m) =>
  BuiltinUpdate m var builtin1 builtin2 ->
  Expr var builtin1 ->
  m (Expr var builtin2)
traverseBuiltinsM f expr = case expr of
  Builtin p b -> f p p b []
  App p1 (Builtin p2 b) args -> do
    args' <- traverse (traverseBuiltinsArg f) args
    f p1 p2 b (NonEmpty.toList args')
  App p fun args -> App p <$> traverseBuiltinsM f fun <*> traverse (traverseBuiltinsArg f) args
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
  (Provenance -> Provenance -> builtin1 -> [Arg var builtin2] -> Expr var builtin2) ->
  Expr var builtin1 ->
  Expr var builtin2
mapBuiltins f e = runIdentity (traverseBuiltinsM (\p1 p2 b args -> return $ f p1 p2 b args) e)

-- | Function for updating a free variable application
type FreeVarUpdate m var builtin =
  (Expr var builtin -> m (Expr var builtin)) ->
  Provenance ->
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
        processFreeVar go p p ident mempty
      App p1 (FreeVar p2 ident) args -> do
        processFreeVar go p1 p2 ident (NonEmpty.toList args)
      App p fun args -> do
        fun' <- go fun
        args' <- traverse (traverse go) args
        return $ App p fun' args'
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
      ( \recGo p1 p2 i args -> do
          args' <- traverse (traverse recGo) args
          tell $ Set.singleton i
          return $ normAppList p1 (FreeVar p2 i) args'
      )

substArgs :: Provenance -> Expr Ix builtin -> [Arg Ix builtin] -> Expr Ix builtin
substArgs p (Lam _ _ body) (arg : args) = do
  substArgs p (argExpr arg `substDBInto` body) args
substArgs p e args = normAppList p e args

type Imports = [Prog Ix Builtin]

mergeImports :: Imports -> Prog Ix Builtin -> Prog Ix Builtin
mergeImports imports userProg = Main $ concatMap (\(Main ds) -> ds) (imports <> [userProg])
