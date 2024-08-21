{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Expr.Standard where

import Control.Monad.Identity (Identity (..))
import Control.Monad.Writer (MonadWriter (..), execWriter)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Prettyprinter (Doc)
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.DeBruijn (Ix (..), substDBInto)
import Vehicle.Data.Expr.Interface
import Vehicle.Prelude (layoutAsText)
import Vehicle.Syntax.AST (Arg, Binder, Expr (..), Identifier, Provenance, argExpr, getBuiltinApp, getFreeVarApp, normAppList, stdlibIdentifier, pattern BuiltinExpr)

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
cheatConvertBuiltin p b = FreeVar p $ stdlibIdentifier (layoutAsText b)

convertExprBuiltins ::
  forall builtin1 builtin2 var.
  (ConvertableBuiltin builtin1 builtin2, Show var) =>
  Expr var builtin1 ->
  Expr var builtin2
convertExprBuiltins = mapBuiltins $ \p b args ->
  normAppList (convertBuiltin p b) args

-----------------------------------------------------------------------------
-- Instances

instance (BuiltinHasStandardTypes builtin) => HasStandardTypes (Expr var builtin) where
  mkType p b = normAppList (Builtin p (mkBuiltinType b))
  getType e = case getBuiltinApp e of
    Just (p, b, args) -> case getBuiltinType b of
      Just t -> Just (p, t, args)
      Nothing -> Nothing
    _ -> Nothing

instance (BuiltinHasStandardData builtin) => HasStandardData (Expr var builtin) where
  mkFunction p b = normAppList (Builtin p (mkBuiltinFunction b))
  getFunction e = case getBuiltinApp e of
    Just (p, b, args) -> case getBuiltinFunction b of
      Just f -> Just (p, f, args)
      Nothing -> Nothing
    _ -> Nothing

  mkConstructor p b = normAppList (Builtin p (mkBuiltinConstructor b))
  getConstructor e = case getBuiltinApp e of
    Just (p, b, args) -> case getBuiltinConstructor b of
      Just f -> Just (p, f, args)
      Nothing -> Nothing
    _ -> Nothing

  mkFreeVar p ident = normAppList (FreeVar p ident)
  getFreeVar e = case getFreeVarApp e of
    Just (p, ident, args) -> Just (p, ident, args)
    _ -> Nothing

  getTypeClassOp e = case getBuiltinApp e of
    Just (p, b, args) -> case getBuiltinTypeClassOp b of
      Just f -> Just (p, f, args)
      Nothing -> Nothing
    _ -> Nothing

instance (BuiltinHasBoolLiterals builtin) => HasBoolLits (Expr var builtin) where
  getBoolLit e = case e of
    Builtin _ (getBoolBuiltinLit -> Just b) -> Just (mempty, b)
    _ -> Nothing
  mkBoolLit p x = Builtin p (mkBoolBuiltinLit x)

instance (BuiltinHasIndexLiterals builtin) => HasIndexLits (Expr var builtin) where
  getIndexLit e = case e of
    Builtin _ (getIndexBuiltinLit -> Just i) -> Just (mempty, i)
    _ -> Nothing
  mkIndexLit p i = Builtin p (mkIndexBuiltinLit i)

instance (BuiltinHasNatLiterals builtin) => HasNatLits (Expr var builtin) where
  getNatLit e = case e of
    Builtin _ (getNatBuiltinLit -> Just n) -> Just (mempty, n)
    _ -> Nothing
  mkNatLit p n = Builtin p (mkNatBuiltinLit n)

instance (BuiltinHasRatLiterals builtin) => HasRatLits (Expr var builtin) where
  getRatLit e = case e of
    Builtin _ (getRatBuiltinLit -> Just r) -> Just (mempty, r)
    _ -> Nothing
  mkRatLit p r = Builtin p (mkRatBuiltinLit r)

instance (BuiltinHasVecLiterals builtin) => HasStandardVecLits (Expr var builtin) where
  getHomoVector = \case
    BuiltinExpr _ (getVecBuiltinLit -> Just {}) (t :| xs) -> Just (t, xs)
    _ -> Nothing
  mkHomoVector t xs = BuiltinExpr mempty (mkVecBuiltinLit (length xs)) (t :| xs)

instance (BuiltinHasListLiterals builtin) => HasStandardListLits (Expr var builtin) where
  getNil = \case
    BuiltinExpr p (isBuiltinNil -> True) [t] -> Just (p, t)
    _ -> Nothing
  mkNil t = BuiltinExpr mempty mkBuiltinNil [t]

  getCons = \case
    BuiltinExpr p (isBuiltinCons -> True) [t, x, xs] -> Just (p, t, x, xs)
    _ -> Nothing
  mkCons t x xs = BuiltinExpr mempty mkBuiltinCons [t, x, xs]
