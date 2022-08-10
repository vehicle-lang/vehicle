{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Elaborate.External
  ( elabProg
  , elabExpr
  ) where

import Control.Monad.Except (throwError, foldM)
import Data.Text (unpack)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Bitraversable (bitraverse)
import Text.Read (readMaybe)

import Vehicle.External.Abs qualified as B

import Vehicle.Prelude
import Vehicle.Language.Sugar
import Vehicle.Compile.Prelude qualified as V
import Vehicle.Compile.Error

--------------------------------------------------------------------------------
-- Public interface

-- | We elaborate from the simple AST generated automatically by BNFC to our
-- more complicated internal version of the AST.
elabProg :: MonadCompile m => B.Prog -> m V.InputProg
elabProg = elab

elabExpr :: MonadCompile m => B.Expr -> m V.InputExpr
elabExpr = elab

--------------------------------------------------------------------------------
-- Algorithm

class Elab vf vc where
  elab :: MonadCompile m => vf -> m vc

instance Elab B.Prog V.InputProg where
  elab (B.Main decls) = V.Main <$> elab decls

instance Elab [B.Decl] [V.InputDecl] where
  elab [] = return []
  elab (decl : decls) = do
    (d', ds) <- elabDeclGroup (decl :| decls)
    ds' <- elab ds
    return $ d' : ds'

elabDeclGroup :: MonadCompile m => NonEmpty B.Decl -> m (V.InputDecl, [B.Decl])
elabDeclGroup dx = do
  logDebug MaxDetail (pretty $ show dx); case dx of
    -- Type definition.
    B.DefType n bs t :| ds -> do
      d' <- elabTypeDef n bs t
      return (d', ds)

    -- Function declaration and body.
    B.DefFunType typeName _ t :| B.DefFunExpr exprName bs e : ds -> do
      d' <- elabDefFun Nothing typeName exprName t bs e
      return (d', ds)

    -- Function body without a declaration.
    B.DefFunExpr n bs e :| ds -> do
      binders <- traverse elab bs
      let unknownType = constructUnknownDefType n binders
      d' <- unfoldDefFun (mkAnn n) Nothing <$> elab n <*> pure unknownType <*> pure binders <*> elab e
      return (d', ds)

    -- ERROR: Function declaration with no body
    B.DefFunType n _tk _t :| _ ->
      throwError $ FunctionNotGivenBody (V.tkProvenance n) (tkSymbol n)

    -- Property declaration.
    B.DefAnn a@B.Property{} opts :| B.DefFunType typeName _tk t : B.DefFunExpr exprName bs e : ds -> do
      checkNoAnnotationOptions a opts
      d' <- elabDefFun (Just V.emptyPropertyInfo) typeName exprName t bs e
      return (d', ds)

    -- ERROR: Property with no body
    B.DefAnn B.Property{} _ :| B.DefFunType n _ _ : _ ->
      throwError $ PropertyNotGivenBody (V.tkProvenance n) (tkSymbol n)

    -- ERROR: Other annotation with body
    B.DefAnn a _ :| B.DefFunType typeName _ _ : B.DefFunExpr bodyName _ _ : _
      | tkSymbol typeName == tkSymbol bodyName ->
        throwError $ ResourceGivenBody (V.tkProvenance typeName) (annotationSymbol a) (tkSymbol typeName)

    --Network declaration.
    B.DefAnn a@B.Network{} opts :| B.DefFunType n _ t : ds -> do
      checkNoAnnotationOptions a opts
      d' <- elabResource n t V.Network
      return (d', ds)

    -- Dataset declaration.
    B.DefAnn a@B.Dataset{} opts :| B.DefFunType n _ t : ds -> do
      checkNoAnnotationOptions a opts
      d' <- elabResource n t V.Dataset
      return (d', ds)

    -- Parameter declaration.
    (B.DefAnn B.Parameter{} opts :| B.DefFunType n _ t : ds) -> do
      r <- elabParameterOptions opts
      d' <- elabResource n t r
      return (d', ds)

    (B.DefAnn a _ :| _) ->
      throwError $ AnnotationWithNoDeclaration (annotationProvenance a) (annotationSymbol a)

annotationSymbol :: B.DeclAnnName -> Symbol
annotationSymbol = \case
  B.Network   tk -> tkSymbol tk
  B.Dataset   tk -> tkSymbol tk
  B.Parameter tk -> tkSymbol tk
  B.Property  tk -> tkSymbol tk

annotationProvenance :: B.DeclAnnName -> V.Provenance
annotationProvenance = \case
  B.Network   tk -> mkAnn tk
  B.Dataset   tk -> mkAnn tk
  B.Parameter tk -> mkAnn tk
  B.Property  tk -> mkAnn tk

checkNoAnnotationOptions :: MonadCompile m => B.DeclAnnName -> B.DeclAnnOpts -> m ()
checkNoAnnotationOptions annName = \case
  B.DeclAnnWithoutOpts      -> return ()
  B.DeclAnnWithOpts []      -> return ()
  B.DeclAnnWithOpts (o : _) -> do
    let B.BooleanOption paramName _ = o
    throwError $ InvalidAnnotationOption (V.tkProvenance paramName) (annotationSymbol annName) (V.tkSymbol paramName) []

elabTypeDef :: MonadCompile m => B.Name -> [B.Binder] -> B.Expr -> m V.InputDecl
elabTypeDef n bs e = unfoldDefType (mkAnn n) <$> elab n <*> traverse elab bs <*> elab e

elabDefFun :: MonadCompile m => Maybe V.PropertyInfo -> B.Name -> B.Name -> B.Expr -> [B.Binder] -> B.Expr -> m V.InputDecl
elabDefFun propertyInfo n1 n2 t bs e
  | tkSymbol n1 /= tkSymbol n2 = throwError $ FunctionWithMismatchedNames (mkAnn n1) (tkSymbol n1) (tkSymbol n2)
  | otherwise = unfoldDefFun (mkAnn n1) propertyInfo <$> elab n1 <*> elab t <*> traverse elab bs <*> elab e

instance Elab B.Expr V.InputExpr where
  elab = \case
    B.Type t                  -> return $ V.TypeUniverse (mkAnn t) (parseTypeLevel t)
    B.Var  n                  -> return $ V.Var  (mkAnn n) (tkSymbol n)
    B.Hole n                  -> return $ V.mkHole (V.tkProvenance n) (tkSymbol n)
    B.Literal l               -> elab l

    B.Ann e tk t              -> op2 V.Ann  tk  (elab e) (elab t)
    B.Fun t1 tk t2            -> op2 V.Pi   tk  (elabFunInputType t1) (elab t2)
    B.VecLiteral tk1 es _tk2  -> elabVecLiteral tk1 es

    B.App e1 e2               -> elabApp e1 e2
    B.Let tk1 ds e            -> unfoldLet (mkAnn tk1) <$> bitraverse (traverse elab) elab (ds, e)
    B.ForallT tk1 ns _tk2 t   -> do checkNonEmpty tk1 ns; unfoldForall (mkAnn tk1) <$> elabBindersAndBody ns t
    B.Lam tk1 ns _tk2 e       -> do checkNonEmpty tk1 ns; unfoldLam    (mkAnn tk1) <$> elabBindersAndBody ns e

    B.Forall    tk1 ns    _tk2 e  -> elabQuantifier   tk1 V.Forall ns e
    B.Exists    tk1 ns    _tk2 e  -> elabQuantifier   tk1 V.Exists ns e
    B.ForallIn  tk1 ns e1 _tk2 e2 -> elabQuantifierIn tk1 V.Forall ns e1 e2
    B.ExistsIn  tk1 ns e1 _tk2 e2 -> elabQuantifierIn tk1 V.Exists ns e1 e2

    B.Foreach   tk1 ns    _tk2 e  -> elabForeach   tk1 ns e

    B.Unit tk                 -> builtin V.Unit   tk []
    B.Bool tk                 -> builtin V.Bool   tk []
    B.Index tk t              -> builtin V.Index  tk [t]
    B.Nat tk                  -> builtin V.Nat    tk []
    B.Int tk                  -> builtin V.Int    tk []
    B.Rat  tk                 -> builtin V.Rat    tk []
    B.List tk t               -> builtin V.List   tk [t]
    B.Vector tk t1 t2         -> builtin V.Vector tk [t1, t2]
    B.Tensor tk t1 t2         -> elabTensor tk t1 t2

    B.Not tk e                -> builtin (V.TypeClassOp V.NotTC)     tk  [e]
    B.Impl e1 tk e2           -> builtin (V.TypeClassOp V.ImpliesTC) tk  [e1, e2]
    B.And e1 tk e2            -> builtin (V.TypeClassOp V.AndTC)     tk  [e1, e2]
    B.Or e1 tk e2             -> builtin (V.TypeClassOp V.OrTC)      tk  [e1, e2]
    B.If tk1 e1 _ e2 _ e3     -> builtin V.If tk1 [e1, e2, e3]

    B.Eq  e1 tk e2            -> builtin (V.TypeClassOp $ V.EqualsTC V.Eq)  tk [e1, e2]
    B.Neq e1 tk e2            -> builtin (V.TypeClassOp $ V.EqualsTC V.Neq) tk [e1, e2]

    B.Le e1 tk e2             -> elabOrder V.Le tk e1 e2
    B.Lt e1 tk e2             -> elabOrder V.Lt tk e1 e2
    B.Ge e1 tk e2             -> elabOrder V.Ge tk e1 e2
    B.Gt e1 tk e2             -> elabOrder V.Gt tk e1 e2

    B.Add e1 tk e2            -> builtin (V.TypeClassOp V.AddTC) tk [e1, e2]
    B.Sub e1 tk e2            -> builtin (V.TypeClassOp V.SubTC) tk [e1, e2]
    B.Mul e1 tk e2            -> builtin (V.TypeClassOp V.MulTC) tk [e1, e2]
    B.Div e1 tk e2            -> builtin (V.TypeClassOp V.DivTC) tk [e1, e2]
    B.Neg tk e                -> builtin (V.TypeClassOp V.NegTC) tk [e]

    B.Nil tk                  -> builtin V.Cons tk []
    B.Cons e1 tk e2           -> builtin V.Cons tk [e1, e2]
    B.At e1 tk e2             -> builtin V.At   tk [e1, e2]
    B.Map tk e1 e2            -> builtin (V.TypeClassOp V.MapTC)  tk [e1, e2]
    B.Fold tk e1 e2 e3        -> builtin (V.TypeClassOp V.FoldTC) tk [e1, e2, e3]

instance Elab B.Arg V.InputArg where
  elab (B.ExplicitArg e) = mkArg V.Explicit <$> elab e
  elab (B.ImplicitArg e) = mkArg V.Implicit <$> elab e
  elab (B.InstanceArg e) = mkArg V.Instance <$> elab e

elabResource :: MonadCompile m => B.Name -> B.Expr -> V.ResourceType -> m V.InputDecl
elabResource n t r = V.DefResource (V.tkProvenance n) r <$> elab n <*> elab t

mkArg :: V.Visibility -> V.InputExpr -> V.InputArg
mkArg v e = V.Arg (V.expandByArgVisibility v (V.provenanceOf e)) v V.Relevant e

instance Elab B.Name V.Identifier where
  elab n = return $ V.Identifier $ tkSymbol n

instance Elab B.Binder V.InputBinder where
  elab (B.ExplicitBinder    n)         = return $ mkBinder n V.Explicit Nothing
  elab (B.ImplicitBinder    n)         = return $ mkBinder n V.Implicit Nothing
  elab (B.InstanceBinder    n)         = return $ mkBinder n V.Instance Nothing
  elab (B.ExplicitBinderAnn n _tk typ) = mkBinder n V.Explicit . Just <$> elab typ
  elab (B.ImplicitBinderAnn n _tk typ) = mkBinder n V.Implicit . Just <$> elab typ
  elab (B.InstanceBinderAnn n _tk typ) = mkBinder n V.Instance . Just <$> elab typ

mkBinder :: B.Name -> V.Visibility -> Maybe V.InputExpr -> V.InputBinder
mkBinder n v e = V.Binder (V.expandByArgVisibility v p) v V.Relevant (Just (tkSymbol n)) t
  where
  (p, t) = case e of
    Nothing  -> (V.tkProvenance n, V.mkHole (V.tkProvenance n) ("typeOf[" <> tkSymbol n <> "]"))
    Just t1  -> (V.fillInProvenance [V.tkProvenance n, V.provenanceOf t1], t1)

instance Elab B.LetDecl (V.InputBinder, V.InputExpr) where
  elab (B.LDecl b e) = bitraverse elab elab (b,e)

instance Elab B.Lit V.InputExpr where
  elab = \case
    B.UnitLiteral   -> return $ V.UnitLiteral mempty
    B.BoolLiteral t -> do
      let p = mkAnn t
      let b = read (unpack $ tkSymbol t)
      return $ V.BoolLiteral p b
    B.NatLiteral   t -> do
      let p = mkAnn t
      let n = readNat (tkSymbol t)
      let fromNat = V.Builtin p (V.TypeClassOp $ V.FromNatTC n)
      return $ app fromNat [V.NatLiteral p n]
    B.RatLiteral   t -> do
      let p = mkAnn t
      let r = readRat (tkSymbol t)
      let fromRat = V.Builtin p (V.TypeClassOp V.FromRatTC)
      return $ app fromRat [V.RatLiteral p r]

elabParameterOptions :: MonadCompile m =>B.DeclAnnOpts -> m V.ResourceType
elabParameterOptions = \case
  B.DeclAnnWithoutOpts -> return V.Parameter
  B.DeclAnnWithOpts opts -> foldM parseOpt V.Parameter opts
  where
  parseOpt :: MonadCompile m => V.ResourceType -> B.DeclAnnOption -> m V.ResourceType
  parseOpt _r (B.BooleanOption nameToken valueToken) = do
    let name = tkSymbol nameToken
    let value = tkSymbol valueToken
    if name /= V.InferableOption
      then throwError $ InvalidAnnotationOption (V.tkProvenance nameToken) "@parameter" name [V.InferableOption]
      else case readMaybe (unpack value) of
          Just True  -> return V.InferableParameter
          Just False -> return V.Parameter
          Nothing    -> throwError $ InvalidAnnotationOptionValue (V.tkProvenance valueToken) name value

parseTypeLevel :: B.TypeToken -> Int
parseTypeLevel s = read (drop 4 (unpack (tkSymbol s)))

op1 :: (MonadCompile m, V.HasProvenance a, IsToken token)
    => (V.Provenance -> a -> b)
    -> token -> m a -> m b
op1 mk t e = do
  ce <- e
  let p = V.fillInProvenance [V.tkProvenance t, V.provenanceOf ce]
  return $ mk p ce

op2 :: (MonadCompile m, V.HasProvenance a, V.HasProvenance b, IsToken token)
    => (V.Provenance -> a -> b -> c)
    -> token -> m a -> m b -> m c

op2 mk t e1 e2 = do
  ce1 <- e1
  ce2 <- e2
  let p = V.fillInProvenance [V.tkProvenance t, V.provenanceOf ce1, V.provenanceOf ce2]
  return $ mk p ce1 ce2

builtin :: (MonadCompile m, IsToken token) => V.Builtin -> token -> [B.Expr] -> m V.InputExpr
builtin b t args = app (V.Builtin (V.tkProvenance t) b) <$> traverse elab args

app :: V.InputExpr -> [V.InputExpr] -> V.InputExpr
app fun argExprs = V.normAppList p' fun args
  where
    p'   = V.fillInProvenance (V.provenanceOf fun : map V.provenanceOf args)
    args = fmap (mkArg V.Explicit) argExprs

elabVecLiteral :: (MonadCompile m, IsToken token) => token -> [B.Expr] -> m V.InputExpr
elabVecLiteral tk xs = do
  let n = length xs
  let p = V.tkProvenance tk
  xs' <- op1 V.LVec tk (traverse elab xs)
  return $ app (V.Builtin p (V.TypeClassOp $ V.FromVecTC n)) [xs']

elabFunInputType :: MonadCompile m => B.Expr -> m V.InputBinder
elabFunInputType t = do
  t' <- elab t
  return $ V.ExplicitBinder (V.provenanceOf t') Nothing t'

elabApp :: MonadCompile m => B.Expr -> B.Arg -> m V.InputExpr
elabApp fun arg = do
  fun' <- elab fun
  arg' <- elab arg
  let p = V.fillInProvenance [V.provenanceOf fun', V.provenanceOf arg']
  return $ V.normAppList p fun' [arg']

elabBindersAndBody :: MonadCompile m => [B.Binder] -> B.Expr -> m ([V.InputBinder], V.InputExpr)
elabBindersAndBody bs body = bitraverse (traverse elab) elab (bs, body)

elabQuantifier :: (MonadCompile m, IsToken token) => token -> V.Quantifier -> [B.Binder] -> B.Expr -> m V.InputExpr
elabQuantifier t q bs body = do
  checkNonEmpty t bs
  let qExpr = V.TypeClassOp $ V.QuantifierTC q
  unfoldQuantifier (mkAnn t) qExpr <$> elabBindersAndBody bs body

elabQuantifierIn :: (MonadCompile m, IsToken token) => token -> V.Quantifier -> [B.Binder] -> B.Expr -> B.Expr -> m V.InputExpr
elabQuantifierIn t q bs container body = do
  checkNonEmpty t bs
  unfoldQuantifierIn (mkAnn t) q <$> elab container <*> elabBindersAndBody bs body

elabTensor :: (MonadCompile m, IsToken token) => token -> B.Expr -> B.Expr -> m V.InputExpr
elabTensor tk tElem tDims = case tDims of
  B.VecLiteral tk1 xs tk2 -> V.TensorType (V.tkProvenance tk) <$> elab tElem <*> elems tk1 xs tk2
  _                   -> builtin V.Tensor tk [tElem, tDims]
  where
    elems tk1 xs tk2 = V.mkList p (V.mkHole p "") <$> traverse elab xs
      where p = V.tkProvenance tk1 <> V.tkProvenance tk2

elabForeach :: (MonadCompile m, IsToken token) => token -> [B.Binder] -> B.Expr -> m V.InputExpr
elabForeach t bs body = do
  checkNonEmpty t bs
  unfoldQuantifier (mkAnn t) V.Foreach <$> elabBindersAndBody bs body

elabOrder :: (MonadCompile m, IsToken token) => V.OrderOp -> token -> B.Expr -> B.Expr -> m V.InputExpr
elabOrder order tk e1 e2 = do
  let Tk tkDetails@(tkPos, _) = toToken tk
  let chainedOrder = case e1 of
        B.Le _ _ e -> Just (V.Le, e)
        B.Lt _ _ e -> Just (V.Lt, e)
        B.Ge _ _ e -> Just (V.Ge, e)
        B.Gt _ _ e -> Just (V.Gt, e)
        _          -> Nothing

  case chainedOrder of
    Nothing -> builtin (V.TypeClassOp $ V.OrderTC order) tk [e1, e2]
    Just (prevOrder, e)
      | not (V.chainable prevOrder order) ->
        throwError $ UnchainableOrders (V.tkProvenance tk) prevOrder order
      | otherwise -> elab $ B.And e1 (B.TokAnd (tkPos, "and")) $ case order of
        V.Le -> B.Le e (B.TokLe tkDetails) e2
        V.Lt -> B.Lt e (B.TokLt tkDetails) e2
        V.Ge -> B.Ge e (B.TokGe tkDetails) e2
        V.Gt -> B.Gt e (B.TokGt tkDetails) e2

mkAnn :: IsToken a => a -> V.Provenance
mkAnn = V.tkProvenance

checkNonEmpty :: (MonadCompile m, IsToken token) => token -> [a] -> m ()
checkNonEmpty tk = checkNonEmpty' (V.tkProvenance tk) (tkSymbol tk)

checkNonEmpty' :: (MonadCompile m) => V.Provenance -> Symbol -> [a] -> m ()
checkNonEmpty' p s []      = throwError $ MissingVariables p s
checkNonEmpty' _ _ (_ : _) = return ()

-- |Constructs a pi type filled with an appropriate number of holes for
-- a definition which has no accompanying type.
constructUnknownDefType :: B.Name -> [V.InputBinder] -> V.InputExpr
constructUnknownDefType n = foldr addArg returnType
  where
  returnType :: V.InputExpr
  returnType = V.mkHole (V.tkProvenance n) (typifyName (tkSymbol n))

  addArg :: V.InputBinder -> V.InputExpr -> V.InputExpr
  addArg b = V.Pi (V.provenanceOf b) (binderToHole b)

  binderToHole :: V.InputBinder -> V.InputBinder
  binderToHole b = V.Binder ann (V.visibilityOf b) (V.relevanceOf b) (Just name) (V.Hole ann name)
    where
    ann  = V.provenanceOf b
    name = typifyName (V.getBinderSymbol b)

  typifyName :: Symbol -> Symbol
  typifyName x = "typeOf_" <> x