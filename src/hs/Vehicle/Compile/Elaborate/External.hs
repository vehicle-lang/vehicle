{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Elaborate.External
  ( elabProg
  , elabExpr
  ) where

import Control.Monad.Except (throwError)
import Data.Text (unpack)
import Data.Bitraversable (bitraverse)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty (groupBy1, head, toList)

import Vehicle.External.Abs qualified as B

import Vehicle.Prelude
import Vehicle.Language.Sugar
import Vehicle.Compile.Prelude qualified as V
import Vehicle.Compile.Error
import Vehicle.Language.Provenance

elabProg :: MonadCompile m => B.Prog -> m V.InputProg
elabProg = elab

elabExpr :: MonadCompile m => B.Expr -> m V.InputExpr
elabExpr = elab

--------------------------------------------------------------------------------
-- Conversion from BNFC AST
--
-- We elabert from the simple AST generated automatically by BNFC to our
-- more complicated internal version of the AST which allows us to annotate
-- terms with sort-dependent types.
--
-- While doing this we:
--  1. extract the positions from the tokens generated by BNFC and elaborate
--     them into `Provenance` annotations.
--  2. combine function types and expressions into a single AST node

-- * Provenance

mkAnn :: IsToken a => a -> V.InputAnn
mkAnn = tkProvenance

-- * Elaboration

class Elab vf vc where
  elab :: MonadCompile m => vf -> m vc

instance Elab B.Prog V.InputProg where
  elab (B.Main decls) = V.Main <$> groupDecls decls

-- |Takes a list of declarations, and groups type and expression
--  declarations by their name.
groupDecls :: MonadCompile m => [B.Decl] -> m [V.InputDecl]
groupDecls []       = return []
groupDecls (d : ds) = NonEmpty.toList <$> traverse elab (NonEmpty.groupBy1 cond (d :| ds))
  where
    cond :: B.Decl -> B.Decl -> Bool
    cond d1 d2 = isDefFun d1 && isDefFun d2 &&
                 tkSymbol (declName d1) == tkSymbol (declName d2)

    isDefFun :: B.Decl -> Bool
    isDefFun (B.DefFunType _name _args _exp) = True
    isDefFun (B.DefFunExpr _ann _name _typ)  = True
    isDefFun _                               = False

-- |Elaborate declarations.
instance Elab (NonEmpty B.Decl) V.InputDecl where
  elab = \case
    -- Elaborate resources.
    (B.DeclNetw      n _tk t :| []) -> elabResource V.Network   n t
    (B.DeclData      n _tk t :| []) -> elabResource V.Dataset   n t
    (B.DeclParam     n _tk t :| []) -> elabResource V.Parameter n t
    (B.DeclImplParam n _tk t :| []) -> elabImplParam n t

    -- Elaborate a type definition.
    (B.DefType n bs e :| []) -> do
      unfoldDefType (mkAnn n) <$> elab n <*> traverse elab bs <*> elab e

    -- Elaborate a function and type definition pair.
    (B.DefFunType n1 _tk t  :| [B.DefFunExpr _n2 bs e]) ->
      unfoldDefFun (mkAnn n1) <$> elab n1 <*> elab t <*> traverse elab bs <*> elab e

    -- Elaborate a function without a type definition.
    (B.DefFunExpr n bs e :| []) -> do
      binders <- traverse elab bs
      let unknownType = constructUnknownDefType n binders
      unfoldDefFun (mkAnn n) <$> elab n <*> pure unknownType <*> pure binders <*> elab e

    -- Why did you write the signature AFTER the function?
    (e1@B.DefFunExpr {} :| [e2@B.DefFunType {}]) ->
      elab (e2 :| [e1])

    -- Missing type or expression declaration.
    (B.DefFunType n _tk _t :| []) ->
      throwError $ MissingDefFunExpr (tkProvenance n) (tkSymbol n)

    -- Multiple type of expression declarations with the same n.
    ds ->
      throwError $ DuplicateName provs symbol
        where
          symbol = tkSymbol $ declName $ NonEmpty.head ds
          provs  = fmap (tkProvenance . declName) ds

instance Elab B.Expr V.InputExpr where
  elab = \case
    B.Type t                  -> return $ V.Type (mkAnn t) (parseTypeLevel t)
    B.Var  n                  -> return $ V.Var  (mkAnn n) (tkSymbol n)
    B.Hole n                  -> return $ V.mkHole (tkProvenance n) (tkSymbol n)
    B.Literal l               -> elab l

    B.Ann e tk t              -> op2 V.Ann tk  (elab e) (elab t)
    B.Fun t1 tk t2            -> op2 V.Pi  tk  (elabFunInputType t1) (elab t2)
    B.LSeq tk1 es _tk2        -> op1 V.mkSeqExpr tk1 (traverse elab es)

    B.App e1 e2               -> elabApp e1 e2
    B.Let tk1 ds e            -> unfoldLet (mkAnn tk1) <$> bitraverse (traverse elab) elab (ds, e)
    B.ForallT tk1 ns _tk2 t   -> do checkNonEmpty tk1 ns; unfoldForall (mkAnn tk1) <$> elabBindersAndBody ns t
    B.Lam tk1 ns _tk2 e       -> do checkNonEmpty tk1 ns; unfoldLam    (mkAnn tk1) <$> elabBindersAndBody ns e

    B.Forall    tk1 ns    _tk2 e  -> elabQuantifier   tk1 V.Forall  ns e
    B.Exists    tk1 ns    _tk2 e  -> elabQuantifier   tk1 V.Exists  ns e
    B.ForallIn  tk1 ns e1 _tk2 e2 -> elabQuantifierIn tk1 V.Forall  ns e1 e2
    B.ExistsIn  tk1 ns e1 _tk2 e2 -> elabQuantifierIn tk1 V.Exists  ns e1 e2

    B.Foreach   tk1 ns    _tk2 e  -> elabForeach   tk1 ns e
    B.ForeachIn tk1 ns e1 _tk2 e2 -> elabForeachIn tk1 ns e1 e2

    B.Bool tk                 -> builtin V.Bool                          tk []
    B.Real tk                 -> builtin (V.NumericType   V.Real)        tk []
    B.Rat  tk                 -> builtin (V.NumericType   V.Rat)         tk []
    B.Int tk                  -> builtin (V.NumericType   V.Int)         tk []
    B.Nat tk                  -> builtin (V.NumericType   V.Nat)         tk []
    B.List tk t               -> builtin (V.ContainerType V.List)        tk [t]
    B.Tensor tk t1 t2         -> builtin (V.ContainerType V.Tensor)      tk [t1, t2]
    B.Index tk t              -> builtin V.Index                         tk [t]

    B.If tk1 e1 _ e2 _ e3     -> builtin V.If                  tk1 [e1, e2, e3]
    B.Not tk e                -> builtin V.Not                 tk  [e]
    B.Impl e1 tk e2           -> builtin (V.BooleanOp2 V.Impl) tk  [e1, e2]
    B.And e1 tk e2            -> builtin (V.BooleanOp2 V.And)  tk  [e1, e2]
    B.Or e1 tk e2             -> builtin (V.BooleanOp2 V.Or)   tk  [e1, e2]

    B.Eq e1 tk e2             -> builtin (V.Equality V.Eq)  tk [e1, e2]
    B.Neq e1 tk e2            -> builtin (V.Equality V.Neq) tk [e1, e2]
    B.Le e1 tk e2             -> elabOrder V.Le tk e1 e2
    B.Lt e1 tk e2             -> elabOrder V.Lt tk e1 e2
    B.Ge e1 tk e2             -> elabOrder V.Ge tk e1 e2
    B.Gt e1 tk e2             -> elabOrder V.Gt tk e1 e2

    B.Mul e1 tk e2            -> builtin (V.NumericOp2 V.Mul) tk [e1, e2]
    B.Div e1 tk e2            -> builtin (V.NumericOp2 V.Div) tk [e1, e2]
    B.Add e1 tk e2            -> builtin (V.NumericOp2 V.Add) tk [e1, e2]
    B.Sub e1 tk e2            -> builtin (V.NumericOp2 V.Sub) tk [e1, e2]
    B.Neg tk e                -> builtin V.Neg tk [e]

    B.Cons e1 tk e2           -> builtin V.Cons tk [e1, e2]
    B.At e1 tk e2             -> builtin V.At   tk [e1, e2]
    B.Map tk e1 e2            -> builtin V.Map  tk [e1, e2]
    B.Fold tk e1 e2 e3        -> builtin V.Fold tk [e1, e2, e3]

    --TypeClass folded into Expressions
    B.HasEq      tk e       -> builtin (V.TypeClass V.HasEq)              tk [e]
    B.HasOrd     tk e       -> builtin (V.TypeClass V.HasOrd)             tk [e]
    B.HasAdd     tk e       -> builtin (V.TypeClass V.HasAdd)             tk [e]
    B.HasSub     tk e       -> builtin (V.TypeClass V.HasSub)             tk [e]
    B.HasMul     tk e       -> builtin (V.TypeClass V.HasMul)             tk [e]
    B.HasDiv     tk e       -> builtin (V.TypeClass V.HasDiv)             tk [e]
    B.HasNeg     tk e       -> builtin (V.TypeClass V.HasNeg)             tk [e]
    B.HasConOps  tk e1 e2   -> builtin (V.TypeClass V.HasConOps)          tk [e1, e2]
    B.HasNatLits tk n e     -> builtin (V.TypeClass (V.HasNatLitsUpTo (fromIntegral n))) tk [e]
    B.HasIntLits tk e       -> builtin (V.TypeClass V.HasIntLits)         tk [e]
    B.HasRatLits tk e       -> builtin (V.TypeClass V.HasRatLits)         tk [e]
    B.HasConLits tk n e1 e2 -> builtin (V.TypeClass (V.HasConLitsOfSize (fromIntegral n))) tk [e1, e2]

instance Elab B.Arg V.InputArg where
  elab (B.ExplicitArg e) = mkArg V.Explicit <$> elab e
  elab (B.ImplicitArg e) = mkArg V.Implicit <$> elab e
  elab (B.InstanceArg e) = mkArg V.Instance <$> elab e

elabResource :: MonadCompile m => V.ResourceType -> B.Name -> B.Expr -> m V.InputDecl
elabResource r n t = V.DefResource (tkProvenance n) r <$> elab n <*> elab t

elabImplParam :: MonadCompile m => B.Name -> B.Expr -> m V.InputDecl
elabImplParam n t = V.DefFunction ann V.NotABoolean <$> elab n <*> elab t <*> pure hole
  where ann = tkProvenance n; hole = V.mkHole (inserted ann) (tkSymbol n)

mkArg :: V.Visibility -> V.InputExpr -> V.InputArg
mkArg v e = V.Arg (expandByArgVisibility v (provenanceOf e)) v e

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
mkBinder n v e = V.Binder (expandByArgVisibility v p) v (Just (tkSymbol n)) t
  where
  (p, t) = case e of
    Nothing  -> (tkProvenance n, V.mkHole (tkProvenance n) ("typeOf[" <> tkSymbol n <> "]"))
    Just t1  -> (fillInProvenance [tkProvenance n, provenanceOf t1], t1)

instance Elab B.LetDecl (V.InputBinder, V.InputExpr) where
  elab (B.LDecl b e) = bitraverse elab elab (b,e)

instance Elab B.Lit V.InputExpr where
  elab = \case
    B.LitTrue  t -> return $ V.LitBool (mkAnn t) True
    B.LitFalse t -> return $ V.LitBool (mkAnn t) False
    B.LitNat   t -> return $ V.LitNat  (mkAnn t) (readNat (tkSymbol t))
    B.LitRat   t -> return $ V.LitRat  (mkAnn t) (readRat (tkSymbol t))

parseTypeLevel :: B.TypeToken -> Int
parseTypeLevel s = read (drop 4 (unpack (tkSymbol s)))

op1 :: (MonadCompile m, HasProvenance a, IsToken token)
    => (V.InputAnn -> a -> b)
    -> token -> m a -> m b
op1 mk t e = do
  ce <- e
  let p = fillInProvenance [tkProvenance t, provenanceOf ce]
  return $ mk p ce

op2 :: (MonadCompile m, HasProvenance a, HasProvenance b, IsToken token)
    => (V.InputAnn -> a -> b -> c)
    -> token -> m a -> m b -> m c

op2 mk t e1 e2 = do
  ce1 <- e1
  ce2 <- e2
  let p = fillInProvenance [tkProvenance t, provenanceOf ce1, provenanceOf ce2]
  return $ mk p ce1 ce2

builtin :: (MonadCompile m, IsToken token) => V.Builtin -> token -> [B.Expr] -> m V.InputExpr
builtin b t args = builtin' b t <$> traverse elab args

builtin' :: IsToken token => V.Builtin -> token -> [V.InputExpr] -> V.InputExpr
builtin' b t argExprs = V.normAppList p' (V.Builtin p b) args
  where
    p    = tkProvenance t
    p'   = fillInProvenance (p : map provenanceOf args)
    args = fmap (mkArg V.Explicit) argExprs

elabFunInputType :: MonadCompile m => B.Expr -> m V.InputBinder
elabFunInputType t = do
  t' <- elab t
  return $ V.ExplicitBinder (provenanceOf t') Nothing t'

elabApp :: MonadCompile m => B.Expr -> B.Arg -> m V.InputExpr
elabApp fun arg = do
  fun' <- elab fun
  arg' <- elab arg
  let p = fillInProvenance [provenanceOf fun', provenanceOf arg']
  return $ V.normAppList p fun' [arg']

elabBindersAndBody :: MonadCompile m => [B.Binder] -> B.Expr -> m ([V.InputBinder], V.InputExpr)
elabBindersAndBody bs body = bitraverse (traverse elab) elab (bs, body)

elabQuantifier :: (MonadCompile m, IsToken token) => token -> V.Quantifier -> [B.Binder] -> B.Expr -> m V.InputExpr
elabQuantifier t q bs body = do
  checkNonEmpty t bs
  unfoldQuantifier (mkAnn t) (V.Quant q) <$> elabBindersAndBody bs body

elabQuantifierIn :: (MonadCompile m, IsToken token) => token -> V.Quantifier -> [B.Binder] -> B.Expr -> B.Expr -> m V.InputExpr
elabQuantifierIn t q bs container body = do
  checkNonEmpty t bs
  unfoldQuantifierIn (mkAnn t) (V.QuantIn q) <$> elab container <*> elabBindersAndBody bs body


elabForeach :: (MonadCompile m, IsToken token) => token -> [B.Binder] -> B.Expr -> m V.InputExpr
elabForeach t bs body = do
  checkNonEmpty t bs
  unfoldQuantifier (mkAnn t) V.Foreach <$> elabBindersAndBody bs body

elabForeachIn :: (MonadCompile m, IsToken token) => token -> [B.Binder] -> B.Expr -> B.Expr -> m V.InputExpr
elabForeachIn t bs container body = do
  checkNonEmpty t bs
  unfoldQuantifierIn (mkAnn t) V.ForeachIn <$> elab container <*> elabBindersAndBody bs body

elabOrder :: (MonadCompile m, IsToken token) => V.Order -> token -> B.Expr -> B.Expr -> m V.InputExpr
elabOrder order tk e1 e2 = do
  let Tk tkDetails@(tkPos, _) = toToken tk
  let chainedOrder = case e1 of
        B.Le _ _ e -> Just (V.Le, e)
        B.Lt _ _ e -> Just (V.Lt, e)
        B.Ge _ _ e -> Just (V.Ge, e)
        B.Gt _ _ e -> Just (V.Gt, e)
        _          -> Nothing

  case chainedOrder of
    Nothing -> builtin (V.Order order) tk [e1, e2]
    Just (prevOrder, e)
      | not (V.chainable prevOrder order) ->
        throwError $ UnchainableOrders (tkProvenance tk) prevOrder order
      | otherwise -> elab $ B.And e1 (B.TokAnd (tkPos, "and")) $ case order of
        V.Le -> B.Le e (B.TokLe tkDetails) e2
        V.Lt -> B.Lt e (B.TokLt tkDetails) e2
        V.Ge -> B.Ge e (B.TokGe tkDetails) e2
        V.Gt -> B.Gt e (B.TokGt tkDetails) e2

-- |Get the name for any declaration.
declName :: B.Decl -> B.Name
declName (B.DeclNetw       n _ _) = n
declName (B.DeclData       n _ _) = n
declName (B.DeclParam      n _ _) = n
declName (B.DeclImplParam  n _ _) = n
declName (B.DefType        n _ _) = n
declName (B.DefFunType     n _ _) = n
declName (B.DefFunExpr     n _ _) = n

checkNonEmpty :: (MonadCompile m, IsToken token) => token -> [a] -> m ()
checkNonEmpty tk = checkNonEmpty' (tkProvenance tk) (tkSymbol tk)

checkNonEmpty' :: (MonadCompile m) => Provenance -> Symbol -> [a] -> m ()
checkNonEmpty' p s []      = throwError $ MissingVariables p s
checkNonEmpty' _ _ (_ : _) = return ()

-- |Constructs a pi type filled with an appropriate number of holes for
-- a definition which has no accompanying type.
constructUnknownDefType :: B.Name -> [V.InputBinder] -> V.InputExpr
constructUnknownDefType n = foldr addArg returnType
  where
  returnType :: V.InputExpr
  returnType = V.mkHole (tkProvenance n) (typifyName (tkSymbol n))

  addArg :: V.InputBinder -> V.InputExpr -> V.InputExpr
  addArg b = V.Pi (V.annotationOf b) (binderToHole b)

  binderToHole :: V.InputBinder -> V.InputBinder
  binderToHole b = V.Binder ann (V.visibilityOf b) (Just name) (V.Hole ann name)
    where
    ann  = V.annotationOf b
    name = typifyName (V.getBinderSymbol b)

  typifyName :: Symbol -> Symbol
  typifyName x = "typeOf_" <> x