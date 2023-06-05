{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Vehicle.Syntax.BNFC.Delaborate.External
  ( Delaborate,
    delab,
  )
where

import Control.Monad.Identity (Identity (runIdentity), IdentityT)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (head, toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Prettyprinter (Doc, Pretty (..), squote, squotes, (<+>))
import Vehicle.Syntax.AST qualified as V
import Vehicle.Syntax.AST.Arg
import Vehicle.Syntax.BNFC.Utils
import Vehicle.Syntax.External.Abs qualified as B
import Vehicle.Syntax.External.Print as External (Print, printTree)
import Vehicle.Syntax.Parse.Error
import Vehicle.Syntax.Parse.Token
import Vehicle.Syntax.Prelude
import Vehicle.Syntax.Sugar

--------------------------------------------------------------------------------
-- Conversion to BNFC AST

delab :: (Show t, Delaborate t bnfc) => t -> bnfc
delab e = runIdentity (delabM e)

--------------------------------------------------------------------------------
-- Implementation

-- | Constraint for the monad stack used by the elaborator.
type MonadDelab m = Monad m

-- * Conversion

class Delaborate t bnfc | t -> bnfc, bnfc -> t where
  delabM :: (MonadDelab m) => t -> m bnfc

instance Delaborate (V.Prog V.Name V.Builtin) B.Prog where
  delabM (V.Main decls) = B.Main . concat <$> traverse delabM decls

instance Delaborate (V.Decl V.Name V.Builtin) [B.Decl] where
  delabM = \case
    V.DefAbstract _ n a t -> do
      defFun <- B.DefFunType (delabIdentifier n) tokElemOf <$> delabM t

      let defAnn = case a of
            V.PostulateDef -> delabAnn postulateAnn []
            V.NetworkDef -> delabAnn networkAnn []
            V.DatasetDef -> delabAnn datasetAnn []
            V.ParameterDef sort -> case sort of
              V.NonInferable -> delabAnn parameterAnn []
              V.Inferable -> delabAnn parameterAnn [mkDeclAnnOption InferableOption True]

      return [defAnn, defFun]
    V.DefFunction _ n anns t e -> do
      annDecls <- traverse delabM anns
      funDecls <- delabFun n t e
      return $ annDecls <> funDecls

instance Delaborate (V.Expr V.Name V.Builtin) B.Expr where
  delabM expr = case expr of
    V.Universe _ u -> return $ delabUniverse u
    V.FreeVar _ n -> return $ B.Var (delabSymbol (V.nameOf n))
    V.BoundVar _ n -> return $ B.Var (delabSymbol n)
    V.Hole _ n -> return $ B.Hole (mkToken B.HoleToken n)
    V.Ann _ e t -> B.Ann <$> delabM e <*> pure tokElemOf <*> delabM t
    V.Pi _ t1 t2 -> delabPi t1 t2
    V.Let _ e1 b e2 -> delabLet e1 b e2
    V.Lam _ binder body -> delabLam binder body
    V.Meta _ m -> return $ B.Var (mkToken B.Name (layoutAsText (pretty m)))
    V.App _ (V.Builtin _ b) args -> delabBuiltin b (NonEmpty.toList args)
    V.App _ fun args -> do
      fun' <- delabM fun
      delabApp fun' (NonEmpty.toList args)
    V.Builtin _ op -> delabBuiltin op []

instance Delaborate (V.Arg V.Name V.Builtin) B.Arg where
  delabM arg = do
    e' <- delabM (V.argExpr arg)
    return $ case V.visibilityOf arg of
      V.Explicit {} -> B.ExplicitArg e'
      V.Implicit {} -> B.ImplicitArg e'
      V.Instance {} -> B.InstanceArg e'

instance Delaborate (V.Binder V.Name V.Builtin) B.BasicBinder where
  delabM binder = do
    let n' = delabSymbol $ fromMaybe "_" (V.nameOf binder)
    t' <- delabM (V.binderType binder)
    return $ case V.visibilityOf binder of
      V.Explicit -> B.ExplicitBinder n' tokElemOf t'
      V.Implicit {} -> B.ImplicitBinder n' tokElemOf t'
      V.Instance {} -> B.InstanceBinder n' tokElemOf t'

instance Delaborate V.Annotation B.Decl where
  delabM = \case
    V.AnnProperty -> return $ delabAnn propertyAnn []
    V.AnnNoInline -> return $ delabAnn noInlineAnn []

-- | Used for things not in the user-syntax.
cheatDelab :: Text -> B.Expr
cheatDelab n = B.Var (delabSymbol n)

delabNameBinder :: (MonadDelab m) => V.Binder V.Name V.Builtin -> m B.NameBinder
delabNameBinder b = case V.binderNamingForm b of
  V.OnlyType {} ->
    developerError $
      "Should not be delaborating the `OnlyType` binder to a `Binder Name`"
  V.NameAndType name -> B.BasicNameBinder <$> delabM b
  V.OnlyName name -> return $ case V.visibilityOf b of
    V.Explicit -> B.ExplicitNameBinder (delabSymbol name)
    V.Implicit {} -> B.ImplicitNameBinder (delabSymbol name)
    V.Instance {} -> B.InstanceNameBinder (delabSymbol name)

delabTypeBinder :: (MonadDelab m) => V.Binder V.Name V.Builtin -> m B.TypeBinder
delabTypeBinder b = case V.binderNamingForm b of
  V.OnlyName {} ->
    developerError $
      "Should not be delaborating an `OnlyName` binder to a `TypeBinder`"
  V.NameAndType {} -> B.BasicTypeBinder <$> delabM b
  V.OnlyType {} -> case V.visibilityOf b of
    V.Explicit -> B.ExplicitTypeBinder <$> delabM (V.binderType b)
    V.Implicit {} -> B.ImplicitTypeBinder <$> delabM (V.binderType b)
    V.Instance {} -> B.InstanceTypeBinder <$> delabM (V.binderType b)

delabLetBinding :: (MonadDelab m) => (V.Binder V.Name V.Builtin, V.Expr V.Name V.Builtin) -> m B.LetDecl
delabLetBinding (binder, bound) = B.LDecl <$> delabNameBinder binder <*> delabM bound

delabBoolLit :: Bool -> B.Boolean
delabBoolLit b = mkToken B.Boolean (pack $ show b)

delabNatLit :: Int -> B.Natural
delabNatLit n = mkToken B.Natural (pack $ show n)

delabRatLit :: Rational -> B.Rational
delabRatLit r = mkToken B.Rational (pack $ show (fromRational r :: Double))

delabSymbol :: Text -> B.Name
delabSymbol = mkToken B.Name

delabIdentifier :: V.Identifier -> B.Name
delabIdentifier (V.Identifier _ n) = mkToken B.Name n

delabApp :: (MonadDelab m) => B.Expr -> [V.Arg V.Name V.Builtin] -> m B.Expr
delabApp fun allArgs = go fun <$> traverse delabM (reverse allArgs)
  where
    go fn [] = fn
    go fn (arg : args) = B.App (go fn args) arg

delabUniverse :: V.UniverseLevel -> B.Expr
delabUniverse = \case
  V.UniverseLevel l -> tokType l

delabBuiltin :: (MonadDelab m) => V.Builtin -> [V.Arg V.Name V.Builtin] -> m B.Expr
delabBuiltin fun args = case fun of
  V.Constructor c -> delabConstructor c args
  V.TypeClassOp tc -> delabTypeClassOp tc args
  V.TypeClass t -> delabTypeClass t args
  V.BuiltinFunction f -> delabBuiltinFunction f args
  V.BuiltinType t -> delabBuiltinType t args

delabBuiltinFunction :: (MonadDelab m) => V.BuiltinFunction -> [V.Arg V.Name V.Builtin] -> m B.Expr
delabBuiltinFunction fun args = case fun of
  V.And -> delabInfixOp2 B.And tokAnd args
  V.Or -> delabInfixOp2 B.Or tokOr args
  V.Implies -> delabInfixOp2 B.Impl tokImpl args
  V.Not -> delabOp1 B.Not tokNot args
  V.If -> delabIf args
  V.FromNat {} -> delabApp (cheatDelab $ layoutAsText $ pretty fun) args
  V.FromRat {} -> delabApp (cheatDelab $ layoutAsText $ pretty fun) args
  V.Neg {} -> delabTypeClassOp V.NegTC args
  V.Add {} -> delabTypeClassOp V.AddTC args
  V.Sub {} -> delabTypeClassOp V.SubTC args
  V.Mul {} -> delabTypeClassOp V.MulTC args
  V.Div {} -> delabTypeClassOp V.DivTC args
  V.Quantifier q _ -> delabTypeClassOp (V.QuantifierTC q) args
  V.Equals _ op -> delabTypeClassOp (V.EqualsTC op) args
  V.Order _ op -> delabTypeClassOp (V.OrderTC op) args
  V.Fold V.FoldList -> delabTypeClassOp V.FoldTC args
  V.Fold V.FoldVector -> delabApp (B.DepFold tokDepFold) args
  V.ConsVector -> delabInfixOp2 B.ConsVector tokConsVector args
  V.At -> delabInfixOp2 B.At tokAt args
  V.Indices -> delabApp (B.Indices tokIndices) args

delabBuiltinType :: (MonadDelab m) => V.BuiltinType -> [V.Arg V.Name V.Builtin] -> m B.Expr
delabBuiltinType fun args = case fun of
  V.Unit -> delabApp (B.Unit tokUnit) args
  V.Bool -> delabApp (B.Bool tokBool) args
  V.Nat -> delabApp (B.Nat tokNat) args
  V.Int -> delabApp (B.Int tokInt) args
  V.Rat -> delabApp (B.Rat tokRat) args
  V.List -> delabApp (B.List tokList) args
  V.Vector -> delabApp (B.Vector tokVector) args
  V.Index -> delabApp (B.Index tokIndex) args

delabTypeClass :: (MonadDelab m) => V.TypeClass -> [V.Arg V.Name V.Builtin] -> m B.Expr
delabTypeClass tc args = case tc of
  V.HasEq eq -> case eq of
    V.Eq -> delabApp (B.HasEq tokHasEq) args
    V.Neq -> delabApp (B.HasNotEq tokHasNotEq) args
  V.HasAdd -> delabApp (B.HasAdd tokHasAdd) args
  V.HasSub -> delabApp (B.HasSub tokHasSub) args
  V.HasMul -> delabApp (B.HasMul tokHasMul) args
  V.HasMap -> delabApp (B.HasMap tokHasMap) args
  V.HasFold -> delabApp (B.HasFold tokHasFold) args
  _ -> delabApp (B.Var (delabSymbol (layoutAsText $ pretty tc))) args

delabConstructor :: (MonadDelab m) => V.BuiltinConstructor -> [V.Arg V.Name V.Builtin] -> m B.Expr
delabConstructor fun args = case fun of
  V.Cons -> delabInfixOp2 B.Cons tokCons args
  V.Nil -> delabApp (B.Nil tokNil) args
  V.LUnit -> return $ B.Literal B.UnitLiteral
  V.LBool b -> return $ B.Literal $ B.BoolLiteral $ delabBoolLit b
  V.LIndex x -> return $ B.Literal $ B.NatLiteral $ delabNatLit x
  V.LNat n -> return $ B.Literal $ B.NatLiteral $ delabNatLit n
  V.LInt i ->
    return $
      if i >= 0
        then B.Literal $ B.NatLiteral $ delabNatLit i
        else B.Neg tokSub (B.Literal $ B.NatLiteral $ delabNatLit (-i))
  V.LRat r -> return $ B.Literal $ B.RatLiteral $ delabRatLit r
  V.LVec _ -> B.VecLiteral tokSeqOpen <$> traverse (delabM . argExpr) args <*> pure tokSeqClose

delabTypeClassOp :: (MonadDelab m) => V.TypeClassOp -> [V.Arg V.Name V.Builtin] -> m B.Expr
delabTypeClassOp op args = case op of
  V.FromNatTC {} -> delabApp (cheatDelab $ layoutAsText $ pretty op) args
  V.FromRatTC {} -> delabApp (cheatDelab $ layoutAsText $ pretty op) args
  V.FromVecTC {} -> delabApp (cheatDelab $ layoutAsText $ pretty op) args
  V.NegTC -> delabOp1 B.Neg tokSub args
  V.AddTC -> delabInfixOp2 B.Add tokAdd args
  V.SubTC -> delabInfixOp2 B.Sub tokSub args
  V.MulTC -> delabInfixOp2 B.Mul tokMul args
  V.DivTC -> delabInfixOp2 B.Div tokDiv args
  V.EqualsTC eq -> case eq of
    V.Eq -> delabInfixOp2 B.Eq tokEq args
    V.Neq -> delabInfixOp2 B.Neq tokNeq args
  V.OrderTC ord -> case ord of
    V.Le -> delabInfixOp2 B.Le tokLe args
    V.Lt -> delabInfixOp2 B.Lt tokLt args
    V.Ge -> delabInfixOp2 B.Ge tokGe args
    V.Gt -> delabInfixOp2 B.Gt tokGt args
  V.MapTC -> delabApp (B.Map tokMap) args
  V.FoldTC -> delabApp (B.Fold tokFold) args
  V.QuantifierTC q -> delabQuantifier q args

delabOp1 :: (MonadDelab m, IsToken token) => (token -> B.Expr -> B.Expr) -> token -> [V.Arg V.Name V.Builtin] -> m B.Expr
delabOp1 op tk [arg]
  | V.isExplicit arg = op tk <$> delabM (argExpr arg)
delabOp1 _ tk args = delabApp (cheatDelab $ tkSymbol tk) args

delabOp2 :: (MonadDelab m, IsToken token) => (token -> B.Expr -> B.Expr -> B.Expr) -> token -> [V.Arg V.Name V.Builtin] -> m B.Expr
delabOp2 op tk args@[arg1, arg2]
  | all V.isExplicit args = op tk <$> delabM (argExpr arg1) <*> delabM (argExpr arg2)
delabOp2 op tk args = delabApp (cheatDelab $ tkSymbol tk) args

delabOp3 :: (MonadDelab m, IsToken token) => (token -> B.Expr -> B.Expr -> B.Expr -> B.Expr) -> token -> [V.Arg V.Name V.Builtin] -> m B.Expr
delabOp3 op tk args@[arg1, arg2, arg3]
  | all V.isExplicit args = op tk <$> delabM (argExpr arg1) <*> delabM (argExpr arg2) <*> delabM (argExpr arg3)
delabOp3 op tk args = delabApp (cheatDelab $ tkSymbol tk) args

delabInfixOp2 :: (MonadDelab m, IsToken token) => (B.Expr -> token -> B.Expr -> B.Expr) -> token -> [V.Arg V.Name V.Builtin] -> m B.Expr
delabInfixOp2 op tk args@[arg1, arg2]
  | all V.isExplicit args = op <$> delabM (argExpr arg1) <*> pure tk <*> delabM (argExpr arg2)
delabInfixOp2 op tk args = delabApp (cheatDelab $ tkSymbol tk) args

delabPartialSection :: Int -> [B.Expr] -> ([B.Expr] -> B.Expr) -> B.Expr
delabPartialSection expectedArgs actualArgs mkOp = do
  -- This is a hack until we get section syntax.
  let missingArgNumbers = [0 .. (expectedArgs - length actualArgs)]
  let missingVarNames = fmap (\v -> delabSymbol (pack "x" <> pack (show v))) missingArgNumbers
  let missingVars = fmap B.Var missingVarNames
  let missingBinders = fmap B.ExplicitNameBinder missingVarNames
  B.Lam tokLambda missingBinders tokArrow (mkOp (actualArgs <> missingVars))

delabIf :: (MonadDelab m) => [V.Arg V.Name V.Builtin] -> m B.Expr
delabIf args@[arg1, arg2, arg3]
  | all V.isExplicit args = do
      e1 <- delabM (argExpr arg1)
      e2 <- delabM (argExpr arg2)
      e3 <- delabM (argExpr arg3)
      return $ B.If tokIf e1 tokThen e2 tokElse e3
delabIf args = delabApp (cheatDelab "if") args

argsError :: Text -> Int -> [B.Arg] -> a
argsError s n args =
  developerError $
    "Expecting"
      <+> pretty n
      <+> "arguments for"
      <+> squotes (pretty s)
      <+> "but found"
      <+> pretty (length args)
      <+> "arguments:"
      <+> squotes (pretty (External.printTree args))

-- | Collapses pi expressions into either a function or a sequence of forall bindings
delabPi :: (MonadDelab m) => V.Binder V.Name V.Builtin -> V.Expr V.Name V.Builtin -> m B.Expr
delabPi binder body = case V.binderNamingForm binder of
  V.OnlyType -> do
    binder' <- delabTypeBinder binder
    body' <- delabM body
    return $ B.Fun binder' tokArrow body'
  _ -> do
    let (foldedBinders, foldedBody) = foldBinders (FoldableBinder PiFold binder) body
    binders' <- traverse delabNameBinder (binder : foldedBinders)
    body' <- delabM foldedBody
    return $ B.ForallT tokForallT binders' tokDot body'

-- | Collapses let expressions into a sequence of let declarations
delabLet :: (MonadDelab m) => V.Expr V.Name V.Builtin -> V.Binder V.Name V.Builtin -> V.Expr V.Name V.Builtin -> m B.Expr
delabLet bound binder body = do
  let (boundExprs, foldedBody) = foldLetBinders body
  binders' <- traverse delabLetBinding boundExprs
  body' <- delabM foldedBody
  return $ B.Let tokLet binders' body'

-- | Collapses consecutative lambda expressions into a sequence of binders
delabLam :: (MonadDelab m) => V.Binder V.Name V.Builtin -> V.Expr V.Name V.Builtin -> m B.Expr
delabLam binder body = do
  let (foldedBinders, foldedBody) = foldBinders (FoldableBinder LamFold binder) body
  binders' <- traverse delabNameBinder (binder : foldedBinders)
  body' <- delabM foldedBody
  return $ B.Lam tokLambda binders' tokArrow body'

delabFun :: (MonadDelab m) => V.Identifier -> V.Expr V.Name V.Builtin -> V.Expr V.Name V.Builtin -> m [B.Decl]
delabFun name typ expr = do
  let n' = delabIdentifier name
  let (binders, body) = foldBinders FunFold expr
  if V.isTypeSynonym typ
    then do
      defType <- B.DefType n' <$> traverse delabNameBinder binders <*> delabM body
      return [defType]
    else do
      defType <- B.DefFunType n' tokElemOf <$> delabM typ
      defExpr <- B.DefFunExpr n' <$> traverse delabNameBinder binders <*> delabM body
      return [defType, defExpr]

delabQuantifier :: (MonadDelab m) => V.Quantifier -> [V.Arg V.Name V.Builtin] -> m B.Expr
delabQuantifier q args = case reverse args of
  V.ExplicitArg _ (V.Lam _ binder body) : _ -> do
    let (foldedBinders, foldedBody) = foldBinders (FoldableBinder (QuantFold q) binder) body
    binders' <- traverse delabNameBinder (binder : foldedBinders)
    body' <- delabM foldedBody
    let mkTk = case q of
          V.Forall -> B.Forall tokForall
          V.Exists -> B.Exists tokExists
    return $ mkTk binders' tokDot body'
  _ -> return $ cheatDelab (layoutAsText $ pretty q)

delabQuantifierIn :: (MonadDelab m) => V.Quantifier -> [V.Arg V.Name V.Builtin] -> m B.Expr
delabQuantifierIn q args = case reverse args of
  V.ExplicitArg _ cont : V.ExplicitArg _ (V.Lam _ binder body) : _ -> do
    binder' <- delabNameBinder binder
    cont' <- delabM cont
    body' <- delabM body
    let mkTk = case q of
          V.Forall -> B.ForallIn tokForall
          V.Exists -> B.ExistsIn tokExists
    return $ mkTk binder' cont' tokDot body'
  _ -> do
    let sym = case q of V.Forall -> tkSymbol tokForall; V.Exists -> tkSymbol tokExists
    argsError sym 2 <$> traverse delabM args

delabForeach :: (MonadDelab m) => [V.Arg V.Name V.Builtin] -> m B.Expr
delabForeach args = case reverse args of
  V.ExplicitArg _ (V.Lam _ binder body) : _ -> do
    binder' <- delabNameBinder binder
    body' <- delabM body
    return $ B.Foreach tokForeach binder' tokDot body'
  _ -> argsError (tkSymbol tokForeach) 1 <$> traverse delabM args

delabAnn :: B.DeclAnnName -> [B.DeclAnnOption] -> B.Decl
delabAnn name [] = B.DefAnn name B.DeclAnnWithoutOpts
delabAnn name ops = B.DefAnn name $ B.DeclAnnWithOpts ops

mkDeclAnnOption :: Text -> Bool -> B.DeclAnnOption
mkDeclAnnOption name value = B.BooleanOption (mkToken B.Name name) (delabBoolLit value)

auxiliaryTypeError :: Doc a -> a
auxiliaryTypeError e =
  developerError $
    "Encountered" <+> squotes e <> ". Should not be delaborating auxiliary-type system code."

primOpError :: V.Builtin -> a
primOpError e =
  developerError $
    "Encountered" <+> squotes (pretty e) <> ". Delaborating primitive builtins not yet supported."

onlyExplicit :: NonEmpty (GenericArg expr) -> [expr]
onlyExplicit args = argExpr <$> filter V.isExplicit (NonEmpty.toList args)
