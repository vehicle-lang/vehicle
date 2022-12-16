{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Vehicle.Syntax.BNFC.Delaborate.External
  ( Delaborate,
    delab,
  )
where

import Control.Monad.Identity (Identity (runIdentity), IdentityT)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (head, toList)
import Data.Text (Text, pack)
import Prettyprinter (Doc, Pretty (..), squote, squotes, (<+>))
import Vehicle.Syntax.AST qualified as V
import Vehicle.Syntax.AST.Arg
import Vehicle.Syntax.BNFC.Utils
import Vehicle.Syntax.External.Abs qualified as B
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
  delabM :: MonadDelab m => t -> m bnfc

instance Delaborate V.NamedProg B.Prog where
  delabM (V.Main decls) = B.Main . concat <$> traverse delabM decls

instance Delaborate V.NamedDecl [B.Decl] where
  delabM = \case
    V.DefResource _ n r t -> do
      defFun <- B.DefFunType (delabIdentifier n) tokElemOf <$> delabM t

      let defAnn = case r of
            V.Network -> delabAnn networkAnn []
            V.Dataset -> delabAnn datasetAnn []
            V.Parameter -> delabAnn parameterAnn []
            V.InferableParameter -> delabAnn parameterAnn [mkDeclAnnOption V.InferableOption True]

      return [defAnn, defFun]
    V.DefFunction _ n isProperty t e ->
      delabFun isProperty n t e
    V.DefPostulate {} ->
      error "Should not be delaborating postulates"

instance Delaborate V.NamedExpr B.Expr where
  delabM expr = case expr of
    V.Universe _ u -> return $ delabUniverse u
    V.Var _ n -> return $ B.Var (delabSymbol n)
    V.Hole _ n -> return $ B.Hole (mkToken B.HoleToken n)
    V.Literal _ l -> return $ delabLiteral l
    V.Ann _ e t -> B.Ann <$> delabM e <*> pure tokElemOf <*> delabM t
    V.LVec _ es -> B.VecLiteral tokSeqOpen <$> traverse delabM es <*> pure tokSeqClose
    V.Pi _ t1 t2 -> delabPi t1 t2
    V.Let _ e1 b e2 -> delabLet e1 b e2
    V.Lam _ binder body -> delabLam binder body
    V.Meta _ m -> return $ B.Var (mkToken B.Name (layoutAsText (pretty m)))
    V.App _ (V.Builtin _ b) args -> delabBuiltin b (onlyExplicit args)
    V.App _ (V.Literal _ l) _args -> return $ delabLiteral l
    V.App _ fun args -> delabApp <$> delabM fun <*> traverse delabM (NonEmpty.toList args)
    V.Builtin _ op -> delabBuiltin op []

instance Delaborate V.NamedArg B.Arg where
  delabM (V.Arg _ v _ e) = case v of
    V.Explicit {} -> B.ExplicitArg <$> delabM e
    V.Implicit {} -> B.ImplicitArg <$> delabM e
    V.Instance {} -> B.InstanceArg <$> delabM e

instance Delaborate V.NamedBinder B.BasicBinder where
  delabM (V.Binder _ _ v _ n t) = case v of
    V.Explicit -> B.ExplicitBinder (delabSymbol n) tokElemOf <$> delabM t
    V.Implicit -> B.ImplicitBinder (delabSymbol n) tokElemOf <$> delabM t
    V.Instance -> B.InstanceBinder (delabSymbol n) tokElemOf <$> delabM t

delabNameBinder :: MonadDelab m => V.NamedBinder -> m B.NameBinder
delabNameBinder b = case V.binderNamingForm b of
  V.OnlyType {} ->
    developerError $
      "Should not be delaborating an `OnlyType` binder of type"
        <+> pretty (show (V.binderRepresentation b))
        <+> "to a `NamedBinder`"
  V.NameAndType {} -> B.BasicNameBinder <$> delabM b
  V.OnlyName {} -> return $ case V.visibilityOf b of
    V.Explicit -> B.ExplicitNameBinder (delabSymbol (V.nameOf b))
    V.Implicit -> B.ImplicitNameBinder (delabSymbol (V.nameOf b))
    V.Instance -> B.InstanceNameBinder (delabSymbol (V.nameOf b))

delabTypeBinder :: MonadDelab m => V.NamedBinder -> m B.TypeBinder
delabTypeBinder b = case V.binderNamingForm b of
  V.OnlyName {} ->
    developerError $
      "Should not be delaborating an `OnlyName` binder of type"
        <+> pretty (show (V.binderRepresentation b))
        <+> "to a `TypeBinder`"
  V.NameAndType {} -> B.BasicTypeBinder <$> delabM b
  V.OnlyType {} -> case V.visibilityOf b of
    V.Explicit -> B.ExplicitTypeBinder <$> delabM (V.binderType b)
    V.Implicit -> B.ImplicitTypeBinder <$> delabM (V.binderType b)
    V.Instance -> B.InstanceTypeBinder <$> delabM (V.binderType b)

delabLetBinding :: MonadDelab m => (V.NamedBinder, V.NamedExpr) -> m B.LetDecl
delabLetBinding (binder, bound) = B.LDecl <$> delabNameBinder binder <*> delabM bound

delabLiteral :: V.Literal -> B.Expr
delabLiteral l = case l of
  V.LUnit -> B.Literal B.UnitLiteral
  V.LBool b -> B.Literal $ B.BoolLiteral $ delabBoolLit b
  V.LIndex _ x -> B.Literal $ B.NatLiteral $ delabNatLit x
  V.LNat n -> B.Literal $ B.NatLiteral $ delabNatLit n
  V.LInt i ->
    if i >= 0
      then B.Literal $ B.NatLiteral $ delabNatLit i
      else B.Neg tokSub (B.Literal $ B.NatLiteral $ delabNatLit (-i))
  V.LRat r -> B.Literal $ B.RatLiteral $ delabRatLit r

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

delabApp :: B.Expr -> [B.Arg] -> B.Expr
delabApp fun allArgs = go fun (reverse allArgs)
  where
    go fn [] = fn
    go fn (arg : args) = B.App (go fn args) arg

delabUniverse :: V.Universe -> B.Expr
delabUniverse = \case
  V.TypeUniv l -> tokType l
  V.PolarityUniv -> auxiliaryTypeError (pretty V.PolarityUniv)
  V.LinearityUniv -> auxiliaryTypeError (pretty V.LinearityUniv)

delabBuiltin :: MonadDelab m => V.Builtin -> [V.NamedExpr] -> m B.Expr
delabBuiltin fun args = case fun of
  V.Constructor c -> delabConstructor c <$> traverse delabM args
  V.Tensor -> do
    args' <- traverse delabM args
    return $ delabApp (B.Tensor tokTensor) (B.ExplicitArg <$> args')
  V.And -> delabTypeClassOp V.AndTC args
  V.Or -> delabTypeClassOp V.OrTC args
  V.Implies -> delabTypeClassOp V.ImpliesTC args
  V.Not -> delabTypeClassOp V.NotTC args
  V.If -> delabIf <$> traverse delabM args
  V.FromNat {} -> delabM (head args)
  V.FromRat {} -> delabM (head args)
  V.FromVec {} -> delabM (head args)
  V.Neg _ -> delabTypeClassOp V.NegTC args
  V.Add _ -> delabTypeClassOp V.AddTC args
  V.Sub _ -> delabTypeClassOp V.SubTC args
  V.Mul _ -> delabTypeClassOp V.MulTC args
  V.Div _ -> delabTypeClassOp V.DivTC args
  V.Equals _ op -> delabTypeClassOp (V.EqualsTC op) args
  V.Order _ op -> delabTypeClassOp (V.OrderTC op) args
  V.Fold _ -> delabTypeClassOp V.FoldTC args
  V.Map _ -> delabTypeClassOp V.MapTC args
  V.At -> delabInfixOp2 B.At tokAt <$> traverse delabM args
  V.Foreach -> delabForeach args
  V.TypeClassOp tc -> delabTypeClassOp tc args

delabConstructor :: V.BuiltinConstructor -> [B.Expr] -> B.Expr
delabConstructor fun args = case fun of
  V.Unit -> B.Unit tokUnit
  V.Bool -> B.Bool tokBool
  V.Nat -> B.Nat tokNat
  V.Int -> B.Int tokInt
  V.Rat -> B.Rat tokRat
  V.List -> delabApp (B.List tokList) (B.ExplicitArg <$> args)
  V.Vector -> delabApp (B.Vector tokVector) (B.ExplicitArg <$> args)
  V.Index -> delabApp (B.Index tokIndex) (B.ExplicitArg <$> args)
  V.Polarity {} -> auxiliaryTypeError (pretty fun)
  V.Linearity {} -> auxiliaryTypeError (pretty fun)
  V.TypeClass tc -> case tc of
    V.HasEq V.Eq -> delabApp (B.HasEq tokHasEq) (B.ExplicitArg <$> args)
    V.HasAdd -> delabApp (B.HasAdd tokHasAdd) (B.ExplicitArg <$> args)
    V.HasSub -> delabApp (B.HasSub tokHasSub) (B.ExplicitArg <$> args)
    V.HasMul -> delabApp (B.HasMul tokHasMul) (B.ExplicitArg <$> args)
    _ -> delabApp (B.Var (delabSymbol (layoutAsText $ pretty tc))) (B.ExplicitArg <$> args)
  V.Nil -> B.Nil tokNil
  V.Cons -> delabInfixOp2 B.Cons tokCons args

delabTypeClassOp :: MonadDelab m => V.TypeClassOp -> [V.NamedExpr] -> m B.Expr
delabTypeClassOp op args = case op of
  V.AndTC -> delabInfixOp2 B.And tokAnd <$> traverse delabM args
  V.OrTC -> delabInfixOp2 B.Or tokOr <$> traverse delabM args
  V.ImpliesTC -> delabInfixOp2 B.Impl tokImpl <$> traverse delabM args
  V.NotTC -> delabOp1 B.Not tokNot <$> traverse delabM args
  V.FromNatTC {} -> delabM $ head args
  V.FromRatTC {} -> delabM $ head args
  V.FromVecTC {} -> delabM $ head args
  V.NegTC -> delabOp1 B.Neg tokSub <$> traverse delabM args
  V.AddTC -> delabInfixOp2 B.Add tokAdd <$> traverse delabM args
  V.SubTC -> delabInfixOp2 B.Sub tokSub <$> traverse delabM args
  V.MulTC -> delabInfixOp2 B.Mul tokMul <$> traverse delabM args
  V.DivTC -> delabInfixOp2 B.Div tokDiv <$> traverse delabM args
  V.EqualsTC eq -> case eq of
    V.Eq -> delabInfixOp2 B.Eq tokEq <$> traverse delabM args
    V.Neq -> delabInfixOp2 B.Neq tokNeq <$> traverse delabM args
  V.OrderTC ord -> case ord of
    V.Le -> delabInfixOp2 B.Le tokLe <$> traverse delabM args
    V.Lt -> delabInfixOp2 B.Lt tokLt <$> traverse delabM args
    V.Ge -> delabInfixOp2 B.Ge tokGe <$> traverse delabM args
    V.Gt -> delabInfixOp2 B.Gt tokGt <$> traverse delabM args
  V.MapTC -> do
    args' <- traverse delabM args
    return $ delabApp (B.Map tokMap) (B.ExplicitArg <$> args')
  V.FoldTC -> do
    args' <- traverse delabM args
    return $ delabApp (B.Fold tokFold) (B.ExplicitArg <$> args')
  V.QuantifierTC q -> delabQuantifier q args
  V.QuantifierInTC q -> delabQuantifierIn q args

delabOp1 :: IsToken token => (token -> B.Expr -> B.Expr) -> token -> [B.Expr] -> B.Expr
delabOp1 op tk [arg] = op tk arg
delabOp1 _ tk args = argsError (tkSymbol tk) 1 args

delabOp2 :: IsToken token => (token -> B.Expr -> B.Expr -> B.Expr) -> token -> [B.Expr] -> B.Expr
delabOp2 op tk = \case
  [arg1, arg2] -> op tk arg1 arg2
  args
    | length args > 2 -> argsError (tkSymbol tk) 2 args
    | otherwise -> delabPartialSection 2 args (delabOp2 op tk)

delabOp3 :: IsToken token => (token -> B.Expr -> B.Expr -> B.Expr -> B.Expr) -> token -> [B.Expr] -> B.Expr
delabOp3 op tk = \case
  [arg1, arg2, arg3] -> op tk arg1 arg2 arg3
  args
    | length args > 3 -> argsError (tkSymbol tk) 3 args
    | otherwise -> delabPartialSection 3 args (delabOp3 op tk)

delabInfixOp2 :: IsToken token => (B.Expr -> token -> B.Expr -> B.Expr) -> token -> [B.Expr] -> B.Expr
delabInfixOp2 op tk = \case
  [arg1, arg2] -> op arg1 tk arg2
  args
    | length args > 2 -> argsError (tkSymbol tk) 2 args
    | otherwise -> delabPartialSection 2 args (delabInfixOp2 op tk)

delabPartialSection :: Int -> [B.Expr] -> ([B.Expr] -> B.Expr) -> B.Expr
delabPartialSection expectedArgs actualArgs mkOp = do
  -- This is a hack until we get section syntax.
  let missingArgNumbers = [0 .. (expectedArgs - length actualArgs)]
  let missingVarNames = fmap (\v -> delabSymbol (pack "x" <> pack (show v))) missingArgNumbers
  let missingVars = fmap B.Var missingVarNames
  let missingBinders = fmap B.ExplicitNameBinder missingVarNames
  B.Lam tokLambda missingBinders tokArrow (mkOp (actualArgs <> missingVars))

delabIf :: [B.Expr] -> B.Expr
delabIf [arg1, arg2, arg3] = B.If tokIf arg1 tokThen arg2 tokElse arg3
delabIf args = argsError "if" 3 args

argsError :: Text -> Int -> [B.Expr] -> a
argsError s n args =
  developerError $
    "Expecting"
      <+> pretty n
      <+> "arguments for"
      <+> squotes (pretty s)
      <+> "but found"
      <+> pretty (length args)
      <+> "arguments:"
      <+> squotes (pretty (show args))

-- | Collapses pi expressions into either a function or a sequence of forall bindings
delabPi :: MonadDelab m => V.NamedBinder -> V.NamedExpr -> m B.Expr
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
delabLet :: MonadDelab m => V.NamedExpr -> V.NamedBinder -> V.NamedExpr -> m B.Expr
delabLet bound binder body = do
  let (boundExprs, foldedBody) = foldLetBinders body
  binders' <- traverse delabLetBinding boundExprs
  body' <- delabM foldedBody
  return $ B.Let tokLet binders' body'

-- | Collapses consecutative lambda expressions into a sequence of binders
delabLam :: MonadDelab m => V.NamedBinder -> V.NamedExpr -> m B.Expr
delabLam binder body = do
  let (foldedBinders, foldedBody) = foldBinders (FoldableBinder LamFold binder) body
  binders' <- traverse delabNameBinder (binder : foldedBinders)
  body' <- delabM foldedBody
  return $ B.Lam tokLambda binders' tokArrow body'

delabFun :: MonadDelab m => Bool -> V.Identifier -> V.NamedExpr -> V.NamedExpr -> m [B.Decl]
delabFun isProperty name typ expr = do
  let n' = delabIdentifier name
  let (binders, body) = foldBinders FunFold expr
  if V.isTypeSynonym typ
    then do
      defType <- B.DefType n' <$> traverse delabNameBinder binders <*> delabM body
      return [defType]
    else do
      defType <- B.DefFunType n' tokElemOf <$> delabM typ
      defExpr <- B.DefFunExpr n' <$> traverse delabNameBinder binders <*> delabM body
      let decl = [defType, defExpr]
      return $
        if isProperty
          then delabAnn propertyAnn [] : decl
          else decl

delabQuantifier :: MonadDelab m => V.Quantifier -> [V.NamedExpr] -> m B.Expr
delabQuantifier q = \case
  [V.Lam _ binder body] -> do
    let (foldedBinders, foldedBody) = foldBinders (FoldableBinder (QuantFold q) binder) body
    binders' <- traverse delabNameBinder (binder : foldedBinders)
    body' <- delabM foldedBody
    let mkTk = case q of
          V.Forall -> B.Forall tokForall
          V.Exists -> B.Exists tokExists
    return $ mkTk binders' tokDot body'
  args -> do
    let sym = case q of V.Forall -> tkSymbol tokForall; V.Exists -> tkSymbol tokExists
    argsError sym 1 <$> traverse delabM args

delabQuantifierIn :: MonadDelab m => V.Quantifier -> [V.NamedExpr] -> m B.Expr
delabQuantifierIn q = \case
  [V.Lam _ binder body, cont] -> do
    let (foldedBinders, foldedBody) = foldBinders (FoldableBinder (QuantInFold q) binder) body
    binders' <- traverse delabNameBinder (binder : foldedBinders)
    cont' <- delabM cont
    body' <- delabM foldedBody
    let mkTk = case q of
          V.Forall -> B.ForallIn tokForall
          V.Exists -> B.ExistsIn tokExists
    return $ mkTk binders' cont' tokDot body'
  args -> do
    let sym = case q of V.Forall -> tkSymbol tokForall; V.Exists -> tkSymbol tokExists
    argsError sym 2 <$> traverse delabM args

delabForeach :: MonadDelab m => [V.NamedExpr] -> m B.Expr
delabForeach = \case
  [V.Lam _ binder body] -> do
    let (foldedBinders, foldedBody) = foldBinders (FoldableBinder ForeachFold binder) body
    binders' <- traverse delabNameBinder (binder : foldedBinders)
    body' <- delabM foldedBody
    return $ B.Foreach tokForeach binders' tokDot body'
  args -> argsError (tkSymbol tokForeach) 1 <$> traverse delabM args

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
