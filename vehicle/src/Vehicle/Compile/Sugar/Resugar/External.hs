{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Vehicle.Compile.Sugar.Resugar.External
  ( Delaborate,
    delab,
  )
where

import Control.Monad.Identity (Identity (runIdentity))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text, pack)
import Vehicle.Compile.Sugar.Binders
import Vehicle.Compile.Sugar.Core
import Vehicle.Data.AST qualified as V
import Vehicle.Data.AST.Arg
import Vehicle.Data.AST.Decl (LHSBinderCount)
import Vehicle.Data.AST.Expr.Desugared qualified as V
import Vehicle.Data.Builtin.Standard.Core qualified as V
import Vehicle.Data.Tensor (Tensor (..))
import Vehicle.Prelude.Error
import Vehicle.Prelude.Prettyprinter
import Vehicle.Syntax.External.Abs qualified as B
import Vehicle.Syntax.Token

--------------------------------------------------------------------------------
-- Conversion to BNFC AST

delab :: (Show t, Delaborate t bnfc) => t -> bnfc
delab e = runIdentity (delabM e)

--------------------------------------------------------------------------------
-- Implementation

-- | Constraint for the monad stack used by the elaborator.
type MonadDelab m = Monad m

-- * Conversion

class Delaborate t bnfc | t -> bnfc where
  delabM :: (MonadDelab m) => t -> m bnfc

instance Delaborate (V.Module V.Builtin) B.Module where
  delabM (V.Module imports decls) = do
    imports' <- traverse delabM imports
    decls' <- concat <$> traverse delabM decls
    return $ B.DefModule imports' decls'

instance Delaborate V.ImportStatement B.ImportStatement where
  delabM (V.ImportStatement (V.ModulePath path)) = do
    let path' = fmap go path
    return $ B.Import path'
    where
      go :: String -> B.ModulePathFragment
      go = B.ModulePathFrag . mkToken B.Name . pack

instance Delaborate (V.Decl V.Builtin) [B.Decl] where
  delabM = \case
    V.DefAbstract _ n a t -> do
      defFun <- B.DefFunType (delabIdentifier n) tokElemOf <$> delabM t

      defAnn <- case a of
        V.BuiltinDef -> return $ delabAnn builtinAnn []
        V.NetworkDef -> return $ delabAnn networkAnn []
        V.DatasetDef -> return $ delabAnn datasetAnn []
        V.ParameterDef sort -> return $ case sort of
          V.NonInferable -> delabAnn parameterAnn []
          V.Inferable -> delabAnn parameterAnn [mkBoolAnnOption InferableOption True]

      return [defAnn, defFun]
    V.DefFunction _ n sort t e -> case sort of
      V.TypeDecl binderCount -> delabTypeDecl n binderCount e
      V.FunctionDecl binderCount ann -> delabFunctionDecl n binderCount ann t e
      V.ProjectionDecl binderCount -> delabFunctionDecl n binderCount Nothing t e
      V.TensorCoercionDecl binderCount -> delabFunctionDecl n binderCount Nothing t e
    V.DefRecord _ n sort t e s -> do
      delabRecordDecl n sort t e s

instance Delaborate (V.Expr V.Builtin) B.Expr where
  delabM expr = case expr of
    V.Universe _ -> return $ B.Type (mkToken B.TokType "Type")
    V.Var _ n -> return $ B.Var (delabSymbol n)
    V.Hole _ n -> return $ B.Hole (mkToken B.HoleToken n)
    V.Pi _ t1 t2 -> delabPi t1 t2
    V.Let _ e1 b e2 -> delabLet e1 b e2
    V.Lam _ binder body -> delabLam binder body
    V.App (V.Builtin _ b) args -> delabBuiltin b (NonEmpty.toList args)
    V.App fun args -> do
      fun' <- delabM fun
      delabApp fun' (NonEmpty.toList args)
    V.Builtin _ op -> delabBuiltin op []
    V.Record _ fields -> delabRecord fields
    V.RecordAcc _ record field -> delabRecordAccess record field

instance Delaborate (V.Arg V.Builtin) B.Arg where
  delabM arg = do
    let modalities = delabRelevance arg
    e' <- delabM (V.argExpr arg)
    return $ case (V.visibilityOf arg, modalities) of
      (V.Explicit {}, []) -> B.ExplicitArg e'
      (V.Explicit {}, m : ms) -> B.ExplicitArgMods m ms e'
      (V.Implicit {}, _) -> B.ImplicitArg modalities e'
      (V.Instance {}, _) -> B.InstanceArg modalities e'

instance Delaborate (V.Binder V.Builtin) B.BasicBinder where
  delabM binder = do
    let n' = delabSymbol $ fromMaybe "_" (V.nameOf binder)
    let m' = delabModalities binder
    t' <- delabM (V.binderValue binder)
    return $ case V.visibilityOf binder of
      V.Explicit -> B.ExplicitBinder m' n' tokElemOf t'
      V.Implicit {} -> B.ImplicitBinder m' n' tokElemOf t'
      V.Instance {} -> B.InstanceBinder m' n' tokElemOf t'

instance Delaborate V.FieldName B.Name where
  delabM (V.FieldName _ name) = return $ mkToken B.Name name

instance Delaborate (V.GenericRecordField (V.Expr V.Builtin)) B.RecordFieldDef where
  delabM (name, typ) = do
    name' <- delabM name
    typ' <- delabM typ
    return $ B.FieldDef name' tokElemOf typ'

instance Delaborate V.DefRecordSort B.Decl where
  delabM = \case
    V.AnnTensor -> return $ delabAnn tensorAnn []
    V.AnnTypeClass -> return $ delabAnn typeClassAnn []

instance Delaborate V.FunctionDeclAnnotation B.Decl where
  delabM = \case
    V.AnnInstance t -> return $ delabAnn instanceAnn $ case t of
      Nothing -> []
      Just v -> [mkMaybeIntAnnOption DefaultOption v]
    V.AnnProperty -> return $ delabAnn propertyAnn []

-- | Used for things not in the user-syntax.
cheatDelab :: Text -> B.Expr
cheatDelab n = B.Var (delabSymbol n)

cheatDelabPretty :: (MonadDelab m, Pretty a) => a -> [V.Arg V.Builtin] -> m B.Expr
cheatDelabPretty x = delabApp (cheatDelab (layoutAsText $ pretty x))

delabRelevance :: (V.HasRelevance a) => a -> [B.Modality]
delabRelevance x = case V.relevanceOf x of
  V.Relevant -> []
  V.Irrelevant -> [B.Irrelevant]

delabRecordAccess :: (MonadDelab m) => V.Expr V.Builtin -> V.FieldName -> m B.Expr
delabRecordAccess expr fieldName = do
  let tok = mkToken B.FieldAccess ("." <> V.nameOf fieldName)
  B.RecordAcc <$> delabM expr <*> pure tok

delabNameBinder :: (MonadDelab m) => V.Binder V.Builtin -> m B.NameBinder
delabNameBinder b = case V.binderNamingForm b of
  V.OnlyType {} ->
    developerError $
      "Should not be delaborating the `OnlyType` binder to a `NamedBinder`"
        <> lineIndent ("binder:" <> pretty (show (V.typeOf b)))
  V.NameAndType {} -> B.BasicNameBinder <$> delabM b
  V.OnlyName name _ -> do
    let modalities = delabModalities b
    let finalName = delabSymbol name
    return $ case (V.visibilityOf b, modalities) of
      (V.Explicit, []) -> B.ExplicitNameBinder finalName
      (V.Explicit, _m : _ms) -> B.ExplicitNameBinder finalName -- B.ExplicitNameBinderMods m ms finalName
      (V.Implicit {}, _) -> B.ImplicitNameBinder modalities finalName
      (V.Instance {}, _) -> B.InstanceNameBinder modalities finalName

delabModalities :: V.Binder V.Builtin -> [B.Modality]
delabModalities binder
  | V.isRelevant binder = mempty
  | otherwise = [B.Irrelevant]

delabTypeBinder :: (MonadDelab m) => V.Binder V.Builtin -> m B.TypeBinder
delabTypeBinder b = case V.binderNamingForm b of
  V.OnlyName {} ->
    developerError
      "Should not be delaborating an `OnlyName` binder to a `TypeBinder`"
  V.NameAndType {} -> B.BasicTypeBinder <$> delabM b
  V.OnlyType {} -> do
    let modalities = delabModalities b
    typ <- delabM (V.binderValue b)
    return $ case (V.visibilityOf b, modalities) of
      (V.Explicit, []) -> B.ExplicitTypeBinder typ
      (V.Explicit, _m : _ms) -> B.ExplicitTypeBinder typ -- B.ExplicitTypeBinderMods m ms typ
      (V.Implicit {}, _) -> B.ImplicitTypeBinder modalities typ
      (V.Instance {}, _) -> B.InstanceTypeBinder modalities typ

delabLetBinding :: (MonadDelab m) => (V.Binder V.Builtin, V.Expr V.Builtin) -> m B.LetDecl
delabLetBinding (binder, bound) = B.LDecl <$> delabNameBinder binder <*> delabM bound

delabNatLit :: Int -> B.Natural
delabNatLit n = mkToken B.Natural (pack $ show n)

{-
delabRatLit :: Rational -> B.Rational
delabRatLit r = mkToken B.Rational (pack $ show (fromRational r :: Double))
-}
delabSymbol :: Text -> B.Name
delabSymbol = mkToken B.Name

delabIdentifier :: V.Identifier -> B.Name
delabIdentifier (V.Identifier _ n) = mkToken B.Name n

delabApp :: (MonadDelab m) => B.Expr -> [V.Arg V.Builtin] -> m B.Expr
delabApp fun allArgs = go fun <$> traverse delabM (reverse allArgs)
  where
    go fn [] = fn
    go fn (arg : args) = B.App (go fn args) arg

delabBuiltin :: (MonadDelab m) => V.Builtin -> [V.Arg V.Builtin] -> m B.Expr
delabBuiltin fun args = case fun of
  V.BuiltinFunction f -> delabBuiltinFunction f args
  V.BuiltinType t -> delabBuiltinType t args
  V.BuiltinConstructor c -> delabConstructor c args
  V.BuiltinCast c -> delabCast c args
  V.TypeClassOp tc -> delabTypeClassOp tc args
  V.TypeClass t -> delabTypeClass t args
  V.NatInDomainConstraint -> cheatDelabPretty fun args
  V.DerivedFunction f -> delabDerivedFunction f args

delabCast :: (MonadDelab m) => V.BuiltinCast -> [V.Arg V.Builtin] -> m B.Expr
delabCast fun args = case fun of
  V.FromNat {} -> rawDelab
  V.FromRat {} -> rawDelab
  V.FromVectorToList {} -> rawDelab
  where
    rawDelab = cheatDelabPretty fun args

delabDerivedFunction :: (MonadDelab m) => V.DerivedFunction -> [V.Arg V.Builtin] -> m B.Expr
delabDerivedFunction fun args = case fun of
  -- Reverse the arguments to make it un-well typed again
  V.TypeAnn -> delabInfixOp2 B.Ann tokElemOf (reverse args)
  V.QuantifyIndex q -> delabQuantifier q args
  V.QuantifyInList q -> delabQuantifierIn q args

delabBuiltinFunction :: (MonadDelab m) => V.BuiltinFunction -> [V.Arg V.Builtin] -> m B.Expr
delabBuiltinFunction fun args = case fun of
  V.Not -> delabOp1 B.Not tokNot args
  V.And -> delabInfixOp2 B.And tokAnd args
  V.Or -> delabInfixOp2 B.Or tokOr args
  V.Implies -> delabInfixOp2 B.Impl tokImpl args
  V.If -> delabIf args
  V.Add _dom -> delabInfixOp2 B.Add tokAdd args
  V.Mul _dom -> delabInfixOp2 B.Mul tokMul args
  V.Neg _dom -> delabTypeClassOp V.NegTC args
  V.Sub _dom -> delabInfixOp2 B.Sub tokSub args
  V.Div _dom -> delabInfixOp2 B.Div tokDiv args
  V.Min _dom -> delabApp (B.Min tokMin) args
  V.Max _dom -> delabApp (B.Max tokMax) args
  V.Pow _dom -> delabInfixOp2 B.Pow tokPow args
  V.Log _dom -> delabApp (B.Log tokLog) args
  V.Exp _dom -> delabApp (B.Exp tokExp) args
  V.QuantifyRatTensor q -> delabQuantifier q args
  V.QuantifyRecord q -> delabQuantifier q args
  V.CompareRatTensor op -> case args of
    [V.headOf . argExpr -> V.Builtin _ (V.BuiltinConstructor V.Nil), _rDims, xs, ys] -> delabCompareReduced op [xs, ys]
    [_pDims, V.headOf . argExpr -> V.Builtin _ (V.BuiltinConstructor V.Nil), xs, ys] -> delabComparePointwise op [xs, ys]
    [xs, ys] -> delabCompareReduced op [xs, ys]
    _ -> cheatDelabPretty op args
  V.CompareIndex op -> delabComparison op args
  V.CompareNat op -> delabComparison op args
  V.FoldList -> delabTypeClassOp V.FoldTC args
  V.MapList -> delabTypeClassOp V.MapTC args
  V.AppendList -> delabApp (B.Append tokAppend) args
  V.AtTensor -> delabInfixOp2 B.At tokAt args
  V.AtVector -> delabInfixOp2 B.At tokAt args
  V.ForeachTensor -> delabForeach args
  V.ForeachVector -> delabForeach args
  V.ReduceAndTensor -> delabApp (B.ReduceAnd tokReduceAnd) args
  V.ReduceOrTensor -> delabApp (B.ReduceOr tokReduceOr) args
  -- Builtins not yet in the surface syntax.
  V.ReduceAddRatTensor -> delabApp (B.ReduceAdd tokReduceAdd) args
  V.ReduceMulRatTensor -> delabApp (B.ReduceMul tokReduceMul) args
  V.ReduceMaxRatTensor -> delabApp (B.ReduceMax tokReduceMax) args
  V.ReduceMinRatTensor -> delabApp (B.ReduceMin tokReduceMin) args
  V.StackTensor {} -> rawDelab
  V.ConstTensor -> delabApp (B.Const tokConst) args
  V.Iterate -> rawDelab
  V.Transpose -> delabApp (B.Transpose tokTranspose) args
  V.SearchRatTensor {} -> rawDelab
  V.WhereTensor {} -> rawDelab
  V.ReverseList -> rawDelab
  where
    rawDelab = cheatDelabPretty fun args

delabBuiltinType :: (MonadDelab m) => V.BuiltinType -> [V.Arg V.Builtin] -> m B.Expr
delabBuiltinType fun args = case fun of
  V.UnitType -> delabApp (B.Unit tokUnit) args
  V.BoolType -> delabApp (B.Bool tokBool) args
  V.RatType -> delabApp (B.Real tokReal) args
  V.IndexType -> delabApp (B.Index tokIndex) args
  V.NatType -> delabApp (B.Nat tokNat) args
  V.ListType -> delabApp (B.List tokList) args
  V.VectorType -> delabApp (B.Vector tokVector) args
  V.TensorType -> delabApp (B.Tensor tokTensor) args

delabTypeClass :: (MonadDelab m) => V.TypeClass -> [V.Arg V.Builtin] -> m B.Expr
delabTypeClass tc args = case tc of
  V.HasMap -> delabApp (B.HasMap tokHasMap) args
  V.HasFold -> delabApp (B.HasFold tokHasFold) args
  _ -> cheat
  where
    cheat = delabApp (B.Var (delabSymbol (layoutAsText $ pretty tc))) args

delabConstructor :: (MonadDelab m) => V.BuiltinConstructor -> [V.Arg V.Builtin] -> m B.Expr
delabConstructor fun args = case fun of
  V.Cons -> delabInfixOp2 B.Cons tokCons args
  V.Nil -> delabApp (B.Nil tokNil) args
  V.UnitLiteral -> return $ B.Literal B.UnitLiteral
  V.NatLiteral x -> return $ B.Literal $ B.NatLiteral $ delabNatLit x
  V.IndexLiteral x -> return $ B.Literal $ B.NatLiteral $ delabNatLit x
  V.VectorLiteral -> delabVecLiteral args
  V.NatTensorLiteral t -> return $ delabTensor t
  V.RatTensorLiteral t -> return $ delabTensor t
  V.BoolTensorLiteral t -> return $ delabTensor t

delabTensor :: (Pretty a) => Tensor a -> B.Expr
delabTensor t = case t of
  ConstantTensor [] value -> cheat value
  ConstantTensor shape value -> B.App (B.App (cheatDelab "const") (B.ExplicitArg $ cheat value)) (B.ExplicitArg $ cheat shape)
  denseTensor -> cheat denseTensor
  where
    cheat :: (Pretty b) => b -> B.Expr
    cheat = cheatDelab . layoutAsText . pretty

delabTypeClassOp :: (MonadDelab m) => V.TypeClassOp -> [V.Arg V.Builtin] -> m B.Expr
delabTypeClassOp op args = case op of
  V.FromNatTC {} -> cheatDelabPretty op args
  V.FromRatTC {} -> cheatDelabPretty op args
  V.VecLiteralTC {} -> delabVecLiteral args
  V.NegTC -> delabOp1 B.Neg tokSub args
  V.MapTC -> delabApp (B.Map tokMap) args
  V.FoldTC -> delabApp (B.Fold tokFold) args
  V.AtTC -> delabInfixOp2 B.At tokAt args
  V.ForeachTC -> delabForeach args
  V.TensorTypeTC -> cheatDelabPretty op args

delabComparison :: (MonadDelab m) => V.ComparisonOp -> [V.Arg V.Builtin] -> m B.Expr
delabComparison op args = case op of
  V.Eq -> delabInfixOp2 B.Eq tokEq args
  V.Ne -> delabInfixOp2 B.Ne tokNe args
  V.Le -> delabInfixOp2 B.Le tokLe args
  V.Lt -> delabInfixOp2 B.Lt tokLt args
  V.Ge -> delabInfixOp2 B.Ge tokGe args
  V.Gt -> delabInfixOp2 B.Gt tokGt args

delabOp1 :: (MonadDelab m, IsToken token) => (token -> B.Expr -> B.Expr) -> token -> [V.Arg V.Builtin] -> m B.Expr
delabOp1 op tk [arg]
  | V.isExplicit arg = op tk <$> delabM (argExpr arg)
delabOp1 _ tk args = delabApp (cheatDelab $ tkSymbol tk) args

delabInfixOp2 :: (MonadDelab m, IsToken token) => (B.Expr -> token -> B.Expr -> B.Expr) -> token -> [V.Arg V.Builtin] -> m B.Expr
delabInfixOp2 op tk args@[arg1, arg2]
  | all V.isExplicit args = op <$> delabM (argExpr arg1) <*> pure tk <*> delabM (argExpr arg2)
delabInfixOp2 _op tk args
  | null args = delabApp (cheatDelab $ "(" <> tkSymbol tk <> ")") []
  | otherwise = delabApp (cheatDelab $ tkSymbol tk) args

delabIf :: (MonadDelab m) => [V.Arg V.Builtin] -> m B.Expr
delabIf args@[arg1, arg2, arg3]
  | all V.isExplicit args = do
      e1 <- delabM (argExpr arg1)
      e2 <- delabM (argExpr arg2)
      e3 <- delabM (argExpr arg3)
      return $ B.If tokIf e1 tokThen e2 tokElse e3
delabIf args = cheatDelabPretty V.If args

delabTelescope :: (MonadDelab m) => V.Binder V.Builtin -> V.Expr V.Builtin -> m ([B.NameBinder], B.Expr)
delabTelescope binder body = do
  let (foldedBinders, foldedBody) = foldPiBinders binder body
  binders' <- traverse delabNameBinder (binder : foldedBinders)
  body' <- delabM foldedBody
  return (binders', body')

-- | Collapses pi expressions into either a function or a sequence of forall bindings
delabPi :: (MonadDelab m) => V.Binder V.Builtin -> V.Expr V.Builtin -> m B.Expr
delabPi binder body = case V.binderNamingForm binder of
  V.OnlyType -> do
    binder' <- delabTypeBinder binder
    body' <- delabM body
    return $ B.Fun binder' tokArrow body'
  _ -> do
    (binders', body') <- delabTelescope binder body
    return $ B.ForallT tokForallT binders' body'

-- | Collapses let expressions into a sequence of let declarations
delabLet :: (MonadDelab m) => V.Expr V.Builtin -> V.Binder V.Builtin -> V.Expr V.Builtin -> m B.Expr
delabLet bound binder body = do
  let (otherBoundExprs, foldedBody) = foldLetBinders body
  let boundExprs = (binder, bound) : otherBoundExprs
  binders' <- traverse delabLetBinding boundExprs
  body' <- delabM foldedBody
  return $ B.Let tokLet binders' body'

-- | Collapses consecutative lambda expressions into a sequence of binders
delabLam :: (MonadDelab m) => V.Binder V.Builtin -> V.Expr V.Builtin -> m B.Expr
delabLam binder body = do
  let (foldedBinders, foldedBody) = foldLamBinders binder body
  binders' <- traverse delabNameBinder (binder : foldedBinders)
  body' <- delabM foldedBody
  return $ B.Lam tokLambda binders' tokArrow body'

delabFunctionDecl ::
  (MonadDelab m) =>
  V.Identifier ->
  LHSBinderCount ->
  Maybe V.FunctionDeclAnnotation ->
  V.Expr V.Builtin ->
  V.Expr V.Builtin ->
  m [B.Decl]
delabFunctionDecl name lhsBinderCount maybeAnn typ expr = do
  annDecls <- maybeToList <$> traverse delabM maybeAnn
  let n' = delabIdentifier name
  let (binders, body) = foldDeclBinders lhsBinderCount expr
  defType <- B.DefFunType n' tokElemOf <$> delabM typ
  defExpr <- B.DefFunExpr n' <$> traverse delabNameBinder binders <*> delabM body
  return $ annDecls <> [defType, defExpr]

delabTypeDecl ::
  (MonadDelab m) =>
  V.Identifier ->
  LHSBinderCount ->
  V.Expr V.Builtin ->
  m [B.Decl]
delabTypeDecl ident binderCount expr = do
  let (binders, body) = foldDeclBinders binderCount expr
  let n' = delabIdentifier ident
  defType <- B.DefType n' <$> traverse delabNameBinder binders <*> delabM body
  return [defType]

delabRecordDecl ::
  (MonadDelab m) =>
  V.Identifier ->
  Maybe V.DefRecordSort ->
  V.Telescope V.Builtin ->
  V.RecordFields V.Builtin ->
  [V.DerivableRecordOperation] ->
  m [B.Decl]
delabRecordDecl ident sort telescope fields supports = do
  annDecl <- traverse delabM $ maybeToList sort
  let n' = delabIdentifier ident
  telescope' <- traverse delabNameBinder telescope
  fields' <- traverse delabM fields
  let supports' = delabSupportedOperations supports
  return $ annDecl <> [B.DefRecord n' telescope' fields' supports']

delabSupportedOperations :: [V.DerivableRecordOperation] -> B.RecordSupports
delabSupportedOperations = \case
  [] -> B.NoSupports
  ops -> B.Supports $ fmap (\op -> mkToken B.Name (pack $ show op)) ops

delabQuantifier :: (MonadDelab m) => V.Quantifier -> [V.Arg V.Builtin] -> m B.Expr
delabQuantifier q args = case reverse args of
  V.RelevantExplicitArg (V.Lam _ binder body) : _rArgs -> do
    (binders', body') <- case _rArgs of
      [upperBound, lowerBound] | q == V.Exists -> do
        -- Special case rendering for when we have just added the domain to the quantifiers in the loss backend.
        -- Should probably handle this better by adding special syntax for this in the grammar...
        let bounds = V.normAppList (V.Var mempty "boundedBy") [lowerBound, upperBound]
        let andExpr = V.normAppList (V.Builtin mempty (V.BuiltinFunction V.And)) [explicit bounds, explicit body]
        return ([binder], andExpr)
      _ -> return $ first (binder :) $ foldQuantifierBinders q binder body

    let mkTk = case q of
          V.Forall -> B.Forall tokForall
          V.Exists -> B.Exists tokExists

    binders'' <- traverse delabNameBinder binders'
    body'' <- delabM body'
    return $ mkTk binders'' body''
  _ -> cheatDelabPretty q args

delabQuantifierIn :: (MonadDelab m) => V.Quantifier -> [V.Arg V.Builtin] -> m B.Expr
delabQuantifierIn q args = case reverse args of
  V.RelevantExplicitArg (V.Lam _ binder body) : V.RelevantExplicitArg container : _ -> do
    binder' <- delabNameBinder binder
    body' <- delabM body
    container' <- delabM container
    let mkTk = case q of
          V.Forall -> B.ForallIn tokForall
          V.Exists -> B.ExistsIn tokExists
    return $ mkTk binder' container' body'
  _ -> cheatDelabPretty q args

delabForeach :: (MonadDelab m) => [V.Arg V.Builtin] -> m B.Expr
delabForeach args = case reverse args of
  V.RelevantExplicitArg (V.Lam _ binder body) : _ -> do
    let (foldedBinders, foldedBody) = foldForeachBinders binder body
    binders' <- traverse delabNameBinder (binder : foldedBinders)
    body' <- delabM foldedBody
    return $ B.Foreach tokForeach binders' body'
  _ -> cheatDelabPretty V.ForeachTC args

delabRecord :: (MonadDelab m) => V.RecordFields V.Builtin -> m B.Expr
delabRecord fields = do
  fields' <- traverse (bitraverse delabM delabM) fields
  return $ B.Record (fmap (uncurry B.FieldAssign) fields')

delabCompareReduced :: (MonadDelab m) => V.ComparisonOp -> [V.Arg V.Builtin] -> m B.Expr
delabCompareReduced op args = case op of
  V.Eq -> delabInfixOp2 B.Eq tokEq args
  V.Ne -> delabInfixOp2 B.Ne tokNe args
  V.Le -> delabInfixOp2 B.Le tokLe args
  V.Lt -> delabInfixOp2 B.Lt tokLt args
  V.Ge -> delabInfixOp2 B.Ge tokGe args
  V.Gt -> delabInfixOp2 B.Gt tokGt args

delabComparePointwise :: (MonadDelab m) => V.ComparisonOp -> [V.Arg V.Builtin] -> m B.Expr
delabComparePointwise op args = case op of
  V.Eq -> delabInfixOp2 B.EqPoint tokEqPoint args
  V.Ne -> delabInfixOp2 B.NePoint tokNePoint args
  V.Le -> delabInfixOp2 B.LePoint tokLePoint args
  V.Lt -> delabInfixOp2 B.LtPoint tokLtPoint args
  V.Ge -> delabInfixOp2 B.GePoint tokGePoint args
  V.Gt -> delabInfixOp2 B.GtPoint tokGtPoint args

delabAnn :: B.TokAnnotation -> [B.DeclAnnOption] -> B.Decl
delabAnn name [] = B.DefAnn name B.DeclAnnWithoutOpts
delabAnn name ops = B.DefAnn name $ B.DeclAnnWithOpts ops

delabVecLiteral :: (MonadDelab m) => [V.Arg V.Builtin] -> m B.Expr
delabVecLiteral args = do
  let explArgs = filter V.isExplicit args
  B.VecLiteral tokSeqOpen <$> traverse (delabM . argExpr) explArgs <*> pure tokSeqClose

mkBoolAnnOption :: Text -> Bool -> B.DeclAnnOption
mkBoolAnnOption name value =
  B.InferAnnOption (mkToken B.TokAnnInferOpt name) (mkToken B.Boolean (layoutAsText $ pretty value))

mkMaybeIntAnnOption :: Text -> Int -> B.DeclAnnOption
mkMaybeIntAnnOption name value =
  B.DefaultAnnOption (mkToken B.TokAnnDefaultOpt name) (delabNatLit value)
