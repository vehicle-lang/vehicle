{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Syntax.BNFC.Elaborate.External
  ( elabModule,
    elaborateExpr,
  )
where

import Control.Monad (foldM_, unless)
import Control.Monad.Except (MonadError (..), throwError)
import Control.Monad.Reader (runReaderT)
import Data.Bitraversable (bitraverse)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map (insert, lookup)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set (fromList, notMember, toList)
import Data.Text (Text, unpack)
import Data.Text qualified as Text
import Data.These (These (..))
import Prettyprinter
import Vehicle.Syntax.AST qualified as V
import Vehicle.Syntax.BNFC.Utils
  ( MonadElab,
    ParseLocation,
    getModule,
    mkProvenance,
    tokArrow,
    tokForallT,
    tokLambda,
    tokType,
    pattern DefaultOption,
    pattern InferableOption,
  )
import Vehicle.Syntax.Builtin qualified as V
import Vehicle.Syntax.External.Abs qualified as B
import Vehicle.Syntax.Parse.Error (ParseError (..))
import Vehicle.Syntax.Parse.Token
import Vehicle.Syntax.Prelude (developerError, readNat, readRat)
import Vehicle.Syntax.Tensor (pattern ZeroDimTensor)

--------------------------------------------------------------------------------
-- Partially elaborating declarations

elabModule ::
  (MonadError ParseError m) =>
  ParseLocation ->
  B.Module ->
  m V.Module
elabModule file (B.DefModule imports decls) = flip runReaderT file $ do
  let imports' = fmap elabImportStatement imports
  decls' <- elabDecls decls
  return $ V.Module imports' decls'

elabImportStatement :: B.ImportStatement -> V.ImportStatement
elabImportStatement (B.Import path) = do
  let fragToString (B.ModulePathFrag frag) = unpack $ tkSymbol frag
  V.ImportStatement $ V.ModulePath $ fmap fragToString path

elabDecls :: (MonadElab m) => [B.Decl] -> m [V.Decl]
elabDecls = \case
  [] -> return []
  decl : decls -> do
    (d', ds) <- elabDeclGroup [] (decl :| decls)
    ds' <- elabDecls ds
    return $ d' : ds'

type Annotation = (B.TokAnnotation, B.DeclAnnOpts)

elabDeclGroup ::
  (MonadElab m) =>
  [Annotation] ->
  NonEmpty B.Decl ->
  m (V.Decl, [B.Decl])
elabDeclGroup anns = \case
  -- Type definition.
  B.DefType n bs t :| ds -> do
    d' <- elabTypeDef anns n bs t
    return (d', ds)

  -- Function declaration and body.
  B.DefFunType typeName _ t :| B.DefFunExpr exprName bs e : ds -> do
    d' <- elabFunctionDef anns typeName exprName t bs e
    return (d', ds)

  -- Function body without a declaration.
  B.DefFunExpr n bs e :| ds -> do
    let unknownType = constructUnknownDefType n bs
    d' <- elabFunctionDef anns n n unknownType bs e
    return (d', ds)

  -- Abstract function declaration with no body
  B.DefFunType n _tk t :| ds -> do
    d' <- elabDefAbstract anns n t
    return (d', ds)

  -- Record declaration
  B.DefRecord name telescope fields :| ds -> do
    d' <- elabRecordDefinition anns name telescope fields
    return (d', ds)

  -- Annotation declaration.
  B.DefAnn ann annOpts :| (d : ds) -> do
    elabDeclGroup ((ann, annOpts) : anns) (d :| ds)

  -- ERROR: Annotation with no body
  B.DefAnn ann _annOpts :| [] -> do
    p <- mkProvenance ann
    throwError $ AnnotationWithNoDef p (tkSymbol ann)

data AnnotationResult
  = FunDeclAnn V.FunctionDeclAnnotation
  | RecordDeclAnn V.DefRecordSort
  | AbstractDeclAnn V.DefAbstractSort

instance Pretty AnnotationResult where
  pretty = \case
    FunDeclAnn ann -> pretty ann
    RecordDeclAnn ann -> pretty ann
    AbstractDeclAnn ann -> pretty ann

parseAnnotation :: (MonadElab m) => Annotation -> m AnnotationResult
parseAnnotation (tkName, opts) = case tkSymbol tkName of
  "@builtin" -> do
    validateEmptyOpts tkName opts
    return $ AbstractDeclAnn V.BuiltinDef
  "@network" -> do
    validateEmptyOpts tkName opts
    return $ AbstractDeclAnn V.NetworkDef
  "@dataset" -> do
    validateEmptyOpts tkName opts
    return $ AbstractDeclAnn V.DatasetDef
  "@parameter" -> do
    let allowedOptions = Set.fromList [InferableOption]
    optsList <- validateOpts tkName allowedOptions opts
    AbstractDeclAnn <$> elabParameterOptions optsList
  "@property" -> do
    validateEmptyOpts tkName opts
    return $ FunDeclAnn V.AnnProperty
  "@instance" -> do
    let allowedOptions = Set.fromList [DefaultOption]
    optsList <- validateOpts tkName allowedOptions opts
    FunDeclAnn <$> elabInstanceOptions optsList
  "@tensor" -> do
    validateEmptyOpts tkName opts
    return $ RecordDeclAnn V.AnnTensor
  "@typeclass" -> do
    validateEmptyOpts tkName opts
    return $ RecordDeclAnn V.AnnTypeClass
  name -> developerError $ "Unknown annotation found" <+> squotes (pretty name)

elabDefAbstract ::
  (MonadElab m) =>
  [Annotation] ->
  B.Name ->
  B.Expr ->
  m V.Decl
elabDefAbstract anns n t = do
  p <- mkProvenance n
  ident <- elabName n

  annotations <- traverse parseAnnotation anns
  annotation <- case annotations of
    [AbstractDeclAnn abstractAnn] ->
      return abstractAnn
    [] ->
      throwError $ UnannotatedAbstractDef p ident
    ann1 : ann2 : _ ->
      throwError $ MultiplyAnnotatedDef p ident (pretty ann1) (pretty ann2)
    [ann] ->
      throwError $ AbstractDefWithNonAbstractAnnotation p ident (pretty ann)

  typ <- elabDeclType t
  return $ V.DefAbstract p ident annotation typ

elabTypeDef ::
  (MonadElab m) =>
  [Annotation] ->
  B.Name ->
  [B.NameBinder] ->
  B.Expr ->
  m V.Decl
elabTypeDef anns name binders expr = do
  p <- mkProvenance name
  ident <- elabName name

  annotations <- traverse parseAnnotation anns
  case annotations of
    ann : _ ->
      throwError $ TypeDefWithAnnotation p ident (pretty ann)
    [] -> return ()

  let typeTyp
        | null binders = tokType 0
        | otherwise = B.ForallT tokForallT binders (tokType 0)

  let sort = V.TypeDecl (length binders)

  elabGenericDefFun p ident sort typeTyp binders expr

elabFunctionDef ::
  (MonadElab m) =>
  [Annotation] ->
  B.Name ->
  B.Name ->
  B.Expr ->
  [B.NameBinder] ->
  B.Expr ->
  m V.Decl
elabFunctionDef anns name1 name2 typ binders expr = do
  p <- mkProvenance name1
  ident1 <- elabName name1
  ident2 <- elabName name2

  unless (ident1 == ident2) $ do
    throwError $ FunctionWithMismatchedNames p ident1 ident2

  annotations <- traverse parseAnnotation anns
  sort <-
    V.FunctionDecl (length binders) <$> case annotations of
      [FunDeclAnn funAnn] ->
        return $ Just funAnn
      [] ->
        return Nothing
      ann1 : ann2 : _ ->
        throwError $ MultiplyAnnotatedDef p ident1 (pretty ann1) (pretty ann2)
      [AbstractDeclAnn absAnn] ->
        throwError $ NonAbstractDefWithAbstractAnnotation p ident1 (pretty absAnn)
      [RecordDeclAnn absAnn] ->
        throwError $ FunctionDefWithRecordAnnotation p ident1 (pretty absAnn)

  elabGenericDefFun p ident1 sort typ binders expr

elabRecordDefinition ::
  (MonadElab m) =>
  [Annotation] ->
  B.Name ->
  [B.NameBinder] ->
  [B.RecordFieldDef] ->
  m V.Decl
elabRecordDefinition anns name telescope fields = do
  p <- mkProvenance name
  ident <- elabName name

  annotations <- traverse parseAnnotation anns
  sort <- case annotations of
    [RecordDeclAnn recordAnn] ->
      return $ Just recordAnn
    [] ->
      return Nothing
    ann1 : ann2 : _ ->
      throwError $ MultiplyAnnotatedDef p ident (pretty ann1) (pretty ann2)
    [AbstractDeclAnn absAnn] ->
      throwError $ NonAbstractDefWithAbstractAnnotation p ident (pretty absAnn)
    [FunDeclAnn absAnn] ->
      throwError $ RecordDefWithFunctionAnnotation p ident (pretty absAnn)

  fields' <- traverse elabRecordFieldDef fields
  telescope' <- traverse (elabNameBinder elabExpr False) telescope

  return $ V.DefRecord p ident sort telescope' fields'

elabGenericDefFun ::
  (MonadElab m) =>
  V.Provenance ->
  V.Identifier ->
  V.DefFunctionSort ->
  B.Expr ->
  [B.NameBinder] ->
  B.Expr ->
  m V.Decl
elabGenericDefFun p ident sort t binders e = do
  -- This is a bit evil, we don't normally store possibly empty set of
  -- binders, but we will use this to indicate the set of LHS variables.
  t' <- elabDeclType t
  let body = case binders of
        [] -> e
        _ -> B.Lam tokLambda binders tokArrow e
  e' <- elabDeclType body
  return $ V.DefFunction p ident sort t' e'

validateOpts :: forall m token. (MonadElab m, IsToken token) => token -> Set Text -> B.DeclAnnOpts -> m [B.DeclAnnOption]
validateOpts _token _allowedNames B.DeclAnnWithoutOpts = return mempty
validateOpts token allowedNames (B.DeclAnnWithOpts opts) = do
  foldM_ processOpt mempty opts
  return opts
  where
    processOpt :: Map Text V.Expr -> B.DeclAnnOption -> m (Map Text V.Expr)
    processOpt found opt = do
      let mkEntry tk value = (,tkSymbol tk,value) <$> mkProvenance tk

      (prov, name, value) <- case opt of
        B.NameAnnOption tk value -> mkEntry tk (B.Var value)
        B.InferAnnOption tk value -> mkEntry tk value
        B.TypeAnnOption tk expr -> mkEntry tk expr
        B.DefaultAnnOption tk value -> mkEntry tk (B.Literal $ B.NatLiteral value)

      let nameTxt = name
      value' <- elabExpr value
      if Set.notMember nameTxt allowedNames
        then throwError $ InvalidAnnotationOption prov (tkSymbol token) nameTxt (Set.toList allowedNames)
        else case Map.lookup nameTxt found of
          Just {} -> throwError $ DuplicateAnnotationOption prov (tkSymbol token) nameTxt
          Nothing -> return $ Map.insert nameTxt value' found

validateEmptyOpts :: (MonadElab m, IsToken token) => token -> B.DeclAnnOpts -> m ()
validateEmptyOpts name opts = do _ <- validateOpts name mempty opts; return ()

elabParameterOptions :: (MonadElab m) => [B.DeclAnnOption] -> m V.DefAbstractSort
elabParameterOptions opts =
  V.ParameterDef <$> case mapMaybe getInferOption opts of
    [] -> return V.NonInferable
    (_, expr) : _ -> do
      expr' <- elabExpr expr
      case expr' of
        V.Builtin _ (V.BuiltinConstructor (V.BoolTensorLiteral (ZeroDimTensor infer)))
          | infer -> return V.Inferable
          | otherwise -> return V.NonInferable
        _ -> throwError $ InvalidAnnotationOptionValue InferableOption expr'

getInferOption :: B.DeclAnnOption -> Maybe (B.TokAnnInferOpt, B.Expr)
getInferOption = \case
  B.InferAnnOption optTk name -> Just (optTk, name)
  _ -> Nothing

elabInstanceOptions ::
  (MonadElab m) =>
  [B.DeclAnnOption] ->
  m V.FunctionDeclAnnotation
elabInstanceOptions opts =
  V.AnnInstance <$> case mapMaybe getInstanceDefaultOption opts of
    [] -> return Nothing
    (_, priority) : _ -> return $ Just priority

getInstanceDefaultOption :: B.DeclAnnOption -> Maybe (B.TokAnnDefaultOpt, Int)
getInstanceDefaultOption = \case
  B.DefaultAnnOption optTk name -> Just (optTk, elabNatLiteral name)
  _ -> Nothing

--------------------------------------------------------------------------------
-- Full elaboration

elabDeclType ::
  (MonadElab m) =>
  B.Expr ->
  m V.Expr
elabDeclType = elabExpr

elaborateExpr ::
  (MonadError ParseError m) =>
  ParseLocation ->
  B.Expr ->
  m V.Expr
elaborateExpr file expr = runReaderT (elabExpr expr) file

elabExpr :: (MonadElab m) => B.Expr -> m V.Expr
elabExpr expr = case expr of
  B.Type t -> V.Universe <$> mkProvenance t
  B.Var n -> V.Var <$> mkProvenance n <*> pure (tkSymbol n)
  B.Hole n -> V.mkHole <$> mkProvenance n <*> pure (tkSymbol n)
  B.Literal l -> elabLiteral l
  B.Fun t1 tk t2 -> op2 V.Pi tk (elabTypeBinder elabExpr False t1) (elabExpr t2)
  B.VecLiteral tk1 es _tk2 -> elabVecLiteral tk1 es
  B.App e1 e2 -> elabApp e1 e2
  B.Let tk1 ds e -> elabLet tk1 ds e
  B.ForallT tk1 ns t -> elabForallT tk1 ns t
  B.Lam tk1 ns _tk2 e -> elabLam tk1 ns e
  B.Record xs -> elabRecord xs
  B.RecordAcc e n -> elabRecordAcc e n
  -- B.Forall tk1 ns e -> elabQuantifier tk1 V.Forall ns e
  B.Forall tk1 ns e -> elabStandardLibQuantifier tk1 "forAllTC" ns e
  -- B.Exists tk1 ns e -> elabQuantifier tk1 V.Exists ns e
  B.Exists tk1 ns e -> elabStandardLibQuantifier tk1 "existsTC" ns e
  B.ForallIn tk1 ns e1 e2 -> elabQuantifierIn tk1 V.Forall ns e1 e2
  B.ExistsIn tk1 ns e1 e2 -> elabQuantifierIn tk1 V.Exists ns e1 e2
  B.Foreach tk1 ns e -> elabForeach tk1 ns e
  B.Unit tk -> builtinType V.UnitType tk []
  B.Index tk -> builtinType V.IndexType tk []
  B.Bool tk -> castToTensorType V.BoolType tk
  B.Real tk -> castToTensorType V.RatType tk
  B.Nat tk -> builtinType V.NatType tk []
  B.List tk -> builtinType V.ListType tk []
  B.Vector tk -> builtinType V.VectorType tk []
  B.Tensor tk -> builtinTypeClassOp V.TensorTypeTC tk []
  B.NonCastingTensor tk -> builtinType V.TensorType tk []
  B.Nil tk -> constructor V.Nil tk []
  B.Cons e1 tk e2 -> constructor V.Cons tk [e1, e2]
  B.Not tk e -> builtinFunction V.Not tk [e]
  B.Impl e1 tk e2 -> builtinFunction V.Implies tk [e1, e2]
  B.And e1 tk e2 -> builtinFunction V.And tk [e1, e2]
  B.Or e1 tk e2 -> builtinFunction V.Or tk [e1, e2]
  B.If tk1 e1 _ e2 _ e3 -> builtinFunction V.If tk1 [e1, e2, e3]
  B.Eq e1 tk e2 -> elabComparison V.Eq tk e1 e2
  B.Ne e1 tk e2 -> elabComparison V.Ne tk e1 e2
  B.Le e1 tk e2 -> elabComparison V.Le tk e1 e2
  B.Lt e1 tk e2 -> elabComparison V.Lt tk e1 e2
  B.Ge e1 tk e2 -> elabComparison V.Ge tk e1 e2
  B.Gt e1 tk e2 -> elabComparison V.Gt tk e1 e2
  B.EqPoint e1 tk e2 -> builtinFunction (V.CompareRatTensorPointwise V.Eq) tk [e1, e2]
  B.NePoint e1 tk e2 -> builtinFunction (V.CompareRatTensorPointwise V.Ne) tk [e1, e2]
  B.LePoint e1 tk e2 -> builtinFunction (V.CompareRatTensorPointwise V.Le) tk [e1, e2]
  B.LtPoint e1 tk e2 -> builtinFunction (V.CompareRatTensorPointwise V.Lt) tk [e1, e2]
  B.GePoint e1 tk e2 -> builtinFunction (V.CompareRatTensorPointwise V.Ge) tk [e1, e2]
  B.GtPoint e1 tk e2 -> builtinFunction (V.CompareRatTensorPointwise V.Gt) tk [e1, e2]
  B.Add e1 tk e2 -> standardLibFunction "addTC" tk [e1, e2]
  B.Sub e1 tk e2 -> standardLibFunction "subTC" tk [e1, e2]
  B.Mul e1 tk e2 -> builtinTypeClassOp V.MulTC tk [e1, e2]
  B.Div e1 tk e2 -> builtinTypeClassOp V.DivTC tk [e1, e2]
  B.Min tk -> builtinFunction (V.Min V.MinRatTensor) tk []
  B.Max tk -> builtinFunction (V.Max V.MaxRatTensor) tk []
  B.Neg tk e -> builtinTypeClassOp V.NegTC tk [e]
  B.AddNat tk -> builtinFunction (V.Add V.AddNat) tk []
  B.AddRealTensor tk -> builtinFunction (V.Add V.AddRatTensor) tk []
  B.SubRealTensor tk -> builtinFunction (V.Sub V.SubRatTensor) tk []
  B.QuantifyForAllNat tk n -> derivedFunction (V.QuantifyIndex V.Forall) tk [n]
  B.QuantifyExistsNat tk n -> derivedFunction (V.QuantifyIndex V.Exists) tk [n]
  B.QuantifyForallRealTensor tk n -> builtinFunction (V.QuantifyRatTensor V.Forall) tk [n]
  B.QuantifyExistsRealTensor tk n -> builtinFunction (V.QuantifyRatTensor V.Exists) tk [n]
  B.QuantifyForallTensorLike tk n -> builtinFunction (V.QuantifyTensorLike V.Forall) tk [n]
  B.QuantifyExistsTensorLike tk n -> builtinFunction (V.QuantifyTensorLike V.Exists) tk [n]
  B.At e1 tk e2 -> builtinTypeClassOp V.AtTC tk [e1, e2]
  B.Map tk -> builtinTypeClassOp V.MapTC tk []
  B.Fold tk -> builtinTypeClassOp V.FoldTC tk []
  B.Const tk -> builtinFunction V.ConstTensor tk []
  B.ReduceOr tk -> builtinFunction V.ReduceOrTensor tk []
  B.ReduceAnd tk -> builtinFunction V.ReduceAndTensor tk []
  B.ReduceAdd tk -> builtinFunction V.ReduceAddRatTensor tk []
  B.ReduceMul tk -> builtinFunction V.ReduceMulRatTensor tk []
  B.ReduceMin tk -> builtinFunction V.ReduceMinRatTensor tk []
  B.ReduceMax tk -> builtinFunction V.ReduceMaxRatTensor tk []
  B.HasEq tk -> builtinTypeClass (V.HasCompare V.Eq) tk []
  B.HasNotEq tk -> builtinTypeClass (V.HasCompare V.Ne) tk []
  B.HasLeq tk -> builtinTypeClass (V.HasCompare V.Le) tk []
  B.HasMul tk -> builtinTypeClass V.HasMul tk []
  B.HasMap tk -> builtinTypeClass V.HasMap tk []
  B.HasFold tk -> builtinTypeClass V.HasFold tk []
  B.IsTensorType tk -> builtinTypeClass V.IsTensorType tk []
  -- NOTE: we reverse the arguments to make it well-typed.
  B.Ann e tk t -> derivedFunction V.TypeAnn tk [t, e]

elabArg :: (MonadElab m) => B.Arg -> m V.Arg
elabArg = \case
  B.ExplicitArg e -> mkArg mempty V.Explicit <$> elabExpr e
  B.ExplicitArgMods modality modalities e -> mkArg (modality : modalities) V.Explicit <$> elabExpr e
  B.ImplicitArg modalities e -> mkArg modalities (V.Implicit False) <$> elabExpr e
  B.InstanceArg modalities e -> mkArg modalities (V.Instance False) <$> elabExpr e

elabName :: (MonadElab m) => B.Name -> m V.Identifier
elabName n = do
  modl <- getModule
  return $ V.Identifier modl $ tkSymbol n

elabRecordFieldName :: (MonadElab m) => B.Name -> m V.FieldName
elabRecordFieldName tk = do
  p <- mkProvenance tk
  return $ V.FieldName p (tkSymbol tk)

elabRecordFieldAssign :: (MonadElab m) => B.RecordFieldAssign -> m V.RecordField
elabRecordFieldAssign (B.FieldAssign name expr) = do
  (,) <$> elabRecordFieldName name <*> elabExpr expr

elabRecordFieldDef :: (MonadElab m) => B.RecordFieldDef -> m V.RecordField
elabRecordFieldDef (B.FieldDef name _ expr) =
  (,) <$> elabRecordFieldName name <*> elabExpr expr

elabRecord :: (MonadElab m) => [B.RecordFieldAssign] -> m V.Expr
elabRecord xs = do
  fields <- traverse elabRecordFieldAssign xs
  -- I'm struggling to make the left/right braces into tokens as the tokenizer doesn't
  -- seem to recognise them correctly. Hence this very ugly hack.
  -- pL <- mkProvenance tkL
  -- pR <- mkProvenance tkR
  -- let p = V.fillInProvenance (pL :| [pR])
  let p = case fields of
        [] -> mempty
        f : fs -> V.fillInProvenance $ fmap V.provenanceOf (f :| fs)
  return $ V.Record p fields

elabRecordAcc :: (MonadElab m) => B.Expr -> B.FieldAccess -> m V.Expr
elabRecordAcc e field = do
  p <- mkProvenance field
  let fieldName = V.FieldName p $ Text.tail $ tkSymbol field
  r <- elabExpr e
  return $ V.RecordAcc p r fieldName

elabBasicBinder ::
  (MonadElab m) =>
  (B.Expr -> m expr) ->
  Bool ->
  B.BasicBinder ->
  m (V.GenericBinder expr)
elabBasicBinder elab folded = \case
  B.ExplicitBinder mods n _tk typ -> mkBinder elab folded mods V.Explicit (These n typ)
  B.ImplicitBinder mods n _tk typ -> mkBinder elab folded mods (V.Implicit False) (These n typ)
  B.InstanceBinder mods n _tk typ -> mkBinder elab folded mods (V.Instance False) (These n typ)

elabNameBinder ::
  (MonadElab m) =>
  (B.Expr -> m expr) ->
  Bool ->
  B.NameBinder ->
  m (V.GenericBinder expr)
elabNameBinder elab folded = \case
  B.ExplicitNameBinder n -> mkBinder elab folded mempty V.Explicit (This n)
  B.ImplicitNameBinder modalities n -> mkBinder elab folded modalities (V.Implicit False) (This n)
  B.InstanceNameBinder modalities n -> mkBinder elab folded modalities (V.Instance False) (This n)
  B.BasicNameBinder b -> elabBasicBinder elab folded b

elabTypeBinder ::
  (MonadElab m) =>
  (B.Expr -> m expr) ->
  Bool ->
  B.TypeBinder ->
  m (V.GenericBinder expr)
elabTypeBinder elab folded = \case
  B.ExplicitTypeBinder t -> mkBinder elab folded mempty V.Explicit $ That t
  B.ImplicitTypeBinder modalities t -> mkBinder elab folded modalities (V.Implicit False) $ That t
  B.InstanceTypeBinder modalities t -> mkBinder elab folded modalities (V.Instance False) $ That t
  B.BasicTypeBinder b -> elabBasicBinder elab folded b

findRelevance :: [B.Modality] -> V.Relevance
findRelevance ms
  | null ms = V.Relevant
  | otherwise = V.Irrelevant

mkArg :: [B.Modality] -> V.Visibility -> V.Expr -> V.Arg
mkArg modalities v = V.Arg v (findRelevance modalities)

mkBinder ::
  (MonadElab m) =>
  (B.Expr -> m expr) ->
  V.BinderFoldingForm ->
  [B.Modality] ->
  V.Visibility ->
  These B.Name B.Expr ->
  m (V.GenericBinder expr)
mkBinder elab folded modalities visibility nameTyp = do
  let relevance = findRelevance modalities
  (form, typ) <- case nameTyp of
    This nameTk -> do
      p <- mkProvenance nameTk
      let name = tkSymbol nameTk
      let typ = mkHole (tkLocation nameTk) $ "typeOf[" <> name <> "]"
      let naming = V.OnlyName name p
      return (naming, typ)
    That typ -> do
      let naming = V.OnlyType
      return (naming, typ)
    These nameTk typ -> do
      p <- mkProvenance nameTk
      let name = tkSymbol nameTk
      let naming = V.NameAndType name p
      return (naming, typ)

  let displayForm = V.BinderDisplayForm form folded
  elabType <- elab typ
  return $ V.Binder displayForm visibility relevance elabType

elabLetDecl :: (MonadElab m) => B.LetDecl -> m (V.Binder, V.Expr)
elabLetDecl (B.LDecl b e) = bitraverse (elabNameBinder elabExpr False) elabExpr (b, e)

elabLiteral :: (MonadElab m) => B.Lit -> m V.Expr
elabLiteral = \case
  B.UnitLiteral ->
    return $ V.Builtin mempty $ V.BuiltinConstructor V.UnitLiteral
  B.BoolLiteral t -> do
    p <- mkProvenance t
    let b = elabBoolLiteral t
    return $ V.Builtin p $ V.BuiltinConstructor $ V.BoolTensorLiteral $ ZeroDimTensor b
  B.NatLiteral t -> do
    p <- mkProvenance t
    let n = elabNatLiteral t
    let fromNat = V.Builtin p (V.TypeClassOp V.FromNatTC)
    return $ app fromNat [V.Builtin p $ V.BuiltinConstructor $ V.NatLiteral n]
  B.RatLiteral t -> do
    p <- mkProvenance t
    let r = readRat (tkSymbol t)
    let fromRat = V.Builtin p (V.TypeClassOp V.FromRatTC)
    return $ app fromRat [V.Builtin p $ V.BuiltinConstructor $ V.RatTensorLiteral $ ZeroDimTensor r]

elabNatLiteral :: B.Natural -> Int
elabNatLiteral t = readNat (tkSymbol t)

elabBoolLiteral :: B.Boolean -> Bool
elabBoolLiteral t = read (unpack $ tkSymbol t)

op2 ::
  (MonadElab m, V.HasProvenance a, V.HasProvenance b, IsToken token) =>
  (V.Provenance -> a -> b -> c) ->
  token ->
  m a ->
  m b ->
  m c
op2 mk t e1 e2 = do
  ce1 <- e1
  ce2 <- e2
  tProv <- mkProvenance t
  let p = V.fillInProvenance (tProv :| [V.provenanceOf ce1, V.provenanceOf ce2])
  return $ mk p ce1 ce2

builtin :: (MonadElab m, IsToken token) => V.Builtin -> token -> [B.Expr] -> m V.Expr
builtin b t args = do
  tProv <- mkProvenance t
  app (V.Builtin tProv b) <$> traverse elabExpr args

constructor :: (MonadElab m, IsToken token) => V.BuiltinConstructor -> token -> [B.Expr] -> m V.Expr
constructor b = builtin (V.BuiltinConstructor b)

builtinType :: (MonadElab m, IsToken token) => V.BuiltinType -> token -> [B.Expr] -> m V.Expr
builtinType b = builtin (V.BuiltinType b)

builtinTypeClass :: (MonadElab m, IsToken token) => V.TypeClass -> token -> [B.Expr] -> m V.Expr
builtinTypeClass b = builtin (V.TypeClass b)

builtinTypeClassOp :: (MonadElab m, IsToken token) => V.TypeClassOp -> token -> [B.Expr] -> m V.Expr
builtinTypeClassOp b = builtin (V.TypeClassOp b)

builtinFunction :: (MonadElab m, IsToken token) => V.BuiltinFunction -> token -> [B.Expr] -> m V.Expr
builtinFunction b = builtin (V.BuiltinFunction b)

derivedFunction :: (MonadElab m, IsToken token) => V.DerivedFunction -> token -> [B.Expr] -> m V.Expr
derivedFunction b = builtin (V.DerivedFunction b)

standardLibFunction :: (MonadElab m, IsToken token) => V.Name -> token -> [B.Expr] -> m V.Expr
standardLibFunction name tk args = do
  p <- mkProvenance tk
  app (V.Var p name) <$> traverse elabExpr args

castToTensorType :: (MonadElab m, IsToken token) => V.BuiltinType -> token -> m V.Expr
castToTensorType tElem tk = do
  p <- mkProvenance tk
  tElem' <- builtinType tElem tk []
  dims <- constructor V.Nil tk []
  let tTensor = V.Builtin p (V.BuiltinType V.TensorType)
  return $ app tTensor [tElem', dims]

app :: V.Expr -> [V.Expr] -> V.Expr
app fun argExprs = V.normAppList fun args
  where
    args = fmap (mkArg mempty V.Explicit) argExprs

elabVecLiteral :: (MonadElab m, IsToken token) => token -> [B.Expr] -> m V.Expr
elabVecLiteral tk xs = do
  p <- mkProvenance tk
  let tCont = V.Arg (V.Implicit True) V.Relevant (V.mkHole p "tCont")
  let tElem = V.Arg (V.Implicit True) V.Relevant (V.mkHole p "tElem")
  let n = V.Arg (V.Implicit True) V.Relevant (V.Builtin p (V.BuiltinConstructor $ V.NatLiteral (length xs)))
  xs' <- fmap (mkArg mempty V.Explicit) <$> traverse elabExpr xs
  return $ V.normAppList (V.Builtin p (V.TypeClassOp V.VecLiteralTC)) (tCont : tElem : n : xs')

elabApp :: (MonadElab m) => B.Expr -> B.Arg -> m V.Expr
elabApp fun arg = do
  fun' <- elabExpr fun
  arg' <- elabArg arg
  return $ V.normAppList fun' [arg']

elabComparison :: (MonadElab m, IsToken token) => V.ComparisonOp -> token -> B.Expr -> B.Expr -> m V.Expr
elabComparison op tk e1 e2 = do
  let Tk tkDetails@(tkPos, _) = toToken tk
  let chainedOrder = case e1 of
        B.Le _ _ e -> Just (V.Le, e)
        B.Lt _ _ e -> Just (V.Lt, e)
        B.Ge _ _ e -> Just (V.Ge, e)
        B.Gt _ _ e -> Just (V.Gt, e)
        B.Eq _ _ e -> Just (V.Eq, e)
        _ -> Nothing

  case chainedOrder of
    Nothing -> builtin (V.TypeClassOp $ V.CompareTC op) tk [e1, e2]
    Just (prevOp, e)
      | not (V.chainable prevOp op) -> do
          p <- mkProvenance tk
          throwError $ UnchainableComparisons p prevOp op
      | otherwise -> elabExpr $ B.And e1 (B.TokAnd (tkPos, "and")) $ case op of
          V.Le -> B.Le e (B.TokLe tkDetails) e2
          V.Lt -> B.Lt e (B.TokLt tkDetails) e2
          V.Ge -> B.Ge e (B.TokGe tkDetails) e2
          V.Gt -> B.Gt e (B.TokGt tkDetails) e2
          V.Eq -> B.Eq e (B.TokEq tkDetails) e2
          V.Ne -> B.Ne e (B.TokNe tkDetails) e2

-- | Unfolds a list of binders into a consecutative forall expressions
elabForallT :: (MonadElab m) => B.TokForallT -> [B.NameBinder] -> B.Expr -> m V.Expr
elabForallT tk binders body = do
  p <- mkProvenance tk
  binders' <- elabNamedBinders tk binders
  body' <- elabExpr body
  return $ foldr (V.Pi p) body' binders'

elabLam :: (MonadElab m) => B.TokLambda -> [B.NameBinder] -> B.Expr -> m V.Expr
elabLam tk binders body = do
  p <- mkProvenance tk
  binders' <- elabNamedBinders tk binders
  body' <- elabExpr body
  return $ foldr (V.Lam p) body' binders'

-- elabQuantifier ::
--   (MonadElab m, IsToken token) =>
--   token ->
--   V.Quantifier ->
--   [B.NameBinder] ->
--   B.Expr ->
--   m V.Expr
-- elabQuantifier tk q binders body = do
--   p <- mkProvenance tk
--   let quantBuiltin = V.Builtin p $ V.TypeClassOp $ V.QuantifierTC q

--   binders' <- elabNamedBinders tk binders
--   body' <- elabExpr body

--   let mkQuantifier binder newBody =
--         V.normAppList
--           quantBuiltin
--           [ mkArg mempty V.Explicit (V.Lam (V.provenanceOf binder) binder newBody)
--           ]

--   return $ foldr mkQuantifier body' binders'

elabStandardLibQuantifier ::
  (MonadElab m, IsToken token) =>
  token ->
  V.Name ->
  [B.NameBinder] ->
  B.Expr ->
  m V.Expr
elabStandardLibQuantifier tk name binders body = do
  p <- mkProvenance tk
  let quant = V.Var p name

  binders' <- elabNamedBinders tk binders
  body' <- elabExpr body

  let mkQuantifier binder newBody =
        V.normAppList
          quant
          [ mkArg mempty V.Explicit (V.Lam (V.provenanceOf binder) binder newBody)
          ]
  return $ foldr mkQuantifier body' binders'

elabQuantifierIn ::
  (MonadElab m, IsToken token) =>
  token ->
  V.Quantifier ->
  B.NameBinder ->
  B.Expr ->
  B.Expr ->
  m V.Expr
elabQuantifierIn tk q binder container body = do
  p <- mkProvenance tk
  let quantBuiltin = V.DerivedFunction $ V.QuantifyInList q
  binder' <- elabNameBinder elabExpr False binder
  container' <- elabExpr container
  body' <- elabExpr body

  let p' = V.provenanceOf binder'
  return $
    V.normAppList
      (V.Builtin p quantBuiltin)
      [ mkArg mempty V.Explicit (V.Lam p' binder' body'),
        mkArg mempty V.Explicit container'
      ]

elabForeach ::
  (MonadElab m, IsToken token) =>
  token ->
  [B.NameBinder] ->
  B.Expr ->
  m V.Expr
elabForeach tk binders body = do
  p <- mkProvenance tk

  binders' <- elabNamedBinders tk binders
  body' <- elabExpr body

  let mkForeach binder newBody =
        V.normAppList
          (V.Builtin p $ V.TypeClassOp V.ForeachTC)
          [ mkArg mempty V.Explicit (V.Lam (V.provenanceOf binder) binder newBody)
          ]

  return $ foldr mkForeach body' binders'

elabLet :: (MonadElab m) => B.TokLet -> [B.LetDecl] -> B.Expr -> m V.Expr
elabLet tk decls body = do
  p <- mkProvenance tk
  decls' <- traverse elabLetDecl decls
  body' <- elabExpr body
  return $ foldr (insertLet p) body' decls'
  where
    insertLet :: V.Provenance -> (V.Binder, V.Expr) -> V.Expr -> V.Expr
    insertLet p (binder, bound) = V.Let p bound binder

elabNamedBinders :: (MonadElab m, IsToken token) => token -> [B.NameBinder] -> m (NonEmpty V.Binder)
elabNamedBinders tk binders = case binders of
  [] -> do
    p <- mkProvenance tk
    throwError $ MissingVariables p (tkSymbol tk)
  (d : ds) -> do
    d' <- elabNameBinder elabExpr False d
    ds' <- traverse (elabNameBinder elabExpr True) ds
    return (d' :| ds')

-- | Constructs a pi type filled with an appropriate number of holes for
--  a definition which has no accompanying type.
constructUnknownDefType :: B.Name -> [B.NameBinder] -> B.Expr
constructUnknownDefType n binders
  | null binders = returnType
  | otherwise = B.ForallT tokForallT binders returnType
  where
    returnType :: B.Expr
    returnType = B.Hole $ mkToken B.HoleToken (typifyName (tkSymbol n))

    typifyName :: Text -> Text
    typifyName x = "typeOf_" <> x

mkHole :: (Int, Int) -> Text -> B.Expr
mkHole location name = B.Hole $ B.HoleToken (location, name)
