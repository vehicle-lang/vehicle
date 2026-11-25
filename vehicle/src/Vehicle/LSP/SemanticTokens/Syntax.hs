module Vehicle.LSP.SemanticTokens.Syntax
  ( collectSyntaxTokens,
  )
where

import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Language.LSP.Protocol.Types as Lsp
  ( SemanticTokenAbsolute,
    SemanticTokenTypes (..),
  )
import Vehicle.LSP.SemanticTokens.Internal (IntermediateSemTok, LspErrorMessage, TokenClass, mkTokPure)
import Vehicle.Prelude
  ( Provenance (..),
    tkProvenance,
  )
import Vehicle.Syntax.External.Abs
import Vehicle.Syntax.Parse (parseExternalModule)
import Vehicle.Syntax.Token
import Vehicle.Verify.Specification.IO.Read (MonadReadSpecification, readSpecification)

collectSyntaxTokens :: (MonadReadSpecification m) => FilePath -> m (Either LspErrorMessage [Lsp.SemanticTokenAbsolute])
collectSyntaxTokens filePath = do
  moduleText <- readSpecification filePath
  return $ case parseExternalModule moduleText of
    Left err -> Left (T.pack err)
    Right modul -> Right $ catMaybes $ collectSyntaxTokensFromModule modul

collectSyntaxTokensFromModule :: Module -> [IntermediateSemTok]
collectSyntaxTokensFromModule (DefModule imports decls) = do
  let importsTokens = concatMap collectSyntaxTokensFromImport imports
  let declsTokens = concatMap collectSyntaxTokensFromDecl decls
  importsTokens ++ declsTokens

type TokenProvenance = FilePath -> Provenance

mkTok :: TokenProvenance -> TokenClass -> IntermediateSemTok
mkTok p = mkTokPure mempty (p mempty)

mkTokFrom :: (IsToken a) => a -> TokenClass -> IntermediateSemTok
mkTokFrom tok = mkTok (tkProvenance tok)

mkElemOfTok :: TokElemOf -> IntermediateSemTok
mkElemOfTok = flip mkTokFrom (SemanticTokenTypes_Operator, [])

mkKeywordTok :: (IsToken a) => a -> IntermediateSemTok
mkKeywordTok = flip mkTokFrom (SemanticTokenTypes_Keyword, [])

mkOperatorTok :: (IsToken a) => a -> IntermediateSemTok
mkOperatorTok = flip mkTokFrom (SemanticTokenTypes_Operator, [])

mkTypeTok :: (IsToken a) => a -> IntermediateSemTok
mkTypeTok = flip mkTokFrom (SemanticTokenTypes_Type, [])

mkClassTok :: (IsToken a) => a -> IntermediateSemTok
mkClassTok = flip mkTokFrom (SemanticTokenTypes_Class, [])

mkNumberTok :: (IsToken a) => a -> IntermediateSemTok
mkNumberTok = flip mkTokFrom (SemanticTokenTypes_Number, [])

-- TODO: Make "import" a position token in BNFC so we can get its provenance here
collectSyntaxTokensFromImport :: ImportStatement -> [IntermediateSemTok]
collectSyntaxTokensFromImport _importStatement = []

collectSyntaxTokensFromDecl :: Decl -> [IntermediateSemTok]
collectSyntaxTokensFromDecl = \case
  DefAnn tokAnnotation declAnnOpts -> do
    let annTok = mkTokFrom tokAnnotation (SemanticTokenTypes_Decorator, [])
    let optToks = case declAnnOpts of
          DeclAnnWithOpts opts -> concatMap collectSyntaxTokensFromDeclAnnOpt opts
          DeclAnnWithoutOpts -> []
    annTok : optToks
  -- TODO: Make "type" a position token
  DefType _name binders expr ->
    collectSyntaxTokensFromNameBinders binders
      ++ collectSyntaxTokensFromExpr expr
  DefFunType _name tokElemOf expr ->
    mkElemOfTok tokElemOf
      : collectSyntaxTokensFromExpr expr
  DefFunExpr _name binders expr ->
    collectSyntaxTokensFromNameBinders binders
      ++ collectSyntaxTokensFromExpr expr
  DefRecord _name binders fields supports -> do
    -- TODO: Make "record", "where", and "supports" position tokens
    let bindersToks = collectSyntaxTokensFromNameBinders binders
    let fieldsToks = map (\(FieldDef _ tok _) -> mkElemOfTok tok) fields
    let supportsToks = case supports of
          NoSupports -> []
          Supports s -> map (\tok -> mkTypeTok tok) s
    bindersToks ++ fieldsToks ++ supportsToks

collectSyntaxTokensFromExprs :: [Expr] -> [IntermediateSemTok]
collectSyntaxTokensFromExprs = concatMap collectSyntaxTokensFromExpr

collectSyntaxTokensFromExpr :: Expr -> [IntermediateSemTok]
collectSyntaxTokensFromExpr = \case
  Type tk -> [mkTypeTok tk]
  Ann e1 tk e2 ->
    mkElemOfTok tk
      : collectSyntaxTokensFromExpr e1
      ++ collectSyntaxTokensFromExpr e2
  Let tk letDecls e ->
    mkKeywordTok tk
      : concatMap collectSyntaxTokensFromLetDecl letDecls
      ++ collectSyntaxTokensFromExpr e
  Lam tkLambda binders tkArrow e ->
    mkKeywordTok tkLambda
      : mkOperatorTok tkArrow
      : collectSyntaxTokensFromNameBinders binders
      ++ collectSyntaxTokensFromExpr e
  Fun binder tk e ->
    mkOperatorTok tk
      : collectSyntaxTokensFromTypeBinder binder
      ++ collectSyntaxTokensFromExpr e
  App e arg ->
    collectSyntaxTokensFromExpr e
      ++ collectSyntaxTokensFromArg arg
  Record recordFieldAssigns ->
    concatMap collectSyntaxTokensFromRecordFieldAssign recordFieldAssigns
  RecordAcc e _fieldAccess ->
    collectSyntaxTokensFromExpr e
  Var _name -> []
  Literal lit -> collectSyntaxTokensFromLiteral lit
  Hole holeToken -> [mkTokFrom holeToken (SemanticTokenTypes_Macro, [])]
  Foreach tokForeach binders e ->
    mkKeywordTok tokForeach
      : collectSyntaxTokensFromNameBinders binders
      ++ collectSyntaxTokensFromExpr e
  Forall tokForall binders e ->
    mkKeywordTok tokForall
      : collectSyntaxTokensFromNameBinders binders
      ++ collectSyntaxTokensFromExpr e
  Exists tokExists binders e ->
    mkKeywordTok tokExists
      : collectSyntaxTokensFromNameBinders binders
      ++ collectSyntaxTokensFromExpr e
  -- TODO: Make "in" a position token
  ForallIn tk binder e1 e2 ->
    mkKeywordTok tk
      : collectSyntaxTokensFromNameBinder binder
      ++ collectSyntaxTokensFromExprs [e1, e2]
  ExistsIn tk binder e1 e2 ->
    mkKeywordTok tk
      : collectSyntaxTokensFromNameBinder binder
      ++ collectSyntaxTokensFromExprs [e1, e2]
  Add e1 tk e2 ->
    mkOperatorTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  Sub e1 tk e2 ->
    mkOperatorTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  Mul e1 tk e2 ->
    mkOperatorTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  Div e1 tk e2 ->
    mkOperatorTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  Pow e1 tk e2 ->
    mkOperatorTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  Neg tk e ->
    mkOperatorTok tk
      : collectSyntaxTokensFromExpr e
  If tokIf e1 tokThen e2 tokElse e3 ->
    mkKeywordTok tokIf
      : mkKeywordTok tokThen
      : mkKeywordTok tokElse
      : collectSyntaxTokensFromExprs [e1, e2, e3]
  Impl e1 tk e2 ->
    mkOperatorTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  And e1 tk e2 ->
    mkKeywordTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  Or e1 tk e2 ->
    mkKeywordTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  Not tk e ->
    mkKeywordTok tk
      : collectSyntaxTokensFromExpr e
  Eq e1 tk e2 ->
    mkOperatorTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  Ne e1 tk e2 ->
    mkOperatorTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  Le e1 tk e2 ->
    mkOperatorTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  Lt e1 tk e2 ->
    mkOperatorTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  Ge e1 tk e2 ->
    mkOperatorTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  Gt e1 tk e2 ->
    mkOperatorTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  EqPoint e1 tk e2 ->
    mkOperatorTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  NePoint e1 tk e2 ->
    mkOperatorTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  LePoint e1 tk e2 ->
    mkOperatorTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  LtPoint e1 tk e2 ->
    mkOperatorTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  GePoint e1 tk e2 ->
    mkOperatorTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  GtPoint e1 tk e2 ->
    mkOperatorTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  VecLiteral tkOpen exprs tkClose ->
    mkOperatorTok tkOpen
      : mkOperatorTok tkClose
      : collectSyntaxTokensFromExprs exprs
  Cons e1 tk e2 ->
    mkOperatorTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  At e1 tk e2 ->
    mkOperatorTok tk
      : collectSyntaxTokensFromExprs [e1, e2]
  Min tk -> [mkKeywordTok tk]
  Max tk -> [mkKeywordTok tk]
  Log tk -> [mkKeywordTok tk]
  Exp tk -> [mkKeywordTok tk]
  AddNat tk -> [mkKeywordTok tk]
  MulNat tk -> [mkKeywordTok tk]
  AddRealTensor tk -> [mkKeywordTok tk]
  SubRealTensor tk -> [mkKeywordTok tk]
  MulRealTensor tk -> [mkKeywordTok tk]
  DivRealTensor tk -> [mkKeywordTok tk]
  QuantifyForAllIndex tk -> [mkKeywordTok tk]
  QuantifyExistsIndex tk -> [mkKeywordTok tk]
  QuantifyForallRealTensor tk -> [mkKeywordTok tk]
  QuantifyExistsRealTensor tk -> [mkKeywordTok tk]
  QuantifyForallTensorLike tk -> [mkKeywordTok tk]
  QuantifyExistsTensorLike tk -> [mkKeywordTok tk]
  CompareIndexEq tk -> [mkKeywordTok tk]
  CompareIndexNe tk -> [mkKeywordTok tk]
  CompareIndexLe tk -> [mkKeywordTok tk]
  CompareIndexLt tk -> [mkKeywordTok tk]
  CompareIndexGe tk -> [mkKeywordTok tk]
  CompareIndexGt tk -> [mkKeywordTok tk]
  CompareNatEq tk -> [mkKeywordTok tk]
  CompareNatNe tk -> [mkKeywordTok tk]
  CompareNatLe tk -> [mkKeywordTok tk]
  CompareNatLt tk -> [mkKeywordTok tk]
  CompareNatGe tk -> [mkKeywordTok tk]
  CompareNatGt tk -> [mkKeywordTok tk]
  CompareRatTensorPointwiseEq tk -> [mkKeywordTok tk]
  CompareRatTensorPointwiseNe tk -> [mkKeywordTok tk]
  CompareRatTensorPointwiseLe tk -> [mkKeywordTok tk]
  CompareRatTensorPointwiseLt tk -> [mkKeywordTok tk]
  CompareRatTensorPointwiseGe tk -> [mkKeywordTok tk]
  CompareRatTensorPointwiseGt tk -> [mkKeywordTok tk]
  CompareRatTensorReducedEq tk -> [mkKeywordTok tk]
  CompareRatTensorReducedNe tk -> [mkKeywordTok tk]
  CompareRatTensorReducedLe tk -> [mkKeywordTok tk]
  CompareRatTensorReducedLt tk -> [mkKeywordTok tk]
  CompareRatTensorReducedGe tk -> [mkKeywordTok tk]
  CompareRatTensorReducedGt tk -> [mkKeywordTok tk]
  Nil tk -> [mkKeywordTok tk]
  Append tk -> [mkKeywordTok tk]
  Const tk -> [mkKeywordTok tk]
  Transpose tk -> [mkKeywordTok tk]
  Map tk -> [mkKeywordTok tk]
  Fold tk -> [mkKeywordTok tk]
  ReduceOr tk -> [mkKeywordTok tk]
  ReduceAnd tk -> [mkKeywordTok tk]
  ReduceAdd tk -> [mkKeywordTok tk]
  ReduceMul tk -> [mkKeywordTok tk]
  ReduceMin tk -> [mkKeywordTok tk]
  ReduceMax tk -> [mkKeywordTok tk]
  Unit tk -> [mkTypeTok tk]
  Bool tk -> [mkTypeTok tk]
  Index tk -> [mkTypeTok tk]
  Nat tk -> [mkTypeTok tk]
  Real tk -> [mkTypeTok tk]
  List tk -> [mkTypeTok tk]
  Vector tk -> [mkTypeTok tk]
  Tensor tk -> [mkTypeTok tk]
  HasMap tk -> [mkClassTok tk]
  HasFold tk -> [mkClassTok tk]
  IsTensorType tk -> [mkClassTok tk]
  NonCastingTensor tk -> [mkClassTok tk]
  NonCastingReal tk -> [mkClassTok tk]

collectSyntaxTokensFromBasicBinder :: BasicBinder -> [IntermediateSemTok]
collectSyntaxTokensFromBasicBinder binder =
  mkElemOfTok tok
    : collectSyntaxTokensFromExpr expr
  where
    (tok, expr) = case binder of
      ExplicitBinder _ _ t e -> (t, e)
      ImplicitBinder _ _ t e -> (t, e)
      InstanceBinder _ _ t e -> (t, e)

collectSyntaxTokensFromNameBinders :: [NameBinder] -> [IntermediateSemTok]
collectSyntaxTokensFromNameBinders = concatMap collectSyntaxTokensFromNameBinder

collectSyntaxTokensFromNameBinder :: NameBinder -> [IntermediateSemTok]
collectSyntaxTokensFromNameBinder = \case
  ExplicitNameBinder _ -> []
  ImplicitNameBinder _ _ -> []
  InstanceNameBinder _ _ -> []
  BasicNameBinder b -> collectSyntaxTokensFromBasicBinder b

collectSyntaxTokensFromTypeBinder :: TypeBinder -> [IntermediateSemTok]
collectSyntaxTokensFromTypeBinder = \case
  ExplicitTypeBinder e -> collectSyntaxTokensFromExpr e
  ExplicitTypeBinderMods _ _ e -> collectSyntaxTokensFromExpr e
  ImplicitTypeBinder e -> collectSyntaxTokensFromExpr e
  ImplicitTypeBinderMods _ _ e -> collectSyntaxTokensFromExpr e
  InstanceTypeBinder _ e -> collectSyntaxTokensFromExpr e
  BasicTypeBinder b -> collectSyntaxTokensFromBasicBinder b

collectSyntaxTokensFromLetDecl :: LetDecl -> [IntermediateSemTok]
collectSyntaxTokensFromLetDecl (LDecl binder expr) =
  collectSyntaxTokensFromNameBinder binder
    ++ collectSyntaxTokensFromExpr expr

collectSyntaxTokensFromArg :: Arg -> [IntermediateSemTok]
collectSyntaxTokensFromArg arg = collectSyntaxTokensFromExpr expr
  where
    expr = case arg of
      ExplicitArg e -> e
      ExplicitArgMods _ _ e -> e
      ImplicitArg _ e -> e
      InstanceArg _ e -> e

collectSyntaxTokensFromLiteral :: Lit -> [IntermediateSemTok]
collectSyntaxTokensFromLiteral lit = case lit of
  UnitLiteral -> [] -- TODO: Make "()" a position token
  BoolLiteral b -> [mkKeywordTok b]
  NatLiteral n -> [mkNumberTok n]
  RatLiteral r -> [mkNumberTok r]
  InfLiteral inf -> [mkKeywordTok inf]

collectSyntaxTokensFromRecordFieldAssign :: RecordFieldAssign -> [IntermediateSemTok]
collectSyntaxTokensFromRecordFieldAssign (FieldAssign _name expr) = collectSyntaxTokensFromExpr expr

collectSyntaxTokensFromDeclAnnOpt :: DeclAnnOption -> [IntermediateSemTok]
collectSyntaxTokensFromDeclAnnOpt = \case
  -- TODO: need to deal with annotations containing names/exprs during semantic pass
  NameAnnOption tokAnnNameOpt _name ->
    [ mkTokFrom tokAnnNameOpt (SemanticTokenTypes_Parameter, [])
    ]
  InferAnnOption tokAnnInferOpt b ->
    [ mkTokFrom tokAnnInferOpt (SemanticTokenTypes_Parameter, []),
      mkKeywordTok b
    ]
  DefaultAnnOption tokAnnDefaultOpt n ->
    [ mkTokFrom tokAnnDefaultOpt (SemanticTokenTypes_Parameter, []),
      mkNumberTok n
    ]
  TypeAnnOption name expr ->
    mkTokFrom name (SemanticTokenTypes_Parameter, [])
      : collectSyntaxTokensFromExpr expr
