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
import Vehicle.LSP.SemanticTokens.Common (IntermediateSemTok, LspErrorMessage, TokenClass, mkTokPure)
import Vehicle.Prelude
  ( Provenance (..),
    tkProvenance,
  )
import Vehicle.Syntax.External.Abs
import Vehicle.Syntax.Parse (parseExternalModule)
import Vehicle.Syntax.Token
import Vehicle.Verify.Specification.IO.Read (MonadReadSpecification, readSpecification)

--------------------------------------------------------------------------------
-- API

-- | Collect tokens from static syntax constructs (i.e. those that don't require semantic info to identify).
collectSyntaxTokens :: (MonadReadSpecification m) => FilePath -> m (Either LspErrorMessage [Lsp.SemanticTokenAbsolute])
collectSyntaxTokens filePath = do
  moduleText <- readSpecification filePath
  return $ case parseExternalModule moduleText of
    Left err -> Left (T.pack err)
    Right modul -> Right $ catMaybes $ collectSyntaxTokensFromModule modul

--------------------------------------------------------------------------------
-- Token creation

mkTok :: (FilePath -> Provenance) -> TokenClass -> IntermediateSemTok
mkTok p = mkTokPure mempty (p mempty)

mkTokFrom :: (IsToken a) => a -> TokenClass -> IntermediateSemTok
mkTokFrom tk = mkTok (tkProvenance tk)

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

--------------------------------------------------------------------------------
-- Token collection

collectSyntaxTokensFromModule :: Module -> [IntermediateSemTok]
collectSyntaxTokensFromModule (DefModule imports decls) =
  concatMap collectSyntaxTokensFromImport imports
    ++ concatMap collectSyntaxTokensFromDecl decls

-- TODO: Process (resolved) import statements in semantic pass
collectSyntaxTokensFromImport :: ImportStatement -> [IntermediateSemTok]
collectSyntaxTokensFromImport (Import tk _) = [mkKeywordTok tk]

collectSyntaxTokensFromDecl :: Decl -> [IntermediateSemTok]
collectSyntaxTokensFromDecl = \case
  DefAnn tk declAnnOpts -> do
    let annTok = mkTokFrom tk (SemanticTokenTypes_Decorator, [])
    let optToks = case declAnnOpts of
          DeclAnnWithOpts opts -> concatMap collectSyntaxTokensFromDeclAnnOpt opts
          DeclAnnWithoutOpts -> []
    annTok : optToks
  DefType tk _name binders expr ->
    mkKeywordTok tk
      : collectSyntaxTokensFromNameBinders binders
      ++ collectSyntaxTokensFromExpr expr
  DefFunType _name tkElemOf expr ->
    mkElemOfTok tkElemOf
      : collectSyntaxTokensFromExpr expr
  DefFunExpr _name binders expr ->
    collectSyntaxTokensFromNameBinders binders
      ++ collectSyntaxTokensFromExpr expr
  DefRecord tkRecord _name binders tkWhere fields supports -> do
    let supportsToks = case supports of
          NoSupports -> []
          -- TODO: This maybe should be the responsibility of the semantic pass?
          Supports tk s -> mkKeywordTok tk : map (\(DerivedClass t) -> mkTypeTok t) s
    mkKeywordTok tkRecord
      : mkKeywordTok tkWhere
      : collectSyntaxTokensFromNameBinders binders
      ++ concatMap collectSyntaxTokensFromRecordFieldDef fields
      ++ supportsToks

collectSyntaxTokensFromRecordFieldDef :: RecordFieldDef -> [IntermediateSemTok]
collectSyntaxTokensFromRecordFieldDef (FieldDef _ tk expr) =
  mkElemOfTok tk
    : collectSyntaxTokensFromExpr expr

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
  Foreach tk binders e ->
    mkKeywordTok tk
      : collectSyntaxTokensFromNameBinders binders
      ++ collectSyntaxTokensFromExpr e
  Forall tk binders e ->
    mkKeywordTok tk
      : collectSyntaxTokensFromNameBinders binders
      ++ collectSyntaxTokensFromExpr e
  Exists tk binders e ->
    mkKeywordTok tk
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
  If tkIf e1 tkThen e2 tkElse e3 ->
    mkKeywordTok tkIf
      : mkKeywordTok tkThen
      : mkKeywordTok tkElse
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
  NegRealTensor tk -> [mkKeywordTok tk]
  AtVector tk -> [mkKeywordTok tk]
  AtTensor tk -> [mkKeywordTok tk]
  ForeachVector tk -> [mkKeywordTok tk]
  ForeachTensor tk -> [mkKeywordTok tk]
  MapList tk -> [mkKeywordTok tk]
  FoldList tk -> [mkKeywordTok tk]
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
  IsTensorType tk -> [mkClassTok tk]
  NonCastingTensor tk -> [mkClassTok tk]
  NonCastingReal tk -> [mkClassTok tk]

collectSyntaxTokensFromBasicBinder :: BasicBinder -> [IntermediateSemTok]
collectSyntaxTokensFromBasicBinder binder =
  mkElemOfTok tk
    : collectSyntaxTokensFromExpr expr
  where
    (tk, expr) = case binder of
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
  UnitLiteral u -> [mkKeywordTok u]
  BoolLiteral b -> [mkKeywordTok b]
  NatLiteral n -> [mkNumberTok n]
  RatLiteral r -> [mkNumberTok r]
  InfLiteral inf -> [mkKeywordTok inf]

collectSyntaxTokensFromRecordFieldAssign :: RecordFieldAssign -> [IntermediateSemTok]
collectSyntaxTokensFromRecordFieldAssign (FieldAssign _name expr) = collectSyntaxTokensFromExpr expr

collectSyntaxTokensFromDeclAnnOpt :: DeclAnnOption -> [IntermediateSemTok]
collectSyntaxTokensFromDeclAnnOpt = \case
  -- TODO: Need to deal with annotations containing names/exprs during semantic pass,
  -- but NameAnnOption and TypeAnnOption currently never make it to the desugared AST.
  -- (See: `Vehicle.Compile.Sugar.Desugar.parseAnnotation`)
  NameAnnOption tk _name ->
    [ mkTokFrom tk (SemanticTokenTypes_Parameter, [])
    ]
  InferAnnOption tk b ->
    [ mkTokFrom tk (SemanticTokenTypes_Parameter, []),
      mkKeywordTok b
    ]
  DefaultAnnOption tk n ->
    [ mkTokFrom tk (SemanticTokenTypes_Parameter, []),
      mkNumberTok n
    ]
  TypeAnnOption name expr ->
    mkTokFrom name (SemanticTokenTypes_Parameter, [])
      : collectSyntaxTokensFromExpr expr
