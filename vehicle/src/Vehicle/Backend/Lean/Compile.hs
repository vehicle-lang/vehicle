{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Vehicle.Backend.Lean.Compile
  ( LeanOptions (..),
    compileProgToLean,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Bifunctor (first)
import Data.Foldable (fold)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Real (denominator, numerator)
import Prettyprinter hiding (hcat, hsep, vcat, vsep)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Sugar.Binders
import Vehicle.Data.AST.Expr.Scoped ()
import Vehicle.Data.Builtin.Decidability
import Vehicle.Data.Builtin.Standard (BuiltinType (..))
import Vehicle.Data.Builtin.Standard hiding (TensorType)
import Vehicle.Data.Tensor (Tensor, toList)
import Vehicle.Data.Universe (UniverseLevel (..))
import Vehicle.Data.Variable.Bound.Context.Name

--------------------------------------------------------------------------------
-- Lean-specific options

data LeanOptions = LeanOptions
  { output :: Maybe FilePath,
    moduleName :: Maybe String
  }
  deriving (Show, Eq)

currentPhase :: Doc ()
currentPhase = "compilation to Lean"

compileProgToLean :: (MonadCompile m) => Prog DecidabilityBuiltin -> LeanOptions -> m (Doc a)
compileProgToLean prog _options =
  logCompilerSection2 MinDetail currentPhase $ do
    programDoc <- runFreshNameBoundContextT $ compileProg prog
    let programStream = layoutPretty defaultLayoutOptions programDoc
    let programDependencies = fold (reAnnotateS fst programStream)

    let leanProgram =
          unAnnotate
            ((vsep2 :: [Code] -> Code)
              [ importStatements programDependencies,
                programDoc
              ]
            )

    return leanProgram

--------------------------------------------------------------------------------
-- Debug functions

type MonadLeanCompile m =
  ( MonadCompile m,
    MonadNameContext m
  )

logEntry :: (MonadLeanCompile m) => Expr DecidabilityBuiltin -> m ()
logEntry e = do
  incrCallDepth
  ctx <- getNameContext
  logDebug MaxDetail $ "compile-entry" <+> prettyExternal (WithContext e ctx)

logExit :: (MonadLeanCompile m) => Code -> m ()
logExit e = do
  logDebug MaxDetail $ "compile-exit " <+> e
  decrCallDepth

--------------------------------------------------------------------------------
-- Modules and dependencies

data Dependency
  = VehicleLib
  | MathlibData
  | MathlibTactic
  | MathlibAlgebra
  | MathlibOrder
  deriving (Eq, Ord)

instance Pretty Dependency where
  pretty = \case
    VehicleLib -> "Vehicle"
    MathlibData -> "Mathlib.Data.List.Basic"
    MathlibTactic -> "Mathlib.Tactic"
    MathlibAlgebra -> "Mathlib.Algebra.Basic"
    MathlibOrder -> "Mathlib.Order.Basic"

importStatement :: Dependency -> Doc a
importStatement dep = "import" <+> pretty dep

importStatements :: Set Dependency -> Doc a
importStatements deps = vsep $ map importStatement (sort (Set.toList deps))

--------------------------------------------------------------------------------
-- Intermediate results of compilation

type Precedence = Int
type Code = Doc (Set Dependency, Precedence)

minPrecedence :: Precedence
minPrecedence = -1000

maxPrecedence :: Precedence
maxPrecedence = 1000

getPrecedence :: Code -> Precedence
getPrecedence e = maybe maxPrecedence snd (docAnn e)

annotateConstant :: [Dependency] -> Code -> Code
annotateConstant dependencies = annotate (Set.fromList dependencies, maxPrecedence)

annotateApp :: (MonadLeanCompile m) => [Dependency] -> Code -> [Arg DecidabilityBuiltin] -> m Code
annotateApp dependencies fun args = do
  if null args
    then return $ annotate (Set.fromList dependencies, maxPrecedence) fun
    else do
      bracketedArgs <- compileArgs minPrecedence args
      return $ annotate (Set.fromList dependencies, 20) $ hsep (fun : bracketedArgs)

argBrackets :: Precedence -> Visibility -> Code -> Code
argBrackets parentPrecedence v e = case v of
  Explicit {}
    | getPrecedence e > parentPrecedence -> e
    | otherwise -> parens e
  Implicit {} -> braces e
  Instance {} -> braces e

binderBrackets :: Bool -> Visibility -> Code -> Code
binderBrackets topLevel = \case
  Explicit {} | topLevel -> id
  Explicit {} | otherwise -> parens
  Implicit {} -> braces
  Instance {} -> braces

--------------------------------------------------------------------------------
-- Compilation of program structure

compileProg :: (MonadLeanCompile m) => Prog DecidabilityBuiltin -> m Code
compileProg (Main ds) = do
  decls <- catMaybes <$> traverse compileDecl ds
  return $ "namespace Vehicle" <> line <> line <> vsep2 decls <> line <> line <> "end Vehicle"

compileDecl :: (MonadLeanCompile m) => Decl DecidabilityBuiltin -> m (Maybe Code)
compileDecl = \case
  DefAbstract _ n _ t ->
    Just <$> compilePostulate n t
  DefFunction p n funSort t e -> case funSort of
    TypeDecl binderCount -> Just <$> compileFunctionDecl n binderCount t e
    FunctionDecl binderCount Nothing -> Just <$> compileFunctionDecl n binderCount t e
    FunctionDecl _ (Just AnnProperty) -> Just <$> compileProperty n e
    FunctionDecl _ (Just AnnInstance {}) -> throwError $ UnimplementedFeature p "Compiling instances to Lean"
    ProjectionDecl {} -> return Nothing
  DefRecord p n _ telescope fields ->
    Just <$> compileRecordDecl p n telescope fields

compilePostulate :: (MonadLeanCompile m) => Identifier -> Type DecidabilityBuiltin -> m Code
compilePostulate n t = do
  let name = compileIdentifier n
  tType <- compileExpr t
  return $ "axiom" <+> name <+> ":" <+> tType

compileProperty :: (MonadLeanCompile m) => Identifier -> Expr DecidabilityBuiltin -> m Code
compileProperty n e = do
  let name = compileIdentifier n
  exprCode <- compileExpr e
  return $ "axiom" <+> name <+> ":" <+> exprCode

extractDeclBinders ::
  LHSBinderCount ->
  Type DecidabilityBuiltin ->
  Expr DecidabilityBuiltin ->
  ([Binder DecidabilityBuiltin], Expr DecidabilityBuiltin)
extractDeclBinders binderCount typ expr
  | binderCount == 0 = ([], expr)
  | otherwise = case (typ, expr) of
      (Pi _ piBinder piBody, Lam _ lamBinder lamBody) -> do
        let compositeBinder = replaceBinderType (typeOf piBinder) lamBinder
        first (compositeBinder :) (extractDeclBinders (binderCount - 1) piBody lamBody)
      (_, _) -> ([], expr)

compileFunctionDecl ::
  (MonadLeanCompile m) =>
  Identifier ->
  LHSBinderCount ->
  Type DecidabilityBuiltin ->
  Expr DecidabilityBuiltin ->
  m Code
compileFunctionDecl ident binderCount t e = do
  let (binders, body) = extractDeclBinders binderCount t e
  binders' <- compileTopLevelBinders binders
  (_, cbody) <- compileBinders binders (compileExpr body)
  defType <- resolveReturnType binders' t
  return $ compileFunDef (compileIdentifier ident) defType binders' cbody

compileRecordDecl ::
  (MonadLeanCompile m) =>
  Provenance ->
  Identifier ->
  Telescope DecidabilityBuiltin ->
  RecordFields DecidabilityBuiltin ->
  m Code
compileRecordDecl p ident telescope fields = do
  t' <-
    if null telescope
      then return (compileType 0)
      else throwError $ UnimplementedFeature p "Compiling parameterised records to Lean"
  fs' <- traverse compileRecordFieldDef fields

  return $
    "structure"
      <+> compileIdentifier ident
      <+> ":"
      <+> t'
      <+> "where"
      <> line
      <> indent 2 (vsep fs')

compileFunDef :: Code -> Code -> [Code] -> Code -> Code
compileFunDef name defType binders body =
  "def" <+> name <+> hsep binders <+> ":" <+> defType <+> ":=" <> line <> indent 2 body

compileIdentifier :: Identifier -> Code
compileIdentifier ident = pretty (nameOf ident :: Name)

compileExpr :: (MonadLeanCompile m) => Expr DecidabilityBuiltin -> m Code
compileExpr expr = do
  logEntry expr
  result <- case expr of
    Hole {} -> resolutionError currentPhase "Hole"
    Meta {} -> resolutionError currentPhase "Meta"
    Universe _ l -> return $ compileType l
    FreeVar _ n -> return $ annotateConstant [] (pretty (nameOf n))
    BoundVar p ix -> do
      n <- ixToProperName p ix
      return $ annotateConstant [] (pretty n)
    Pi _ binder result -> do
      cInput <- compileBinder binder
      cOutput <- addNameToContext binder $ compileExpr result
      return $ "∀" <+> cInput <> "," <+> cOutput
    App fun args -> compileApp fun args
    Lam _ binder body -> do
      cBinder <- compileBinder binder
      cBody <- addNameToContext binder $ compileExpr body
      return $ "fun" <+> cBinder <+> "=>" <+> cBody
    Let _ bound binder body -> do
      cBoundExpr <- compileLetBinder (binder, bound)
      cBody <- addNameToContext binder $ compileExpr body
      return $ "let" <+> cBoundExpr <+> "in" <+> cBody
    Builtin _p b -> compileBuiltin b []
    Record _p _i fs -> do
      fs' <- traverse compileRecordField fs
      return $ encloseSep langle rangle (semi <> space) fs'
    RecordProj _p _t r field -> do
      cr <- compileExpr r
      return $ cr <> "." <> pretty (nameOf field)
  logExit result
  return result

compileType :: UniverseLevel -> Code
compileType (UniverseLevel l)
  | l == 0 = "Type"
  | l == 1 = "Type 1"
  | otherwise = "Type" <+> pretty l

compileBuiltin :: (MonadLeanCompile m) => DecidabilityBuiltin -> [Arg DecidabilityBuiltin] -> m Code
compileBuiltin b args = case b of
  StandardBuiltinType t -> case t of
    BoolType -> return $ annotateConstant [MathlibData] "Bool"
    RatType -> return $ annotateConstant [MathlibAlgebra] "ℚ"
    UnitType -> return "Unit"
    NatType -> return "ℕ"
    ListType -> annotateApp [MathlibData] "List" args
    TensorType -> annotateApp [VehicleLib] "'T" args
    IndexType -> annotateApp [MathlibData] "Fin" args
    VectorType -> annotateApp [MathlibData] "Vector" args
  StandardBuiltinConstructor c -> case c of
    Nil -> return $ annotateConstant [MathlibData] "[]"
    Cons -> annotateApp [MathlibData] "::" args
    UnitLiteral -> return $ annotateConstant [] "()"
    IndexLiteral n -> return $ compileIndexLiteral n
    NatLiteral n -> return $ compileNatLiteral n
    NatTensorLiteral t -> return $ compileTensorLiteral compileNatLiteral t
    BoolTensorLiteral t -> return $ compileTensorLiteral compileBoolLiteral t
    RatTensorLiteral t -> return $ compileTensorLiteral compileRatLiteral t
    VectorLiteral -> compileVecLiteral args
  StandardBuiltinFunction f -> case f of
    And -> annotateApp [] "&&" args
    Or -> annotateApp [] "||" args
    Not -> annotateApp [] "!" args
    Implies -> annotateApp [] "→" args
    If -> annotateApp [] "ite" args
    Add {} -> annotateApp [] "+" args
    Sub {} -> annotateApp [] "-" args
    Mul {} -> annotateApp [] "*" args
    Div {} -> annotateApp [] "/" args
    Neg {} -> annotateApp [] "-" args
    Min {} -> annotateApp [] "min" args
    Max {} -> annotateApp [] "max" args
    PowRat -> annotateApp [] "^" args
    ReduceAndTensor -> annotateApp [VehicleLib] "reduceAnd" args
    ReduceOrTensor -> annotateApp [VehicleLib] "reduceOr" args
    QuantifyRatTensor {} -> annotateApp [] "forall" args
    QuantifyTensorLike {} -> annotateApp [] "forall" args
    CompareNat op -> compileComparison op args
    CompareIndex op -> compileComparison op args
    CompareRatTensorPointwise op -> compileComparison op args
    AtTensor -> annotateApp [VehicleLib] "at_tensor" args
    StackTensor -> annotateApp [VehicleLib] "stack_tensor" args
    ConstTensor -> annotateApp [VehicleLib] "const_tensor" args
    ForeachTensor -> annotateApp [VehicleLib] "foreach_tensor" args
    AtVector -> annotateApp [MathlibData] "Vector.get" args
    ForeachVector -> annotateApp [MathlibData] "Vector.map" args
    FoldList -> annotateApp [MathlibData] "List.foldl" args
    MapList -> annotateApp [MathlibData] "List.map" args
    Iterate -> annotateApp [] "Nat.rec" args
    ReduceAddRatTensor -> annotateApp [VehicleLib] "reduceAdd" args
    ReduceMulRatTensor -> annotateApp [VehicleLib] "reduceMul" args
    ReduceMinRatTensor -> annotateApp [VehicleLib] "reduceMin" args
    ReduceMaxRatTensor -> annotateApp [VehicleLib] "reduceMax" args
  StandardBuiltinDerivedFunction f -> compileDerivedFunction f args
  DecidabilityBuiltinFunction f -> compileDecidabilityBuiltinFunction f args
  DecidabilityBuiltinTypeClass {} -> developerError "Monomorphisation should have eliminated type classes"
  DecidabilityBuiltinTypeClassOp {} -> developerError "Monomorphisation should have eliminated type classes"

compileDecidabilityBuiltinFunction ::
  (MonadLeanCompile m) =>
  DecidabilityBuiltinFunction ->
  [Arg DecidabilityBuiltin] ->
  m Code
compileDecidabilityBuiltinFunction fn args = case fn of
  PropQuantifyIndex q -> case q of
    Forall -> annotateApp [VehicleLib] "forallIndex" args
    Exists -> annotateApp [VehicleLib] "existsIndex" args
  PropQuantifyInList q -> case q of
    Forall -> annotateApp [VehicleLib] "forallInList" args
    Exists -> annotateApp [VehicleLib] "existsInList" args
  _ -> developerError $ "compilation of builtin" <+> quotePretty fn <+> "to Lean unsupported"

compileComparison :: (MonadLeanCompile m) => ComparisonOp -> [Arg DecidabilityBuiltin] -> m Code
compileComparison op args = annotateApp [] (pretty opText) args
  where
    opText = case op of
      Le -> ("≤" :: String)
      Lt -> "<"
      Ge -> "≥"
      Gt -> ">"
      Eq -> "="
      Ne -> "≠"

compileDerivedFunction :: (MonadLeanCompile m) => DerivedFunction -> [Arg DecidabilityBuiltin] -> m Code
compileDerivedFunction fn args = case fn of
  QuantifyIndex q -> case q of
    Exists -> annotateApp [VehicleLib] "existsIndex" args
    Forall -> annotateApp [VehicleLib] "forallIndex" args
  QuantifyInList q -> case q of
    Exists -> annotateApp [VehicleLib] "existsInList" args
    Forall -> annotateApp [VehicleLib] "forallInList" args
  TypeAnn -> annotateApp [] ":" args
  _ -> developerError $ "Unsupported derived function" <+> quotePretty fn

compileApp :: (MonadLeanCompile m) => Expr DecidabilityBuiltin -> NonEmpty (Arg DecidabilityBuiltin) -> m Code
compileApp fun args = do
  let userArgs = NonEmpty.filter (not . wasInsertedByCompiler) args
  case fun of
    Builtin _p b ->
      compileBuiltin b userArgs
    _ -> do
      cFun <- compileExpr fun
      compiledArgs <- compileArgs minPrecedence userArgs
      return $ annotate (Set.empty, minPrecedence) (cFun <+> hsep compiledArgs)

compileArg :: (MonadLeanCompile m) => Precedence -> Arg DecidabilityBuiltin -> m Code
compileArg precedence arg = do
  body <- compileExpr (argExpr arg)
  return $ argBrackets precedence (visibilityOf arg) body

compileArgs :: (MonadLeanCompile m) => Precedence -> [Arg DecidabilityBuiltin] -> m [Code]
compileArgs precedence = traverse (compileArg precedence)

compileBinder :: (MonadLeanCompile m) => Binder DecidabilityBuiltin -> m Code
compileBinder binder = do
  binderType <- compileExpr (typeOf binder)
  (binderDoc, noExplicitBrackets) <- case binderNamingForm binder of
    OnlyName name _ -> return (pretty name, True)
    OnlyType -> return (binderType, True)
    NameAndType name _ -> do
      let annName = annotate (Set.empty, minPrecedence) (pretty name <+> ":" <+> binderType)
      return (annName, False)

  return $ binderBrackets noExplicitBrackets (visibilityOf binder) binderDoc

compileTopLevelBinders :: (MonadLeanCompile m) => [Binder DecidabilityBuiltin] -> m [Code]
compileTopLevelBinders [] = return []
compileTopLevelBinders (b : bs) = do
  b' <- compileTopLevelBinder b
  addNameToContext b $ case b' of
    Nothing -> compileTopLevelBinders bs
    Just bc -> do
      bsc <- compileTopLevelBinders bs
      return $ bc : bsc

compileTopLevelBinder :: (MonadLeanCompile m) => Binder DecidabilityBuiltin -> m (Maybe Code)
compileTopLevelBinder binder
  | visibilityOf binder /= Explicit = pure Nothing
  | otherwise = do
      let binderName = pretty (getBinderName binder)
      binderType <- compileExpr (typeOf binder)
      pure . Just . parens $ binderName <+> ":" <+> binderType

compileBinders :: (MonadLeanCompile m) => [Binder DecidabilityBuiltin] -> m Code -> m ([Code], Code)
compileBinders [] c = ([],) <$> c
compileBinders (b : bs) c = do
  (cbs, cc) <- addNameToContext b $ compileBinders bs c
  cb <- compileBinder b
  return (cb : cbs, cc)

resolveReturnType :: (MonadLeanCompile m) => [Code] -> Expr DecidabilityBuiltin -> m Code
resolveReturnType binders t = do
  cType <- compileExpr t
  return $ case binders of
    [] -> cType
    _ -> foldr (\b acc -> b <+> "->" <+> acc) cType binders

compileLetBinder :: (MonadLeanCompile m) => LetBinder (Expr DecidabilityBuiltin) -> m Code
compileLetBinder (binder, expr) = do
  let binderName = pretty (getBinderName binder)
  cExpr <- compileExpr expr
  return $ binderName <+> ":=" <+> cExpr

compileRecordFieldDef :: (MonadLeanCompile m) => GenericRecordField (Expr DecidabilityBuiltin) -> m Code
compileRecordFieldDef (field, fieldType) = do
  cFieldType <- compileExpr fieldType
  return $ pretty field <+> ":" <+> cFieldType

compileRecordField :: (MonadLeanCompile m) => GenericRecordField (Expr DecidabilityBuiltin) -> m Code
compileRecordField (field, fieldValue) = do
  cFieldValue <- compileExpr fieldValue
  return $ pretty field <+> "=" <+> cFieldValue

compileIndexLiteral :: Int -> Code
compileIndexLiteral i = annotateConstant [MathlibData] $ "⟨" <> pretty i <> "⟩"

compileNatLiteral :: Int -> Code
compileNatLiteral i = annotateConstant [] $ pretty i

compileBoolLiteral :: Bool -> Code
compileBoolLiteral = \case
  True -> "true"
  False -> "false"

compileRatLiteral :: Rational -> Code
compileRatLiteral r =
  annotateConstant [MathlibAlgebra] $
    "(" <> pretty (numerator r) <> " : ℚ)" <> " / " <> pretty (denominator r)

compileTensorLiteral :: (a -> Code) -> Tensor a -> Code
compileTensorLiteral compileElement t =
  annotateConstant [VehicleLib] $ "⟨" <> concatWith (surround "; ") (map compileElement (toList t)) <> "⟩"

compileVecLiteral :: (MonadLeanCompile m) => [Arg DecidabilityBuiltin] -> m Code
compileVecLiteral _xs =
  return $ annotateConstant [MathlibData] "Vector.mk"
