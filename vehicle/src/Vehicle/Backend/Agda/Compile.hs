module Vehicle.Backend.Agda.Compile
  ( AgdaOptions (..),
    compileProgToAgda,
  )
where

import Data.Foldable (fold)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Real (denominator, numerator)
import Prettyprinter hiding (hcat, hsep, vcat, vsep)
import System.FilePath (takeBaseName)
import Vehicle.Backend.Agda.CapitaliseTypeNames (capitaliseTypeNames)
import Vehicle.Compile.Context.Bound (getNamedBoundCtx)
import Vehicle.Compile.Context.Name (MonadNameContext, addNameToContext, ixToProperName, runFreshNameContextT)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Data.Builtin.Decidability
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Standard (BuiltinType (..))
import Vehicle.Data.Builtin.Standard hiding (TensorType)
import Vehicle.Data.Code.Expr ()
import Vehicle.Data.Code.Interface (IsArgs (..), VecLitArgs (..))
import Vehicle.Data.Universe (UniverseLevel (..))
import Vehicle.Syntax.Sugar
import Vehicle.Syntax.Tensor (Tensor, TensorShape, foldMapTensor)

--------------------------------------------------------------------------------
-- Agda-specific options

data AgdaOptions = AgdaOptions
  { verificationCache :: Maybe FilePath,
    output :: Maybe FilePath,
    moduleName :: Maybe String
  }

compileProgToAgda :: (MonadCompile m) => Prog DecidabilityBuiltin -> AgdaOptions -> m (Doc a)
compileProgToAgda prog options =
  logCompilerSection2 MinDetail currentPhase $ do
    logDebug MaxDetail $ prettyExternal prog
    prog2 <- capitaliseTypeNames prog
    programDoc <- runFreshNameContextT $ compileProg options prog2
    let programStream = layoutPretty defaultLayoutOptions programDoc
    -- Collects dependencies by first discarding precedence info and then
    -- folding using Set Monoid
    let progamDependencies = fold (reAnnotateS fst programStream)

    let nameOfModule = Text.pack $ case moduleName options of
          Just name -> name
          _ -> maybe "Spec" takeBaseName (output options)

    let agdaProgram =
          unAnnotate
            ( (vsep2 :: [Code] -> Code)
                [ optionStatements ["allow-exec"],
                  importStatements progamDependencies,
                  moduleHeader nameOfModule,
                  programDoc
                ]
            )

    return agdaProgram

--------------------------------------------------------------------------------
-- Debug functions

type MonadAgdaCompile m =
  ( MonadCompile m,
    MonadNameContext m
  )

logEntry :: (MonadAgdaCompile m) => Expr DecidabilityBuiltin -> m ()
logEntry e = do
  incrCallDepth
  ctx <- getNamedBoundCtx (Proxy @())
  logDebug MaxDetail $ "compile-entry" <+> prettyExternal (WithContext e ctx)

logExit :: (MonadAgdaCompile m) => Code -> m ()
logExit e = do
  logDebug MaxDetail $ "compile-exit " <+> e
  decrCallDepth

--------------------------------------------------------------------------------
-- Modules

-- | All possible Agda modules the program may depend on.
data Dependency
  = -- Vehicle Agda library (hopefully will migrate these with time)
    VehicleCore
  | VehicleUtils
  | DataTensor
  | DataTensorInstances
  | DataTensorAll
  | DataTensorAny
  | -- Standard library
    DataUnit
  | DataEmpty
  | DataProduct
  | DataSum
  | DataNat
  | DataNatInstances
  | DataNatDivMod
  | DataInteger
  | DataIntegerInstances
  | DataIntegerDivMod
  | DataRat
  | DataRatInstances
  | DataBool
  | DataBoolInstances
  | DataFin
  | DataFinAll
  | DataFinAny
  | DataList
  | DataListInstances
  | DataListAll
  | DataListAny
  | DataVector
  | DataVectorInstances
  | DataProductNary
  | FunctionBase
  | PropEquality
  | RelNullary
  | RelNullaryDecidable
  deriving (Eq, Ord)

instance Pretty Dependency where
  pretty = \case
    VehicleCore -> "Vehicle"
    VehicleUtils -> "Vehicle.Utils"
    DataTensor -> "Vehicle.Data.Tensor"
    DataTensorInstances -> "Vehicle.Data.Tensor.Instances"
    DataTensorAll -> "Vehicle.Data.Tensor.Relation.Unary.All as" <+> tensorQualifier
    DataTensorAny -> "Vehicle.Data.Tensor.Relation.Unary.Any as" <+> tensorQualifier
    DataUnit -> "Data.Unit"
    DataEmpty -> "Data.Empty"
    DataProduct -> "Data.Product"
    DataSum -> "Data.Sum"
    DataNat -> "Data.Nat as" <+> natQualifier <+> "using" <+> parens "ℕ"
    DataNatInstances -> "Data.Nat.Instances"
    DataNatDivMod -> "Data.Nat.DivMod as" <+> natQualifier
    DataInteger -> "Data.Integer as" <+> intQualifier <+> "using" <+> parens "ℤ"
    DataIntegerInstances -> "Data.Integer.Instances"
    DataIntegerDivMod -> "Data.Int.DivMod as" <+> intQualifier
    DataRat -> "Data.Rational as" <+> ratQualifier <+> "using" <+> parens "ℚ"
    DataRatInstances -> "Data.Rational.Instances"
    DataBool -> "Data.Bool as" <+> boolQualifier <+> "using" <+> parens "Bool; true; false; if_then_else_"
    DataBoolInstances -> "Data.Bool.Instances"
    DataFin -> "Data.Fin as" <+> finQualifier <+> "using" <+> parens "Fin; #_"
    DataFinAll -> "Data.Vec.Functional.Relation.Unary.All as" <+> finQualifier
    DataFinAny -> "Data.Vec.Functional.Relation.Unary.Any as" <+> finQualifier
    DataList -> "Data.List.Base"
    DataListInstances -> "Data.List.Instances"
    DataListAll -> "Data.List.Relation.Unary.All as" <+> listQualifier
    DataListAny -> "Data.List.Relation.Unary.Any as" <+> listQualifier
    DataVector -> "Data.Vec.Functional" <+> "renaming" <+> parens "[] to []ᵥ; _∷_ to _∷ᵥ_"
    DataVectorInstances -> "Data.Vec.Functional.Instances"
    DataProductNary -> "Data.Product.Nary.NonDependent"
    FunctionBase -> "Function.Base"
    PropEquality -> "Relation.Binary.PropositionalEquality"
    RelNullary -> "Relation.Nullary"
    RelNullaryDecidable -> "Relation.Nullary.Decidable"

optionStatement :: Text -> Doc a
optionStatement option = "{-# OPTIONS --" <> pretty option <+> "#-}"

optionStatements :: [Text] -> Doc a
optionStatements = vsep . map optionStatement

importStatement :: Dependency -> Doc a
importStatement dep = "open import" <+> pretty dep

importStatements :: Set Dependency -> Doc a
importStatements deps = vsep $ map importStatement dependencies
  where
    dependencies = sort (VehicleCore : Set.toList deps)

moduleHeader :: Text -> Doc a
moduleHeader moduleName = "module" <+> pretty moduleName <+> "where"

boolQualifier :: Doc a
boolQualifier = "𝔹"

finQualifier :: Doc a
finQualifier = "Fin"

natQualifier :: Doc a
natQualifier = "ℕ"

intQualifier :: Doc a
intQualifier = "ℤ"

ratQualifier :: Doc a
ratQualifier = "ℚ"

listQualifier :: Doc a
listQualifier = "List"

tensorQualifier :: Doc a
tensorQualifier = "𝕋"

indentCode :: Code -> Code
indentCode = indent 2

scopeCode :: Code -> Code -> Code
scopeCode keyword code = keyword <> line <> indentCode code

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

annotateApp :: (MonadAgdaCompile m) => [Dependency] -> Maybe Code -> Code -> [Arg DecidabilityBuiltin] -> m Code
annotateApp dependencies qualifier fun args = do
  let funDoc = maybe "" (<> ".") qualifier <> fun

  (precedence, annDoc) <-
    if null args
      then return (getPrecedence fun, funDoc)
      else do
        let precedence = 20
        bracketedArgs <- compileArgs precedence args
        return (20, hsep (funDoc : bracketedArgs))

  return $ annotate (Set.fromList dependencies, precedence) annDoc

annotateInfixApp ::
  (MonadAgdaCompile m) =>
  [Dependency] ->
  Precedence ->
  Maybe Code ->
  Text ->
  [Arg DecidabilityBuiltin] ->
  m Code
annotateInfixApp dependencies precedence qualifier op args
  | not (all isExplicit args) = annotateApp dependencies qualifier (pretty op) args
  | otherwise = do
      bracketedArgs <- compileArgs precedence args
      doc <- insertInfixArgs qualifier op bracketedArgs
      return $ annotate (Set.fromList dependencies, precedence) doc

-- | Inserts infix args into the correct positions
--
-- e.g. insertInfixArgs (Just "B") "if_then_else_" [a, b] = B.if a then b else_
insertInfixArgs :: (MonadCompile m) => Maybe Code -> Text -> [Code] -> m Code
insertInfixArgs qual rawOp as = do
  let components = go qual rawOp as
  return $ hsep components
  where
    go :: Maybe Code -> Text -> [Code] -> [Code]
    go qualifier opText = \case
      []
        | Text.null opText -> []
        | otherwise -> [pretty opText]
      arg : args -> do
        let (prefix, maybeSuffix) = Text.break (== '_') opText
        let suffix = case Text.uncons maybeSuffix of
              Just (_underscore, suff) -> suff
              Nothing ->
                developerError $
                  "the operation"
                    <+> pretty rawOp
                    <+> "has too many arguments"
                    <+> list as

        if Text.null prefix
          then arg : go qualifier suffix args
          else do
            let back = go Nothing suffix args
            let front = pretty prefix
            let qualifiedFront = maybe front (\q -> q <> "." <> front) qualifier
            qualifiedFront : arg : back

argBrackets :: Precedence -> Visibility -> Code -> Code
argBrackets parentPrecedence v e = case v of
  Explicit {}
    | getPrecedence e > parentPrecedence -> e
    | otherwise -> parens e
  Implicit {} -> braces e
  Instance {} -> braces (braces e)

binderBrackets :: Bool -> Visibility -> Code -> Code
binderBrackets topLevel = \case
  Explicit {} | topLevel -> id
  Explicit {} | otherwise -> parens
  Implicit {} -> braces
  Instance {} -> braces . braces

--------------------------------------------------------------------------------
-- Compilation of program structure

compileProg :: (MonadAgdaCompile m) => AgdaOptions -> Prog DecidabilityBuiltin -> m Code
compileProg opts (Main ds) = vsep2 <$> traverse (compileDecl opts) ds

compileDecl :: (MonadAgdaCompile m) => AgdaOptions -> Decl DecidabilityBuiltin -> m Code
compileDecl opts = \case
  DefAbstract _ n _ t ->
    compilePostulate (compileIdentifier n) <$> compileExpr t
  DefFunction _ n anns t e -> do
    let (binders, body) = foldDeclBinders e
    if isProperty anns
      then compileProperty opts (compileIdentifier n) =<< compileExpr e
      else do
        let binders' = mapMaybe compileTopLevelBinder binders
        (_, cbody) <- compileBinders binders (compileExpr body)
        compileFunDef (compileIdentifier n) <$> compileExpr t <*> pure binders' <*> pure cbody
  DefRecord _ n _ t fs -> do
    t' <- compileExpr t
    fs' <- traverseRecordFields compileExpr fs
    return $
      "record"
        <+> compileIdentifier n
        <+> ":"
        <+> t'
        <+> "where"
        <> line
        <> indent 2 "field"
        <> indent 4 (vsep $ fmap (\(field, fieldType) -> pretty field <+> ":" <+> fieldType) fs')

compileExpr :: (MonadAgdaCompile m) => Expr DecidabilityBuiltin -> m Code
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
    Pi _ binder result -> case binderNamingForm binder of
      OnlyType -> do
        cInput <- compileBinder binder
        cOutput <- addNameToContext binder $ compileExpr result
        return $ cInput <+> "→" <+> cOutput
      _ -> do
        let (binders, body) = foldBinders PiBinder binder result
        compileTypeLevelQuantifier Forall (binder :| binders) body
    Let _ bound binder body -> do
      cBoundExpr <- compileLetBinder (binder, bound)
      cBody <- addNameToContext binder $ compileExpr body
      return $ "let" <+> cBoundExpr <+> "in" <+> cBody
    Lam _ binder body -> compileLam binder body
    Builtin _ b -> compileBuiltin b []
    App fun args -> compileApp fun args
    Record _p _i fs -> do
      fs' <- traverse compileRecordField fs
      return $
        "record"
          <> line
          <> indent 2 "{"
          <> concatWith (\x y -> x <> line <> ";" <+> y) fs'
          <> line
          <> "}"
    RecordAcc _p r (_i, field) ->
      annotateApp [] Nothing (pretty field) [explicit r]
  logExit result
  return result

compileArg :: (MonadAgdaCompile m) => Precedence -> Arg DecidabilityBuiltin -> m Code
compileArg precedence arg = do
  body <- compileExpr (argExpr arg)
  return $ argBrackets precedence (visibilityOf arg) body

compileArgs :: (MonadAgdaCompile m) => Precedence -> [Arg DecidabilityBuiltin] -> m [Code]
compileArgs precedence = traverse (compileArg precedence)

compileLetBinder ::
  (MonadAgdaCompile m) =>
  LetBinder (Expr DecidabilityBuiltin) ->
  m Code
compileLetBinder (binder, expr) = do
  let binderName = pretty (getBinderName binder)
  cExpr <- compileExpr expr
  return $ binderName <+> "=" <+> cExpr

compileLam :: (MonadAgdaCompile m) => Binder DecidabilityBuiltin -> Expr DecidabilityBuiltin -> m Code
compileLam binder expr = do
  let (binders, body) = foldBinders LamBinder binder expr
  (cBinders, cBody) <- compileBinders (binder : binders) (compileExpr body)
  return $ annotate (mempty, minPrecedence) ("λ" <+> hsep cBinders <+> "→" <+> cBody)

compileIdentifier :: Identifier -> Code
compileIdentifier ident = pretty (nameOf ident :: Name)

compileType :: UniverseLevel -> Code
compileType (UniverseLevel l)
  | l == 0 = "Set"
  | otherwise = annotateConstant [] ("Set" <> pretty l)

compileTopLevelBinder :: Binder DecidabilityBuiltin -> Maybe Code
compileTopLevelBinder binder
  | visibilityOf binder /= Explicit = Nothing
  | otherwise = do
      let binderName = pretty (getBinderName binder)
      let addBrackets = binderBrackets True (visibilityOf binder)
      Just $ addBrackets binderName

compileBinders :: (MonadAgdaCompile m) => [Binder DecidabilityBuiltin] -> m Code -> m ([Code], Code)
compileBinders [] c = ([],) <$> c
compileBinders (b : bs) c = do
  (cbs, cc) <- addNameToContext b $ compileBinders bs c
  cb <- compileBinder b
  return (cb : cbs, cc)

compileBinder :: (MonadAgdaCompile m) => Binder DecidabilityBuiltin -> m Code
compileBinder binder = do
  binderType <- compileExpr (typeOf binder)
  (binderDoc, noExplicitBrackets) <- case binderNamingForm binder of
    OnlyName name -> return (pretty name, True)
    OnlyType -> return (binderType, True)
    NameAndType name -> do
      let annName = "(" <> pretty name <+> ":" <+> binderType <> ")"
      return (annName, False)

  return $ binderBrackets noExplicitBrackets (visibilityOf binder) binderDoc

compileRecordField :: (MonadAgdaCompile m) => RecordField (Expr DecidabilityBuiltin) -> m Code
compileRecordField (field, fieldValue) = do
  fieldValue' <- compileExpr fieldValue
  return $ pretty field <+> "=" <+> fieldValue'

compileApp :: (MonadAgdaCompile m) => Expr DecidabilityBuiltin -> NonEmpty (Arg DecidabilityBuiltin) -> m Code
compileApp fun args = do
  let userArgs = NonEmpty.filter (not . wasInsertedByCompiler) args
  case fun of
    Builtin _p b ->
      compileBuiltin b userArgs
    _ -> do
      cFun <- compileExpr fun
      annotateApp [] Nothing cFun userArgs

compileDerivedFunction ::
  (MonadAgdaCompile m) =>
  DerivedFunction ->
  [Arg DecidabilityBuiltin] ->
  m Code
compileDerivedFunction fn args = case fn of
  QuantifyIndex q -> case q of
    Exists -> annotateApp [VehicleUtils] Nothing "existsIndex" args
    Forall -> annotateApp [VehicleUtils] Nothing "forallIndex" args
  QuantifyInList q -> case q of
    Exists -> annotateApp [DataList] (Just listQualifier) "any" args
    Forall -> annotateApp [DataList] (Just listQualifier) "all" args
  TypeAnn -> annotateInfixApp [FunctionBase] 0 Nothing "_∋_" args
  CompareRatTensorReduced op ->
    annotateInfixApp [DataTensor] 4 Nothing ("_" <> comparisonOperatorBase True op <> "_") args

--------------------------------------------------------------------------------
-- Compilation of builtins

compileBuiltin :: (MonadAgdaCompile m) => DecidabilityBuiltin -> [Arg DecidabilityBuiltin] -> m Code
compileBuiltin b args = case b of
  StandardBuiltinType t -> compileBuiltinType t args
  StandardBuiltinConstructor c -> compileBuiltinConstructor c args
  StandardBuiltinFunction f -> compileBuiltinFunction f args
  StandardBuiltinDerivedFunction f -> compileDerivedFunction f args
  DecidabilityBuiltinFunction f -> compileDecidabilityBuiltinFunction f args
  DecidabilityBuiltinTypeClass {} -> monoError b
  DecidabilityBuiltinTypeClassOp {} -> monoError b

compileBuiltinType ::
  (MonadAgdaCompile m) =>
  BuiltinType ->
  [Arg DecidabilityBuiltin] ->
  m Code
compileBuiltinType t args = case t of
  UnitType -> return $ annotateConstant [DataUnit] "⊤"
  BoolType -> return $ compileType (UniverseLevel 0)
  IndexType -> annotateApp [DataFin] Nothing "Fin" args
  NatType -> return $ annotateConstant [DataNat] natQualifier
  RatType -> return $ annotateConstant [DataRat] ratQualifier
  ListType -> annotateApp [DataList] Nothing "List" args
  VectorType -> annotateApp [DataVector] Nothing "Vector" args
  TensorType -> annotateApp [DataTensor] Nothing "Tensor" args

compileBuiltinConstructor ::
  (MonadAgdaCompile m) =>
  BuiltinConstructor ->
  [Arg DecidabilityBuiltin] ->
  m Code
compileBuiltinConstructor c args = case c of
  Nil -> return $ annotateConstant [DataList] "[]"
  Cons -> annotateInfixApp [DataList] 5 Nothing "_∷_" args
  UnitLiteral -> return $ annotateConstant [DataUnit] "tt"
  IndexLiteral n -> return $ compileIndexLiteral n
  NatLiteral n -> return $ compileNatLiteral n
  VectorLiteral -> compileVecLiteral args
  NatTensorLiteral t -> return $ compileTensorLiteral compileNatLiteral t
  BoolTensorLiteral t -> return $ compileTensorLiteral compileBoolLiteral t
  RatTensorLiteral t -> return $ compileTensorLiteral compileRatLiteral t

compileBuiltinFunction ::
  (MonadAgdaCompile m) =>
  BuiltinFunction ->
  [Arg DecidabilityBuiltin] ->
  m Code
compileBuiltinFunction f args = case f of
  And -> annotateInfixApp [DataBool] 6 Nothing "_∧_" args
  Or -> annotateInfixApp [DataBool] 5 Nothing "_∨_" args
  Not -> annotateApp [DataBool] Nothing "not" args
  Implies -> annotateInfixApp [VehicleUtils] 4 Nothing "_⇒_" args
  Add AddNat -> annotateInfixApp [DataNat] 6 (Just natQualifier) "_⊕_" args
  Mul MulNat -> annotateInfixApp [DataNat] 7 (Just natQualifier) "_*_" args
  Add AddRatTensor -> annotateInfixApp [DataTensor] 6 (Just tensorQualifier) "_⊕_" args
  Sub SubRatTensor -> annotateInfixApp [DataTensor] 6 (Just tensorQualifier) "_⊖_" args
  Mul MulRatTensor -> annotateInfixApp [DataTensor] 7 (Just tensorQualifier) "_*_" args
  Div DivRatTensor -> annotateInfixApp [DataTensor] 7 (Just tensorQualifier) "_÷_" args
  Neg NegRatTensor -> annotateInfixApp [DataTensor] 8 (Just tensorQualifier) "-_" args
  Min MinRatTensor -> annotateInfixApp [DataTensor] 6 (Just tensorQualifier) "_⊓_" args
  Max MaxRatTensor -> annotateInfixApp [DataTensor] 7 (Just tensorQualifier) "_⊔_" args
  CompareIndex op -> annotateInfixApp [VehicleUtils, DataFin] 4 Nothing (comparisonOperator True op) args
  CompareNat op -> annotateInfixApp [VehicleUtils, DataNat] 4 Nothing (comparisonOperator True op) args
  CompareRatTensorPointwise op -> annotateInfixApp [VehicleUtils, DataTensor] 4 Nothing ("_" <> comparisonOperatorBase True op <> "∙_") args
  FoldList -> annotateApp [DataList] (Just listQualifier) "foldr" args
  MapList -> annotateApp [DataList] (Just listQualifier) "map" args
  ReduceAndTensor -> annotateApp [DataTensor] Nothing "reduceAnd" args
  ReduceOrTensor -> annotateApp [DataTensor] Nothing "reduceOr" args
  ReduceAddRatTensor -> annotateApp [DataTensor] Nothing "reduceAdd" args
  ReduceMinRatTensor -> annotateApp [DataTensor] Nothing "reduceMin" args
  ReduceMaxRatTensor -> annotateApp [DataTensor] Nothing "reduceMax" args
  ReduceMulRatTensor -> annotateApp [DataTensor] Nothing "reduceMul" args
  ConstTensor -> annotateApp [DataTensor] Nothing "constTensor" args
  QuantifyRatTensor q -> case reverse args of
    (ExplicitArg _ _ (Lam _ binder body)) : _ -> compileTypeLevelQuantifier q [binder] body
    _ -> unsupportedArgsError
  AtTensor -> annotateInfixApp [DataTensor] (-1) Nothing "_!_" args
  AtVector -> annotateInfixApp [FunctionBase] (-1) Nothing "_$_" args
  If -> annotateInfixApp [DataBool] 0 Nothing "if_then_else_" args
  ForeachTensor -> annotateApp [DataTensor] Nothing "foreach" args
  ForeachVector -> annotateApp [VehicleUtils] Nothing "foreachVector" args
  StackTensor {} -> annotateApp [DataTensor] Nothing "stack" args
  Iterate -> unsupportedError
  PowRat -> unsupportedError
  where
    unsupportedError :: a
    unsupportedError =
      developerError $
        "compilation of builtin" <+> quotePretty f <+> "to Agda unsupported"

    unsupportedArgsError :: a
    unsupportedArgsError = do
      developerError $
        "compilation of"
          <+> quotePretty f
          <+> "with args"
          <+> prettyVerbose args
          <+> "to Agda unsupported"

compileDecidabilityBuiltinFunction ::
  (MonadAgdaCompile m) =>
  DecidabilityBuiltinFunction ->
  [Arg DecidabilityBuiltin] ->
  m Code
compileDecidabilityBuiltinFunction f args = case f of
  PropType -> return $ compileType 0
  BoolTensorToProp -> monoError f
  BoolVectorToProp -> monoError f
  PropTrue -> return $ annotateConstant [DataUnit] "⊤"
  PropFalse -> return $ annotateConstant [DataEmpty] "⊥"
  PropNot -> annotateInfixApp [RelNullary] 3 Nothing "¬_" args
  PropAnd -> annotateInfixApp [DataProduct] 2 Nothing "_×_" args
  PropOr -> annotateInfixApp [DataSum] 1 Nothing "_⊎_" args
  PropImplies -> annotateInfixApp [] minPrecedence Nothing "_→_" args
  PropCompareIndex op -> annotateInfixApp [VehicleUtils, DataFin] 4 Nothing (comparisonOperator False op) args
  PropCompareNat op -> annotateInfixApp [VehicleUtils, DataNat] 4 Nothing (comparisonOperator False op) args
  PropCompareRatTensorPointwise op -> annotateInfixApp [VehicleUtils, DataTensor] 4 Nothing (comparisonOperator False op) args
  PropQuantifyIndex q -> case q of
    Forall -> annotateApp [DataFinAll] (Just finQualifier) "All" args
    Exists -> annotateApp [DataFinAny] (Just finQualifier) "Any" args
  PropQuantifyInList q -> case q of
    Forall -> annotateApp [DataListAll] (Just listQualifier) "All" args
    Exists -> annotateApp [DataListAny] (Just listQualifier) "Any" args
  PropNaryProduct -> annotateApp [DataProductNary] Nothing "Product" args
  PropNaryProductAt -> annotateApp [DataProductNary] Nothing "projₙ" args
  PropNaryProductForeach -> annotateApp [VehicleUtils] Nothing "foreachNary" args

compileTypeLevelQuantifier ::
  (MonadAgdaCompile m) =>
  Quantifier ->
  NonEmpty (Binder DecidabilityBuiltin) ->
  Expr DecidabilityBuiltin ->
  m Code
compileTypeLevelQuantifier q binders body = do
  (cBinders, cBody) <- compileBinders (NonEmpty.toList binders) (compileExpr body)
  quant <- case q of
    Forall -> return "∀"
    Exists -> return $ annotateConstant [DataProduct] "∃ λ"
  return $ quant <+> hsep cBinders <+> "→" <+> cBody

compileIndexLiteral :: Int -> Code
compileIndexLiteral i = annotate ([DataFin], 10) ("#" <+> pretty i)

compileNatLiteral :: Int -> Code
compileNatLiteral = pretty

compileIntLiteral :: Int -> Code
compileIntLiteral i
  | i >= 0 = annotate ([DataInteger], 8) (intQualifier <> ".+" <+> pretty i)
  | otherwise = annotate ([DataInteger], 6) (intQualifier <> ".-" <+> compileIntLiteral (-i))

compileRatLiteral :: Rational -> Code
compileRatLiteral r
  | denominator r == 1 = pretty $ numerator r
  | otherwise = annotate ([DataRat], 7) (num <+> "/" <+> denom)
  where
    num = compileIntLiteral (fromInteger $ numerator r)
    denom = compileNatLiteral (fromInteger $ denominator r)

-- | Compiling vector literals. No literals in Agda so have to go via cons.
toVec :: [Code] -> Code
toVec = foldr (\v vs -> annotate ([], 5) (v <> "∷ᵥ" <> vs)) "[]ᵥ"

compileVecLiteral :: (MonadAgdaCompile m) => [Arg DecidabilityBuiltin] -> m Code
compileVecLiteral xs = case getExpr accessSpine xs of
  Just (VecLitArgs _t _d ds) -> toVec <$> traverse compileExpr ds
  Nothing -> developerError "Malformed type-checked vector literal"

compileTensorLiteral :: (a -> Code) -> Tensor a -> Code
compileTensorLiteral compileElement =
  foldMapTensor compileElement compileTensorLayer
  where
    compileTensorLayer :: TensorShape -> [Code] -> Code
    compileTensorLayer _shape = toVec

compileBoolLiteral :: Bool -> Code
compileBoolLiteral = \case
  True -> annotateConstant [DataBool] "true"
  False -> annotateConstant [DataBool] "false"

comparisonOperatorBase :: Bool -> ComparisonOp -> Text
comparisonOperatorBase decidable order = case order of
  Le -> if decidable then "≤ᵇ" else "≤"
  Lt -> if decidable then "<ᵇ" else "<"
  Ge -> if decidable then "≥ᵇ" else "≥"
  Gt -> if decidable then ">ᵇ" else ">"
  Eq -> if decidable then "≡ᵇ" else "≈"
  Ne -> if decidable then "≢ᵇ" else "≉"

comparisonOperator :: Bool -> ComparisonOp -> Text
comparisonOperator decidable order = do
  let orderDoc = comparisonOperatorBase decidable order
  "_" <> orderDoc <> "_"

compileFunDef :: Code -> Code -> [Code] -> Code -> Code
compileFunDef n t ns e =
  n
    <+> ":"
    <+> align t
    <> line
    <> n
    <+> (if null ns then mempty else hsep ns <> " ")
    <> "="
    <+> e

-- | Compile a `network` declaration
compilePostulate :: Code -> Code -> Code
compilePostulate name t = "postulate" <+> name <+> ":" <+> align t

compileProperty :: (MonadAgdaCompile m) => AgdaOptions -> Code -> Code -> m Code
compileProperty options propertyName propertyBody = do
  let maybeVerificationCache = verificationCache options
  return $
    case maybeVerificationCache of
      Nothing ->
        "postulate" <+> propertyName <+> ":" <+> align propertyBody
      Just verificationCache ->
        scopeCode "abstract" $
          propertyName
            <+> ":"
            <+> align propertyBody
            <> line
            <> propertyName
            <+> "= checkSpecification record"
            <> line
            <> indentCode
              ( "{ cache ="
                  <+> dquotes (pretty verificationCache)
                  <> line
                  <> "}"
              )

currentPhase :: Doc ()
currentPhase = "compilation to Agda"

monoError :: (Pretty b) => b -> a
monoError f =
  developerError $
    "Monomorphisation should have got rid of builtin"
      <+> quotePretty f
