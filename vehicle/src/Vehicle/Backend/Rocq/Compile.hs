module Vehicle.Backend.Rocq.Compile
  ( RocqOptions (..),
    compileProgToRocq,
  )
where

import Data.Data (Proxy (..))
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Internal.Read qualified as Text.Read
import GHC.Real (denominator, numerator)
import Prettyprinter hiding (hcat, hsep, vcat, vsep)
import Vehicle.Compile.Context.Bound (getNamedBoundCtx)
import Vehicle.Compile.Context.Name (MonadNameContext, addNameToContext, ixToProperName, runFreshNameContextT)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude hiding (Module)
import Vehicle.Compile.Print
import Vehicle.Data.Builtin.Decidability
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Standard hiding (TensorType)
import Vehicle.Data.Code.Expr ()
import Vehicle.Data.Code.Interface (IsArgs (..), VecLitArgs (..))
import Vehicle.Data.Universe (UniverseLevel (..))
import Vehicle.Syntax.Builtin
import Vehicle.Syntax.Sugar
  ( BinderType (..),
    LetBinder,
    foldBinders,
    foldDeclBinders,
  )
import Vehicle.Syntax.Tensor
  ( Tensor (..),
    TensorShape,
    foldMapTensor,
    shapeOf,
    toList,
  )

--------------------------------------------------------------------------------
-- Rocq-specific options

data RocqOptions = RocqOptions
  { output :: Maybe FilePath,
    moduleName :: Maybe String
  }

currentPhase :: Doc ()
currentPhase = "compilation to Rocq"

compileProgToRocq :: (MonadCompile m) => Prog DecidabilityBuiltin -> RocqOptions -> m (Doc a)
compileProgToRocq prog options =
  logCompilerSection2 MinDetail currentPhase $ do
    logDebug MaxDetail $ prettyExternal prog
    programDoc <- runFreshNameContextT $ compileProg options prog
    let programStream = layoutPretty defaultLayoutOptions programDoc
    -- Collects dependencies by first discarding precedence info and then
    -- folding using Set Monoid
    let programDependencies = fold (reAnnotateS fst programStream)

    let rocqProgram =
          unAnnotate
            ( (vsep2 :: [Code] -> Code)
                [ importStatements programDependencies,
                  preamble programDependencies,
                  programDoc
                ]
            )

    return rocqProgram

--------------------------------------------------------------------------------
-- Debug functions

logEntry :: (MonadRocqCompile m) => Expr DecidabilityBuiltin -> m ()
logEntry e = do
  incrCallDepth
  ctx <- getNamedBoundCtx (Proxy @())
  logDebug MaxDetail $ "compile-entry" <+> prettyExternal (WithContext e ctx)

logExit :: (MonadRocqCompile m) => Code -> m ()
logExit e = do
  logDebug MaxDetail $ "compile-exit " <+> e
  decrCallDepth

--------------------------------------------------------------------------------
-- Modules

data Dependency
  = RequireImport Library
  | Import Module
  | Open Scope
  deriving (Eq, Ord)

instance Pretty Dependency where
  pretty = \case
    RequireImport l -> "Require Import" <+> pretty l <> "."
    Import m -> "Import" <+> pretty m <> "."
    Open s -> "Open Scope" <+> pretty s <> "."

data Library
  = MathcompSsreflectSsrbool
  | MathcompAlgebraSsralg
  | MathcompSsreflectSsrnat
  | MathcompSsreflectEqtype
  | MathcompSsreflectOrder
  | MathcompSsreflectFintype
  | MathcompSsreflectSeq
  | MathcompSsreflectTuple
  | MathcompAlgebraZmodp
  | MathcompRealsReals
  | VehicleTensor
  | VehicleUtils
  deriving (Eq, Ord)

instance Pretty Library where
  pretty = \case
    VehicleTensor -> "vehicle.tensor"
    VehicleUtils -> "vehicle.utils"
    MathcompAlgebraSsralg -> "mathcomp.algebra.ssralg"
    MathcompSsreflectOrder -> "mathcomp.ssreflect.order"
    MathcompSsreflectFintype -> "mathcomp.ssreflect.fintype"
    MathcompSsreflectSsrbool -> "mathcomp.ssreflect.ssrbool"
    MathcompSsreflectSeq -> "mathcomp.ssreflect.seq"
    MathcompSsreflectTuple -> "mathcomp.ssreflect.tuple"
    MathcompAlgebraZmodp -> "mathcomp.algebra.zmodp"
    MathcompRealsReals -> "mathcomp.reals.reals"
    MathcompSsreflectSsrnat -> "mathcomp.ssreflect.ssrnat"
    MathcompSsreflectEqtype -> "mathcomp.ssreflect.eqtype"

data Module
  = OrderDef
  deriving (Eq, Ord)

instance Pretty Module where
  pretty = \case
    OrderDef -> "Order.Def"

data Scope
  = RingScope
  | OrderScope
  | FormScope
  deriving (Eq, Ord)

instance Pretty Scope where
  pretty = \case
    RingScope -> "ring_scope"
    OrderScope -> "order_scope"
    FormScope -> "form_scope"

importStatements :: Set Dependency -> Code
importStatements deps = vsep $ map pretty (Set.toList deps)

preamble :: Set Dependency -> Code
preamble deps =
  if Set.member (RequireImport MathcompRealsReals) deps
    then compilePostulate "R" "realType"
    else ""

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

annotateApp :: (MonadRocqCompile m) => [Dependency] -> Code -> [Arg DecidabilityBuiltin] -> m Code
annotateApp dependencies fun args = do
  (precedence, annDoc) <-
    if null args
      then return (getPrecedence fun, fun)
      else do
        let precedence = 200
        bracketedArgs <- compileArgs precedence args
        return (precedence, hsep (fun : bracketedArgs))

  return $ annotate (Set.fromList dependencies, precedence) annDoc

annotateNotation ::
  (MonadRocqCompile m) =>
  [Dependency] ->
  Precedence ->
  Text ->
  Maybe Text ->
  [Arg DecidabilityBuiltin] ->
  m Code
annotateNotation dependencies precedence op mFn args
  | not (all isExplicit args) = fallback
  | otherwise = do
      bracketedArgs <- compileArgs precedence args
      let doc = insertNotationArgs op bracketedArgs
      maybe fallback (return . annotate (Set.fromList dependencies, precedence)) doc
  where
    fallback = case mFn of
      Just fn -> annotateApp dependencies (pretty fn) args
      Nothing ->
        developerError $
          "Failed to process notation:"
            <+> pretty op
            <+> "with"
            <+> pretty (length args)
            <+> "arguments"

-- | Inserts arguments to Rocq-style notation
-- e.g. insertNotationArgs "'T[$2]_($1)'" [nil, R] = Just "'T[R]_(nil)'"
-- supports placeholders $0 .. $9, 10 arguments
insertNotationArgs :: Text -> [Code] -> Maybe Code
insertNotationArgs rawOp as = concatWith (<>) <$> go rawOp
  where
    go :: Text -> Maybe [Code]
    go opText = case Text.break (== '$') opText of
      (_, t) | Text.null t -> Just [pretty opText]
      (prefix, t) -> do
        (_, t') <- Text.uncons t
        (nText, maybeSuffix) <- Text.uncons t'
        let n = Text.Read.digitToInt nText
        arg <- atMaybe as n
        suffix <- go maybeSuffix
        return $ pretty prefix : arg : suffix

    atMaybe :: [a] -> Int -> Maybe a
    atMaybe [] _ = Nothing
    atMaybe (x : _) 0 = Just x
    atMaybe (_ : xs) n = atMaybe xs (n - 1)

argBrackets :: Precedence -> Visibility -> Code -> Code
argBrackets parentPrecedence v e = case v of
  Explicit {}
    | getPrecedence e > parentPrecedence -> e
    | otherwise -> parens e
  Implicit {} -> braces e
  Instance {} -> braces (braces e)

binderBrackets :: Bool -> Visibility -> Code -> Code
binderBrackets True Explicit {} = id
binderBrackets False Explicit {} = parens
binderBrackets _topLevel Implicit {} = braces
binderBrackets _topLevel Instance {} = braces . braces

--------------------------------------------------------------------------------
-- Monad stack

type MonadRocqCompile m =
  ( MonadCompile m,
    MonadNameContext m
  )

--------------------------------------------------------------------------------
-- Program Compilation

compileProg :: (MonadRocqCompile m) => RocqOptions -> Prog DecidabilityBuiltin -> m Code
compileProg opts (Main ds) = vsep2 <$> traverse (compileDecl opts) ds

compileDecl :: (MonadRocqCompile m) => RocqOptions -> Decl DecidabilityBuiltin -> m Code
compileDecl _opts = \case
  DefAbstract _ n _ t ->
    compilePostulate (compileIdentifier n) <$> compileExpr t
  DefFunction _ n anns t e -> do
    let (binders, body) = foldDeclBinders e
    if isProperty anns
      then compileProperty (compileIdentifier n) <$> compileExpr e
      else do
        binders' <- compileTopLevelBinders binders
        (_, cbody) <- compileBinders binders (compileExpr body)
        defType <- resolveReturnType binders' t
        return $ compileFunDef (compileIdentifier n) defType binders' cbody
  DefRecord _ n _ t fs -> do
    t' <- compileExpr t
    fs' <- traverseRecordFields compileExpr fs
    return $
      "Record"
        <+> compileIdentifier n
        <+> ":"
        <+> t'
        <+> ":="
        <> line
        <> indent 2 (encloseSep (lbrace <> space) (line <> rbrace) (semi <> space) $ fmap (\(field, fieldType) -> pretty field <+> ":" <+> fieldType) fs')
        <> "."

-- | Compile a 'network' declaration
compilePostulate :: Code -> Code -> Code
compilePostulate name t = "Parameter" <+> name <+> ":" <+> align t <> "."

compileExpr :: (MonadRocqCompile m) => Expr DecidabilityBuiltin -> m Code
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
        return $ annotate ([], 99) $ cInput <+> "->" <+> cOutput
      _ -> do
        let (binders, body) = foldBinders PiBinder binder result
        compileTypeLevelQuantifier Forall (binder :| binders) body
    Let _ bound binder body -> do
      cBoundExpr <- compileLetBinder (binder, bound)
      cBody <- addNameToContext binder $ compileExpr body
      return $ "let" <+> cBoundExpr <+> "in" <+> cBody
    Lam _ binder body -> compileLam binder body
    Builtin _p b -> compileBuiltin b []
    App fun args -> compileApp fun args
    Record _p _i fs -> do
      fs' <- traverse compileRecordField fs
      return $ encloseSep (lbrace <> "|" <> space) (space <> "|" <> rbrace) (semi <> space) fs'
    RecordAcc _p r (_i, field) -> annotateNotation [] 200 ("$0.(" <> nameOf field <> ")") (Just $ nameOf field) [explicit r]
  logExit result
  return result

compileType :: UniverseLevel -> Code
compileType (UniverseLevel l)
  | l == 0 = "Type"
  | otherwise =
      developerError
        "compilation of higher-level universes to Rocq unsupported"

compileLetBinder ::
  (MonadRocqCompile m) =>
  LetBinder (Expr DecidabilityBuiltin) ->
  m Code
compileLetBinder (binder, expr) = do
  let binderName = pretty (getBinderName binder)
  cExpr <- compileExpr expr
  return $ binderName <+> ":=" <+> cExpr

compileIdentifier :: Identifier -> Code
compileIdentifier ident = pretty (nameOf ident :: Name)

compileProperty :: Code -> Code -> Code
compileProperty propertyName propertyBody =
  "Axiom" <+> propertyName <+> ":" <+> propertyBody <> "."

compileTopLevelBinders :: (MonadRocqCompile m) => [Binder DecidabilityBuiltin] -> m [Code]
compileTopLevelBinders [] = return []
compileTopLevelBinders (b : bs) = do
  b' <- compileTopLevelBinder b
  addNameToContext b $ case b' of
    Nothing -> compileTopLevelBinders bs
    Just bc -> do
      bsc <- compileTopLevelBinders bs
      return $ bc : bsc

compileTopLevelBinder :: (MonadRocqCompile m) => Binder DecidabilityBuiltin -> m (Maybe Code)
compileTopLevelBinder binder
  | visibilityOf binder /= Explicit = pure Nothing
  | otherwise = do
      let binderName = pretty (getBinderName binder)
      binderType <- compileExpr (typeOf binder)
      pure . Just . parens $ binderName <+> ":" <+> binderType

compileBinders :: (MonadRocqCompile m) => [Binder DecidabilityBuiltin] -> m Code -> m ([Code], Code)
compileBinders [] c = ([],) <$> c
compileBinders (b : bs) c = do
  (cbs, cc) <- addNameToContext b $ compileBinders bs c
  cb <- compileBinder b
  return (cb : cbs, cc)

compileBinder :: (MonadRocqCompile m) => Binder DecidabilityBuiltin -> m Code
compileBinder binder = do
  binderType <- compileExpr (typeOf binder)
  (binderDoc, noExplicitBrackets) <- case binderNamingForm binder of
    OnlyName name -> return (pretty name, True)
    OnlyType -> return (binderType, True)
    NameAndType name -> do
      let annName = annotate (Set.empty, minPrecedence) (pretty name <+> ":" <+> binderType)
      return (annName, False)

  return $ binderBrackets noExplicitBrackets (visibilityOf binder) binderDoc

resolveReturnType :: (MonadRocqCompile m) => [Code] -> Expr DecidabilityBuiltin -> m Code
resolveReturnType (_ : bs) (Pi _ binder r) = addNameToContext binder $ resolveReturnType bs r
resolveReturnType _ e = compileExpr e

compileRecordField :: (MonadRocqCompile m) => RecordField (Expr DecidabilityBuiltin) -> m Code
compileRecordField (field, fieldValue) = do
  fieldValue' <- compileExpr fieldValue
  return $ pretty field <+> ":=" <+> fieldValue'

compileFunDef :: Code -> Code -> [Code] -> Code -> Code
compileFunDef name t bindings e =
  "Definition"
    <+> name
    <+> (if null bindings then mempty else hsep bindings <> " ")
    <> ":"
    <+> align t
    <+> ":="
    <+> e
    <> "."

-- Default precedence for standard operations can be found at https://coq.inria.fr/doc/V8.18.0/refman/language/coq-library.html#notations
compileBuiltin :: (MonadRocqCompile m) => DecidabilityBuiltin -> [Arg DecidabilityBuiltin] -> m Code
compileBuiltin b args = case b of
  StandardBuiltinType t -> case t of
    BoolType -> return $ compileType (UniverseLevel 0)
    -- For the Rocq backend, rationals are promoted to reals
    RatType -> return $ annotateConstant [RequireImport MathcompRealsReals] "R"
    UnitType -> return "unit"
    NatType -> return "nat"
    ListType -> annotateApp [RequireImport MathcompSsreflectSeq] "seq" args
    TensorType -> annotateNotation [RequireImport VehicleTensor] 0 "'nT[$0]_($1)" Nothing args
    IndexType -> annotateNotation [RequireImport MathcompSsreflectFintype] 0 "'I_$0" (Just "ordinal") args
    VectorType -> annotateNotation [RequireImport MathcompSsreflectTuple] 2 "$0.-tuple $1" Nothing args
  StandardBuiltinConstructor c -> case c of
    Nil -> return "nil"
    Cons -> annotateNotation [RequireImport MathcompSsreflectSeq] 60 "$0 :: $1" (Just "cons") args
    UnitLiteral -> return "tt"
    IndexLiteral n -> return $ compileIndexLiteral n
    NatLiteral n -> return $ compileNatLiteral n
    NatTensorLiteral t -> return $ compileTensorLiteral compileNatLiteral t
    BoolTensorLiteral t -> return $ compileTensorLiteral compileBoolLiteral t
    RatTensorLiteral t -> return $ compileTensorLiteral compileRatLiteral t
    VectorLiteral -> compileVecLiteral args
  StandardBuiltinFunction f -> case f of
    And -> annotateNotation [] 40 "$0 && $1" (Just "andb") args
    Or -> annotateNotation [] 50 "$0 || $1" (Just "orb") args
    Not -> annotateNotation [RequireImport MathcompSsreflectSsrbool] 35 "~~ $0" (Just "negb") args
    Implies -> annotateNotation [RequireImport MathcompSsreflectSsrbool] 55 "$0 ==> $1" (Just "implb") args
    Add AddNat -> annotateNotation [RequireImport MathcompAlgebraSsralg, Open RingScope] 50 "$0 + $1" (Just "+%R") args
    Mul MulNat -> annotateNotation [RequireImport MathcompAlgebraSsralg, Open RingScope] 40 "$0 * $1" (Just "*%R") args
    Add AddRatTensor -> annotateNotation [RequireImport VehicleTensor] 50 "$0 + $1" (Just "+%R") args
    Sub SubRatTensor -> annotateNotation [RequireImport VehicleTensor] 50 "$0 - $1" Nothing args
    Mul MulRatTensor -> annotateNotation [RequireImport VehicleTensor] 40 "$0 * $1" (Just "*%R") args
    Div DivRatTensor -> annotateNotation [RequireImport VehicleTensor] 40 "$0 / $1" Nothing args
    Neg NegRatTensor -> annotateNotation [RequireImport VehicleTensor] 80 "- $0" (Just "-%R") args
    Min MinRatTensor -> annotateApp [RequireImport VehicleTensor, Import OrderDef] "min" args
    Max MaxRatTensor -> annotateApp [RequireImport VehicleTensor, Import OrderDef] "max" args
    CompareIndex op -> compileComparison CIndex op args
    CompareNat op -> compileComparison CNat op args
    CompareRatTensorPointwise op -> compileComparison CRatTensor op args
    FoldList -> annotateApp [RequireImport MathcompSsreflectSeq] "foldr" args
    MapList -> annotateApp [RequireImport MathcompSsreflectSeq] "map" args
    ReduceAndTensor -> annotateApp [RequireImport VehicleUtils] "reduceAnd" args
    ReduceOrTensor -> annotateApp [RequireImport VehicleUtils] "reduceOr" args
    ReduceAddRatTensor -> annotateApp [] "reduceAdd" args
    ReduceMinRatTensor -> unsupportedError
    ReduceMaxRatTensor -> unsupportedError
    ReduceMulRatTensor -> annotateApp [] "reduceMul" args
    ConstTensor -> annotateApp [RequireImport VehicleTensor] "const_t" args
    QuantifyRatTensor q -> case reverse args of
      (ExplicitArg _ _ (Lam _ binder body)) : _ -> compileTypeLevelQuantifier q [binder] body
      _ -> unsupportedArgsError
    AtTensor -> annotateNotation [RequireImport VehicleTensor] 201 "$0^^$1" (Just "nindex") args
    If -> annotateNotation [RequireImport MathcompSsreflectSsrbool] minPrecedence "if $0 then $1 else $2" Nothing args
    ForeachTensor -> annotateApp [RequireImport VehicleTensor] "nstack" args
    StackTensor -> compileStack args
    Iterate -> unsupportedError
    PowRat -> unsupportedError
    AtVector -> annotateApp [RequireImport MathcompSsreflectTuple] "tnth" args
    ForeachVector -> annotateApp [RequireImport VehicleUtils] "foreachTuple" args
  DecidabilityBuiltinFunction f -> case f of
    PropType -> return "Prop"
    PropTrue -> return "True"
    PropFalse -> return "False"
    PropNot -> annotateNotation [] 75 "~ $0" (Just "not") args
    PropAnd -> annotateNotation [] 80 "$0 /\\ $1" (Just "and") args
    PropOr -> annotateNotation [] 85 "$0 \\/ $1" (Just "or") args
    PropImplies -> annotateNotation [RequireImport MathcompSsreflectSsrbool] minPrecedence "$0 -> $1" (Just "implies") args
    PropCompareIndex op -> compileComparison CIndex op args
    PropCompareNat op -> compileComparison CNat op args
    PropCompareRatTensorPointwise op -> compileComparison CRatTensor op args
    BoolTensorToProp -> monoError
    BoolVectorToProp -> monoError
    PropQuantifyIndex q -> case q of
      Forall -> annotateApp [RequireImport VehicleUtils] "forallIndex" args
      Exists -> annotateApp [RequireImport VehicleUtils] "existsIndex" args
    PropQuantifyInList q -> case q of
      Forall -> annotateApp [RequireImport VehicleUtils] "forallInList" args
      Exists -> annotateApp [RequireImport VehicleUtils] "existsInList" args
    PropNaryProduct -> unsupportedError
    PropNaryProductForeach -> unsupportedError
    PropNaryProductAt -> unsupportedError
  DecidabilityBuiltinTypeClass {} -> monoError
  DecidabilityBuiltinTypeClassOp {} -> monoError
  StandardBuiltinDerivedFunction f -> compileDerivedFunction f args
  where
    unsupportedError :: a
    unsupportedError =
      developerError $
        "compilation of builtin" <+> quotePretty b <+> "to Rocq unsupported"

    unsupportedArgsError :: (MonadRocqCompile m) => m a
    unsupportedArgsError = do
      compilerDeveloperError $
        "compilation of"
          <+> quotePretty b
          <+> "with args"
          <+> prettyVerbose args
          <+> "to Rocq unsupported"

    monoError :: a
    monoError =
      developerError $
        "Monomorphisation should have got rid of"
          <+> quotePretty (show b)

compileApp :: (MonadRocqCompile m) => Expr DecidabilityBuiltin -> NonEmpty (Arg DecidabilityBuiltin) -> m Code
compileApp fun args = do
  let userArgs = NonEmpty.filter (not . wasInsertedByCompiler) args
  case fun of
    Builtin _p b ->
      compileBuiltin b userArgs
    _ -> do
      cFun <- compileExpr fun
      annotateApp [] cFun userArgs

compileDerivedFunction :: (MonadRocqCompile m) => DerivedFunction -> [Arg DecidabilityBuiltin] -> m Code
compileDerivedFunction fn args = case fn of
  QuantifyIndex q -> case q of
    Exists -> annotateApp [RequireImport VehicleUtils] "existsIndex" args
    Forall -> annotateApp [RequireImport VehicleUtils] "forallIndex" args
  QuantifyInList {} -> unsupported
  TypeAnn -> annotateNotation [] minPrecedence "$1 : $0" Nothing args
  CompareRatTensorReduced op ->
    annotateApp
      [RequireImport VehicleUtils]
      ( case op of
          Le -> "leRatTensorReduced"
          Lt -> "ltRatTensorReduced"
          Ge -> "geRatTensorReduced"
          Gt -> "gtRatTensorReduced"
          Eq -> "eqRatTensorReduced"
          Ne -> "neRatTensorReduced"
      )
      args
  where
    unsupported = developerError $ "Compilation of stdlib function" <+> quotePretty fn <+> "not implemented"

compileTypeLevelQuantifier ::
  (MonadRocqCompile m) =>
  Quantifier ->
  NonEmpty (Binder DecidabilityBuiltin) ->
  Expr DecidabilityBuiltin ->
  m Code
compileTypeLevelQuantifier q binders body = do
  (cBinders, cBody) <- compileBinders (NonEmpty.toList binders) (compileExpr body)
  quant <- case q of
    Forall -> return "forall"
    Exists -> return "exists"
  return $ quant <+> hsep cBinders <> "," <+> cBody

compileArg :: (MonadRocqCompile m) => Precedence -> Arg DecidabilityBuiltin -> m Code
compileArg precedence arg = do
  body <- compileExpr (argExpr arg)
  return $ argBrackets precedence (visibilityOf arg) body

compileArgs :: (MonadRocqCompile m) => Precedence -> [Arg DecidabilityBuiltin] -> m [Code]
compileArgs precedence = traverse (compileArg precedence)

compileIndexLiteral :: Int -> Code
compileIndexLiteral i =
  annotateConstant
    [ RequireImport MathcompAlgebraSsralg,
      RequireImport MathcompSsreflectFintype,
      RequireImport MathcompAlgebraZmodp,
      Open RingScope
    ]
    (pretty i)

compileNatLiteral :: Int -> Code
compileNatLiteral i = annotate ([RequireImport MathcompSsreflectSsrnat], maxPrecedence) $ pretty i <> "%N"

compileTensorLiteral :: (a -> Code) -> Tensor a -> Code
compileTensorLiteral compileElement t = annotate ([RequireImport VehicleTensor], 200) $ case (shapeOf t, toList t) of
  ([], [x]) -> "const_t" <+> compileElement x
  _ -> foldMapTensor compileElement toTensor t
  where
    toTensor :: TensorShape -> [Code] -> Code
    toTensor shape values = case shape of
      [] -> "[tensor^^=" <+> concatWith (surround "; ") values <> "]"
      _ -> "[tensor^^" <+> concatWith (surround "; ") values <> "]"

compileBoolLiteral :: Bool -> Code
compileBoolLiteral = \case
  True -> "true"
  False -> "false"

compileRatLiteral :: Rational -> Code
compileRatLiteral r = parens $ annotate ([RequireImport MathcompRealsReals, RequireImport MathcompAlgebraSsralg, Open RingScope], minPrecedence) rat
  where
    num = pretty $ numerator r
    denom = pretty $ denominator r
    rat = (if denominator r == 1 then num else num <+> "/" <+> denom) <+> ":" <+> "R"

compileLam :: (MonadRocqCompile m) => Binder DecidabilityBuiltin -> Expr DecidabilityBuiltin -> m Code
compileLam binder expr = do
  let (binders, body) = foldBinders LamBinder binder expr
  (cBinders, cBody) <- compileBinders (binder : binders) (compileExpr body)
  return $ annotate (mempty, minPrecedence) ("fun" <+> hsep cBinders <+> "=>" <+> cBody)

data ComparisonDomain
  = CIndex
  | CNat
  | CRatTensor
  deriving (Eq)

compileComparison :: (MonadRocqCompile m) => ComparisonDomain -> ComparisonOp -> [Arg DecidabilityBuiltin] -> m Code
compileComparison domain op = do
  let (opDoc, dependencies) = case op of
        Le -> ("<=", orderDeps)
        Lt -> ("<", orderDeps)
        Ge -> (">=", orderDeps)
        Gt -> (">", orderDeps)
        Eq -> ("==", eqDeps)
        Ne -> ("!=", eqDeps)
  let typeDeps = case (domain, op) of
        (CIndex, _) -> [RequireImport MathcompSsreflectSsrnat]
        (CNat, _) -> [RequireImport MathcompSsreflectSsrnat]
        (CRatTensor, Eq) -> [RequireImport VehicleTensor]
        (CRatTensor, Ne) -> [RequireImport VehicleTensor]
        (CRatTensor, _) -> [RequireImport VehicleTensor]
  let (opDoc', dependencies') =
        if domain == CIndex
          then ("$0 " <> opDoc <> " $1 :> nat", dependencies ++ [RequireImport MathcompSsreflectSsrnat])
          else ("$0 " <> opDoc <> " $1", dependencies)
  annotateNotation (dependencies' <> typeDeps) 70 opDoc' Nothing
  where
    orderDeps = [RequireImport MathcompSsreflectSsrbool, RequireImport MathcompSsreflectOrder, Open OrderScope]
    eqDeps = [RequireImport MathcompSsreflectSsrbool, RequireImport MathcompSsreflectEqtype]

compileStack :: (MonadRocqCompile m) => [Arg DecidabilityBuiltin] -> m Code
compileStack args = do
  as <- compileArgs minPrecedence args
  return $ annotate ([RequireImport VehicleTensor], 200) $ "nstack_tuple" <+> toVec as

compileVecLiteral :: (MonadRocqCompile m) => [Arg DecidabilityBuiltin] -> m Code
compileVecLiteral xs = case getExpr accessSpine xs of
  Just (VecLitArgs _t _d ds) -> toVec <$> traverse compileExpr ds
  Nothing -> developerError "Malformed type-checked vector literal"

toVec :: [Code] -> Code
toVec xs = annotate ([RequireImport MathcompSsreflectTuple, Open FormScope], maxPrecedence) "[tuple" <+> concatWith (surround "; ") xs <> "]"
