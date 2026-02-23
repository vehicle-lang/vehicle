module Vehicle.Backend.ITP.Rocq
  ( RocqOptions (..),
    compileProgToRocq,
    writeRocqFile,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Internal.Read qualified as Text.Read
import Data.Version (makeVersion)
import GHC.Real (denominator, numerator)
import Prettyprinter hiding (hcat, hsep, vcat, vsep)
import Vehicle.Backend.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Sugar.Binders
import Vehicle.Data.AST.Expr.Scoped ()
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Decidability
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Code.Interface (IsArgs (..), VecLitArgs (..))
import Vehicle.Data.Tensor
  ( Tensor (..),
    TensorShape,
    foldMapTensor,
    shapeOf,
    toList,
  )
import Vehicle.Data.Universe (UniverseLevel (..))
import Vehicle.Data.Variable.Bound.Context.Name

--------------------------------------------------------------------------------
-- Rocq-specific options

data RocqOptions = RocqOptions
  { output :: Maybe FilePath,
    moduleName :: Maybe String,
    constructiveReals :: Bool
  }

currentPhase :: Doc ()
currentPhase = "compilation to Rocq"

compileProgToRocq :: (MonadCompile m) => Prog DecidabilityBuiltin -> RocqOptions -> m (Doc a)
compileProgToRocq prog _options =
  logCompilerSection2 MinDetail currentPhase $ do
    programDoc <- runFreshNameBoundContextT $ compileProg prog
    let programStream = layoutPretty defaultLayoutOptions programDoc
    -- Collects dependencies by first discarding precedence info and then
    -- folding using Set Monoid
    let programDependencies = fold (reAnnotateS fst programStream)
    -- If using constructive reals, add the required import
    let programDependencies' =
          if constructiveReals _options
            then
              Set.insert (MathcompImport Rstruct) $
                Set.insert (RequireImport ConstructiveReals) programDependencies
            else programDependencies

    let rocqProgram =
          unAnnotate
            ( (vsep2 :: [Code] -> Code)
                [ importStatements programDependencies',
                  preamble programDependencies',
                  programDoc
                ]
            )

    return rocqProgram

writeRocqFile ::
  (MonadLogger m, MonadIO m, MonadStdIO m) =>
  Maybe FilePath ->
  Doc a ->
  m ()
writeRocqFile = writeResultToFile (Just rocqOutputFormat)

rocqOutputFormat :: ExternalOutputFormat
rocqOutputFormat =
  ExternalOutputFormat
    { formatName = "Rocq",
      formatVersion = Just $ makeVersion [9, 0, 0],
      commentStyle = Block "(*" "*)",
      emptyLines = True
    }

--------------------------------------------------------------------------------
-- Debug functions

logEntry :: (MonadRocqCompile m) => Expr DecidabilityBuiltin -> m ()
logEntry e = do
  incrCallDepth
  ctx <- getNameContext
  logDebug MaxDetail $ "compile-entry" <+> prettyExternal (WithContext e ctx)

logExit :: (MonadRocqCompile m) => Code -> m ()
logExit e = do
  logDebug MaxDetail $ "compile-exit " <+> e
  decrCallDepth

--------------------------------------------------------------------------------
-- Modules

data Dependency
  = MathcompImport Mathcomp
  | RequireImport Library
  | Import RocqModule
  | Open Scope
  deriving (Eq, Ord)

instance Pretty Dependency where
  pretty = \case
    MathcompImport l -> "From mathcomp Require Import" <+> pretty l <> "."
    RequireImport l -> "Require Import" <+> pretty l <> "."
    Import m -> "Import" <+> pretty m <> "."
    Open s -> "Open Scope" <+> pretty s <> "."

data Mathcomp
  = Boot
  | Algebra
  | Reals
  | Rstruct
  deriving (Eq, Ord)

instance Pretty Mathcomp where
  pretty = \case
    Boot -> "all_boot"
    Algebra -> "all_algebra"
    Reals -> "all_reals"
    Rstruct -> "Rstruct"

data Library
  = VehicleTensor
  | VehicleUtils
  | ConstructiveReals
  deriving (Eq, Ord)

instance Pretty Library where
  pretty = \case
    VehicleTensor -> "vehicle.tensor"
    VehicleUtils -> "vehicle.utils"
    ConstructiveReals -> "Stdlib.Reals.Reals"

data RocqModule
  = OrderDef
  deriving (Eq, Ord)

instance Pretty RocqModule where
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
preamble deps
  | Set.member (RequireImport ConstructiveReals) deps =
      "Notation" <+> "R" <+> ":=" <+> align "Rdefinitions.R" <> "."
  | Set.member (MathcompImport Reals) deps =
      "Parameter" <+> "R" <+> ":" <+> align "realType" <> "."
  | otherwise = ""

--------------------------------------------------------------------------------
-- Intermediate results of compilation

type Precedence = Int

functionApplicationPrecedence :: Maybe Precedence
functionApplicationPrecedence = Just 0

data Associativity
  = LeftAssociative
  | RightAssociative
  | NotAssociative

type Code = Doc (Set Dependency, Maybe Precedence)

getPrecedence :: Code -> Maybe Precedence
getPrecedence e = maybe (developerError $ "missing annotation for" <+> e) snd (docAnn e)

annotateConstant :: [Dependency] -> Code -> Code
annotateConstant dependencies = annotate (Set.fromList dependencies, Nothing)

compileApplication :: (MonadRocqCompile m) => [Dependency] -> Code -> [Arg DecidabilityBuiltin] -> m Code
compileApplication dependencies fun args = do
  (precedence, annDoc) <-
    if null args
      then return (getPrecedence fun, fun)
      else do
        compiledArgs <- traverseArgs compileExpr args
        bracketedArgs <- bracketArgs functionApplicationPrecedence compiledArgs
        return (functionApplicationPrecedence, hsep (fun : bracketedArgs))

  return $ annotate (Set.fromList dependencies, precedence) annDoc

compileNotationAndArgs ::
  (MonadRocqCompile m) =>
  [Dependency] ->
  Associativity ->
  Maybe Precedence ->
  Text ->
  Maybe Text ->
  [Arg DecidabilityBuiltin] ->
  m Code
compileNotationAndArgs dependencies _associativity precedence op mFn args
  | not (all isExplicit args) = fallback
  | otherwise = do
      compiledArgs <- traverseArgs compileExpr args
      bracketedArgs <- bracketArgs precedence compiledArgs
      let doc = insertNotationArgs op bracketedArgs
      maybe fallback (return . annotate (Set.fromList dependencies, precedence)) doc
  where
    fallback = case mFn of
      Just fn -> compileApplication dependencies (pretty fn) args
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

--------------------------------------------------------------------------------
-- Monad stack

type MonadRocqCompile m =
  ( MonadCompile m,
    MonadNameContext m
  )

--------------------------------------------------------------------------------
-- Program Compilation

compileProg :: (MonadRocqCompile m) => Prog DecidabilityBuiltin -> m Code
compileProg (Main ds) = do
  decls <- catMaybes <$> traverse compileDecl ds
  return $ vsep2 decls

compileDecl :: (MonadRocqCompile m) => Decl DecidabilityBuiltin -> m (Maybe Code)
compileDecl = \case
  DefAbstract _ n _ t ->
    Just <$> compilePostulate n t
  DefFunction p n funSort t e -> case funSort of
    TypeDecl binderCount -> Just <$> compileFunctionDecl n binderCount t e
    FunctionDecl binderCount Nothing -> Just <$> compileFunctionDecl n binderCount t e
    FunctionDecl _ (Just AnnProperty) -> Just <$> compileProperty n e
    FunctionDecl _ (Just AnnInstance {}) -> throwError $ UnimplementedFeature p "Compiling instances to Rocq"
    ProjectionDecl {} -> return Nothing
  DefRecord p n _ telescope fields ->
    Just <$> compileRecordDecl p n telescope fields

compileFunctionDecl ::
  (MonadRocqCompile m) =>
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
  (MonadRocqCompile m) =>
  Provenance ->
  Identifier ->
  Telescope DecidabilityBuiltin ->
  RecordFields DecidabilityBuiltin ->
  m Code
compileRecordDecl p ident telescope fields = do
  t' <-
    if null telescope
      then return (compileType 0)
      else throwError $ UnimplementedFeature p "Compiling parameterised records to Rocq"
  fs' <- traverseRecordFields compileExpr fields
  return $
    "Record"
      <+> compileIdentifier ident
      <+> ":"
      <+> t'
      <+> ":="
      <> line
      <> indent 2 (encloseSep (lbrace <> space) (line <> rbrace) (semi <> space) $ fmap (\(field, fieldType) -> pretty field <+> ":" <+> fieldType) fs')
      <> "."

extractDeclBinders ::
  LHSBinderCount ->
  Type DecidabilityBuiltin ->
  Expr DecidabilityBuiltin ->
  ([Binder DecidabilityBuiltin], Expr DecidabilityBuiltin)
extractDeclBinders binderCount typ expr
  | binderCount == 0 = ([], expr)
  | otherwise = case (typ, expr) of
      (Pi _ piBinder piBody, Lam _ lamBinder lamBody) -> do
        -- We want the name from the lambda binder and the type from the
        -- pi binder as this is usually what the user will write.
        let compositeBinder = replaceBinderType (typeOf piBinder) lamBinder
        first (compositeBinder :) (extractDeclBinders (binderCount - 1) piBody lamBody)
      (_, _) -> ([], expr)

-- | Compile a 'network' declaration
compilePostulate ::
  (MonadRocqCompile m) =>
  Identifier ->
  Type DecidabilityBuiltin ->
  m Code
compilePostulate ident t = do
  let name = compileIdentifier ident
  typ <- compileExpr t
  return $ "Parameter" <+> name <+> ":" <+> align typ <> "."

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
      OnlyType -> compileFunctionType [argFromBinder binder (typeOf binder), explicit result]
      _ -> do
        let (binders, body) = foldPiBinders binder result
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
    RecordProj _p _t r field ->
      compileNotationAndArgs [] NotAssociative Nothing ("$0.(" <> nameOf field <> ")") (Just $ nameOf field) [explicit r]
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

compileProperty :: (MonadRocqCompile m) => Identifier -> Expr DecidabilityBuiltin -> m Code
compileProperty ident expr = do
  let propertyName = compileIdentifier ident
  propertyBody <- compileExpr expr
  return $ "Axiom" <+> propertyName <+> ":" <+> propertyBody <> "."

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
    OnlyName name _ -> return (pretty name, True)
    OnlyType -> return (binderType, True)
    NameAndType name _ -> do
      let annName = annotate (Set.empty, Nothing) (pretty name <+> ":" <+> binderType)
      return (annName, False)

  return $ binderBrackets noExplicitBrackets (visibilityOf binder) binderDoc

binderBrackets :: Bool -> Visibility -> Code -> Code
binderBrackets True Explicit {} = id
binderBrackets False Explicit {} = parens
binderBrackets _topLevel Implicit {} = braces
binderBrackets _topLevel Instance {} = braces . braces

resolveReturnType :: (MonadRocqCompile m) => [Code] -> Expr DecidabilityBuiltin -> m Code
resolveReturnType (_ : bs) (Pi _ binder r) = addNameToContext binder $ resolveReturnType bs r
resolveReturnType _ e = compileExpr e

compileRecordField :: (MonadRocqCompile m) => GenericRecordField (Expr DecidabilityBuiltin) -> m Code
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
    RatType -> return $ annotateConstant [MathcompImport Reals] "R"
    UnitType -> return $ annotateConstant [] "unit"
    NatType -> return $ annotateConstant [] "nat"
    ListType -> compileApplication [MathcompImport Boot] "seq" args
    TensorType -> compileNotationAndArgs [RequireImport VehicleTensor] NotAssociative Nothing "'nT[$0]_($1)" Nothing args
    IndexType -> compileNotationAndArgs [MathcompImport Boot] NotAssociative Nothing "'I_$0" (Just "ordinal") args
    VectorType -> compileNotationAndArgs [MathcompImport Boot] NotAssociative (Just 2) "$0.-tuple $1" Nothing args
  StandardBuiltinConstructor c -> case c of
    Nil -> return $ annotateConstant [MathcompImport Boot] "nil"
    Cons -> compileNotationAndArgs [MathcompImport Boot] RightAssociative (Just 60) "$0 :: $1" (Just "cons") args
    UnitLiteral -> return $ annotateConstant [] "tt"
    IndexLiteral n -> return $ compileIndexLiteral n
    NatLiteral n -> return $ compileNatLiteral n
    NatTensorLiteral t -> return $ compileTensorLiteral compileNatLiteral t
    BoolTensorLiteral t -> return $ compileTensorLiteral compileBoolLiteral t
    RatTensorLiteral t -> return $ compileTensorLiteral compileRatLiteral t
    VectorLiteral -> compileVecLiteral args
  StandardBuiltinFunction f -> case f of
    And -> compileNotationAndArgs [] LeftAssociative (Just 40) "$0 && $1" (Just "andb") args
    Or -> compileNotationAndArgs [] LeftAssociative (Just 50) "$0 || $1" (Just "orb") args
    Not -> compileNotationAndArgs [MathcompImport Boot] LeftAssociative (Just 35) "~~ $0" (Just "negb") args
    Implies -> compileNotationAndArgs [MathcompImport Boot] RightAssociative (Just 55) "$0 ==> $1" (Just "implb") args
    Add AddNat -> compileNotationAndArgs [MathcompImport Algebra, Open RingScope] LeftAssociative (Just 50) "$0 + $1" (Just "+%R") args
    Mul MulNat -> compileNotationAndArgs [MathcompImport Algebra, Open RingScope] LeftAssociative (Just 40) "$0 * $1" (Just "*%R") args
    Add AddRatTensor -> compileNotationAndArgs [RequireImport VehicleTensor] LeftAssociative (Just 50) "$0 + $1" (Just "+%R") args
    Sub SubRatTensor -> compileNotationAndArgs [RequireImport VehicleTensor] LeftAssociative (Just 50) "$0 - $1" Nothing args
    Mul MulRatTensor -> compileNotationAndArgs [RequireImport VehicleTensor] LeftAssociative (Just 40) "$0 * $1" (Just "*%R") args
    Div DivRatTensor -> compileNotationAndArgs [RequireImport VehicleTensor] LeftAssociative (Just 40) "$0 / $1" Nothing args
    Neg NegRatTensor -> compileNotationAndArgs [RequireImport VehicleTensor] NotAssociative (Just 80) "- $0" (Just "-%R") args
    Min MinRatTensor -> compileApplication [RequireImport VehicleTensor, Import OrderDef] "min" args
    Max MaxRatTensor -> compileApplication [RequireImport VehicleTensor, Import OrderDef] "max" args
    CompareIndex op -> compileComparison CIndex op args
    CompareNat op -> compileComparison CNat op args
    CompareRatTensorPointwise op -> compileComparison CRatTensor op args
    FoldList -> compileApplication [MathcompImport Boot] "foldr" args
    MapList -> compileApplication [MathcompImport Boot] "map" args
    ReduceAndTensor -> compileApplication [RequireImport VehicleUtils] "reduceAnd" args
    ReduceOrTensor -> compileApplication [RequireImport VehicleUtils] "reduceOr" args
    ReduceAddRatTensor -> compileApplication [] "reduceAdd" args
    ReduceMinRatTensor -> unsupportedError
    ReduceMaxRatTensor -> unsupportedError
    ReduceMulRatTensor -> compileApplication [] "reduceMul" args
    ConstTensor -> compileApplication [RequireImport VehicleTensor] "const_t" args
    QuantifyRatTensor q -> case reverse args of
      (ExplicitArg _ (Lam _ binder body)) : _ -> compileTypeLevelQuantifier q [binder] body
      _ -> unsupportedArgsError
    AtTensor -> compileNotationAndArgs [RequireImport VehicleTensor] LeftAssociative (Just 2) "$0 ^^ $1" (Just "nindex") args
    If -> compileNotationAndArgs [MathcompImport Boot] NotAssociative (Just 0) "if $0 then $1 else $2" Nothing args
    ForeachTensor -> compileApplication [RequireImport VehicleTensor] "nstack" args
    StackTensor -> compileStack args
    Iterate -> unsupportedError
    PowRat -> unsupportedError
    AtVector -> compileApplication [MathcompImport Boot] "tnth" args
    ForeachVector -> compileApplication [RequireImport VehicleUtils] "foreachTuple" args
    QuantifyTensorLike _ -> unsupportedTensorLikeQuantifier
  DecidabilityBuiltinFunction f -> case f of
    PropType -> return $ annotateConstant [] "Prop"
    PropTrue -> return $ annotateConstant [] "True"
    PropFalse -> return $ annotateConstant [] "False"
    PropNot -> compileNotationAndArgs [] RightAssociative (Just 75) "~ $0" (Just "not") args
    PropAnd -> compileNotationAndArgs [] RightAssociative (Just 80) "$0 /\\ $1" (Just "and") args
    PropOr -> compileNotationAndArgs [] RightAssociative (Just 85) "$0 \\/ $1" (Just "or") args
    PropImplies -> compileFunctionType args
    PropCompareIndex op -> compileComparison CIndex op args
    PropCompareNat op -> compileComparison CNat op args
    PropCompareRatTensorPointwise op -> compileComparison CRatTensor op args
    BoolTensorToProp -> monoError
    BoolVectorToProp -> monoError
    PropQuantifyIndex q -> case q of
      Forall -> compileApplication [RequireImport VehicleUtils] "forallIndex" args
      Exists -> compileApplication [RequireImport VehicleUtils] "existsIndex" args
    PropQuantifyInList q -> case q of
      Forall -> compileApplication [RequireImport VehicleUtils] "forallInList" args
      Exists -> compileApplication [RequireImport VehicleUtils] "existsInList" args
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

compileFunctionType :: (MonadRocqCompile m) => [Arg DecidabilityBuiltin] -> m Code
compileFunctionType = compileNotationAndArgs [MathcompImport Boot] RightAssociative (Just 100) "$0 -> $1" (Just "implies")

compileApp :: (MonadRocqCompile m) => Expr DecidabilityBuiltin -> NonEmpty (Arg DecidabilityBuiltin) -> m Code
compileApp fun args = do
  let userArgs = NonEmpty.filter (not . wasInsertedByCompiler) args
  case fun of
    Builtin _p b ->
      compileBuiltin b userArgs
    _ -> do
      cFun <- compileExpr fun
      compileApplication [] cFun userArgs

compileDerivedFunction :: (MonadRocqCompile m) => DerivedFunction -> [Arg DecidabilityBuiltin] -> m Code
compileDerivedFunction fn args = case fn of
  QuantifyIndex q -> case q of
    Exists -> compileApplication [RequireImport VehicleUtils] "existsIndex" args
    Forall -> compileApplication [RequireImport VehicleUtils] "forallIndex" args
  QuantifyInList {} -> unsupported
  TypeAnn ->
    compileNotationAndArgs [] NotAssociative (Just 99) "$1 : $0" Nothing args
  CompareRatTensorReduced op ->
    compileApplication
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
  return $ annotate (mempty, Just 100) (quant <+> hsep cBinders <> "," <+> cBody)

bracketArgs :: (MonadRocqCompile m) => Maybe Precedence -> [GenericArg Code] -> m [Code]
bracketArgs maybeParentPrecedence = traverse bracketArg
  where
    bracketArg :: (MonadRocqCompile m) => GenericArg Code -> m Code
    bracketArg arg = do
      let body = argExpr arg
      logDebug MaxDetail $
        "!!!"
          <+> body
          <+> pretty (getPrecedence body)
          <+> ">="
          <+> pretty maybeParentPrecedence
          <+> "="
          <+> pretty (getPrecedence body >= maybeParentPrecedence)
      return $ case visibilityOf arg of
        Instance {} -> annotate (mempty, Nothing) $ braces (braces body)
        Implicit {} -> annotate (mempty, Nothing) $ braces body
        Explicit {} -> case (getPrecedence body, maybeParentPrecedence) of
          (Just argPrecedence, Just parentPrecedence)
            | argPrecedence >= parentPrecedence -> annotate (mempty, Nothing) $ parens body
            | otherwise -> body
          _ -> body

compileIndexLiteral :: Int -> Code
compileIndexLiteral i =
  annotateConstant
    [ MathcompImport Algebra,
      MathcompImport Boot,
      Open RingScope
    ]
    (pretty i)

compileNatLiteral :: Int -> Code
compileNatLiteral i = annotateConstant [MathcompImport Boot] $ pretty i <> "%N"

compileTensorLiteral :: (a -> Code) -> Tensor a -> Code
compileTensorLiteral compileElement t = annotate ([RequireImport VehicleTensor], Nothing) $ case (shapeOf t, toList t) of
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
compileRatLiteral r = parens $ annotate ([MathcompImport Reals, MathcompImport Algebra, Open RingScope], Nothing) rat
  where
    num = pretty $ numerator r
    denom = pretty $ denominator r
    rat = (if denominator r == 1 then num else num <+> "/" <+> denom) <+> ":" <+> "R"

compileLam :: (MonadRocqCompile m) => Binder DecidabilityBuiltin -> Expr DecidabilityBuiltin -> m Code
compileLam binder expr = do
  let (binders, body) = foldLamBinders binder expr
  (cBinders, cBody) <- compileBinders (binder : binders) (compileExpr body)
  return $ annotate (mempty, Just 100) ("fun" <+> hsep cBinders <+> "=>" <+> cBody)

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
        (CIndex, _) -> [MathcompImport Boot]
        (CNat, _) -> [MathcompImport Boot]
        (CRatTensor, Eq) -> [RequireImport VehicleTensor]
        (CRatTensor, Ne) -> [RequireImport VehicleTensor]
        (CRatTensor, _) -> [RequireImport VehicleTensor]
  let (opDoc', dependencies') =
        if domain == CIndex
          then ("$0 " <> opDoc <> " $1 :> nat", dependencies ++ [MathcompImport Boot])
          else ("$0 " <> opDoc <> " $1", dependencies)
  compileNotationAndArgs (dependencies' <> typeDeps) NotAssociative (Just 70) opDoc' Nothing
  where
    orderDeps = [MathcompImport Boot, Open OrderScope]
    eqDeps = [MathcompImport Boot]

compileStack :: (MonadRocqCompile m) => [Arg DecidabilityBuiltin] -> m Code
compileStack args = do
  vecExpr <- toVec args
  return $ annotate ([RequireImport VehicleTensor], functionApplicationPrecedence) $ "nstack_tuple" <+> vecExpr

compileVecLiteral :: (MonadRocqCompile m) => [Arg DecidabilityBuiltin] -> m Code
compileVecLiteral xs = case getExpr accessSpine xs of
  Just (VecLitArgs _t _d ds) -> toVec (fmap explicit ds)
  Nothing -> developerError "Malformed type-checked vector literal"

toVec :: (MonadRocqCompile m) => [Arg DecidabilityBuiltin] -> m Code
toVec xs = do
  let text = layoutAsText $ "[tuple" <+> concatWith (surround "; ") ["$" <> pretty x | x <- [0 .. length xs - 1]] <> "]"
  compileNotationAndArgs [MathcompImport Boot, Open FormScope] NotAssociative Nothing text Nothing xs
