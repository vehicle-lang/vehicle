module Vehicle.Backend.ITP.Imandra
  ( ImandraOptions (..),
    compileProgToImandra,
    writeImandraFile,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (runStateT)
import Control.Monad.State.Class (MonadState, modify)
import Data.Bifunctor (Bifunctor (..))
import Data.Char (isUpper, toLower)
import Data.Foldable (fold)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Real (denominator, numerator)
import Prettyprinter hiding (hcat, hsep, vcat, vsep)
import System.FilePath (takeBaseName)
import Vehicle.Backend.ITP.Core (ComparisonType (..), builtinAppArgs, decideIfPointwiseOrReductionComparison)
import Vehicle.Backend.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Sugar.Binders
import Vehicle.Data.AST.Expr.Scoped ()
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Decidability
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Code.Interface (IsArgs (..), VectorLitArgs (..))
import Vehicle.Data.Real (ExtendedRational (..))
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
-- Imandra-specific options

data ImandraOptions = ImandraOptions
  { output :: Maybe FilePath,
    moduleName :: Maybe String
  }

currentPhase :: Doc ()
currentPhase = "compilation to Imandra"

compileProgToImandra :: (MonadCompile m) => Prog DecidabilityBuiltin -> ImandraOptions -> m (Doc a)
compileProgToImandra (Main ds) options =
  logCompilerSection2 MinDetail currentPhase $ do
    logDebug MaxDetail $ prettyExternal (Main ds)

    -- Collect networks, properties, type defs, and value defs
    ((moduleNets, moduleProps, typeDefsDoc, valueDefsDoc), _) <-
      runStateT
        ( runFreshNameBoundContextT $ do
            moduleNets <- fmap concat (traverse (gatherModuleNetworks options) ds)
            (typeDefsDoc, valueDefsDoc) <- compileProg options moduleNets (Main ds)
            moduleProps <- fmap concat (traverse (gatherModuleProperties options moduleNets) ds)
            return (moduleNets, moduleProps, typeDefsDoc, valueDefsDoc)
        )
        Set.empty

    let programDependencies =
          collectCodeDependencies typeDefsDoc
            `Set.union` collectCodeDependencies valueDefsDoc
            `Set.union` collectModuleDependencies moduleNets
            `Set.union` collectModuleDependencies moduleProps

    let nameOfModule = Text.pack $ case moduleName options of
          Just name -> name
          _ -> maybe "Spec" takeBaseName (output options)

    -- Order: types → networks (opaque) → value definitions → axioms
    let imandraProgram =
          unAnnotate
            ( (vsep2 :: [Code] -> Code)
                [ preamble nameOfModule programDependencies,
                  indent 2 typeDefsDoc,
                  indent 2 (vsep2 (map prettyModuleDef (filter onlyNetworkDefStmt moduleNets))),
                  indent 2 valueDefsDoc,
                  indent 2 (vsep2 (map prettyModuleDef (filter onlyPropertyDefStmt moduleProps))),
                  postamble
                ]
            )

    return imandraProgram

writeImandraFile ::
  (MonadLogger m, MonadIO m, MonadStdIO m) =>
  Maybe FilePath ->
  Doc a ->
  m ()
writeImandraFile = writeResultToFileWide (Just imandraOutputFormat)

imandraOutputFormat :: ExternalOutputFormat
imandraOutputFormat =
  ExternalOutputFormat
    { formatName = "Imandra",
      formatVersion = Nothing,
      commentStyle = Block "(*" "*)",
      emptyLines = True
    }

-- | Collect dependencies from a 'Code' document by discarding precedence
--   and folding all dependency annotations.
collectCodeDependencies :: Code -> Set Dependency
collectCodeDependencies doc = do
  let stream = layoutPretty defaultLayoutOptions doc
  fold (reAnnotateS fst stream)

-- | Collect dependencies arising from module definitions.
collectModuleDependencies :: [ModuleDef] -> Set Dependency
collectModuleDependencies = Set.unions . fmap deps
  where
    deps :: ModuleDef -> Set Dependency
    deps = \case
      NetworkDefStmt _name ty -> collectCodeDependencies ty
      PropertyDefStmt stmt -> collectCodeDependencies stmt

--------------------------------------------------------------------------------
-- Debug functions

logEntry :: (MonadImandraCompile m) => Expr DecidabilityBuiltin -> m ()
logEntry e = do
  incrCallDepth
  ctx <- getNameContext
  logDebug MaxDetail $ "compile-entry" <+> prettyExternal (WithContext e ctx)

logExit :: (MonadImandraCompile m) => Code -> m ()
logExit e = do
  logDebug MaxDetail $ "compile-exit " <+> e
  decrCallDepth

--------------------------------------------------------------------------------
-- Dependencies and module structure

data Dependency
  = RequireImport Library
  deriving (Eq, Ord)

data ModuleDef
  = NetworkDefStmt Code Code
  | PropertyDefStmt Code

prettyModuleDef :: ModuleDef -> Code
prettyModuleDef = \case
  NetworkDefStmt name ty ->
    "let" <+> unAnnotate name <+> ":" <+> unAnnotate ty <+> "= () [@@opaque]"
  PropertyDefStmt stmt -> unAnnotate stmt

instance Pretty Dependency where
  pretty = \case
    RequireImport l -> pretty l

data Library
  = ImlTensor
  | ImlSubtensor
  | ImlAdd
  | ImlScalarMult
  | ImlVehicle
  deriving (Eq, Ord)

instance Pretty Library where
  pretty = \case
    ImlTensor -> "[@@@import \"tensor.iml\"]"
    ImlSubtensor -> "[@@@import \"subtensor.iml\"]"
    ImlAdd -> "[@@@import \"add.iml\"]"
    ImlScalarMult -> "[@@@import \"scalar_mult.iml\"]"
    ImlVehicle -> "[@@@import \"vehicle.iml\"]"

onlyNetworkDefStmt :: ModuleDef -> Bool
onlyNetworkDefStmt = \case
  NetworkDefStmt _ _ -> True
  _ -> False

onlyPropertyDefStmt :: ModuleDef -> Bool
onlyPropertyDefStmt = \case
  PropertyDefStmt _ -> True
  _ -> False

preamble :: Text -> Set Dependency -> Code
preamble modName _deps =
  (vsep2 :: [Code] -> Code)
    [ (vsep :: [Code] -> Code)
        [ "[@@@import \"tensor.iml\"]",
          "[@@@import \"subtensor.iml\"]",
          "[@@@import \"add.iml\"]",
          "[@@@import \"scalar_mult.iml\"]",
          "[@@@import \"vehicle.iml\"]"
        ],
      "open Vehicle",
      "module" <+> pretty modName <+> "= struct",
      indent 2 "type flex_index = int"
    ]

postamble :: Code
postamble = "end"

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

annotateApp :: (MonadImandraCompile m) => [ModuleDef] -> [Dependency] -> Code -> [Arg DecidabilityBuiltin] -> m Code
annotateApp moduleDefs dependencies fun args = do
  (precedence, annDoc) <-
    if null args
      then return (getPrecedence fun, fun)
      else do
        bracketedArgs <- compileArgs moduleDefs 200 args
        return (maxPrecedence, parens $ group $ nest 2 $ fun <> foldMap (\a -> line <> a) bracketedArgs)

  return $ annotate (Set.fromList dependencies, precedence) annDoc

annotateBinOp ::
  (MonadImandraCompile m) =>
  [ModuleDef] ->
  [Dependency] ->
  Precedence ->
  Code ->
  [Arg DecidabilityBuiltin] ->
  m Code
annotateBinOp moduleDefs dependencies precedence op args = do
  bracketedArgs <- compileArgs moduleDefs precedence args
  case bracketedArgs of
    [lhs, rhs] ->
      return $
        annotate (Set.fromList dependencies, maxPrecedence) $
          parens $
            group $
              nest 2 $
                lhs <> line <> op <+> rhs
    _ -> developerError "Binary operator expects exactly 2 arguments"

annotateNotation ::
  (MonadImandraCompile m) =>
  [ModuleDef] ->
  [Dependency] ->
  Precedence ->
  Text ->
  Maybe Text ->
  [Arg DecidabilityBuiltin] ->
  m Code
annotateNotation moduleDefs dependencies precedence op mFn args
  | not (all isExplicit args) = fallback
  | otherwise = do
      bracketedArgs <- compileArgs moduleDefs precedence args
      let doc = insertNotationArgs op bracketedArgs
      maybe fallback (return . annotate (Set.fromList dependencies, precedence)) doc
  where
    fallback = case mFn of
      Just fn -> annotateApp moduleDefs dependencies (pretty fn) args
      Nothing ->
        developerError $
          "Failed to process notation:"
            <+> pretty op
            <+> "with"
            <+> pretty (length args)
            <+> "arguments"

-- | Inserts arguments to notation patterns like "$0 && $1"
insertNotationArgs :: Text -> [Code] -> Maybe Code
insertNotationArgs rawOp as = concatWith (<>) <$> go rawOp
  where
    go :: Text -> Maybe [Code]
    go opText = case Text.break (== '$') opText of
      (_, t) | Text.null t -> Just [pretty opText]
      (prefix, t) -> do
        (_, t') <- Text.uncons t
        (nText, maybeSuffix) <- Text.uncons t'
        let n = fromIntegral (fromEnum nText - fromEnum '0')
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

--------------------------------------------------------------------------------
-- Monad stack

type MonadImandraCompile m =
  ( MonadCompile m,
    MonadNameContext m,
    MonadState (Set Name) m
  )

--------------------------------------------------------------------------------
-- Program Compilation

compileProg :: (MonadImandraCompile m) => ImandraOptions -> [ModuleDef] -> Prog DecidabilityBuiltin -> m (Code, Code)
compileProg opts moduleDefs (Main ds) = do
  let relevant = filter filterRelevantDecls ds
  let (typeDefs, valueDefs) = partition isTypeLevelDecl relevant
  typeDoc <- vsep2 <$> traverse (compileDecl opts moduleDefs) typeDefs
  valueDoc <- vsep2 <$> traverse (compileDecl opts moduleDefs) valueDefs
  return (typeDoc, valueDoc)

isTypeLevelDecl :: Decl DecidabilityBuiltin -> Bool
isTypeLevelDecl = \case
  DefFunction _ _ (TypeDecl _) _ _ -> True
  DefRecord {} -> True
  _ -> False

gatherModuleNetworks :: (MonadImandraCompile m) => ImandraOptions -> Decl DecidabilityBuiltin -> m [ModuleDef]
gatherModuleNetworks _opts = \case
  DefAbstract _ n _ t -> do
    cExpr <- compileExpr False [] t
    pure [compilePostulate (compileIdentifier n) cExpr]
  _ -> pure []

gatherModuleProperties :: (MonadImandraCompile m) => ImandraOptions -> [ModuleDef] -> Decl DecidabilityBuiltin -> m [ModuleDef]
gatherModuleProperties _opts moduleDefs = \case
  DefFunction _ n funSort _ e -> case funSort of
    FunctionDecl _ (Just AnnProperty) -> do
      cExpr <- compilePropertyExpr moduleDefs n e
      pure [PropertyDefStmt cExpr]
    _ -> pure []
  _ -> pure []

compileDecl :: (MonadImandraCompile m) => ImandraOptions -> [ModuleDef] -> Decl DecidabilityBuiltin -> m Code
compileDecl _opts moduleDefs = \case
  DefAbstract _ _ _ _ -> do
    developerError "DefAbstract should have been filtered out"
  DefFunction p n funSort t e -> case funSort of
    TypeDecl binderCount -> compileFunctionDecl moduleDefs n binderCount t e
    FunctionDecl binderCount Nothing -> compileFunctionDecl moduleDefs n binderCount t e
    FunctionDecl _ (Just AnnProperty) -> developerError "Properties should have been filtered out"
    FunctionDecl _ (Just AnnInstance {}) -> throwError $ UnimplementedFeature p "Compiling instances to Imandra"
    ProjectionDecl {} -> developerError "ProjectionDecl should have been filtered out"
    TensorCoercionDecl binderCount -> compileFunctionDecl moduleDefs n binderCount t e
  DefRecord p n _ telescope fields _supports -> compileRecordDecl moduleDefs p n telescope fields

filterRelevantDecls :: Decl DecidabilityBuiltin -> Bool
filterRelevantDecls = \case
  DefAbstract _ _ _ _ -> False
  DefFunction _ _ funSort _ _ -> case funSort of
    FunctionDecl _ (Just AnnProperty) -> False
    ProjectionDecl {} -> False
    _ -> True
  DefRecord {} -> True

compileFunctionDecl ::
  (MonadImandraCompile m) =>
  [ModuleDef] ->
  Identifier ->
  LHSBinderCount ->
  Type DecidabilityBuiltin ->
  Expr DecidabilityBuiltin ->
  m Code
compileFunctionDecl moduleDefs ident binderCount t e = do
  let (binders, body) = extractDeclBinders binderCount t e
  compileFunDef moduleDefs ident t binders body

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

compileRecordDecl ::
  (MonadImandraCompile m) =>
  [ModuleDef] ->
  Provenance ->
  Identifier ->
  Telescope DecidabilityBuiltin ->
  RecordFields DecidabilityBuiltin ->
  m Code
compileRecordDecl moduleDefs p ident telescope fields = do
  if null telescope
    then do
      fs' <- traverseRecordFields (compileExpr False moduleDefs) fields
      modify (Set.fromList (map (nameOf . fst) fs') `Set.union`)
      return $
        "type"
          <+> compileIdentifier ident
          <+> "= {"
          <> line
          <> indent 2 ((vsep :: [Code] -> Code) $ fmap (\(field, fieldType) -> pretty field <+> ":" <+> fieldType <> ";") fs')
          <> line
          <> "}"
    else throwError $ UnimplementedFeature p "Compiling parameterized records to Imandra"

-- | Compile a 'network' declaration as an opaque let binding
compilePostulate :: Code -> Code -> ModuleDef
compilePostulate name t = NetworkDefStmt name t

-- | Compile a property declaration as an axiom.
--   Properties of the form `forall x. body` become `axiom name x = body`.
compilePropertyExpr ::
  (MonadImandraCompile m) =>
  [ModuleDef] ->
  Identifier ->
  Expr DecidabilityBuiltin ->
  m Code
compilePropertyExpr moduleDefs ident expr = do
  let name = compileIdentifier ident
  (binderNames, cBody) <- extractForallBinders moduleDefs expr
  return $ "axiom" <+> name <+> hsep binderNames <+> "=" <> line <> indent 2 cBody

-- | Extract forall binders from a property expression, returning binder names and compiled body.
--   Handles both Pi binders and quantifier builtins (QuantifyRatTensor, PropQuantifyIndex, etc.)
extractForallBinders ::
  (MonadImandraCompile m) =>
  [ModuleDef] ->
  Expr DecidabilityBuiltin ->
  m ([Code], Code)
extractForallBinders moduleDefs = \case
  -- Handle Pi binders (forall at type level)
  Pi _ binder result
    | binderNamingForm binder /= OnlyType -> do
        let binderName = pretty (getBinderName binder)
        (restNames, cBody) <- addNameToContext binder $ extractForallBinders moduleDefs result
        return (binderName : restNames, cBody)
  -- Handle QuantifyRatTensor Forall applied to a lambda
  App (Builtin _ (StandardBuiltinFunction (QuantifyRatTensor Forall))) args
    | [ExplicitArg _ (Lam _ binder body)] <- NonEmpty.filter (not . wasInsertedByCompiler) args -> do
        let binderName = pretty (getBinderName binder)
        (restNames, cBody) <- addNameToContext binder $ extractForallBinders moduleDefs body
        return (binderName : restNames, cBody)
  e -> do
    cBody <- compileExpr False moduleDefs e
    return ([], cBody)

compileFunDef :: (MonadImandraCompile m) => [ModuleDef] -> Identifier -> Expr DecidabilityBuiltin -> [Binder DecidabilityBuiltin] -> Expr DecidabilityBuiltin -> m Code
compileFunDef _moduleDefs name (Universe _ _) _ body = do
  -- Type definition: compile depending on the body shape
  res <- case body of
    App (Builtin _p (StandardBuiltinType TensorType)) [_tensT, _maxIdx] -> do
      -- Tensor type becomes a simple alias
      return $ "type" <+> compileIdentifier name <+> "= real Tensor.tensor"
    App (Builtin _p (StandardBuiltinType IndexType)) [_i] -> do
      -- Index type becomes int
      return $ "type" <+> compileIdentifier name <+> "= int"
    _ -> developerError $ "Only tensor and index types are currently supported for custom type definitions."
  return res
compileFunDef moduleDefs n t binders body = do
  -- Regular function definition: let name (arg : type) ... : returnType = body
  (binderDocs, cbody) <- compileBinders moduleDefs binders (compileExpr False moduleDefs body)
  retType <- resolveReturnType moduleDefs binders t
  let name = compileIdentifier n
  let typedBinders = zipBinderDocs binders binderDocs
  return $
    if null typedBinders
      then "let" <+> name <+> ":" <+> retType <+> "=" <+> cbody
      else
        group $
          "let"
            <+> name
            <+> hsep typedBinders
            <+> ":"
            <+> retType
            <+> "="
            <> nest 2 (line <> cbody)

-- | Generate typed binder docs like (arg : type) for function parameters
zipBinderDocs :: [Binder DecidabilityBuiltin] -> [Code] -> [Code]
zipBinderDocs [] _ = []
zipBinderDocs _ [] = []
zipBinderDocs (b : bs) (d : ds)
  | visibilityOf b /= Explicit = zipBinderDocs bs ds
  | otherwise = d : zipBinderDocs bs ds

resolveReturnType :: (MonadImandraCompile m) => [ModuleDef] -> [Binder DecidabilityBuiltin] -> Expr DecidabilityBuiltin -> m Code
resolveReturnType moduleDefs (_ : bs) (Pi _ binder r) = addNameToContext binder $ resolveReturnType moduleDefs bs r
resolveReturnType moduleDefs _ e = compileExpr True moduleDefs e

idxBasedOp :: (MonadImandraCompile m) => [ModuleDef] -> Code -> [Arg DecidabilityBuiltin] -> m Code
idxBasedOp moduleDefs op args = case args of
  [(ExplicitArg _ (Lam _ binder _body))] -> case (typeOf binder) of
    (App (Builtin _p (StandardBuiltinType IndexType)) [maxIdx]) -> do
      idxArg <- (compileExpr False moduleDefs (argExpr maxIdx))
      annotateApp moduleDefs [RequireImport ImlVehicle] (op <+> idxArg) args
    _ -> developerError $ "foreach/forall/exists tensor operations are currently only supports explicit lambda arguments with indexing type"
  _ -> developerError $ "foreach/forall/exists tensor operations are currently only supports a single lambda argument"

--------------------------------------------------------------------------------
-- Expression Compilation

compileExpr :: (MonadImandraCompile m) => Bool -> [ModuleDef] -> Expr DecidabilityBuiltin -> m Code
compileExpr isOutType moduleDefs expr = do
  logEntry expr
  result <- case expr of
    Hole {} -> resolutionError currentPhase "Hole"
    Meta {} -> resolutionError currentPhase "Meta"
    Universe _ l -> return $ compileType l
    FreeVar _ n -> return $ annotateConstant [] (pretty (toSnakeCase (nameOf n)))
    BoundVar p ix -> do
      n <- ixToProperName p ix
      return $ annotateConstant [] (pretty n)
    Pi _ binder result -> case binderNamingForm binder of
      OnlyType -> do
        cInput <- compileBinder moduleDefs binder
        cOutput <- addNameToContext binder $ compileExpr True moduleDefs result
        return $ annotate ([], 99) $ cInput <+> "->" <+> cOutput
      _ -> do
        let (binders, body) = foldPiBinders binder result
        compileTypeLevelQuantifier moduleDefs Forall (binder :| binders) body
    Let _ bound binder body -> do
      cBoundExpr <- compileLetBinder moduleDefs (binder, bound)
      cBody <- addNameToContext binder $ compileExpr False moduleDefs body
      return $ group $ "let" <+> cBoundExpr <+> "in" <> line <> cBody
    Lam _ binder body -> compileLam moduleDefs binder body
    Builtin _p b -> compileBuiltin isOutType moduleDefs b []
    App fun args -> compileApp isOutType moduleDefs fun args
    Record _p _i fs -> compileRecord moduleDefs fs
    RecordProj _p _t _r _field -> developerError "Record projection should have been eliminated before compilation"
  logExit result
  return result

compileRecord :: (MonadImandraCompile m) => [ModuleDef] -> [GenericRecordField (Expr DecidabilityBuiltin)] -> m Code
compileRecord moduleDefs fs = do
  fs' <- traverse (compileRecordField moduleDefs) fs
  return $ encloseSep ("{" <> space) (space <> "}") (";" <> space) fs'

compileType :: UniverseLevel -> Code
compileType _ = developerError "compilation of higher-level universes to Imandra unsupported"

compileLetBinder ::
  (MonadImandraCompile m) =>
  [ModuleDef] ->
  LetBinder (Expr DecidabilityBuiltin) ->
  m Code
compileLetBinder moduleDefs (binder, expr) = do
  let binderName = pretty (getBinderName binder)
  cExpr <- compileExpr False moduleDefs expr
  return $ binderName <+> "=" <+> cExpr

compileIdentifier :: Identifier -> Code
compileIdentifier ident = pretty (toSnakeCase (nameOf ident :: Name))

-- | Convert a CamelCase name to snake_case for IML compatibility.
--   IML requires type and value names to start with a lowercase letter.
toSnakeCase :: Name -> Text
toSnakeCase = Text.pack . go . Text.unpack
  where
    go [] = []
    go (c : cs)
      | isUpper c = toLower c : insertUnderscores cs
      | otherwise = c : insertUnderscores cs
    insertUnderscores [] = []
    insertUnderscores (c : cs)
      | isUpper c = '_' : toLower c : insertUnderscores cs
      | otherwise = c : insertUnderscores cs

compileBinders :: (MonadImandraCompile m) => [ModuleDef] -> [Binder DecidabilityBuiltin] -> m a -> m ([Code], a)
compileBinders _ [] c = ([],) <$> c
compileBinders moduleDefs (b : bs) c = do
  (cbs, cc) <- addNameToContext b $ compileBinders moduleDefs bs c
  cb <- compileBinder moduleDefs b
  return (cb : cbs, cc)

compileBinder :: (MonadImandraCompile m) => [ModuleDef] -> Binder DecidabilityBuiltin -> m Code
compileBinder moduleDefs binder = do
  binderType <- compileExpr False moduleDefs (typeOf binder)
  case binderNamingForm binder of
    OnlyName name _ -> return $ parens (pretty name <+> ":" <+> binderType)
    OnlyType -> return binderType
    NameAndType name _ -> do
      return $ parens (pretty name <+> ":" <+> binderType)

compileRecordField :: (MonadImandraCompile m) => [ModuleDef] -> GenericRecordField (Expr DecidabilityBuiltin) -> m Code
compileRecordField moduleDefs (field, fieldValue) = do
  fieldValue' <- compileExpr False moduleDefs fieldValue
  return $ pretty field <+> "=" <+> fieldValue'

compileArg :: (MonadImandraCompile m) => [ModuleDef] -> Precedence -> Arg DecidabilityBuiltin -> m Code
compileArg moduleDefs precedence arg = do
  body <- compileExpr False moduleDefs (argExpr arg)
  return $ argBrackets precedence (visibilityOf arg) body

compileArgs :: (MonadImandraCompile m) => [ModuleDef] -> Precedence -> [Arg DecidabilityBuiltin] -> m [Code]
compileArgs moduleDefs precedence = traverse (compileArg moduleDefs precedence)

--------------------------------------------------------------------------------
-- Builtin Compilation

compileBuiltin :: (MonadImandraCompile m) => Bool -> [ModuleDef] -> DecidabilityBuiltin -> [Arg DecidabilityBuiltin] -> m Code
compileBuiltin _isOutType moduleDefs b args = case b of
  StandardBuiltinType t -> case t of
    BoolType -> return "bool"
    RatType -> return $ annotateConstant [] "real"
    UnitType -> return "unit"
    NatType -> return "int"
    ListType -> annotateNotation moduleDefs [] 2 "$0 list" Nothing args
    TensorType ->
      annotateNotation
        moduleDefs
        [RequireImport ImlVehicle]
        0
        "$0 Tensor.tensor"
        Nothing
        args
    IndexType -> annotateNotation moduleDefs [] 0 "int" Nothing args
    VectorType -> annotateNotation moduleDefs [] 2 "$0 list" Nothing args
  StandardBuiltinConstructor c -> case c of
    Nil -> return "[]"
    Cons -> annotateNotation moduleDefs [] 60 "$0 :: $1" (Just "cons") args
    UnitLiteral -> return "()"
    IndexLiteral n -> return $ compileIndexLiteral n
    NatLiteral n -> return $ compileNatLiteral n
    NatTensorLiteral t -> return $ compileTensorLiteral compileNatLiteral t
    BoolTensorLiteral t -> return $ compileTensorLiteral compileBoolLiteral t
    RatTensorLiteral t -> return $ compileTensorLiteral compileRealLiteral t
    VectorLiteral -> compileVecLiteral moduleDefs args
  StandardBuiltinFunction f -> case f of
    And -> annotateBinOp moduleDefs [] 40 "&&" args
    Or -> annotateBinOp moduleDefs [] 50 "||" args
    Not -> annotateNotation moduleDefs [] 35 "(not $0)" (Just "not") args
    Implies -> annotateBinOp moduleDefs [] 55 "==>" args
    Add AddNat -> annotateNotation moduleDefs [] 50 "($0 + $1)" (Just "+") args
    Mul MulNat -> annotateNotation moduleDefs [] 40 "($0 * $1)" (Just "*") args
    Add AddRatTensor -> annotateApp moduleDefs [RequireImport ImlVehicle] "tensor_plus_real" args
    Sub SubRatTensor -> annotateNotation moduleDefs [RequireImport ImlVehicle] 50 "(tensor_plus_real $0 (tensor_cdot (-1.0) $1))" Nothing args
    Mul MulRatTensor -> annotateApp moduleDefs [RequireImport ImlVehicle] "hadamard_prod_real" args
    Div DivRatTensor -> annotateApp moduleDefs [RequireImport ImlVehicle] "pointwise_div_real" args
    Neg NegRatTensor -> annotateApp moduleDefs [RequireImport ImlVehicle] "tensor_cdot (-1.0)" args
    Min MinRatTensor -> annotateApp moduleDefs [RequireImport ImlVehicle] "pointwise_min_real" args
    Max MaxRatTensor -> annotateApp moduleDefs [RequireImport ImlVehicle] "pointwise_max_real" args
    CompareIndex op -> compileComparison moduleDefs CIndex op args
    CompareNat op -> compileComparison moduleDefs CNat op args
    CompareRatTensor op -> case decideIfPointwiseOrReductionComparison args of
      Pointwise as -> compileTensorComparison moduleDefs CRatTensor op as
      Reduced as ->
        annotateApp
          moduleDefs
          [RequireImport ImlVehicle]
          ( case op of
              Le -> "leq_tensor_reduced_real"
              Lt -> "lt_tensor_reduced_real"
              Ge -> "geq_tensor_reduced_real"
              Gt -> "gt_tensor_reduced_real"
              Eq -> "eq_tensor_reduced_real"
              Ne -> "ne_tensor_reduced_real"
          )
          as
    FoldList -> annotateApp moduleDefs [] "List.fold_right" args
    MapList -> annotateApp moduleDefs [] "List.map" args
    ReverseList -> annotateApp moduleDefs [] "List.rev" args
    AppendList {} -> unsupportedError
    ReduceAndTensor -> annotateApp moduleDefs [RequireImport ImlVehicle] "reduce_and" args
    ReduceOrTensor -> annotateApp moduleDefs [RequireImport ImlVehicle] "reduce_or" args
    ReduceAddRatTensor -> annotateApp moduleDefs [RequireImport ImlVehicle] "reduce_sum" args
    ReduceMinRatTensor -> unsupportedError
    ReduceMaxRatTensor -> unsupportedError
    ReduceMulRatTensor -> annotateApp moduleDefs [RequireImport ImlVehicle] "reduce_mul" args
    ConstTensor -> do
      bracketedArgs <- compileArgs moduleDefs 1 args
      return $ annotate ([RequireImport ImlVehicle], maxPrecedence) (parens $ "flextensor_from_vec [" <> pretty (length args) <> "] [" <> concatWith (\x y -> x <> "; " <> y) bracketedArgs <> "]")
    QuantifyRatTensor q -> case reverse args of
      (ExplicitArg _ (Lam _ binder body)) : _ -> compileTypeLevelQuantifier moduleDefs q [binder] body
      _ -> unsupportedArgsError
    QuantifyRecord _ -> unsupportedTensorLikeQuantifier
    AtTensor -> annotateApp moduleDefs [RequireImport ImlVehicle] "flex_subtensor" args
    If -> annotateNotation moduleDefs [] minPrecedence "if $0 then $1 else $2" Nothing args
    ForeachTensor -> idxBasedOp moduleDefs "foreach" args
    StackTensor -> compileStack moduleDefs args
    Transpose -> annotateApp moduleDefs [RequireImport ImlVehicle] "tensor_transpose" args
    AtVector -> annotateApp moduleDefs [] "List.nth" args
    ForeachVector -> idxBasedOp moduleDefs "foreach_tuple" args
    SearchRatTensor {} -> unsupportedError
    Iterate -> unsupportedError
    Pow {} -> unsupportedError
    Log {} -> unsupportedError
    Exp {} -> unsupportedError
  DecidabilityBuiltinFunction f -> case f of
    PropType -> return "bool"
    PropTrue -> return "true"
    PropFalse -> return "false"
    PropNot -> annotateNotation moduleDefs [] 75 "(not $0)" (Just "not") args
    PropAnd -> annotateBinOp moduleDefs [] 80 "&&" args
    PropOr -> annotateBinOp moduleDefs [] 85 "||" args
    PropImplies -> annotateBinOp moduleDefs [] minPrecedence "==>" args
    PropCompareIndex op -> compileComparison moduleDefs CIndex op args
    PropCompareNat op -> compileComparison moduleDefs CNat op args
    PropCompareRatTensor op -> compileTensorComparison moduleDefs CRatTensor op args
    BoolTensorToProp -> monoError
    BoolVectorToProp -> monoError
    PropQuantifyIndex q -> case q of
      Forall -> idxBasedOp moduleDefs "forall_index" args
      Exists -> idxBasedOp moduleDefs "exists_index" args
    PropQuantifyInList q -> case q of
      Forall -> annotateApp moduleDefs [RequireImport ImlVehicle] "forall_in_list" args
      Exists -> annotateApp moduleDefs [RequireImport ImlVehicle] "exists_in_list" args
    PropNaryProduct -> unsupportedError
    PropNaryProductForeach -> unsupportedError
    PropNaryProductAt -> unsupportedError
  DecidabilityBuiltinTypeClass {} -> monoError
  DecidabilityBuiltinTypeClassOp {} -> monoError
  StandardBuiltinDerivedFunction f -> compileDerivedFunction moduleDefs f args
  where
    unsupportedError :: a
    unsupportedError =
      developerError $
        "compilation of builtin" <+> quotePretty b <+> "to Imandra unsupported"

    unsupportedArgsError :: (MonadImandraCompile m) => m a
    unsupportedArgsError = do
      compilerDeveloperError $
        "compilation of"
          <+> quotePretty b
          <+> "with args"
          <+> prettyVerbose args
          <+> "to Imandra unsupported"

    monoError :: a
    monoError =
      developerError $
        "Monomorphisation should have got rid of"
          <+> quotePretty (show b)

compileApp :: (MonadImandraCompile m) => Bool -> [ModuleDef] -> Expr DecidabilityBuiltin -> NonEmpty (Arg DecidabilityBuiltin) -> m Code
compileApp _isOutType moduleDefs fun args = case fun of
  Builtin _p b -> do
    let userArgs = builtinAppArgs b args
    compileBuiltin False moduleDefs b userArgs
  _ -> do
    cFun <- compileExpr False moduleDefs fun
    let userArgs = NonEmpty.filter (not . wasInsertedByCompiler) args
    annotateApp moduleDefs [] cFun userArgs

compileDerivedFunction :: (MonadImandraCompile m) => [ModuleDef] -> DerivedFunction -> [Arg DecidabilityBuiltin] -> m Code
compileDerivedFunction moduleDefs fn args = case fn of
  QuantifyIndex q -> case q of
    Exists -> annotateApp moduleDefs [RequireImport ImlVehicle] "exists_index" args
    Forall -> annotateApp moduleDefs [RequireImport ImlVehicle] "forall_index" args
  QuantifyInList {} -> unsupported
  TypeAnn -> annotateNotation moduleDefs [] minPrecedence "($1 : $0)" Nothing args
  where
    unsupported = developerError $ "Compilation of stdlib function" <+> quotePretty fn <+> "not implemented"

compileTypeLevelQuantifier ::
  (MonadImandraCompile m) =>
  [ModuleDef] ->
  Quantifier ->
  NonEmpty (Binder DecidabilityBuiltin) ->
  Expr DecidabilityBuiltin ->
  m Code
compileTypeLevelQuantifier moduleDefs _q binders body = do
  -- In IML, quantifiers at the type level are not directly expressible.
  -- For properties, they are handled by extractForallBinders.
  -- For expressions, we compile as a lambda (the forall is implicit in axiom context).
  (cBinders, cBody) <- compileBinders moduleDefs (NonEmpty.toList binders) (compileExpr False moduleDefs body)
  return $ parens $ group $ "fun" <+> hsep cBinders <+> "->" <> nest 2 (line <> cBody)

--------------------------------------------------------------------------------
-- Literals

compileIndexLiteral :: Int -> Code
compileIndexLiteral i = annotateConstant [] (pretty i)

compileNatLiteral :: Int -> Code
compileNatLiteral i = annotate ([], maxPrecedence) $ pretty i

compileTensorLiteral :: (a -> Code) -> Tensor a -> Code
compileTensorLiteral compileElement t = annotate ([RequireImport ImlVehicle], maxPrecedence) $ case (shapeOf t, toList t) of
  ([], [x]) -> parens $ "flextensor_from_vec [] [" <+> compileElement x <+> "]"
  _ -> foldMapTensor compileElement toTensor t
  where
    toTensor :: TensorShape -> [Code] -> Code
    toTensor shape values = case shape of
      [] -> parens $ "flextensor_from_vec [" <> pretty (length values) <> "] [" <> concatWith (surround "; ") values <> "]"
      _ -> parens $ "combine_subtensors [" <> concatWith (surround "; ") values <> "]"

compileBoolLiteral :: Bool -> Code
compileBoolLiteral = \case
  True -> "true"
  False -> "false"

compileRealLiteral :: ExtendedRational -> Code
compileRealLiteral = \case
  Finite r -> do
    let num = pretty $ numerator r
    let denom = pretty $ denominator r
    let rat
          | denominator r == 1 = "Real.(" <> num <> ".0)"
          | otherwise = "Real.(" <> num <> ".0 /. " <> denom <> ".0)"
    parens $ annotate ([], minPrecedence) rat
  _ -> developerError "Compiling infinite values to Imandra not supported"

compileLam :: (MonadImandraCompile m) => [ModuleDef] -> Binder DecidabilityBuiltin -> Expr DecidabilityBuiltin -> m Code
compileLam moduleDefs binder expr = do
  let (binders, body) = foldLamBinders binder expr
  (cBinders, cBody) <- compileBinders moduleDefs (binder : binders) (compileExpr False moduleDefs body)
  return $ annotate (mempty, minPrecedence) (group $ "fun" <+> hsep cBinders <+> "->" <> nest 2 (line <> cBody))

--------------------------------------------------------------------------------
-- Comparisons

data ComparisonDomain
  = CIndex
  | CNat
  | CRatTensor
  deriving (Eq)

compileComparison :: (MonadImandraCompile m) => [ModuleDef] -> ComparisonDomain -> ComparisonOp -> [Arg DecidabilityBuiltin] -> m Code
compileComparison moduleDefs _domain op =
  annotateNotation moduleDefs [] 70 opDoc Nothing
  where
    opDoc = case op of
      Le -> "$0 <= $1"
      Lt -> "$0 < $1"
      Ge -> "$0 >= $1"
      Gt -> "$0 > $1"
      Eq -> "$0 = $1"
      Ne -> "$0 <> $1"

compileTensorComparison :: (MonadImandraCompile m) => [ModuleDef] -> ComparisonDomain -> ComparisonOp -> [Arg DecidabilityBuiltin] -> m Code
compileTensorComparison moduleDefs _ op =
  annotateApp moduleDefs [RequireImport ImlVehicle] opDoc
  where
    opDoc = case op of
      Le -> "leq_tensor_reduced_real"
      Lt -> "lt_tensor_reduced_real"
      Ge -> "geq_tensor_reduced_real"
      Gt -> "gt_tensor_reduced_real"
      Eq -> "eq_tensor_reduced_real"
      Ne -> "ne_tensor_reduced_real"

compileStack :: (MonadImandraCompile m) => [ModuleDef] -> [Arg DecidabilityBuiltin] -> m Code
compileStack moduleDefs args = do
  as <- compileArgs moduleDefs minPrecedence args
  return $ annotate ([RequireImport ImlVehicle], maxPrecedence) $ parens $ "combine_subtensors" <+> toVec as

compileVecLiteral :: (MonadImandraCompile m) => [ModuleDef] -> [Arg DecidabilityBuiltin] -> m Code
compileVecLiteral moduleDefs xs = case getExpr accessSpine xs of
  Just (VectorLitArgs _t _d ds) -> toVec <$> traverse (compileExpr False moduleDefs) ds
  Nothing -> developerError "Malformed type-checked vector literal"

toVec :: [Code] -> Code
toVec xs = annotate ([], maxPrecedence) "[" <> concatWith (surround "; ") xs <> "]"
