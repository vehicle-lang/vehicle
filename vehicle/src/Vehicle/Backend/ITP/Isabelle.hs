module Vehicle.Backend.ITP.Isabelle
  ( IsabelleOptions (..),
    compileProgToIsabelle,
    writeIsabelleFile,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Internal.Read qualified as Text.Read
import Data.Version (makeVersion)
import GHC.Real (denominator, numerator)
import Prettyprinter hiding (hcat, hsep, vcat, vsep)
import Prettyprinter.Render.Text (renderStrict)
import System.FilePath (takeBaseName)
import Vehicle.Backend.ITP.Core (ComparisonType (..), builtinAppArgs, decideIfPointwiseOrReductionComparison)
import Vehicle.Backend.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Sugar.Binders
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Decidability
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Code.Interface (IsArgs (..), VectorLitArgs (..))
import Vehicle.Data.Real
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
-- Isabelle-specific options

data IsabelleOptions = IsabelleOptions
  { output :: Maybe FilePath,
    localeName :: Maybe String
  }

currentPhase :: Doc ()
currentPhase = "compilation to Isabelle"

compileProgToIsabelle :: (MonadCompile m) => Prog DecidabilityBuiltin -> IsabelleOptions -> m (Doc a)
compileProgToIsabelle (Main ds) options =
  logCompilerSection2 MinDetail currentPhase $ do
    logDebug MaxDetail $ prettyExternal (Main ds)
    -- Combine the printed documents

    let typedefDeps = typedefDependencies ds

    -- Extract all locale assumptions (not as Doc annotations)
    (localeNets, localeDefs, localeAssms, programDoc) <-
      runFreshNameBoundContextT $ do
        localeNets <- fmap concat (traverse (gatherLocaleNetworks options) ds)
        localeDefs <- fmap concat (traverse (gatherLocaleDefines typedefDeps localeNets) ds)
        programDoc <- compileProg options typedefDeps localeNets (Main ds)
        localeAssms <- fmap concat (traverse (gatherLocaleStatements options localeNets) ds)
        return (localeNets, localeDefs, localeAssms, programDoc)
    let programDependencies =
          collectCodeDependencies programDoc
            `Set.union` collectLocaleDependencies localeNets
            `Set.union` collectLocaleDependencies localeDefs
            `Set.union` collectLocaleDependencies localeAssms

    let nameOfLocale = Text.pack $ case localeName options of
          Just name -> name
          _ -> maybe "Spec" takeBaseName (output options)

    let isabelleProgram =
          unAnnotate
            ( (vsep2 :: [Code] -> Code)
                [ preamble nameOfLocale programDependencies localeAssms,
                  indent 2 programDoc,
                  postamble nameOfLocale (localeNets ++ localeDefs ++ localeAssms)
                ]
            )

    return isabelleProgram

writeIsabelleFile ::
  (MonadLogger m, MonadIO m, MonadStdIO m) =>
  Maybe FilePath ->
  Doc a ->
  m ()
writeIsabelleFile = writeResultToFile (Just isabelleOutputFormat)

isabelleOutputFormat :: ExternalOutputFormat
isabelleOutputFormat =
  ExternalOutputFormat
    { formatName = "Isabelle",
      formatVersion = Just $ makeVersion [2024],
      commentStyle = Block "(*" "*)",
      emptyLines = True
    }

-- | Collect dependencies from a 'Code' document by discarding precedence
--   and folding all dependency annotations.
collectCodeDependencies :: Code -> Set Dependency
collectCodeDependencies doc = do
  let stream = layoutPretty defaultLayoutOptions doc
  fold (reAnnotateS fst stream)

-- | Collect dependencies arising from a locale definition. This ensures
--   that imports required by statements printed in the postamble are
--   included in the preamble import list.
collectLocaleDependencies :: [LocaleDef] -> Set Dependency
collectLocaleDependencies = Set.unions . fmap deps
  where
    deps :: LocaleDef -> Set Dependency
    deps = \case
      NetworkDefStatement name ty -> collectCodeDependencies name `Set.union` collectCodeDependencies ty
      PropertyDefStatement stmt -> collectCodeDependencies stmt
      TensorTypeDefStmt _ shape body -> collectCodeDependencies shape `Set.union` collectCodeDependencies body
      IndexTypeDefStmt _ maxI body -> collectCodeDependencies maxI `Set.union` collectCodeDependencies body
      DefinesFixesStatement name ty -> collectCodeDependencies name `Set.union` collectCodeDependencies ty
      DefinesEqStatement name body ->
        collectCodeDependencies name `Set.union` collectCodeDependencies body

--------------------------------------------------------------------------------
-- Debug functions

logEntry :: (MonadIsabelleCompile m) => Expr DecidabilityBuiltin -> m ()
logEntry e = do
  incrCallDepth
  ctx <- getNameContext
  logDebug MaxDetail $ "compile-entry" <+> prettyExternal (WithContext e ctx)

logExit :: (MonadIsabelleCompile m) => Code -> m ()
logExit e = do
  logDebug MaxDetail $ "compile-exit " <+> e
  decrCallDepth

--------------------------------------------------------------------------------
-- Modules

data Dependency
  = RequireImport Library
  deriving (Eq, Ord)

data LocaleDef
  = NetworkDefStatement Code Code
  | PropertyDefStatement Code
  | TensorTypeDefStmt Identifier Code Code
  | IndexTypeDefStmt Identifier Code Code
  | DefinesFixesStatement Code Code
  | DefinesEqStatement Code Code

instance Pretty LocaleDef where
  pretty = \case
    NetworkDefStatement n t -> ("fixes " <+> name <+> " :: \"" <+> tun <+> "\"")
      where
        name = unAnnotate n
        tun = unAnnotate t
    PropertyDefStatement l -> unAnnotate l
    TensorTypeDefStmt n shape l -> unAnnotate (compileTensorTypeDef n shape l)
    IndexTypeDefStmt n maxI l -> unAnnotate (compileIndexTypeDef n maxI l)
    DefinesFixesStatement n t -> ("fixes " <+> name <+> " :: \"" <+> tun <+> "\"")
      where
        name = unAnnotate n
        tun = unAnnotate t
    DefinesEqStatement n body ->
      "defines " <+> unAnnotate n <> "_def: \"" <+> unAnnotate n <+> "\\<equiv>" <+> unAnnotate body <+> "\""

instance Pretty Dependency where
  pretty = \case
    RequireImport l -> pretty l

data Library
  = VehicleTensor
  | VehicleTensorSubtensor
  | VehicleTensorScalarMult
  | VehicleUtils
  deriving (Eq, Ord)

instance Pretty Library where
  pretty = \case
    VehicleTensor -> "\"Deep_Learning.Tensor\""
    VehicleTensorSubtensor -> "\"Deep_Learning.Tensor_Subtensor\""
    VehicleTensorScalarMult -> "\"Deep_Learning.Tensor_Scalar_Mult\""
    VehicleUtils -> "\"Vehicle.Vehicle\""

onlyNetworkDef :: LocaleDef -> Bool
onlyNetworkDef = \case
  NetworkDefStatement _ _ -> True
  _ -> False

onlyPropertyDef :: LocaleDef -> Bool
onlyPropertyDef = \case
  PropertyDefStatement _ -> True
  _ -> False

onlyTypeDef :: LocaleDef -> Bool
onlyTypeDef = \case
  TensorTypeDefStmt _ _ _ -> True
  IndexTypeDefStmt _ _ _ -> True
  _ -> False

onlyDefinesFixes :: LocaleDef -> Bool
onlyDefinesFixes = \case
  DefinesFixesStatement _ _ -> True
  _ -> False

onlyDefinesEq :: LocaleDef -> Bool
onlyDefinesEq = \case
  DefinesEqStatement _ _ -> True
  _ -> False

preamble :: Text -> Set Dependency -> [LocaleDef] -> Code
preamble locale deps localeAssms =
  (vsep2 :: [Code] -> Code)
    [ ("theory " <+> pretty locale),
      ("  imports"),
      ("    \"Complex_Main\""),
      indent 4 (vsep (map pretty (Set.toList deps))),
      "begin",
      (indent 2 "type_synonym R = \"real\""),
      (indent 2 (vsep (map pretty (filter onlyTypeDef localeAssms))))
    ]

postamble :: Text -> [LocaleDef] -> Code
postamble locale localeAssms =
  (vsep2 :: [Code] -> Code) $
    concat @[]
      [ [("  locale " <+> pretty locale <+> " = ")],
        section onlyNetworkDef,
        section onlyDefinesFixes,
        section onlyDefinesEq,
        section onlyPropertyDef,
        [indent 4 "begin", indent 4 "end", "end"]
      ]
  where
    section :: (LocaleDef -> Bool) -> [Code]
    section p =
      let picked = filter p localeAssms
       in if null picked then [] else [indent 4 (vsep (map pretty picked))]

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

annotateApp :: (MonadIsabelleCompile m) => [LocaleDef] -> [Dependency] -> Code -> [Arg DecidabilityBuiltin] -> m Code
annotateApp localeAssms dependencies fun args = do
  (precedence, annDoc) <-
    if null args
      then return (getPrecedence fun, fun)
      else do
        let precedence = 200
        bracketedArgs <- compileArgs localeAssms precedence args
        return (precedence, parens $ hsep (fun : bracketedArgs))

  return $ annotate (Set.fromList dependencies, precedence) annDoc

annotateNotation ::
  (MonadIsabelleCompile m) =>
  [LocaleDef] ->
  [Dependency] ->
  Precedence ->
  Text ->
  Maybe Text ->
  [Arg DecidabilityBuiltin] ->
  m Code
annotateNotation localeAssms dependencies precedence op mFn args
  | not (all isExplicit args) = fallback
  | otherwise = do
      bracketedArgs <- compileArgs localeAssms precedence args
      let doc = insertNotationArgs op bracketedArgs
      maybe fallback (return . annotate (Set.fromList dependencies, precedence)) doc
  where
    fallback = case mFn of
      Just fn -> annotateApp localeAssms dependencies (pretty fn) args
      Nothing ->
        developerError $
          "Failed to process notation:"
            <+> pretty op
            <+> "with"
            <+> pretty (length args)
            <+> "arguments"

-- | Inserts arguments to Isabelle-style notation
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

type MonadIsabelleCompile m =
  ( MonadCompile m,
    MonadNameContext m
  )

--------------------------------------------------------------------------------
-- Typedef-dependency classifier

typedefDependencies :: [Decl DecidabilityBuiltin] -> Set Identifier
typedefDependencies ds = fixpoint (foldMap seedFor ds)
  where
    seedFor :: Decl DecidabilityBuiltin -> Set Identifier
    seedFor = \case
      DefFunction _ _ (TypeDecl _) _ e -> freeVarsIn e
      _ -> Set.empty

    functionBodies :: [(Identifier, Expr DecidabilityBuiltin)]
    functionBodies = [(identifierOf d, e) | d@(DefFunction _ _ _ _ e) <- ds]

    reachable :: Set Identifier -> (Identifier, Expr DecidabilityBuiltin) -> Set Identifier
    reachable seen (n, e)
      | n `Set.member` seen = freeVarsIn e
      | otherwise = Set.empty

    step :: Set Identifier -> Set Identifier
    step seen = seen `Set.union` foldMap (reachable seen) functionBodies

    fixpoint :: Set Identifier -> Set Identifier
    fixpoint seen = do
      let seen' = step seen
      if seen' == seen then seen else fixpoint seen'

--------------------------------------------------------------------------------
-- Program Compilation

compileProg ::
  (MonadIsabelleCompile m) =>
  IsabelleOptions ->
  Set Identifier ->
  [LocaleDef] ->
  Prog DecidabilityBuiltin ->
  m Code
compileProg opts typedefDeps localeAssms (Main ds) =
  vsep2 <$> traverse (compileDecl opts localeAssms) (filter (filterRelevantDecls typedefDeps) ds)

gatherLocaleNetworks :: (MonadIsabelleCompile m) => IsabelleOptions -> Decl DecidabilityBuiltin -> m [LocaleDef]
gatherLocaleNetworks _opts = \case
  DefAbstract _ n _ t -> do
    cExpr <- compileExpr False [] t
    pure [(compilePostulate (compileIdentifier n) cExpr)]
  _ -> pure []

gatherLocaleStatements :: (MonadIsabelleCompile m) => IsabelleOptions -> [LocaleDef] -> Decl DecidabilityBuiltin -> m [LocaleDef]
gatherLocaleStatements _opts localeNets = \case
  DefFunction _ n funSort _ e -> case funSort of
    FunctionDecl _ (Just AnnProperty) -> do
      cExpr <- compileExpr False localeNets e
      pure [(compileProperty (compileIdentifier n) cExpr)]
    _ -> pure []
  _ -> pure []

gatherLocaleDefines ::
  forall m.
  (MonadIsabelleCompile m) =>
  Set Identifier ->
  [LocaleDef] ->
  Decl DecidabilityBuiltin ->
  m [LocaleDef]
gatherLocaleDefines typedefDeps localeNets = \case
  DefFunction _ n funSort t e
    | n `Set.member` typedefDeps -> pure []
    | otherwise -> case funSort of
        FunctionDecl binderCount Nothing -> emit binderCount
        TensorCoercionDecl binderCount -> emit binderCount
        _ -> pure []
    where
      emit :: LHSBinderCount -> m [LocaleDef]
      emit binderCount = do
        let (binders, body) = extractDeclBinders binderCount t e
        bindersT <- compileTopLevelBinders compileTopLevelBinderT localeNets binders
        bindersV <- compileTopLevelBinders compileTopLevelBinderV localeNets binders
        defType <- resolveReturnType localeNets bindersT t
        (_, cBody) <- compileBinders localeNets binders (compileExpr False localeNets body)
        let cType
              | null bindersT = defType
              | otherwise = concatWith (\x y -> x <> " \\<Rightarrow> " <> y) bindersT <> " \\<Rightarrow> " <> defType
        -- Wrap the body in a lambda rather than emitting the equational
        -- form `name x \<equiv> body`: Isabelle's coercion inserter can
        -- rewrite the LHS `x` and produce `Bad arguments on lhs`.
        let cRhs
              | null bindersV = cBody
              | otherwise = "\\<lambda>" <+> hsep bindersV <+> ". " <> cBody
        let name = compileIdentifier n
        pure
          [ DefinesFixesStatement name cType,
            DefinesEqStatement name cRhs
          ]
  _ -> pure []

compileDecl :: (MonadIsabelleCompile m) => IsabelleOptions -> [LocaleDef] -> Decl DecidabilityBuiltin -> m Code
compileDecl _opts localeAssms = \case
  DefAbstract _ _ _ _ -> do
    developerError "DefAbstract should have been filtered out"
  DefFunction p n funSort t e -> case funSort of
    TypeDecl binderCount -> compileFunctionDecl localeAssms n binderCount t e
    FunctionDecl binderCount Nothing -> compileTermDef localeAssms n binderCount t e
    FunctionDecl _ (Just AnnProperty) -> developerError "Properties should have been filtered out"
    FunctionDecl _ (Just AnnInstance {}) -> throwError $ UnimplementedFeature p "Compiling instances to Isabelle"
    ProjectionDecl {} -> developerError "ProjectionDecl should have been filtered out"
    TensorCoercionDecl binderCount -> compileTermDef localeAssms n binderCount t e
  DefRecord p n _ telescope fields _supports -> compileRecordDecl localeAssms p n telescope fields

filterRelevantDecls :: Set Identifier -> Decl DecidabilityBuiltin -> Bool
filterRelevantDecls typedefDeps = \case
  DefAbstract _ _ _ _ -> False
  DefFunction _ n funSort _ _ -> case funSort of
    TypeDecl {} -> True
    FunctionDecl _ Nothing -> n `Set.member` typedefDeps
    TensorCoercionDecl _ -> n `Set.member` typedefDeps
    _ -> False
  DefRecord {} -> True

compileFunctionDecl ::
  (MonadIsabelleCompile m) =>
  [LocaleDef] ->
  Identifier ->
  LHSBinderCount ->
  Type DecidabilityBuiltin ->
  Expr DecidabilityBuiltin ->
  m Code
compileFunctionDecl localeAssms ident binderCount t e = do
  let (binders, body) = extractDeclBinders binderCount t e
  compileTypeDef localeAssms ident t binders body

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

compileRecordDecl ::
  (MonadIsabelleCompile m) =>
  [LocaleDef] ->
  Provenance ->
  Identifier ->
  Telescope DecidabilityBuiltin ->
  RecordFields DecidabilityBuiltin ->
  m Code
compileRecordDecl localeAssms p ident telescope fields = do
  if null telescope
    then do
      fs' <- traverseRecordFields (compileExpr False localeAssms) fields
      return $
        "record"
          <+> compileIdentifier ident
          <+> "="
          <> line
          <> indent 2 ((vsep :: [Code] -> Code) $ fmap (\(field, fieldType) -> pretty field <+> "::" <+> "\"" <> fieldType <> "\"") fs')
    else throwError $ UnimplementedFeature p "Compiling parameterized records to Isabelle"

-- | Compile a 'network' declaration
compilePostulate :: Code -> Code -> LocaleDef
compilePostulate name t = (NetworkDefStatement name t)

compileExprUnfoldings :: Expr DecidabilityBuiltin -> [Code]
compileExprUnfoldings expr = case expr of
  App fun args -> concatMap compileExprUnfoldings (fun : (fmap argExpr (NonEmpty.toList args)))
  Lam _ binder body -> compileExprUnfoldings (typeOf binder) ++ compileExprUnfoldings body
  Pi _ binder result -> compileExprUnfoldings (typeOf binder) ++ compileExprUnfoldings result
  Let _ bound binder body -> compileExprUnfoldings bound ++ compileExprUnfoldings (typeOf binder) ++ compileExprUnfoldings body
  Record _ _ fs -> concatMap (compileExprUnfoldings . snd) fs
  RecordProj _ _ r _ -> compileExprUnfoldings r
  Builtin _ _ -> []
  FreeVar _ n -> ["unfolding " <> pretty (nameOf n) <> "_def"]
  BoundVar _ _ -> []
  Universe _ _ -> []
  Meta _ _ -> []
  Hole _ _ -> []

compileExpr :: (MonadIsabelleCompile m) => Bool -> [LocaleDef] -> Expr DecidabilityBuiltin -> m Code
compileExpr isOutType localeAssms expr = do
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
        cInput <- compileBinder localeAssms binder
        cOutput <- addNameToContext binder $ compileExpr True localeAssms result
        return $ annotate ([], 99) $ cInput <+> "\\<Rightarrow>" <+> cOutput
      _ -> do
        let (binders, body) = foldPiBinders binder result
        compileTypeLevelQuantifier localeAssms Forall (binder :| binders) body
    Let _ bound binder body -> do
      cBoundExpr <- compileLetBinder localeAssms (binder, bound)
      cBody <- addNameToContext binder $ compileExpr False localeAssms body
      return $ "let" <+> cBoundExpr <+> "in" <+> cBody
    Lam _ binder body -> compileLam localeAssms binder body
    Builtin _p b -> compileBuiltin isOutType localeAssms b []
    App fun args -> compileApp isOutType localeAssms fun args
    Record _p _i fs -> compileRecord localeAssms fs
    RecordProj _p _t _r _field -> developerError "Record projection should have been eliminated before compilation"
  logExit result
  return result

compileRecord :: (MonadIsabelleCompile m) => [LocaleDef] -> [GenericRecordField (Expr DecidabilityBuiltin)] -> m Code
compileRecord localeAssms fs = do
  fs' <- traverse (compileRecordField localeAssms) fs
  return $ encloseSep ("\\<lparr>" <> space) (space <> "\\<rparr>") ("," <> space) fs'

compileType :: UniverseLevel -> Code
compileType _ = developerError "compilation of higher-level universes to Isabelle unsupported"

compileLetBinder ::
  (MonadIsabelleCompile m) =>
  [LocaleDef] ->
  LetBinder (Expr DecidabilityBuiltin) ->
  m Code
compileLetBinder localeAssms (binder, expr) = do
  let binderName = pretty (getBinderName binder)
  cExpr <- compileExpr False localeAssms expr
  return $ binderName <+> "=" <+> cExpr

compileIdentifier :: Identifier -> Code
compileIdentifier ident = pretty (nameOf ident :: Name)

compileProperty :: Code -> Code -> LocaleDef
compileProperty propertyName propertyBody = (PropertyDefStatement codeSnippet)
  where
    codeSnippet = ("assumes " <+> propertyName <+> ":  \"" <+> propertyBody <+> "\"")

compileTopLevelBinders ::
  (MonadIsabelleCompile m) =>
  ([LocaleDef] -> Binder DecidabilityBuiltin -> m (Maybe Code)) ->
  [LocaleDef] ->
  [Binder DecidabilityBuiltin] ->
  m [Code]
compileTopLevelBinders _ _ [] = return []
compileTopLevelBinders compileTopLevelBinder localeAssms (b : bs) = do
  b' <- compileTopLevelBinder localeAssms b
  addNameToContext b $ case b' of
    Nothing -> compileTopLevelBinders compileTopLevelBinder localeAssms bs
    Just bc -> do
      bsc <- compileTopLevelBinders compileTopLevelBinder localeAssms bs
      return $ bc : bsc

compileTopLevelBinderT :: (MonadIsabelleCompile m) => [LocaleDef] -> Binder DecidabilityBuiltin -> m (Maybe Code)
compileTopLevelBinderT localeAssms binder
  | visibilityOf binder /= Explicit = pure Nothing
  | otherwise = do
      binderType <- compileExpr False localeAssms (typeOf binder)
      pure . Just . parens $ binderType

compileTopLevelBinderV :: (MonadIsabelleCompile m) => [LocaleDef] -> Binder DecidabilityBuiltin -> m (Maybe Code)
compileTopLevelBinderV _localeAssms binder
  | visibilityOf binder /= Explicit = pure Nothing
  | otherwise = do
      let binderName = pretty (getBinderName binder)
      pure . Just $ binderName

compileBinders :: (MonadIsabelleCompile m) => [LocaleDef] -> [Binder DecidabilityBuiltin] -> m Code -> m ([Code], Code)
compileBinders _ [] c = ([],) <$> c
compileBinders localeAssms (b : bs) c = do
  (cbs, cc) <- addNameToContext b $ compileBinders localeAssms bs c
  cb <- compileBinder localeAssms b
  return (cb : cbs, cc)

compileBinder :: (MonadIsabelleCompile m) => [LocaleDef] -> Binder DecidabilityBuiltin -> m Code
compileBinder localeAssms binder = do
  binderType <- compileExpr False localeAssms (typeOf binder)
  (binderDoc, noExplicitBrackets) <- case binderNamingForm binder of
    OnlyName name _ -> return (pretty name, True)
    OnlyType -> return (binderType, True)
    NameAndType name _ -> do
      let annName = annotate (Set.empty, minPrecedence) (pretty name <+> "::" <+> binderType)
      return (annName, False)

  return $ binderBrackets noExplicitBrackets (visibilityOf binder) binderDoc

resolveReturnType :: (MonadIsabelleCompile m) => [LocaleDef] -> [Code] -> Expr DecidabilityBuiltin -> m Code
resolveReturnType localeAssms (_ : bs) (Pi _ binder r) = addNameToContext binder $ resolveReturnType localeAssms bs r
resolveReturnType localeAssms _ e = compileExpr True localeAssms e

compileRecordField :: (MonadIsabelleCompile m) => [LocaleDef] -> GenericRecordField (Expr DecidabilityBuiltin) -> m Code
compileRecordField localeAssms (field, fieldValue) = do
  fieldValue' <- compileExpr False localeAssms fieldValue
  return $ pretty field <+> "=" <+> (parens fieldValue')

compileTensorTypeDef :: Identifier -> Code -> Code -> Code
compileTensorTypeDef n shape e =
  ( (vsep :: [Code] -> Code)
      [ ("typedef" <+> (compileIdentifier n) <+> " = " <+> e),
        (compileTensorTypeDefCoercions n shape),
        (compileTypeDefRewrites n shape)
      ]
  )

compileIndexTypeDef :: Identifier -> Code -> Code -> Code
compileIndexTypeDef n maxI e =
  ( (vsep :: [Code] -> Code)
      [ ("typedef" <+> (compileIdentifier n) <+> " = " <+> e),
        (compileIndexTypeDefCoercions n maxI)
      ]
  )

compileTensorTypeDefCoercions :: Identifier -> Code -> Code
compileTensorTypeDefCoercions n shape =
  ( (vsep :: [Code] -> Code)
      [ ("(* Type Coercions *)"),
        ("declare [[coercion Rep_" <> (compileIdentifier n) <> "]]"),
        ("definition to_" <> (compileIdentifier n) <> " :: \"R FlexTensor \\<Rightarrow> " <> compileIdentifier n <> "\""),
        (indent 2 $ "where[simp]: \"to_" <> compileIdentifier n <> " a = ("),
        (indent 4 $ "let t = Rep_FlexTensor a"),
        (indent 4 $ "in (if dims t = (" <> shape <> ") then Abs_" <> compileIdentifier n <> " t else undefined))\""),
        ("declare [[coercion to_" <> compileIdentifier n <> "]]")
      ]
  )

compileIndexTypeDefCoercions :: Identifier -> Code -> Code
compileIndexTypeDefCoercions n maxI =
  ( (vsep :: [Code] -> Code)
      [ ("(* Type Coercions *)"),
        ("declare [[coercion Rep_" <> (compileIdentifier n) <> "]]"),
        ("definition to_" <> (compileIdentifier n) <> " :: \"FlexIndex \\<Rightarrow> " <> compileIdentifier n <> "\""),
        (indent 2 $ "where[simp]: \"to_" <> compileIdentifier n <> " a = ("),
        (indent 4 $ "let i = Rep_FlexIndex a"),
        (indent 4 $ "in (if i < " <> maxI <> " then Abs_" <> compileIdentifier n <> " i else undefined))\""),
        ("declare [[coercion to_" <> compileIdentifier n <> "]]")
      ]
  )

compileTypeDefRewrites :: Identifier -> Code -> Code
compileTypeDefRewrites n shape =
  ( (vsep :: [Code] -> Code)
      [ ("(* Type Rewrite Rules *)"),
        ("lemma " <> compileIdentifier n <> "_tensor_rewrite0[simp]:"),
        (indent 2 $ "assumes \"prod_list shape = length elems\""),
        (indent 2 $ "    and \"shape = " <> shape <> "\""),
        (indent 2 $ "shows \"(Rep_tensor (Rep_" <> compileIdentifier n <> " (Abs_" <> compileIdentifier n <> " (Abs_tensor (shape,elems))))) =  (shape,elems)\""),
        ("proof -"),
        (indent 2 $ "have \"Rep_" <> compileIdentifier n <> " (Abs_" <> compileIdentifier n <> " (Abs_tensor (shape,elems)))"),
        (indent 4 $ "= Abs_tensor (shape,elems)\""),
        (indent 4 $ "  using Abs_" <> compileIdentifier n <> "_inverse[of \"Abs_tensor (shape,elems)\"]"),
        (indent 4 $ "  using Abs_tensor_inverse[of \"(shape, elems)\"]"),
        (indent 4 $ "  unfolding dims_def"),
        (indent 4 $ "  using assms"),
        (indent 4 $ "  by (simp)"),
        (indent 2 $ "moreover have \"Rep_tensor (Abs_tensor (shape,elems)) = (shape,elems)\""),
        (indent 4 $ "  using assms"),
        (indent 4 $ "  by (simp add: Abs_tensor_inverse)"),
        (indent 2 $ "ultimately show ?thesis by simp"),
        ("qed")
      ]
  )

compileTypeDef :: (MonadIsabelleCompile m) => [LocaleDef] -> Identifier -> Expr DecidabilityBuiltin -> [Binder DecidabilityBuiltin] -> Expr DecidabilityBuiltin -> m Code
compileTypeDef localeAssms name (Universe _ _) _ body = case body of
  App (Builtin _p (StandardBuiltinType TensorType)) [tensT, maxIdx] -> do
    cbody <-
      annotateNotation
        localeAssms
        [RequireImport VehicleTensor]
        0
        ( "\"{ a :: $0 tensor. (dims a) = ($1) }\"\n"
            <> "  using dims_tensor_from_lookup by blast\n"
        )
        Nothing
        [tensT, maxIdx]
    shape <- compileExpr False localeAssms (argExpr maxIdx)
    return $ compileTensorTypeDef name shape cbody
  App (Builtin _p (StandardBuiltinType IndexType)) [i] -> do
    let unfoldings = compileExprUnfoldings (argExpr i)
    let unfoldingsText
          | null unfoldings = ""
          | otherwise = renderStrict (layoutCompact (vsep unfoldings)) <> "\n"
    cbody <-
      annotateNotation
        localeAssms
        [RequireImport VehicleUtils]
        0
        ( "\"{ i :: nat. i < ($0) }\"\n"
            <> unfoldingsText
            <> "  by (simp, rule_tac x = \"0\" in exI, linarith)\n"
        )
        Nothing
        [i]
    maxI <- compileExpr False localeAssms (argExpr i)
    return $ compileIndexTypeDef name maxI cbody
  _ -> developerError "Only tensor and index types are currently supported for custom type definitions."
compileTypeDef _ n _ _ _ = developerError $ "compileTypeDef reached with a non-Universe body:" <+> pretty (nameOf n :: Name)

compileTermDef ::
  (MonadIsabelleCompile m) =>
  [LocaleDef] ->
  Identifier ->
  LHSBinderCount ->
  Type DecidabilityBuiltin ->
  Expr DecidabilityBuiltin ->
  m Code
compileTermDef localeAssms n binderCount t e = do
  let (binders, body) = extractDeclBinders binderCount t e
  bindersT <- compileTopLevelBinders compileTopLevelBinderT localeAssms binders
  bindersV <- compileTopLevelBinders compileTopLevelBinderV localeAssms binders
  defType <- resolveReturnType localeAssms bindersT t
  (_, cbody) <- compileBinders localeAssms binders (compileExpr False localeAssms body)
  let name = compileIdentifier n
  pure
    ( "definition"
        <+> name
        <+> " :: \""
        <+> (if null bindersT then mempty else concatWith (\x y -> x <> " \\<Rightarrow> " <> y) bindersT <> " \\<Rightarrow> ")
        <+> align defType
        <+> "\"\n  where \""
        <+> name
        <+> (if null bindersV then mempty else " ")
        <+> hsep bindersV
        <+> "="
        <+> "("
        <+> cbody
        <+> ") \""
    )

idxBasedOp :: (MonadIsabelleCompile m) => [LocaleDef] -> Code -> [Arg DecidabilityBuiltin] -> m Code
idxBasedOp localeAssms op args = case args of
  [(ExplicitArg _ (Lam _ binder _body))] -> case (typeOf binder) of
    (App (Builtin _p (StandardBuiltinType IndexType)) [maxIdx]) -> do
      idxArg <- (compileExpr False localeAssms (argExpr maxIdx))
      annotateApp localeAssms [RequireImport VehicleTensor] (op <+> idxArg <> " ") args
    _ -> developerError $ "foreach/forall/exists tensor operations are currently only supports explicit lambda arguments with indexing type"
  _ -> developerError $ "foreach/forall/exists tensor operations are currently only supports a single lambda argument"

compileBuiltin :: (MonadIsabelleCompile m) => Bool -> [LocaleDef] -> DecidabilityBuiltin -> [Arg DecidabilityBuiltin] -> m Code
compileBuiltin isOutType localeAssms b args = case b of
  StandardBuiltinType t -> case t of
    BoolType -> return "bool"
    -- For the Isabelle backend, rationals are promoted to reals
    RatType -> return $ annotateConstant [] "R"
    UnitType -> return "unit"
    NatType -> return "nat"
    ListType -> annotateNotation localeAssms [] 2 "$0 list" Nothing args
    TensorType ->
      annotateNotation
        localeAssms
        [RequireImport VehicleTensor, RequireImport VehicleUtils]
        0
        ("$0 " <> (if isOutType then "FlexTensor" else "tensor"))
        Nothing
        args
    IndexType -> annotateNotation localeAssms [] 0 (if isOutType then "FlexIndex" else "nat") (Just "ordinal") args
    VectorType -> annotateNotation localeAssms [] 2 "$0 list" Nothing args
  StandardBuiltinConstructor c -> case c of
    Nil -> return "[]"
    Cons -> annotateNotation localeAssms [] 60 "$0 # $1" (Just "cons") args
    UnitLiteral -> return "tt"
    IndexLiteral n -> return $ compileIndexLiteral n
    NatLiteral n -> return $ compileNatLiteral n
    NatTensorLiteral t -> return $ compileTensorLiteral compileNatLiteral t
    BoolTensorLiteral t -> return $ compileTensorLiteral compileBoolLiteral t
    RatTensorLiteral t -> return $ compileTensorLiteral compileRealLiteral t
    VectorLiteral -> compileVecLiteral localeAssms args
  StandardBuiltinFunction f -> case f of
    And -> annotateNotation localeAssms [] 40 "($0 \\<and> $1)" (Just "andb") args
    Or -> annotateNotation localeAssms [] 50 "($0 \\<or> $1)" (Just "orb") args
    Not -> annotateNotation localeAssms [] 35 "(\\<not> $0)" (Just "negb") args
    Implies -> annotateNotation localeAssms [] 55 "($0 \\<longrightarrow> $1)" (Just "implb") args
    Add AddNat -> annotateNotation localeAssms [] 50 "($0 + $1)" (Just "+%R") args
    Mul MulNat -> annotateNotation localeAssms [] 40 "($0 * $1)" (Just "*%R") args
    Add AddRatTensor -> annotateNotation localeAssms [RequireImport VehicleTensor, RequireImport VehicleUtils] 50 "(tensor_plus $0 $1)" (Just "+%R") args
    Sub SubRatTensor -> annotateNotation localeAssms [RequireImport VehicleTensor, RequireImport VehicleUtils, RequireImport VehicleTensorScalarMult] 50 "(tensor_plus $0 (tensor_cdot (-1 :: R) $1))" Nothing args
    Mul MulRatTensor -> annotateNotation localeAssms [RequireImport VehicleTensor, RequireImport VehicleUtils] 40 "(hadamard_prod $0 $1)" (Just "*%R") args
    Div DivRatTensor -> annotateNotation localeAssms [RequireImport VehicleTensor, RequireImport VehicleUtils] 40 "(pointwise_div $0 $1)" Nothing args
    Neg NegRatTensor -> annotateNotation localeAssms [RequireImport VehicleTensor, RequireImport VehicleTensorScalarMult] 80 "(tensor_cdot (-1 :: R) $0)" (Just "-%R") args
    Min MinRatTensor -> annotateApp localeAssms [RequireImport VehicleTensor, RequireImport VehicleUtils] "pointwise_min" args
    Max MaxRatTensor -> annotateApp localeAssms [RequireImport VehicleTensor, RequireImport VehicleUtils] "pointwise_max" args
    CompareIndex op -> compileComparison localeAssms CIndex op args
    CompareNat op -> compileComparison localeAssms CNat op args
    CompareRatTensor op -> case decideIfPointwiseOrReductionComparison args of
      Pointwise as -> compileTensorComparison localeAssms CRatTensor op as
      Reduced as ->
        annotateApp
          localeAssms
          [RequireImport VehicleUtils]
          ( case op of
              Le -> "leRatTensorReduced"
              Lt -> "ltRatTensorReduced"
              Ge -> "geRatTensorReduced"
              Gt -> "gtRatTensorReduced"
              Eq -> "eqRatTensorReduced"
              Ne -> "neRatTensorReduced"
          )
          as
    FoldList -> annotateApp localeAssms [] "foldr" args
    MapList -> annotateApp localeAssms [] "map" args
    ReverseList -> annotateApp localeAssms [] "rev" args
    AppendList {} -> unsupportedError
    Transpose -> annotateApp localeAssms [RequireImport VehicleTensor] "tensor_transpose" args
    ReduceAndTensor -> annotateApp localeAssms [RequireImport VehicleUtils] "reduceAnd" args
    ReduceOrTensor -> annotateApp localeAssms [RequireImport VehicleUtils] "reduceOr" args
    ReduceAddRatTensor -> annotateApp localeAssms [] "reduceAdd" args
    ReduceMinRatTensor -> unsupportedError
    ReduceMaxRatTensor -> unsupportedError
    ReduceMulRatTensor -> annotateApp localeAssms [] "reduceMul" args
    ConstTensor -> do
      bracketedArgs <- compileArgs localeAssms 1 args
      return $ annotate ([RequireImport VehicleTensor], 1) (parens ("flextensor_from_vec [" <> (pretty (length args)) <> "] [" <> concatWith (\x y -> x <> ", " <> y) bracketedArgs) <> "]")
    QuantifyRatTensor q -> case reverse args of
      (ExplicitArg _ (Lam _ binder body)) : _ -> compileTypeLevelQuantifier localeAssms q [binder] body
      _ -> unsupportedArgsError
    QuantifyRecord _ -> unsupportedTensorLikeQuantifier
    AtTensor -> annotateNotation localeAssms [RequireImport VehicleTensor, RequireImport VehicleTensorSubtensor, RequireImport VehicleUtils] 201 "(flex_subtensor $0 $1)" (Just "nindex") args
    If -> annotateNotation localeAssms [] minPrecedence "if $0 then $1 else $2" Nothing args
    ForeachTensor -> idxBasedOp localeAssms "foreach" args
    StackTensor -> compileStack localeAssms args
    AtVector -> annotateApp localeAssms [] "tnth" args
    ForeachVector -> idxBasedOp localeAssms "foreachTuple" args
    SearchRatTensor {} -> unsupportedError
    Iterate -> unsupportedError
    Pow {} -> unsupportedError
    Log {} -> unsupportedError
    Exp {} -> unsupportedError
  DecidabilityBuiltinFunction f -> case f of
    PropType -> return "bool"
    PropTrue -> return "True"
    PropFalse -> return "False"
    PropNot -> annotateNotation localeAssms [] 75 "\\<not> $0" (Just "not") args
    PropAnd -> annotateNotation localeAssms [] 80 "$0 \\<and> $1" (Just "and") args
    PropOr -> annotateNotation localeAssms [] 85 "$0 \\<or> $1" (Just "or") args
    PropImplies -> annotateNotation localeAssms [] minPrecedence "$0 \\<longrightarrow> $1" (Just "implies") args
    PropCompareIndex op -> compileComparison localeAssms CIndex op args
    PropCompareNat op -> compileComparison localeAssms CNat op args
    PropCompareRatTensor op -> compileTensorComparison localeAssms CRatTensor op args
    BoolTensorToProp -> monoError
    BoolVectorToProp -> monoError
    PropQuantifyIndex q -> case q of
      Forall -> idxBasedOp localeAssms "forallIndex" args
      Exists -> idxBasedOp localeAssms "existsIndex" args
    PropQuantifyInList q -> case q of
      Forall -> annotateApp localeAssms [RequireImport VehicleUtils] "forallInList" args
      Exists -> annotateApp localeAssms [RequireImport VehicleUtils] "existsInList" args
    PropNaryProduct -> unsupportedError
    PropNaryProductForeach -> unsupportedError
    PropNaryProductAt -> unsupportedError
  DecidabilityBuiltinTypeClass {} -> monoError
  DecidabilityBuiltinTypeClassOp {} -> monoError
  StandardBuiltinDerivedFunction f -> compileDerivedFunction localeAssms f args
  where
    unsupportedError :: a
    unsupportedError =
      developerError $
        "compilation of builtin" <+> quotePretty b <+> "to Isabelle unsupported"

    unsupportedArgsError :: (MonadIsabelleCompile m) => m a
    unsupportedArgsError = do
      compilerDeveloperError $
        "compilation of"
          <+> quotePretty b
          <+> "with args"
          <+> prettyVerbose args
          <+> "to Isabelle unsupported"

    monoError :: a
    monoError =
      developerError $
        "Monomorphisation should have got rid of"
          <+> quotePretty (show b)

compileApp :: (MonadIsabelleCompile m) => Bool -> [LocaleDef] -> Expr DecidabilityBuiltin -> NonEmpty (Arg DecidabilityBuiltin) -> m Code
compileApp isOutType localeAssms fun args = case fun of
  Builtin _p b -> do
    let userArgs = builtinAppArgs b args
    compileBuiltin isOutType localeAssms b userArgs
  _ -> do
    cFun <- compileExpr False localeAssms fun
    let userArgs = NonEmpty.filter (not . wasInsertedByCompiler) args
    annotateApp localeAssms [] cFun userArgs

compileDerivedFunction :: (MonadIsabelleCompile m) => [LocaleDef] -> DerivedFunction -> [Arg DecidabilityBuiltin] -> m Code
compileDerivedFunction localeAssms fn args = case fn of
  QuantifyIndex q -> case q of
    Exists -> annotateApp localeAssms [RequireImport VehicleUtils] "existsIndex" args
    Forall -> annotateApp localeAssms [RequireImport VehicleUtils] "forallIndex" args
  QuantifyInList {} -> unsupported
  TypeAnn -> annotateNotation localeAssms [] minPrecedence "$1 :: $0" Nothing args
  where
    unsupported = developerError $ "Compilation of stdlib function" <+> quotePretty fn <+> "not implemented"

compileTypeLevelQuantifier ::
  (MonadIsabelleCompile m) =>
  [LocaleDef] ->
  Quantifier ->
  NonEmpty (Binder DecidabilityBuiltin) ->
  Expr DecidabilityBuiltin ->
  m Code
compileTypeLevelQuantifier localeAssms q binders body = do
  (cBinders, cBody) <- compileBinders localeAssms (NonEmpty.toList binders) (compileExpr False localeAssms body)
  quant <- case q of
    Forall -> return "\\<forall>"
    Exists -> return "\\<exists>"
  return $ parens $ quant <+> hsep cBinders <> "." <+> cBody

compileArg :: (MonadIsabelleCompile m) => [LocaleDef] -> Precedence -> Arg DecidabilityBuiltin -> m Code
compileArg localeAssms precedence arg = do
  body <- compileExpr False localeAssms (argExpr arg)
  return $ argBrackets precedence (visibilityOf arg) body

compileArgs :: (MonadIsabelleCompile m) => [LocaleDef] -> Precedence -> [Arg DecidabilityBuiltin] -> m [Code]
compileArgs localeAssms precedence = traverse (compileArg localeAssms precedence)

compileIndexLiteral :: Int -> Code
compileIndexLiteral i =
  annotateConstant
    []
    ("(Abs_FlexIndex " <> pretty i <> ")")

compileNatLiteral :: Int -> Code
compileNatLiteral i = annotate ([], maxPrecedence) $ "(" <> pretty i <> " :: nat)"

compileTensorLiteral :: (a -> Code) -> Tensor a -> Code
compileTensorLiteral compileElement t = annotate ([RequireImport VehicleTensor], 200) $ case (shapeOf t, toList t) of
  ([], [x]) -> parens $ "flextensor_from_vec [] [" <+> compileElement x <+> "]"
  _ -> foldMapTensor compileElement toTensor t
  where
    toTensor :: TensorShape -> [Code] -> Code
    toTensor shape values = case shape of
      [] -> "(flextensor_from_vec [" <> pretty (length values) <> "] [" <> concatWith (surround ", ") values <> "])"
      _ -> "(combine_subtensors [" <> concatWith (surround ", ") values <> "])"

compileBoolLiteral :: Bool -> Code
compileBoolLiteral = \case
  True -> "True"
  False -> "False"

compileRealLiteral :: ExtendedRational -> Code
compileRealLiteral = \case
  Finite r -> do
    let num = pretty $ numerator r
    let denom = pretty $ denominator r
    let rat = parens $ (parens (num <+> ":: R") <+> if denominator r == 1 then mempty else "/" <+> denom)
    parens $ annotate ([], minPrecedence) rat
  _ -> developerError "Compiling infinite values to Isabelle not supported"

compileLam :: (MonadIsabelleCompile m) => [LocaleDef] -> Binder DecidabilityBuiltin -> Expr DecidabilityBuiltin -> m Code
compileLam localeAssms binder expr = do
  let (binders, body) = foldLamBinders binder expr
  (cBinders, cBody) <- compileBinders localeAssms (binder : binders) (compileExpr False localeAssms body)
  return $ annotate (mempty, minPrecedence) ("\\<lambda> " <+> hsep cBinders <+> "." <+> (parens cBody))

data ComparisonDomain
  = CIndex
  | CNat
  | CRatTensor
  deriving (Eq)

compileComparison :: (MonadIsabelleCompile m) => [LocaleDef] -> ComparisonDomain -> ComparisonOp -> [Arg DecidabilityBuiltin] -> m Code
compileComparison localeAssms domain op = do
  let (opDoc, dependencies) = case op of
        Le -> ("\\<le>", orderDeps)
        Lt -> ("<", orderDeps)
        Ge -> ("\\<ge>", orderDeps)
        Gt -> (">", orderDeps)
        Eq -> ("=", eqDeps)
        Ne -> ("\\<noteq>", eqDeps)
  let typeDeps = []
  let (opDoc', dependencies') =
        if domain == CIndex
          then ("$0 " <> opDoc <> " $1", dependencies ++ [])
          else ("$0 " <> opDoc <> " $1", dependencies)
  annotateNotation localeAssms (dependencies' <> typeDeps) 70 opDoc' Nothing
  where
    orderDeps = []
    eqDeps = []

compileTensorComparison :: (MonadIsabelleCompile m) => [LocaleDef] -> ComparisonDomain -> ComparisonOp -> [Arg DecidabilityBuiltin] -> m Code
compileTensorComparison localeAssms _ op = do
  let (opDoc) = case op of
        Le -> ("leqTensorReduced")
        Lt -> ("ltTensorReduced")
        Ge -> ("geqTensorReduced")
        Gt -> ("gtTensorReduced")
        Eq -> ("eqTensorReduced")
        Ne -> ("neTensorReduced")
  let typeDeps = [RequireImport VehicleTensor, RequireImport VehicleUtils]
  let opDesc = ("(" <> opDoc <> " $0 $1)")
  annotateNotation localeAssms (typeDeps) 70 opDesc Nothing

compileStack :: (MonadIsabelleCompile m) => [LocaleDef] -> [Arg DecidabilityBuiltin] -> m Code
compileStack localeAssms args = do
  as <- compileArgs localeAssms minPrecedence args
  return $ annotate ([RequireImport VehicleTensor], 200) $ parens $ "combine_subtensors" <+> toVec as

compileVecLiteral :: (MonadIsabelleCompile m) => [LocaleDef] -> [Arg DecidabilityBuiltin] -> m Code
compileVecLiteral localeAssms xs = case getExpr accessSpine xs of
  Just (VectorLitArgs _t _d ds) -> toVec <$> traverse (compileExpr False localeAssms) ds
  Nothing -> developerError "Malformed type-checked vector literal"

toVec :: [Code] -> Code
toVec xs = annotate ([], maxPrecedence) "[" <> concatWith (surround ",") xs <> "]"
