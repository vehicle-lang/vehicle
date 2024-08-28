module Vehicle.Backend.Agda.Compile
  ( AgdaOptions (..),
    compileProgToAgda,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), runReaderT)
import Data.Bifunctor (Bifunctor (..))
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
import Vehicle.Backend.Prelude
import Vehicle.Compile.Context.Bound (getNamedBoundCtx)
import Vehicle.Compile.Descope (MonadDescope, addBinderNameToContext, ixToProperName, runMonadDescopeT)
import Vehicle.Compile.Error
import Vehicle.Compile.Monomorphisation
import Vehicle.Compile.Normalise.Builtin (findInstanceArg)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.Interface
import Vehicle.Data.Universe (UniverseLevel (..))
import Vehicle.Libraries.StandardLibrary.Definitions
import Vehicle.Syntax.Sugar

--------------------------------------------------------------------------------
-- Agda-specific options

data AgdaOptions = AgdaOptions
  { verificationCache :: Maybe FilePath,
    output :: Maybe FilePath,
    moduleName :: Maybe String
  }

compileProgToAgda :: (MonadCompile m) => Prog Builtin -> AgdaOptions -> m (Doc a)
compileProgToAgda prog options = logCompilerPass MinDetail currentPhase $
  flip runReaderT (options, BoolLevel) $ do
    monoProg <- monomorphise isPropertyDecl "-" prog
    let prog2 = capitaliseTypeNames monoProg
    programDoc <- runMonadDescopeT $ compileProg prog2
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

logEntry :: (MonadAgdaCompile m) => Expr Builtin -> m ()
logEntry e = do
  incrCallDepth
  ctx <- getNamedBoundCtx (Proxy @())
  logDebug MaxDetail $ "compile-entry" <+> prettyFriendly (WithContext e ctx)

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
  | DataList
  | DataListInstances
  | DataListAll
  | DataListAny
  | DataVector
  | DataVectorInstances
  | DataVectorAll
  | DataVectorAny
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
    DataList -> "Data.List.Base"
    DataListInstances -> "Data.List.Instances"
    DataListAll -> "Data.List.Relation.Unary.All as" <+> listQualifier
    DataListAny -> "Data.List.Relation.Unary.Any as" <+> listQualifier
    DataVector -> "Data.Vec.Functional" <+> "renaming" <+> parens "[] to []ᵥ; _∷_ to _∷ᵥ_"
    DataVectorInstances -> "Data.Vec.Functional.Instances"
    DataVectorAll -> "Data.Vec.Functional.Relation.Unary.All as" <+> vectorQualifier
    DataVectorAny -> "Data.Vec.Functional.Relation.Unary.Any as" <+> vectorQualifier
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

vectorQualifier :: Doc a
vectorQualifier = "Vector"

tensorQualifier :: Doc a
tensorQualifier = "Tensor"

indentCode :: Code -> Code
indentCode = indent 2

scopeCode :: Code -> Code -> Code
scopeCode keyword code = keyword <> line <> indentCode code

--------------------------------------------------------------------------------
-- Intermediate results of compilation

-- | Marks if the current boolean expression is compiled to `Set` or `Bool`
data BoolLevel = TypeLevel | BoolLevel
  deriving (Eq)

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

annotateApp :: [Dependency] -> Code -> [Code] -> Code
annotateApp dependencies fun [] =
  let precedence = getPrecedence fun
   in annotate (Set.fromList dependencies, precedence) fun
annotateApp dependencies fun args =
  let precedence = 20
   in let bracketedArgs = map (bracketIfRequired precedence) args
       in annotate (Set.fromList dependencies, precedence) (hsep (fun : bracketedArgs))

annotateInfixOp1 ::
  [Dependency] ->
  Precedence ->
  Maybe Code ->
  Code ->
  [Code] ->
  Code
annotateInfixOp1 dependencies precedence qualifier op args = result
  where
    bracketedArgs = map (bracketIfRequired precedence) args
    qualifierDoc = maybe "" (<> ".") qualifier
    doc = case bracketedArgs of
      [] -> qualifierDoc <> op <> "_"
      [e1] -> qualifierDoc <> op <+> e1
      _ ->
        developerError $
          "was expecting no more than 1 argument for"
            <+> op
            <+> "but found the following arguments:"
            <+> list args
    result = annotate (Set.fromList dependencies, precedence) doc

annotateInfixOp2 ::
  [Dependency] ->
  Precedence ->
  (Code -> Code) ->
  Maybe Code ->
  Code ->
  [Code] ->
  Code
annotateInfixOp2 dependencies precedence opBraces qualifier op args = result
  where
    bracketedArgs = map (bracketIfRequired precedence) args
    qualifierDoc = maybe "" (<> ".") qualifier
    doc = case bracketedArgs of
      [] -> qualifierDoc <> "_" <> op <> "_"
      [e1] -> e1 <+> qualifierDoc <> op <> "_"
      [e1, e2] -> e1 <+> qualifierDoc <> op <+> e2
      allArgs -> annotateApp [] ("_" <> op <> "_") allArgs
    result = annotate (Set.fromList dependencies, precedence) (opBraces doc)

bracketIfRequired :: Precedence -> Code -> Code
bracketIfRequired parentPrecedence expr =
  if getPrecedence expr <= parentPrecedence
    then parens expr
    else expr

argBrackets :: Visibility -> Code -> Code
argBrackets Explicit {} = id
argBrackets Implicit {} = braces
argBrackets Instance {} = braces . braces

binderBrackets :: Bool -> Visibility -> Code -> Code
binderBrackets True Explicit {} = id
binderBrackets False Explicit {} = parens
binderBrackets _topLevel Implicit {} = braces
binderBrackets _topLevel Instance {} = braces . braces

boolBraces :: Code -> Code
boolBraces c = annotateConstant [RelNullaryDecidable] "⌊" <+> c <+> "⌋"

arrow :: Code
arrow = "→" -- <> softline'

--------------------------------------------------------------------------------
-- Monad stack

type MonadAgdaCompile m =
  ( MonadCompile m,
    MonadReader (AgdaOptions, BoolLevel) m,
    MonadDescope m
  )

getVerificationCache :: (MonadAgdaCompile m) => m (Maybe FilePath)
getVerificationCache = do
  (options, _) <- ask
  return $ verificationCache options

getBoolLevel :: (MonadAgdaCompile m) => m BoolLevel
getBoolLevel = do
  (_, boolLevel) <- ask
  return boolLevel

setBoolLevel :: (MonadAgdaCompile m) => BoolLevel -> m a -> m a
setBoolLevel level = local (second (const level))

--------------------------------------------------------------------------------
-- Program Compilation

compileProg :: (MonadAgdaCompile m) => Prog Builtin -> m Code
compileProg (Main ds) = vsep2 <$> traverse compileDecl ds

compileDecl :: (MonadAgdaCompile m) => Decl Builtin -> m Code
compileDecl = \case
  DefAbstract _ n _ t ->
    compilePostulate (compileIdentifier n) <$> compileExpr t
  DefFunction _ n anns t e -> do
    let (binders, body) = foldDeclBinders e
    setBoolLevel TypeLevel $ do
      if isProperty anns
        then compileProperty (compileIdentifier n) =<< compileExpr e
        else do
          let binders' = mapMaybe compileTopLevelBinder binders
          (_, cbody) <- compileBinders binders (compileExpr body)
          compileFunDef (compileIdentifier n) <$> compileExpr t <*> pure binders' <*> pure cbody

compileExpr :: (MonadAgdaCompile m) => Expr Builtin -> m Code
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
        cOutput <- addBinderNameToContext binder $ compileExpr result
        return $ annotateInfixOp2 [] minPrecedence id Nothing arrow [cInput, cOutput]
      _ -> do
        let (binders, body) = foldBinders PiBinder binder result
        compileTypeLevelQuantifier Forall (binder :| binders) body
    Let _ bound binder body -> do
      cBoundExpr <- compileLetBinder (binder, bound)
      cBody <- addBinderNameToContext binder $ compileExpr body
      return $ "let" <+> cBoundExpr <+> "in" <+> cBody
    Lam _ binder body -> compileLam binder body
    Builtin p b -> compileBuiltin p b []
    App fun args -> compileApp fun args

  logExit result
  return result

compileApp :: (MonadAgdaCompile m) => Expr Builtin -> NonEmpty (Arg Builtin) -> m Code
compileApp fun args = do
  specialResult <- case fun of
    Builtin p b -> Just <$> compileBuiltin p b (NonEmpty.toList args)
    FreeVar _ ident -> case findStdLibFunction ident of
      Nothing -> return Nothing
      Just stdlibFn -> compileStdLibFunction stdlibFn args
    _ -> return Nothing

  case specialResult of
    Just v -> return v
    Nothing -> do
      cFun <- compileExpr fun
      cArgs <- traverse compileExpr (filterOutNonExplicitArgs args)
      return $ annotateApp [] cFun cArgs

compileStdLibFunction :: (MonadAgdaCompile m) => StdLibFunction -> NonEmpty (Arg Builtin) -> m (Maybe Code)
compileStdLibFunction fn args = case fn of
  StdTensor -> do
    let fun' = annotateConstant [DataTensor] "Tensor"
    args' <- traverse compileExpr (filterOutNonExplicitArgs args)
    return $ Just $ annotateApp [] fun' args'
  StdForeachIndex -> Just <$> compileExpr (argExpr $ NonEmpty.last args)
  StdVectorToVector -> Just <$> compileExpr (argExpr $ NonEmpty.last args)
  StdVectorToList -> case args of
    [_, _, RelevantExplicitArg _ (IVecLiteral tElem xs)] ->
      Just <$> compileExpr (mkListExpr (argExpr tElem) (fmap argExpr xs))
    _ -> return Nothing
  StdForallIn -> case args of
    [_, RelevantImplicitArg _ tCont, _, _, RelevantExplicitArg _ lam, RelevantExplicitArg _ cont] ->
      Just <$> compileQuantIn Forall tCont lam cont
    _ -> return Nothing
  StdExistsIn -> case args of
    [RelevantImplicitArg _ tCont, _, _, RelevantExplicitArg _ lam, RelevantExplicitArg _ cont] ->
      Just <$> compileQuantIn Exists tCont lam cont
    _ -> return Nothing
  StdTypeAnn -> case args of
    [RelevantExplicitArg _ t, RelevantExplicitArg _ e] -> do
      e' <- compileExpr e
      t' <- compileExpr t
      return (Just (annotateInfixOp2 [FunctionBase] 0 id Nothing "∋" [t', e']))
    _ -> return Nothing
  _ -> return Nothing

compileLetBinder ::
  (MonadAgdaCompile m) =>
  LetBinder (Expr Builtin) ->
  m Code
compileLetBinder (binder, expr) = do
  let binderName = pretty (getBinderName binder)
  cExpr <- compileExpr expr
  return $ binderName <+> "=" <+> cExpr

compileLam :: (MonadAgdaCompile m) => Binder Builtin -> Expr Builtin -> m Code
compileLam binder expr = do
  let (binders, body) = foldBinders LamBinder binder expr
  (cBinders, cBody) <- compileBinders (binder : binders) (compileExpr body)
  return $ annotate (mempty, minPrecedence) ("λ" <+> hsep cBinders <+> arrow <+> cBody)

compileArg :: (MonadAgdaCompile m) => Arg Builtin -> m Code
compileArg arg = argBrackets (visibilityOf arg) <$> compileExpr (argExpr arg)

compileArgs :: (MonadAgdaCompile m) => [Arg Builtin] -> m [Code]
compileArgs args = traverse compileArg (filter (not . wasInsertedByCompiler) args)

compileBooleanType :: (MonadAgdaCompile m) => m Code
compileBooleanType = do
  boolLevel <- getBoolLevel
  return $ case boolLevel of
    TypeLevel -> compileType (UniverseLevel 0)
    BoolLevel -> annotateConstant [DataBool] "Bool"

compileIdentifier :: Identifier -> Code
compileIdentifier ident = pretty (nameOf ident :: Name)

compileType :: UniverseLevel -> Code
compileType (UniverseLevel l)
  | l == 0 = "Set"
  | otherwise = annotateConstant [] ("Set" <> pretty l)

compileTopLevelBinder :: Binder Builtin -> Maybe Code
compileTopLevelBinder binder
  | visibilityOf binder /= Explicit = Nothing
  | otherwise = do
      let binderName = pretty (getBinderName binder)
      let addBrackets = binderBrackets True (visibilityOf binder)
      Just $ addBrackets binderName

compileBinders :: (MonadAgdaCompile m) => [Binder Builtin] -> m Code -> m ([Code], Code)
compileBinders [] c = ([],) <$> c
compileBinders (b : bs) c = do
  (cbs, cc) <- addBinderNameToContext b $ compileBinders bs c
  cb <- compileBinder b
  return (cb : cbs, cc)

compileBinder :: (MonadAgdaCompile m) => Binder Builtin -> m Code
compileBinder binder = do
  binderType <- compileExpr (typeOf binder)
  (binderDoc, noExplicitBrackets) <- case binderNamingForm binder of
    OnlyName name -> return (pretty name, True)
    OnlyType -> return (binderType, True)
    NameAndType name -> do
      let annName = annotateInfixOp2 [] minPrecedence id Nothing ":" [pretty name, binderType]
      return (annName, False)

  return $ binderBrackets noExplicitBrackets (visibilityOf binder) binderDoc

agdaNegInt :: [Code] -> Code
agdaNegInt = annotateInfixOp1 [DataInteger] 6 (Just intQualifier) "-"

agdaPosInt :: [Code] -> Code
agdaPosInt = annotateInfixOp1 [DataInteger] 8 (Just intQualifier) "+"

agdaDivRat :: [Code] -> Code
agdaDivRat = annotateInfixOp2 [DataRat] 7 id (Just ratQualifier) "/"

agdaNatToFin :: [Code] -> Code
agdaNatToFin = annotateInfixOp1 [DataFin] 10 Nothing "#"

compileBuiltin :: (MonadAgdaCompile m) => Provenance -> Builtin -> [Arg Builtin] -> m Code
compileBuiltin _p b args = case b of
  TypeClass c -> case c of
    HasEq {} -> annotateApp [] "HasEq" <$> compileArgs args
    HasOrd {} -> annotateApp [] "HasOrd" <$> compileArgs args
    HasAdd -> annotateApp [] "HasAdd" <$> compileArgs args
    HasSub -> annotateApp [] "HasSub" <$> compileArgs args
    HasMul -> annotateApp [] "HasMul" <$> compileArgs args
    HasDiv -> annotateApp [] "HasDiv" <$> compileArgs args
    HasNeg -> annotateApp [] "HasNeg" <$> compileArgs args
    HasNatLits -> annotateApp [] "HasNatLits" <$> compileArgs args
    HasRatLits -> annotateApp [] "HasRatLits" <$> compileArgs args
    HasQuantifier {} -> unsupportedError
    HasVecLits -> unsupportedError
    HasFold -> unsupportedError
    HasMap -> unsupportedError
    HasQuantifierIn {} -> unsupportedError
  TypeClassOp op -> case op of
    QuantifierTC q -> case reverse args of
      (ExplicitArg _ _ (Lam _ binder body)) : _ -> compileTypeLevelQuantifier q [binder] body
      _ -> unsupportedArgsError
    OrderTC {} -> resolveInstance b args
    EqualsTC eq -> case args of
      [] -> unsupportedArgsError
      tElem : _ -> do
        deps <- equalityDependencies (argExpr tElem)
        case eq of
          Eq -> compileEquality deps =<< compileArgs args
          Neq -> compileInequality deps =<< compileArgs args
    FromNatTC {} -> resolveInstance b args
    FromRatTC {} -> resolveInstance b args
    FromVecTC {} -> resolveInstance b args
    AddTC {} -> annotateInfixOp2 [VehicleUtils] 6 id Nothing "⊕" <$> compileArgs args
    SubTC {} -> annotateInfixOp2 [VehicleUtils] 6 id Nothing "⊖" <$> compileArgs args
    -- TODO we should really have our own Agda type-classes for all of these
    NegTC {} -> resolveInstance b args
    MulTC {} -> resolveInstance b args
    DivTC {} -> resolveInstance b args
    MapTC {} -> resolveInstance b args
    FoldTC {} -> resolveInstance b args
  BuiltinType t -> case t of
    Unit -> return compileUnit
    Bool -> compileBooleanType
    Nat -> return $ annotateConstant [DataNat] natQualifier
    Rat -> return $ annotateConstant [DataRat] ratQualifier
    List -> annotateApp [DataList] "List" <$> compileArgs args
    Vector -> annotateApp [DataVector] "Vector" <$> compileArgs args
    Index -> annotateApp [DataFin] "Fin" <$> compileArgs args
  BuiltinConstructor c -> case c of
    Nil -> return compileNil
    Cons -> compileCons <$> compileArgs args
    LUnit -> return $ annotateConstant [DataUnit] "tt"
    LBool v -> compileBoolOp0 v
    LIndex n -> return $ compileIndexLiteral (toInteger n)
    LNat n -> return $ compileNatLiteral (toInteger n)
    LRat r -> return $ compileRatLiteral r
    LVec _ -> case args of
      _tElem : elems -> compileVecLiteral (fmap argExpr elems)
      [] -> unsupportedArgsError
  BuiltinFunction f -> case f of
    And -> compileAnd =<< compileArgs args
    Or -> compileOr =<< compileArgs args
    Implies -> compileImplies =<< compileArgs args
    Not -> compileNot =<< compileArgs args
    Add dom -> compileAdd dom <$> compileArgs args
    Sub dom -> compileSub dom <$> compileArgs args
    Mul dom -> compileMul dom <$> compileArgs args
    Div dom -> compileDiv dom <$> compileArgs args
    Neg dom -> compileNeg dom <$> compileArgs args
    MinRat -> compileMin <$> compileArgs args
    MaxRat -> compileMax <$> compileArgs args
    PowRat -> unsupportedError
    Equals dom Eq -> compileEquality (equalityDomDependencies dom) =<< compileArgs args
    Equals dom Neq -> compileEquality (equalityDomDependencies dom) =<< compileArgs args
    Order dom ord -> compileOrder ord dom =<< compileArgs args
    FoldList -> annotateApp [DataList] (listQualifier <> ".foldr") <$> compileArgs args
    FoldVector -> annotateApp [DataVector] (vectorQualifier <> ".foldr") <$> compileArgs args
    MapList -> annotateApp [DataList] (listQualifier <> ".map") <$> compileArgs args
    MapVector -> annotateApp [DataVector] (vectorQualifier <> ".map") <$> compileArgs args
    ZipWithVector -> annotateApp [DataVector] (vectorQualifier <> ".zipWith") <$> compileArgs args
    Indices {} -> annotateApp [DataFin] "allFin" <$> compileArgs args
    Quantifier q -> case reverse args of
      (ExplicitArg _ _ (Lam _ binder body)) : _ -> compileTypeLevelQuantifier q [binder] body
      _ -> unsupportedArgsError
    FromNat dom -> case reverse args of
      (_inst : value : _) -> compileFromNat dom <$> compileArg value
      _ -> unsupportedArgsError
    FromRat dom -> case reverse args of
      (value : _) -> compileFromRat dom <$> compileArg value
      _ -> unsupportedArgsError
    -- Needs to be special cased as `At` in Agda is simply function application.
    At -> case args of
      [_tElem, _size, xs, index] -> annotateApp [] <$> compileExpr (argExpr xs) <*> compileArgs [index]
      _ -> annotateInfixOp2 [FunctionBase] (-1) id Nothing "$" <$> compileArgs args
    -- Needs to be special cased as we need to enforce that the decision
    -- procedure is decidable. Would be fixed by
    -- https://github.com/vehicle-lang/vehicle/issues/694.
    If -> case args of
      [_, e1, e2, e3] -> do
        ce1 <- setBoolLevel BoolLevel $ compileArg e1
        ce2 <- compileArg e2
        ce3 <- compileArg e3
        return $
          annotate
            (Set.singleton DataBool, 0)
            ("if" <+> ce1 <+> "then" <+> ce2 <+> "else" <+> ce3)
      _ -> unsupportedArgsError
  NatInDomainConstraint -> unsupportedError
  where
    unsupportedError :: (MonadAgdaCompile m) => m a
    unsupportedError =
      compilerDeveloperError $
        "compilation of builtin" <+> quotePretty b <+> "to Agda unsupported"

    unsupportedArgsError :: (MonadAgdaCompile m) => m a
    unsupportedArgsError = do
      ctx <- getNamedBoundCtx (Proxy @())
      compilerDeveloperError $
        "compilation of"
          <+> prettyFriendly (WithContext (normAppList (Builtin mempty b) args) ctx)
          <+> "to Agda unsupported"

    resolveInstance :: (MonadAgdaCompile m) => Builtin -> [Arg Builtin] -> m Code
    resolveInstance op as = do
      (fn, newArgs) <- findInstanceArg op as
      compileExpr (normAppList fn newArgs)

compileTypeLevelQuantifier ::
  (MonadAgdaCompile m) =>
  Quantifier ->
  NonEmpty (Binder Builtin) ->
  Expr Builtin ->
  m Code
compileTypeLevelQuantifier q binders body = do
  (cBinders, cBody) <- compileBinders (NonEmpty.toList binders) (compileExpr body)
  quant <- case q of
    Forall -> return "∀"
    Exists -> return $ annotateConstant [DataProduct] "∃ λ"
  return $ quant <+> hsep cBinders <+> arrow <+> cBody

compileQuantIn :: (MonadAgdaCompile m) => Quantifier -> Expr Builtin -> Expr Builtin -> Expr Builtin -> m Code
compileQuantIn q tCont fn cont = do
  boolLevel <- getBoolLevel

  (quant, qualifier, dep) <- case (boolLevel, q, tCont) of
    (TypeLevel, Forall, IRawListType {}) -> return ("All", listQualifier, DataListAll)
    (TypeLevel, Exists, IRawListType {}) -> return ("Any", listQualifier, DataListAny)
    (BoolLevel, Forall, IRawListType {}) -> return ("all", listQualifier, DataList)
    (BoolLevel, Exists, IRawListType {}) -> return ("any", listQualifier, DataList)
    (TypeLevel, Forall, _) -> return ("All", tensorQualifier, DataTensorAll)
    (TypeLevel, Exists, _) -> return ("Any", tensorQualifier, DataTensorAny)
    (BoolLevel, Forall, _) -> return ("all", tensorQualifier, DataTensor)
    (BoolLevel, Exists, _) -> return ("any", tensorQualifier, DataTensor)

  annotateApp [dep] (qualifier <> "." <> quant) <$> traverse compileExpr [fn, cont]

compileIndexLiteral :: Integer -> Code
compileIndexLiteral i = agdaNatToFin [pretty i]

compileNatLiteral :: Integer -> Code
compileNatLiteral = pretty

compileIntLiteral :: Integer -> Code
compileIntLiteral i
  | i >= 0 = agdaPosInt [pretty i]
  | otherwise = agdaNegInt [compileIntLiteral (-i)]

compileRatLiteral :: Rational -> Code
compileRatLiteral r = agdaDivRat [num, denom]
  where
    num = compileIntLiteral (numerator r)
    denom = compileNatLiteral (denominator r)

-- | Compiling vector literals. No literals in Agda so have to go via cons.
compileVecLiteral :: (MonadAgdaCompile m) => [Expr Builtin] -> m Code
compileVecLiteral = \case
  [] -> return $ annotateConstant [DataVector] "[]ᵥ"
  (x : xs) -> do
    cx <- compileExpr x
    cxs <- compileVecLiteral xs
    return $ annotateInfixOp2 [] 5 id Nothing "∷ᵥ" [cx, cxs]

compileUnit :: Code
compileUnit = annotateConstant [DataUnit] "⊤"

compileNil :: Code
compileNil = annotateConstant [DataList] "[]"

compileCons :: [Code] -> Code
compileCons = annotateInfixOp2 [DataList] 5 id Nothing "∷"

-- | Compiling boolean constants
compileBoolOp0 :: (MonadAgdaCompile m) => Bool -> m Code
compileBoolOp0 value = do
  boolLevel <- getBoolLevel
  let (deps, code) = case (value, boolLevel) of
        (True, BoolLevel) -> ([DataBool], "true")
        (True, TypeLevel) -> ([DataUnit], "⊤")
        (False, BoolLevel) -> ([DataBool], "false")
        (False, TypeLevel) -> ([DataEmpty], "⊥")
  return $ annotateConstant deps code

-- | Compiling boolean negation
compileNot :: (MonadAgdaCompile m) => [Code] -> m Code
compileNot args = do
  boolLevel <- getBoolLevel
  return $ case boolLevel of
    BoolLevel -> annotateApp [DataBool] "not" args
    TypeLevel -> annotateInfixOp1 [RelNullary] 3 Nothing "¬" args

compileAnd :: (MonadAgdaCompile m) => [Code] -> m Code
compileAnd args = do
  boolLevel <- getBoolLevel
  let (opDoc, precedence, dependencies) = case boolLevel of
        BoolLevel -> ("∧", 6, [DataBool])
        TypeLevel -> ("×", 2, [DataProduct])
  return $ annotateInfixOp2 dependencies precedence id Nothing opDoc args

compileOr :: (MonadAgdaCompile m) => [Code] -> m Code
compileOr args = do
  boolLevel <- getBoolLevel
  let (opDoc, precedence, dependencies) = case boolLevel of
        BoolLevel -> ("∨", 5, [DataBool])
        TypeLevel -> ("⊎", 1, [DataSum])
  return $ annotateInfixOp2 dependencies precedence id Nothing opDoc args

compileImplies :: (MonadAgdaCompile m) => [Code] -> m Code
compileImplies args = do
  boolLevel <- getBoolLevel
  let (opDoc, precedence, dependencies) = case boolLevel of
        BoolLevel -> ("⇒", 4, [VehicleUtils])
        TypeLevel -> (arrow, minPrecedence, [])
  return $ annotateInfixOp2 dependencies precedence id Nothing opDoc args

-- | Compiling numeric unary operations
compileNeg :: NegDomain -> [Code] -> Code
compileNeg dom args = do
  let (qualifier, dependency) = case dom of
        NegRat -> (ratQualifier, DataRat)

  annotateInfixOp1 [dependency] 8 (Just qualifier) "-" args

compileFromNat :: FromNatDomain -> Code -> Code
compileFromNat dom value = case dom of
  FromNatToIndex -> agdaNatToFin [value]
  FromNatToNat -> value
  FromNatToRat -> agdaDivRat [agdaPosInt [value], "1"]

compileFromRat :: FromRatDomain -> Code -> Code
compileFromRat dom arg = case dom of
  FromRatToRat -> arg

compileAdd :: AddDomain -> [Code] -> Code
compileAdd dom args = do
  let (qualifier, dependency) = case dom of
        AddNat -> (natQualifier, DataNat)
        AddRat -> (ratQualifier, DataRat)

  annotateInfixOp2 [dependency] 6 id (Just qualifier) "+" args

compileSub :: SubDomain -> [Code] -> Code
compileSub dom args = do
  let (qualifier, dependency) = case dom of
        SubRat -> (ratQualifier, DataRat)

  annotateInfixOp2 [dependency] 6 id (Just qualifier) "-" args

compileMul :: MulDomain -> [Code] -> Code
compileMul mul args = do
  let (qualifier, dependency) = case mul of
        MulNat -> (natQualifier, DataNat)
        MulRat -> (ratQualifier, DataRat)

  annotateInfixOp2 [dependency] 7 id (Just qualifier) "*" args

compileDiv :: DivDomain -> [Code] -> Code
compileDiv dom args = do
  let (qualifier, dependency) = case dom of
        DivRat -> (ratQualifier, DataRat)

  annotateInfixOp2 [dependency] 7 id (Just qualifier) "÷" args

compileMin :: [Code] -> Code
compileMin = annotateInfixOp2 [DataRat] 6 id (Just ratQualifier) "⊓"

compileMax :: [Code] -> Code
compileMax = annotateInfixOp2 [DataRat] 7 id (Just ratQualifier) "⊔"

compileOrder :: (MonadAgdaCompile m) => OrderOp -> OrderDomain -> [Code] -> m Code
compileOrder originalOrder dom originalArgs = do
  boolLevel <- getBoolLevel

  -- HACK because v1.7 of stdlib doesn't have >? and >=?. Fixed in v2.0 so remove when released.
  let (order, args) =
        if dom == OrderRat && boolLevel == BoolLevel && (originalOrder == Ge || originalOrder == Gt)
          then (flipOrder originalOrder, reverse originalArgs)
          else (originalOrder, originalArgs)

  (qualifier, elemDep) <- case dom of
    OrderIndex -> return (finQualifier, DataFin)
    OrderNat -> return (natQualifier, DataNat)
    OrderRat -> return (ratQualifier, DataRat)

  let (boolDecDoc, boolDeps, opBraces) = case boolLevel of
        BoolLevel -> ("?", [RelNullary], boolBraces)
        TypeLevel -> ("", [], id)

  let orderDoc = case order of
        Le -> "≤"
        Lt -> "<"
        Ge -> "≥"
        Gt -> ">"

  let dependencies = [elemDep] <> boolDeps
  let opDoc = orderDoc <> boolDecDoc
  return $ annotateInfixOp2 dependencies 4 opBraces (Just qualifier) opDoc args

compileEquality :: (MonadAgdaCompile m) => [Dependency] -> [Code] -> m Code
compileEquality dependencies args = do
  boolLevel <- getBoolLevel
  case boolLevel of
    TypeLevel -> return $ annotateInfixOp2 [PropEquality] 4 id Nothing "≡" args
    BoolLevel -> do
      -- Boolean function equality is more complicated as we need an actual decision procedure.
      -- We handle this using instance arguments
      return $ annotateInfixOp2 ([RelNullary] <> dependencies) 4 boolBraces Nothing "≟" args

compileInequality :: (MonadAgdaCompile m) => [Dependency] -> [Code] -> m Code
compileInequality deps args = do
  boolLevel <- getBoolLevel
  case boolLevel of
    TypeLevel -> return $ annotateInfixOp2 [PropEquality] 4 id Nothing "≢" args
    BoolLevel -> do
      eq <- compileEquality deps args
      compileNot [eq]

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
compilePostulate name t =
  "postulate" <+> name <+> ":" <+> align t

compileProperty :: (MonadAgdaCompile m) => Code -> Code -> m Code
compileProperty propertyName propertyBody = do
  maybeVerificationCache <- getVerificationCache
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

equalityDomDependencies :: EqualityDomain -> [Dependency]
equalityDomDependencies = \case
  EqIndex -> [DataFin]
  EqNat -> [DataNat]
  EqRat -> [DataRat]

-- Calculates the dependencies needed for equality over the provided type
equalityDependencies :: (MonadAgdaCompile m) => Expr Builtin -> m [Dependency]
equalityDependencies = \case
  IndexType _ _ -> return [DataFin]
  INatType {} -> return [DataNatInstances]
  IRatType {} -> return [DataRatInstances]
  IBoolType {} -> return [DataBoolInstances]
  IListType _ tElem -> do
    deps <- equalityDependencies tElem
    return $ [DataListInstances] <> deps
  IVectorType _ tElem _tDims -> do
    deps <- equalityDependencies tElem
    return $ [DataVectorInstances] <> deps
  TensorType _ tElem _tDims -> do
    deps <- equalityDependencies tElem
    return $ [DataTensorInstances] <> deps
  FreeVar p n -> throwError $ UnsupportedPolymorphicEquality Agda p (nameOf n)
  BoundVar p ix -> do
    n <- ixToProperName p ix
    throwError $ UnsupportedPolymorphicEquality Agda p n
  t -> unexpectedTypeError t (map pretty [Bool, Index, Nat, Rat, List, Vector] <> [pretty (identifierName TensorIdent)])

unexpectedTypeError :: (MonadAgdaCompile m) => Expr Builtin -> [Doc ()] -> m a
unexpectedTypeError actualType expectedTypes = do
  ctx <- getNamedBoundCtx (Proxy @())
  compilerDeveloperError $
    "Unexpected type found."
      <+> "Was expecting one of"
      <+> list expectedTypes
      <+> "but found"
      <+> prettyFriendly (WithContext actualType ctx)
      <+> "at"
      <+> pretty (provenanceOf actualType)
      <> "."

currentPhase :: Doc ()
currentPhase = "compilation to Agda"

-- | Ugly hack until we work out how to bind builtins in the
-- user syntax.
pattern TensorType ::
  Provenance ->
  Expr Builtin ->
  Expr Builtin ->
  Expr Builtin
pattern TensorType p tElem tDims <-
  App
    (FreeVar p TensorIdent)
    [ RelevantExplicitArg _ tElem,
      IrrelevantExplicitArg _ tDims
      ]

pattern IndexType ::
  Provenance ->
  Expr Builtin ->
  Expr Builtin
pattern IndexType p size <-
  App
    (Builtin p (BuiltinType Index))
    [ IrrelevantExplicitArg _ size
      ]
