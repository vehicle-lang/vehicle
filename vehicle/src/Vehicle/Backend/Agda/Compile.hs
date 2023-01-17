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
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Real (denominator, numerator)
import Prettyprinter hiding (hcat, hsep, vcat, vsep)
import System.FilePath (takeBaseName)
import Vehicle.Backend.Prelude
import Vehicle.Compile.CapitaliseTypeNames (capitaliseTypeNames)
import Vehicle.Compile.Descope (descopeNamed)
import Vehicle.Compile.Error
import Vehicle.Compile.Monomorphisation (monomorphise)
import Vehicle.Compile.Normalise (nfTypeClassOp)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type (getUnnormalised)
import Vehicle.Libraries.StandardLibrary (pattern TensorIdent)
import Vehicle.Libraries.StandardLibrary.Names
  ( StdLibFunction,
    findStdLibFunction,
  )
import Vehicle.Syntax.Sugar

--------------------------------------------------------------------------------
-- Agda-specific options

data AgdaOptions = AgdaOptions
  { proofCacheLocation :: Maybe FilePath,
    outputFile :: Maybe FilePath,
    moduleName :: Maybe String
  }

compileProgToAgda :: MonadCompile m => TypedProg -> AgdaOptions -> m (Doc a)
compileProgToAgda prog options = logCompilerPass MinDetail currentPhase $
  flip runReaderT (options, BoolLevel) $ do
    unnormalisedProg <- traverse getUnnormalised prog
    monoProg <- monomorphise unnormalisedProg

    let prog2 = capitaliseTypeNames monoProg
    let prog3 = descopeNamed prog2
    programDoc <- compileProg prog3
    let programStream = layoutPretty defaultLayoutOptions programDoc
    -- Collects dependencies by first discarding precedence info and then
    -- folding using Set Monoid
    let progamDependencies = fold (reAnnotateS fst programStream)

    let nameOfModule = Text.pack $ case moduleName options of
          Just name -> name
          _ -> maybe "Spec" takeBaseName (outputFile options)

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

logEntry :: MonadAgdaCompile m => OutputExpr -> m ()
logEntry e = do
  incrCallDepth
  logDebug MaxDetail $ "compile-entry" <+> prettyVerbose e

logExit :: MonadAgdaCompile m => Code -> m ()
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
      _ ->
        developerError $
          "was expecting no more than 2 arguments for"
            <+> op
            <+> "but found the following arguments:"
            <+> list args
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
    MonadReader (AgdaOptions, BoolLevel) m
  )

getProofCacheLocation :: MonadAgdaCompile m => m (Maybe FilePath)
getProofCacheLocation = do
  (options, _) <- ask
  return $ proofCacheLocation options

getBoolLevel :: MonadAgdaCompile m => m BoolLevel
getBoolLevel = do
  (_, boolLevel) <- ask
  return boolLevel

setBoolLevel :: MonadAgdaCompile m => BoolLevel -> m a -> m a
setBoolLevel level = local (second (const level))

--------------------------------------------------------------------------------
-- Program Compilation

compileProg :: MonadAgdaCompile m => OutputProg -> m Code
compileProg (Main ds) = vsep2 <$> traverse compileDecl ds

compileDecl :: MonadAgdaCompile m => OutputDecl -> m Code
compileDecl = \case
  DefResource _ n _ t ->
    compilePostulate (compileIdentifier n) <$> compileExpr t
  DefPostulate _ n t ->
    compilePostulate (compileIdentifier n) <$> compileExpr t
  DefFunction _ n isProperty t e -> do
    let (binders, body) = foldBinders FunFold e
    setBoolLevel TypeLevel $ do
      if isProperty
        then compileProperty (compileIdentifier n) =<< compileExpr e
        else do
          let binders' = mapMaybe compileTopLevelBinder binders
          compileFunDef (compileIdentifier n) <$> compileExpr t <*> pure binders' <*> compileExpr body

compileExpr :: MonadAgdaCompile m => OutputExpr -> m Code
compileExpr expr = do
  logEntry expr
  result <- case expr of
    Hole {} -> resolutionError currentPhase "Hole"
    Meta {} -> resolutionError currentPhase "Meta"
    Universe _ u -> case u of
      TypeUniv l -> return $ compileType l
      _ -> resolutionError currentPhase (pretty u)
    Var _ n -> compileVar n
    Pi _ binder result -> case binderNamingForm binder of
      OnlyType -> do
        cInput <- compileBinder binder
        cOutput <- compileExpr result
        return $ annotateInfixOp2 [] minPrecedence id Nothing arrow [cInput, cOutput]
      _ -> do
        let (binders, body) = foldBinders (FoldableBinder PiFold binder) result
        compileTypeLevelQuantifier Forall (binder :| binders) body
    Ann _ e t -> compileAnn <$> compileExpr e <*> compileExpr t
    Let _ bound binding body -> do
      cBoundExpr <- compileLetBinder (binding, bound)
      cBody <- compileExpr body
      return $ "let" <+> cBoundExpr <+> "in" <+> cBody
    {-
    -- TODO re-enable let folding - Agda separates by whitespace though so it's complicated.
    let (boundExprs, body) = foldLet expr
    cBoundExprs <- traverse compile boundExprs
    cBody       <- compile body
    return $ "let" <+> vsep (punctuate ";" cBoundExprs) <+> "in" <+> cBody
    -}

    Lam _ binder body -> compileLam binder body
    Builtin _ b -> compileBuiltin b []
    Literal _ l -> compileLiteral l
    LVec _ xs -> compileVecLiteral xs
    App _ fun args -> compileApp fun args

  logExit result
  return result

compileVar :: MonadAgdaCompile m => InputVar -> m Code
compileVar var = return $ case var of
  -- Standard library operators that we would like to compile
  "Tensor" -> annotateConstant [DataTensor] "Tensor"
  -- Other variables
  v -> annotateConstant [] (pretty v)

compileApp :: MonadAgdaCompile m => OutputExpr -> NonEmpty OutputArg -> m Code
compileApp fun args = do
  specialResult <- case fun of
    Builtin _ b -> Just <$> compileBuiltin b (NonEmpty.toList args)
    Var _ i -> case findStdLibFunction i of
      Nothing -> return Nothing
      Just f -> Just <$> compileStdLibFunction f args
    _ -> return Nothing

  case specialResult of
    Just v -> return v
    Nothing -> do
      cFun <- compileExpr fun
      cArgs <- traverse compileExpr (filterOutNonExplicitArgs args)
      return $ annotateApp [] cFun cArgs

compileLetBinder ::
  MonadAgdaCompile m =>
  LetBinder OutputBinding OutputVar ->
  m Code
compileLetBinder (binder, expr) = do
  let binderName = pretty (getBinderName binder)
  cExpr <- compileExpr expr
  return $ binderName <+> "=" <+> cExpr

compileLam :: MonadAgdaCompile m => OutputBinder -> OutputExpr -> m Code
compileLam binder expr = do
  let (binders, body) = foldBinders (FoldableBinder LamFold binder) expr
  cBinders <- traverse compileBinder (binder : binders)
  cBody <- compileExpr body
  return $ annotate (mempty, minPrecedence) ("λ" <+> hsep cBinders <+> arrow <+> cBody)

compileArg :: MonadAgdaCompile m => OutputArg -> m Code
compileArg arg = argBrackets (visibilityOf arg) <$> compileExpr (argExpr arg)

compileAnn :: Code -> Code -> Code
compileAnn e t = annotateInfixOp2 [FunctionBase] 0 id Nothing "∋" [t, e]

compileBooleanType :: MonadAgdaCompile m => m Code
compileBooleanType = do
  boolLevel <- getBoolLevel
  return $ case boolLevel of
    TypeLevel -> compileType 0
    BoolLevel -> annotateConstant [DataBool] "Bool"

compileIdentifier :: Identifier -> Code
compileIdentifier ident = pretty (nameOf ident :: Name)

compileType :: UniverseLevel -> Code
compileType 0 = "Set"
compileType l = annotateConstant [] ("Set" <> pretty l)

compileTopLevelBinder :: OutputBinder -> Maybe Code
compileTopLevelBinder binder
  | visibilityOf binder /= Explicit = Nothing
  | otherwise = do
      let binderName = pretty (getBinderName binder)
      let addBrackets = binderBrackets True (visibilityOf binder)
      Just $ addBrackets binderName

compileBinder :: MonadAgdaCompile m => OutputBinder -> m Code
compileBinder binder = do
  binderType <- compileExpr (typeOf binder)
  (binderDoc, noExplicitBrackets) <- case binderNamingForm binder of
    OnlyName name -> return (pretty name, True)
    OnlyType -> return (binderType, True)
    NameAndType name -> do
      let annName = annotateInfixOp2 [] minPrecedence id Nothing ":" [pretty name, binderType]
      return (annName, False)

  return $ binderBrackets noExplicitBrackets (visibilityOf binder) binderDoc

compileStdLibFunction :: MonadAgdaCompile m => StdLibFunction -> NonEmpty OutputArg -> m Code
compileStdLibFunction f allArgs = case embedStdLib f allArgs of
  Nothing -> compilerDeveloperError $ "Compilation of stdlib function" <+> quotePretty f <+> "not yet supported"
  Just v -> case v of
    EqualsVector {} -> eqError
    NotEqualsVector {} -> eqError
    EqualsBool {} -> eqError
    NotEqualsBool {} -> eqError
    ExistsVector {} -> quantError
    ForallVector {} -> quantError
    where
      quantError = compilerDeveloperError "Quantifier type-class ops should not have been normalised out."
      eqError = compilerDeveloperError "Equality type-class ops should not have been normalised out."

isTypeClassInAgda :: TypeClassOp -> Bool
isTypeClassInAgda = \case
  AddTC {} -> True
  SubTC {} -> True
  EqualsTC {} -> True
  OrderTC {} -> True
  QuantifierTC {} -> True
  QuantifierInTC {} -> True
  _ -> False

agdaNegInt :: [Code] -> Code
agdaNegInt = annotateInfixOp1 [DataInteger] 6 (Just intQualifier) "-"

agdaPosInt :: [Code] -> Code
agdaPosInt = annotateInfixOp1 [DataInteger] 8 (Just intQualifier) "+"

agdaDivRat :: [Code] -> Code
agdaDivRat = annotateInfixOp2 [DataRat] 7 id (Just ratQualifier) "/"

agdaNatToFin :: [Code] -> Code
agdaNatToFin = annotateInfixOp1 [DataFin] 10 Nothing "#"

compileBuiltin :: MonadAgdaCompile m => Builtin -> [OutputArg] -> m Code
compileBuiltin (TypeClassOp op) allArgs
  | not (isTypeClassInAgda op) = do
      let result = nfTypeClassOp mempty op allArgs
      case result of
        Nothing ->
          compilerDeveloperError $
            "Unable to normalise type-class op:" <+> pretty op
        Just res -> do
          (fn, args) <- res
          compileApp fn args
compileBuiltin op allArgs = case normAppList mempty (Builtin mempty op) allArgs of
  BoolType {} -> compileBooleanType
  NatType {} -> return $ annotateConstant [DataNat] natQualifier
  IntType {} -> return $ annotateConstant [DataInteger] intQualifier
  RatType {} -> return $ annotateConstant [DataRat] ratQualifier
  ListType _ tElem -> annotateApp [DataList] listQualifier <$> traverse compileExpr [tElem]
  VectorType _ tElem tDim -> annotateApp [DataVector] vectorQualifier <$> traverse compileExpr [tElem, tDim]
  IndexType _ size -> annotateApp [DataFin] finQualifier <$> traverse compileExpr [size]
  FromNatExpr _ _n dom args -> compileFromNat dom <$> traverse compileArg (NonEmpty.toList args)
  FromRatExpr _ dom args -> compileFromRat dom <$> traverse compileArg (NonEmpty.toList args)
  FromVecExpr _ _n dom args -> case (dom, args) of
    -- To avoid printing lots of nasty `fromVecs`, we cheat and manually apply it.
    (FromVecToList, [ExplicitArg p (VecLiteral _ tElem xs)]) -> compileExpr (mkList p tElem xs)
    _ -> compileFromVec dom <$> traverse compileArg args
  IfExpr _ _ [e1, e2, e3] -> do
    ce1 <- setBoolLevel BoolLevel $ compileArg e1
    ce2 <- compileArg e2
    ce3 <- compileArg e3
    return $
      annotate
        (Set.singleton DataBool, 0)
        ("if" <+> ce1 <+> "then" <+> ce2 <+> "else" <+> ce3)
  AndExpr _ args -> compileAnd =<< traverse compileArg (NonEmpty.toList args)
  OrExpr _ args -> compileOr =<< traverse compileArg (NonEmpty.toList args)
  ImpliesExpr _ args -> compileImplies =<< traverse compileArg (NonEmpty.toList args)
  NotExpr _ args -> compileNot =<< traverse compileArg (NonEmpty.toList args)
  AddExpr _ dom args -> compileAdd dom <$> traverse compileArg (NonEmpty.toList args)
  SubExpr _ dom args -> compileSub dom <$> traverse compileArg (NonEmpty.toList args)
  MulExpr _ dom args -> compileMul dom <$> traverse compileArg (NonEmpty.toList args)
  DivExpr _ dom args -> compileDiv dom <$> traverse compileArg (NonEmpty.toList args)
  NegExpr _ dom args -> compileNeg dom <$> traverse compileArg (NonEmpty.toList args)
  ForallTCExpr _ binder body -> compileTypeLevelQuantifier Forall [binder] body
  ExistsTCExpr _ binder body -> compileTypeLevelQuantifier Exists [binder] body
  ForeachExpr _ _ _ lam -> compileExpr lam
  ForallInTCExpr p tCont binder body cont -> compileQuantIn Forall tCont (Lam p binder body) cont
  ExistsInTCExpr p tCont binder body cont -> compileQuantIn Exists tCont (Lam p binder body) cont
  OrderTCExpr _ ord t1 _ _ _ args -> compileOrder ord t1 =<< traverse compileArg args
  EqualityTCExpr _ eq t1 _ _ args -> case eq of
    Eq -> compileEquality t1 =<< traverse compileArg args
    Neq -> compileInequality t1 =<< traverse compileArg args
  AddTCExpr _ args -> annotateInfixOp2 [VehicleUtils] 6 id Nothing "⊕" <$> traverse compileArg args
  SubTCExpr _ args -> annotateInfixOp2 [VehicleUtils] 6 id Nothing "⊖" <$> traverse compileArg args
  NilExpr _ _ -> return compileNil
  ConsExpr _ _ args -> compileCons <$> traverse compileArg args
  AtExpr _ _ _ [xs, i] -> compileAt (argExpr xs) (argExpr i)
  HasEqExpr _ _ t _ _ -> compileTypeClass "HasEq" t
  HasOrdExpr _ _ t _ _ -> compileTypeClass "HasOrd" t
  HasSubExpr _ t _ _ -> compileTypeClass "HasSub" t
  HasDivExpr _ t _ _ -> compileTypeClass "HasDiv" t
  HasNegExpr _ t _ -> compileTypeClass "HasNeg" t
  HasNatLitsExpr _ _ t -> compileTypeClass "HasNatLits" t
  HasRatLitsExpr _ t -> compileTypeClass "HasRatLits" t
  HasVecLitsExpr {} ->
    compilerDeveloperError "Compilation of HasVecLits type-class constraint to Agda not yet supported"
  BuiltinTypeClass _ NatInDomainConstraint {} [t] -> compileTypeClass "NatInDomain" (argExpr t)
  _ -> do
    let e = normAppList mempty (Builtin mempty op) allArgs
    compilerDeveloperError $
      "unexpected application of builtin found during compilation to Agda:"
        <+> squotes (prettyVerbose e)
        <+> parens (pretty $ provenanceOf e)

compileTypeClass :: MonadAgdaCompile m => Code -> OutputExpr -> m Code
compileTypeClass name arg = do
  arg' <- compileExpr arg
  return $ annotateApp [] name [arg']

compileTypeLevelQuantifier ::
  MonadAgdaCompile m =>
  Quantifier ->
  NonEmpty OutputBinder ->
  OutputExpr ->
  m Code
compileTypeLevelQuantifier q binders body = do
  cBinders <- traverse compileBinder binders
  cBody <- compileExpr body
  quant <- case q of
    Forall -> return "∀"
    Exists -> return $ annotateConstant [DataProduct] "∃ λ"
  return $ quant <+> hsep cBinders <+> arrow <+> cBody

compileQuantIn :: MonadAgdaCompile m => Quantifier -> OutputExpr -> OutputExpr -> OutputExpr -> m Code
compileQuantIn q tCont fn cont = do
  boolLevel <- getBoolLevel

  (quant, qualifier, dep) <- case (boolLevel, q, tCont) of
    (TypeLevel, Forall, ListType {}) -> return ("All", listQualifier, DataListAll)
    (TypeLevel, Exists, ListType {}) -> return ("Any", listQualifier, DataListAny)
    (TypeLevel, Forall, ITensorType {}) -> return ("All", tensorQualifier, DataTensorAll)
    (TypeLevel, Exists, ITensorType {}) -> return ("Any", tensorQualifier, DataTensorAny)
    (BoolLevel, Forall, ListType {}) -> return ("all", listQualifier, DataList)
    (BoolLevel, Exists, ListType {}) -> return ("any", listQualifier, DataList)
    (BoolLevel, Forall, ITensorType {}) -> return ("all", tensorQualifier, DataTensor)
    (BoolLevel, Exists, ITensorType {}) -> return ("any", tensorQualifier, DataTensor)
    _ -> unexpectedTypeError tCont [pretty List, pretty (identifierName TensorIdent)]

  annotateApp [dep] (qualifier <> "." <> quant) <$> traverse compileExpr [fn, cont]

compileLiteral :: MonadAgdaCompile m => Literal -> m Code
compileLiteral e = case e of
  LUnit -> return $ annotateConstant [DataUnit] "tt"
  LBool b -> compileBoolOp0 b
  LIndex _ n -> return $ compileIndexLiteral (toInteger n)
  LNat n -> return $ compileNatLiteral (toInteger n)
  LInt i -> return $ compileIntLiteral (toInteger i)
  LRat p -> return $ compileRatLiteral p

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
compileVecLiteral :: MonadAgdaCompile m => [OutputExpr] -> m Code
compileVecLiteral = \case
  [] -> return $ annotateConstant [DataVector] "[]ᵥ"
  (x : xs) -> do
    cx <- compileExpr x
    cxs <- compileVecLiteral xs
    return $ annotateInfixOp2 [] 5 id Nothing "∷ᵥ" [cx, cxs]

compileNil :: Code
compileNil = annotateConstant [DataList] "[]"

compileCons :: [Code] -> Code
compileCons = annotateInfixOp2 [DataList] 5 id Nothing "∷"

-- | Compiling boolean constants
compileBoolOp0 :: MonadAgdaCompile m => Bool -> m Code
compileBoolOp0 value = do
  boolLevel <- getBoolLevel
  let (deps, code) = case (value, boolLevel) of
        (True, BoolLevel) -> ([DataBool], "true")
        (True, TypeLevel) -> ([DataUnit], "⊤")
        (False, BoolLevel) -> ([DataBool], "false")
        (False, TypeLevel) -> ([DataEmpty], "⊥")
  return $ annotateConstant deps code

-- | Compiling boolean negation
compileNot :: MonadAgdaCompile m => [Code] -> m Code
compileNot args = do
  boolLevel <- getBoolLevel
  return $ case boolLevel of
    BoolLevel -> annotateApp [DataBool] "not" args
    TypeLevel -> annotateInfixOp1 [RelNullary] 3 Nothing "¬" args

compileAnd :: MonadAgdaCompile m => [Code] -> m Code
compileAnd args = do
  boolLevel <- getBoolLevel
  let (opDoc, precedence, dependencies) = case boolLevel of
        BoolLevel -> ("∧", 6, [DataBool])
        TypeLevel -> ("×", 2, [DataProduct])
  return $ annotateInfixOp2 dependencies precedence id Nothing opDoc args

compileOr :: MonadAgdaCompile m => [Code] -> m Code
compileOr args = do
  boolLevel <- getBoolLevel
  let (opDoc, precedence, dependencies) = case boolLevel of
        BoolLevel -> ("∨", 5, [DataBool])
        TypeLevel -> ("⊎", 1, [DataSum])
  return $ annotateInfixOp2 dependencies precedence id Nothing opDoc args

compileImplies :: MonadAgdaCompile m => [Code] -> m Code
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
        NegInt -> (intQualifier, DataInteger)
        NegRat -> (ratQualifier, DataRat)

  annotateInfixOp1 [dependency] 8 (Just qualifier) "-" args

compileFromNat :: FromNatDomain -> [Code] -> Code
compileFromNat dom args = case dom of
  FromNatToIndex -> agdaNatToFin (tail args)
  FromNatToNat -> head args
  FromNatToInt -> agdaPosInt args
  FromNatToRat -> agdaDivRat [agdaPosInt [head args], "1"]

compileFromRat :: FromRatDomain -> [Code] -> Code
compileFromRat dom args = case dom of
  FromRatToRat -> head args

compileFromVec :: FromVecDomain -> [Code] -> Code
compileFromVec dom args = case dom of
  FromVecToVec -> head args
  FromVecToList -> annotateInfixOp1 [DataVector] 7 (Just vectorQualifier) "toList" args

compileAdd :: AddDomain -> [Code] -> Code
compileAdd dom args = do
  let (qualifier, dependency) = case dom of
        AddNat -> (natQualifier, DataNat)
        AddInt -> (intQualifier, DataInteger)
        AddRat -> (ratQualifier, DataRat)

  annotateInfixOp2 [dependency] 6 id (Just qualifier) "+" args

compileSub :: SubDomain -> [Code] -> Code
compileSub dom args = do
  let (qualifier, dependency) = case dom of
        SubInt -> (intQualifier, DataInteger)
        SubRat -> (ratQualifier, DataRat)

  annotateInfixOp2 [dependency] 6 id (Just qualifier) "-" args

compileMul :: MulDomain -> [Code] -> Code
compileMul mul args = do
  let (qualifier, dependency) = case mul of
        MulNat -> (natQualifier, DataNat)
        MulInt -> (intQualifier, DataInteger)
        MulRat -> (ratQualifier, DataRat)

  annotateInfixOp2 [dependency] 7 id (Just qualifier) "*" args

compileDiv :: DivDomain -> [Code] -> Code
compileDiv dom args = do
  let (qualifier, dependency) = case dom of
        DivRat -> (ratQualifier, DataRat)

  annotateInfixOp2 [dependency] 7 id (Just qualifier) "÷" args

isRatType :: Type binder var -> Bool
isRatType RatType {} = True
isRatType _ = False

compileOrder :: MonadAgdaCompile m => OrderOp -> OutputExpr -> [Code] -> m Code
compileOrder originalOrder elemType originalArgs = do
  boolLevel <- getBoolLevel

  -- HACK because v1.7 of stdlib doesn't have >? and >=?. Fixed in v2.0 so remove when released.
  let (order, args) =
        if isRatType elemType && boolLevel == BoolLevel && (originalOrder == Ge || originalOrder == Gt)
          then (flipOrder originalOrder, reverse originalArgs)
          else (originalOrder, originalArgs)

  (qualifier, elemDep) <- case elemType of
    IndexType {} -> return (finQualifier, DataFin)
    NatType {} -> return (natQualifier, DataNat)
    IntType {} -> return (intQualifier, DataInteger)
    RatType {} -> return (ratQualifier, DataRat)
    _ -> unexpectedTypeError elemType $ map pretty [Nat, Int, Rat, Index]

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

compileAt :: MonadAgdaCompile m => OutputExpr -> OutputExpr -> m Code
compileAt xs i = annotateApp [] <$> compileExpr xs <*> traverse compileExpr [i]

compileEquality :: MonadAgdaCompile m => OutputExpr -> [Code] -> m Code
compileEquality tElem args = do
  boolLevel <- getBoolLevel
  case boolLevel of
    TypeLevel -> return $ annotateInfixOp2 [PropEquality] 4 id Nothing "≡" args
    BoolLevel -> do
      -- Boolean function equality is more complicated as we need an actual decision procedure.
      -- We handle this using instance arguments
      instanceArgDependencies <- equalityDependencies tElem
      return $ annotateInfixOp2 ([RelNullary] <> instanceArgDependencies) 4 boolBraces Nothing "≟" args

compileInequality :: MonadAgdaCompile m => OutputExpr -> [Code] -> m Code
compileInequality tElem args = do
  boolLevel <- getBoolLevel
  case boolLevel of
    TypeLevel -> return $ annotateInfixOp2 [PropEquality] 4 id Nothing "≢" args
    BoolLevel -> do
      eq <- compileEquality tElem args
      compileNot [eq]

compileFunDef :: Code -> Code -> [Code] -> Code -> Code
compileFunDef n t ns e =
  n
    <+> ":"
    <+> align t
      <> line
      <> n
    <+> (if null ns then mempty else hsep ns <> " ") <> "="
    <+> e

-- | Compile a `network` declaration
compilePostulate :: Code -> Code -> Code
compilePostulate name t =
  "postulate" <+> name <+> ":" <+> align t

compileProperty :: MonadAgdaCompile m => Code -> Code -> m Code
compileProperty propertyName propertyBody = do
  proofCache <- getProofCacheLocation
  return $
    case proofCache of
      Nothing ->
        "postulate" <+> propertyName <+> ":" <+> align propertyBody
      Just loc ->
        scopeCode "abstract" $
          propertyName
            <+> ":"
            <+> align propertyBody
              <> line
              <> propertyName
            <+> "= checkSpecification record"
              <> line
              <> indentCode
                ( "{ proofCache   ="
                    <+> dquotes (pretty loc)
                      <> line
                      <> "}"
                )

-- Calculates the dependencies needed for equality over the provided type
equalityDependencies :: MonadAgdaCompile m => OutputExpr -> m [Dependency]
equalityDependencies = \case
  NatType _ -> return [DataNatInstances]
  IntType _ -> return [DataIntegerInstances]
  BoolType _ -> return [DataBoolInstances]
  ListType _ tElem -> do
    deps <- equalityDependencies tElem
    return $ [DataListInstances] <> deps
  VectorType _ tElem _tDims -> do
    deps <- equalityDependencies tElem
    return $ [DataVectorInstances] <> deps
  ITensorType _ tElem _tDims -> do
    deps <- equalityDependencies tElem
    return $ [DataTensorInstances] <> deps
  Var p n -> throwError $ UnsupportedPolymorphicEquality AgdaBackend p n
  t -> unexpectedTypeError t (map pretty [Bool, Nat, Int, List, Vector] <> [pretty (identifierName TensorIdent)])

unexpectedTypeError :: MonadCompile m => OutputExpr -> [Doc ()] -> m a
unexpectedTypeError actualType expectedTypes =
  compilerDeveloperError $
    "Unexpected type found."
      <+> "Was expecting one of"
      <+> list expectedTypes
      <+> "but found"
      <+> prettyVerbose actualType
      <+> "at"
      <+> pretty (provenanceOf actualType) <> "."

currentPhase :: Doc ()
currentPhase = "compilation to Agda"
