module Vehicle.Backend.Agda.Compile
  ( AgdaOptions(..)
  , compileProgToAgda
  ) where

import GHC.Real (numerator, denominator)
import Control.Monad.Except (MonadError(..))
import Control.Monad.Reader (MonadReader(..), runReaderT, asks)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Foldable (fold)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.List (sort)
import System.FilePath (takeBaseName)
import Prettyprinter hiding (hsep, vsep, hcat, vcat)

import Vehicle.Language.Print
import Vehicle.Language.Sugar
import Vehicle.Compile.Prelude hiding (CompileOptions(..))
import Vehicle.Compile.Error
import Vehicle.Compile.CapitaliseTypeNames (capitaliseTypeNames)
import Vehicle.Compile.SupplyNames (supplyDBNames)
import Vehicle.Compile.Descope (runDescopeProg)
import Vehicle.Backend.Prelude


--------------------------------------------------------------------------------
-- Agda-specific options

data AgdaOptions = AgdaOptions
  { proofCacheLocation  :: Maybe FilePath
  , outputFile          :: Maybe FilePath
  , modulePrefix        :: Maybe String
  }

compileProgToAgda :: MonadCompile m => AgdaOptions -> CheckedProg -> m (Doc a)
compileProgToAgda options prog1 = logCompilerPass MinDetail currentPhase $
  flip runReaderT (options, BoolLevel) $ do
    let prog2 = capitaliseTypeNames prog1
    let prog3 = supplyDBNames prog2
    let prog4 = runDescopeProg prog3
    programDoc <- compileProg prog4
    let programStream = layoutPretty defaultLayoutOptions programDoc
    -- Collects dependencies by first discarding precedence info and then
    -- folding using Set Monoid
    let progamDependencies = fold (reAnnotateS fst programStream)

    let baseModule = maybe "Spec" takeBaseName (outputFile options)
    let moduleName = Text.pack $ maybe "" (<> ".") (modulePrefix options) <> baseModule
    return $ unAnnotate ((vsep2 :: [Code] -> Code)
      [ optionStatements ["allow-exec"]
      , importStatements progamDependencies
      , moduleHeader moduleName
      , programDoc
      ])

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

-- |All possible Agda modules the program may depend on.
data Dependency
  -- Vehicle Agda library (hopefully will migrate these with time)
  = VehicleCore
  | VehicleUtils
  | DataTensor
  | DataTensorInstances
  | DataTensorAll
  | DataTensorAny
  -- Standard library
  | DataUnit
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
  | FunctionBase
  | PropEquality
  | RelNullary
  | RelNullaryDecidable
  deriving (Eq, Ord)

instance Pretty Dependency where
  pretty = \case
    VehicleCore          -> "Vehicle"
    VehicleUtils         -> "Vehicle.Utils"
    DataTensor           -> "Vehicle.Data.Tensor"
    DataTensorInstances  -> "Vehicle.Data.Tensor.Instances"
    DataTensorAll        -> "Vehicle.Data.Tensor.Relation.Unary.All as" <+> tensorQualifier
    DataTensorAny        -> "Vehicle.Data.Tensor.Relation.Unary.Any as" <+> tensorQualifier
    DataUnit             -> "Data.Unit"
    DataEmpty            -> "Data.Empty"
    DataProduct          -> "Data.Product"
    DataSum              -> "Data.Sum"
    DataNat              -> "Data.Nat as" <+> natQualifier <+> "using" <+> parens "ℕ"
    DataNatInstances     -> "Data.Nat.Instances"
    DataNatDivMod        -> "Data.Nat.DivMod as" <+> natQualifier
    DataInteger          -> "Data.Integer as" <+> intQualifier <+> "using" <+> parens "ℤ"
    DataIntegerInstances -> "Data.Integer.Instances"
    DataIntegerDivMod    -> "Data.Int.DivMod as" <+> intQualifier
    DataRat              -> "Data.Rational as" <+> ratQualifier <+> "using" <+> parens "ℚ"
    DataRatInstances     -> "Data.Rational.Instances"
    DataBool             -> "Data.Bool as" <+> boolQualifier <+> "using" <+> parens "Bool; true; false; if_then_else_"
    DataBoolInstances    -> "Data.Bool.Instances"
    DataFin              -> "Data.Fin as" <+> finQualifier <+> "using" <+> parens "Fin; #_"
    DataList             -> "Data.List"
    DataListInstances    -> "Data.List.Instances"
    DataListAll          -> "Data.List.Relation.Unary.All as" <+> listQualifier
    DataListAny          -> "Data.List.Relation.Unary.Any as" <+> listQualifier
    FunctionBase         -> "Function.Base"
    PropEquality         -> "Relation.Binary.PropositionalEquality"
    RelNullary           -> "Relation.Nullary"
    RelNullaryDecidable  -> "Relation.Nullary.Decidable"

optionStatement :: Text -> Doc a
optionStatement option = "{-# OPTIONS --" <> pretty option <+> "#-}"

optionStatements :: [Text] -> Doc a
optionStatements = vsep . map optionStatement

importStatement :: Dependency -> Doc a
importStatement dep = "open import" <+> pretty dep

importStatements :: Set Dependency -> Doc a
importStatements deps = vsep $ map importStatement dependencies
  where dependencies = sort (VehicleCore : Set.toList deps)

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
tensorQualifier = "Tensor"

indentCode :: Code -> Code
indentCode = indent 2

scopeCode :: Code -> Code -> Code
scopeCode keyword code = keyword <> line <> indentCode code

--------------------------------------------------------------------------------
-- Intermediate results of compilation

-- | Marks if the current boolean expression is compiled to `Set` or `Bool`
data BoolLevel = TypeLevel | BoolLevel

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
annotateApp dependencies fun [] = annotateConstant dependencies fun
annotateApp dependencies fun args =
  let precedence = 20 in
  let bracketedArgs = map (bracketIfRequired precedence) args in
  annotate (Set.fromList dependencies, precedence) (hsep (fun : bracketedArgs))

annotateInfixOp1 :: [Dependency]
                 -> Precedence
                 -> Maybe Code
                 -> Code
                 -> [Code]
                 -> Code
annotateInfixOp1 dependencies precedence qualifier op args = result
  where
    bracketedArgs = map (bracketIfRequired precedence) args
    qualifierDoc  = maybe "" (<> ".") qualifier
    doc = case bracketedArgs of
      []       -> qualifierDoc <> op <> "_"
      [e1]     -> qualifierDoc <> op <+> e1
      _        -> developerError $ "was expecting no more than 1 argument for" <+> op <+>
                                   "but found the following arguments:" <+> list args
    result = annotate (Set.fromList dependencies, precedence) doc

annotateInfixOp2 :: [Dependency]
                 -> Precedence
                 -> (Code -> Code)
                 -> Maybe Code
                 -> Code
                 -> [Code]
                 -> Code
annotateInfixOp2 dependencies precedence opBraces qualifier op args = result
  where
    bracketedArgs = map (bracketIfRequired precedence) args
    qualifierDoc  = maybe "" (<> ".") qualifier
    doc = case bracketedArgs of
      []       -> qualifierDoc <> "_" <> op <> "_"
      [e1]     -> e1 <+> qualifierDoc <> op <> "_"
      [e1, e2] -> e1 <+> qualifierDoc <> op <+> e2
      _        -> developerError $ "was expecting no more than 2 arguments for" <+> op <+>
                                   "but found the following arguments:" <+> list args
    result = annotate (Set.fromList dependencies, precedence) (opBraces doc)


bracketIfRequired :: Precedence -> Code -> Code
bracketIfRequired parentPrecedence expr =
  if getPrecedence expr <= parentPrecedence
    then parens expr
    else expr

argBrackets :: Visibility -> Code -> Code
argBrackets Explicit = id
argBrackets Implicit = braces
argBrackets Instance = braces . braces

binderBrackets :: Bool -> Visibility -> Code -> Code
binderBrackets True      Explicit = id
binderBrackets False     Explicit = parens
binderBrackets _topLevel Implicit = braces
binderBrackets _topLevel Instance = braces . braces

boolBraces :: Code -> Code
boolBraces c = annotateConstant [RelNullaryDecidable] "⌊" <+> c <+> "⌋"

arrow :: Code
arrow = "→" -- <> softline'

--------------------------------------------------------------------------------
-- Monad stack

type MonadAgdaCompile m =
  ( MonadCompile m
  , MonadReader (AgdaOptions, BoolLevel) m
  )

getBoolLevel :: MonadAgdaCompile m => m BoolLevel
getBoolLevel = asks snd

setBoolLevel :: MonadAgdaCompile m => BoolLevel -> m a -> m a
setBoolLevel level = local (\(opts, _) -> (opts, level))

--------------------------------------------------------------------------------
-- Program Compilation

compileProg :: MonadAgdaCompile m => OutputProg -> m Code
compileProg (Main ds) = vsep2 <$> traverse compileDecl ds

compileDecl :: MonadAgdaCompile m => OutputDecl -> m Code
compileDecl = \case
  DefResource _ _ n t ->
    compilePostulate (compileIdentifier n) <$> compileExpr t

  DefPostulate _ n t ->
    compilePostulate (compileIdentifier n) <$> compileExpr t

  DefFunction _ann propertyInfo n t e -> do
    let (binders, body) = foldLam e
    setBoolLevel TypeLevel $
      if isProperty propertyInfo
        then compileProperty (compileIdentifier n) =<< compileExpr e
        else do
          let binders' = mapMaybe compileTopLevelBinder binders
          compileFunDef (compileIdentifier n) <$> compileExpr t <*> pure binders' <*> compileExpr body

compileExpr :: MonadAgdaCompile m => OutputExpr -> m Code
compileExpr expr = do
  logEntry expr
  result <- case expr of
    Hole{}     -> resolutionError currentPhase "Hole"
    Meta{}     -> resolutionError currentPhase "Meta"
    PrimDict{} -> typeError       currentPhase "PrimDict"

    Universe _ u -> case u of
      TypeUniv l -> return $ compileType l
      _          -> resolutionError currentPhase (pretty u)

    Var  _ n   -> return $ annotateConstant [] (pretty n)

    Pi ann binder result -> case foldPi ann binder result of
      Left (binders, body)  -> compileTypeLevelQuantifier Forall binders body
      Right (input, output) ->
        annotateInfixOp2 [] minPrecedence id Nothing arrow <$> traverse compileExpr [input, output]

    Ann _ann e t -> compileAnn <$> compileExpr e <*> compileExpr t

    Let _ann bound binding body -> do
      cBoundExpr <- compileLetBinder (binding, bound)
      cBody      <- compileExpr body
      return $ "let" <+> cBoundExpr <+> "in" <+> cBody
      {-
      -- TODO re-enable let folding - Agda separates by whitespace though so it's complicated.
      let (boundExprs, body) = foldLet expr
      cBoundExprs <- traverse compile boundExprs
      cBody       <- compile body
      return $ "let" <+> vsep (punctuate ";" cBoundExprs) <+> "in" <+> cBody
      -}

    Lam{} -> do
      let (binders, body) = foldLam expr
      cBinders <- traverse compileBinder binders
      cBody    <- compileExpr body
      return $ annotate (mempty, minPrecedence) ("λ" <+> hsep cBinders <+> arrow <+> cBody)

    Builtin{} -> compileBuiltin expr
    Literal{} -> compileLiteral expr

    SeqExpr ann _ tCont xs -> compileSeq ann tCont xs
    LSeq{} -> caseError currentPhase "LSeq" ["SeqExpr"]

    App _ fun args -> case fun of
      Builtin{}    -> compileBuiltin expr
      Literal{}    -> compileLiteral expr
      _            -> do
        cFun   <- compileExpr fun
        cArgs  <- traverse compileExpr (filterOutNonExplicitArgs args)
        return $ annotateApp [] cFun cArgs

  logExit result
  return result

compileLetBinder :: MonadAgdaCompile m
                 => LetBinder OutputBinding OutputVar
                 -> m Code
compileLetBinder (binder, expr) = do
  let binderName = pretty (nameOf binder :: OutputBinding)
  cExpr <- compileExpr expr
  return $ binderName <+> "=" <+> cExpr

compileArg :: MonadAgdaCompile m => OutputArg -> m Code
compileArg arg = argBrackets (visibilityOf arg) <$> compileExpr (argExpr arg)

compileBooleanType :: MonadAgdaCompile m => m Code
compileBooleanType = do
  boolLevel <- getBoolLevel
  return $ case boolLevel of
    TypeLevel -> compileType 0
    BoolLevel -> annotateConstant [DataBool] "Bool"

compileIdentifier :: Identifier -> Code
compileIdentifier ident = pretty (nameOf ident :: Symbol)

compileType :: UniverseLevel -> Code
compileType 0 = "Set"
compileType l = annotateConstant [] ("Set" <> pretty l)

compileTopLevelBinder :: OutputBinder -> Maybe Code
compileTopLevelBinder binder
  | visibilityOf binder /= Explicit = Nothing
  | otherwise = do
    let binderName = pretty (nameOf binder :: OutputBinding)
    let addBrackets = binderBrackets True (visibilityOf binder)
    Just $ addBrackets binderName

compileBinder :: MonadAgdaCompile m => OutputBinder -> m Code
compileBinder binder = do
  let binderName = pretty (nameOf binder :: OutputBinding)
  let addBrackets = binderBrackets False (visibilityOf binder)
  binderType <- compileExpr (typeOf binder)
  let annBinder = annotateInfixOp2 [] minPrecedence id Nothing ":" [binderName, binderType]
  return $ addBrackets annBinder

compileBuiltin :: MonadAgdaCompile m => OutputExpr -> m Code
compileBuiltin e = case e of
  BoolType{} -> compileBooleanType
  NatType{}  -> return $ annotateConstant [DataNat] natQualifier
  IntType{}  -> return $ annotateConstant [DataInteger] intQualifier
  RatType{}  -> return $ annotateConstant [DataRat] ratQualifier

  ListType   _ tElem       -> annotateApp [DataList]   listQualifier   <$> traverse compileExpr [tElem]
  TensorType _ tElem tDims -> annotateApp [DataTensor] tensorQualifier <$> traverse compileExpr [tElem, tDims]
  IndexType  _ size        -> annotateApp [DataFin]    finQualifier    <$> traverse compileExpr [size]

  IfExpr _ _ [e1, e2, e3] -> do
    ce1 <- setBoolLevel BoolLevel $ compileArg e1
    ce2 <- compileArg e2
    ce3 <- compileArg e3
    return $ annotate (Set.singleton DataBool, 0)
      ("if" <+> ce1 <+> "then" <+> ce2 <+> "else" <+> ce3)

  AndExpr     _       args -> compileAnd     =<< traverse compileArg args
  OrExpr      _       args -> compileOr      =<< traverse compileArg args
  ImpliesExpr _       args -> compileImplies =<< traverse compileArg args
  NotExpr     _       args -> compileNot     =<< traverse compileArg args
  AddExpr     _ t _ _ args -> compileAdd t   =<< traverse compileArg args
  SubExpr     _ t _ _ args -> compileSub t   =<< traverse compileArg args
  MulExpr     _ t _ _ args -> compileMul t   =<< traverse compileArg args
  DivExpr     _ t _ _ args -> compileDiv t   =<< traverse compileArg args
  NegExpr     _ t _   args -> compileNeg t   =<< traverse compileArg args

  (ForallExpr  _  binder body) -> compileTypeLevelQuantifier Forall [binder] body
  (ExistsExpr  _  binder body) -> compileTypeLevelQuantifier Exists [binder] body
  (ForeachExpr ann _ _)        -> throwError $ UnsupportedBuiltin AgdaBackend ann Foreach

  (ForallInExpr  ann tCont binder body cont) -> compileQuantIn Forall tCont (Lam ann binder body) cont
  (ExistsInExpr  ann tCont binder body cont) -> compileQuantIn Exists tCont (Lam ann binder body) cont
  (ForeachInExpr ann _ _ _       _    _)     -> throwError $ UnsupportedBuiltin AgdaBackend ann ForeachIn

  (OrderExpr    _ ord t1 args) -> compileOrder ord  t1 =<< traverse compileArg args
  (EqualityExpr _ Eq  t1 args) -> compileEquality   t1 =<< traverse compileArg args
  (EqualityExpr _ Neq t1 args) -> compileInequality t1 =<< traverse compileArg args

  (ConsExpr _ tElem               args) -> compileCons tElem =<< traverse compileArg args
  (AtExpr ann _tElem _tDim _tDims args) -> compileAt ann (map argExpr args)

  HasEqExpr  _ _ t _ _ -> compileTypeClass "HasEq"  t
  HasOrdExpr _ _ t _ _ -> compileTypeClass "HasOrd" t
  HasAddExpr _ t _ _   -> compileTypeClass "HasAdd" t
  HasMulExpr _ t _ _   -> compileTypeClass "HasMul" t
  HasSubExpr _ t _ _   -> compileTypeClass "HasSub" t
  HasDivExpr _ t _ _   -> compileTypeClass "HasDiv" t
  HasNegExpr _ t _     -> compileTypeClass "HasNeg" t

  HasNatLitsUpToExpr   _   _ t -> compileTypeClass "HasNatLits" t
  HasIntLitsExpr           _ t -> compileTypeClass "HasIntLits" t
  HasRatLitsExpr           _ t -> compileTypeClass "HasRatLits" t

  HasConLitsOfSizeExpr _ n _ _ -> throwError $ UnsupportedBuiltin AgdaBackend (provenanceOf e) (TypeClass (HasConLitsOfSize n))
  MapExpr{}                    -> throwError $ UnsupportedBuiltin AgdaBackend (provenanceOf e) Map
  FoldExpr{}                   -> throwError $ UnsupportedBuiltin AgdaBackend (provenanceOf e) Fold

  _ -> compilerDeveloperError $
    "unexpected application of builtin found during compilation to Agda:" <+>
    squotes (prettyVerbose e) <+> parens (pretty $ provenanceOf e)

compileAnn :: Code -> Code -> Code
compileAnn e t = annotateInfixOp2 [FunctionBase] 0 id Nothing "∋" [t,e]

compileTypeLevelQuantifier :: MonadAgdaCompile m
                           => Quantifier
                           -> [OutputBinder]
                           -> OutputExpr
                           -> m Code
compileTypeLevelQuantifier q binders body = do
  cBinders  <- traverse compileBinder binders
  cBody     <- compileExpr body
  quant     <- case q of
    Forall  -> return "∀"
    Exists  -> return $ annotateConstant [DataProduct] "∃ λ"
  return $ quant <+> hsep cBinders <+> arrow <+> cBody

compileQuantIn :: MonadAgdaCompile m => Quantifier -> OutputExpr -> OutputExpr -> OutputExpr -> m Code
compileQuantIn q tCont fn cont = do
  boolLevel <- getBoolLevel

  (quant, qualifier, dep) <- case (boolLevel, q, tCont) of
    (TypeLevel, Forall, ListType{})   -> return ("All", listQualifier,   DataListAll)
    (TypeLevel, Exists, ListType{})   -> return ("Any", listQualifier,   DataListAny)
    (TypeLevel, Forall, TensorType{}) -> return ("All", tensorQualifier, DataTensorAll)
    (TypeLevel, Exists, TensorType{}) -> return ("Any", tensorQualifier, DataTensorAny)
    (BoolLevel, Forall, ListType{})   -> return ("all", listQualifier,   DataList)
    (BoolLevel, Exists, ListType{})   -> return ("any", listQualifier,   DataList)
    (BoolLevel, Forall, TensorType{}) -> return ("all", tensorQualifier, DataTensor)
    (BoolLevel, Exists, TensorType{}) -> return ("any", tensorQualifier, DataTensor)
    _                                 -> unexpectedTypeError tCont [List, Tensor]

  annotateApp [dep] (qualifier <> "." <> quant) <$> traverse compileExpr [fn, cont]

compileLiteral :: MonadAgdaCompile m => OutputExpr -> m Code
compileLiteral e = case e of
  BoolLiteralExpr _ b             -> compileBoolOp0 b
  NatLiteralExpr  _ IndexType{} n -> return $ compileIndexLiteral (toInteger n)
  NatLiteralExpr  _ NatType{}   n -> return $ compileNatLiteral   (toInteger n)
  NatLiteralExpr  _ IntType{}   n -> return $ compileIntLiteral   (toInteger n)
  NatLiteralExpr  _ RatType{}   n -> return $ compileRatLiteral   (toRational n)
  IntLiteralExpr  _ IntType{}   i -> return $ compileIntLiteral   (toInteger i)
  IntLiteralExpr  _ RatType{}   i -> return $ compileRatLiteral   (toRational i)
  RatLiteralExpr  _ RatType{}   p -> return $ compileRatLiteral   p
  -- Literals using type-classes
  App _ (Literal _ l) [ImplicitArg _ Var{}, _] -> case l of
    LNat  n -> return $ compileNatLiteral (toInteger n)
    LInt  n -> return $ compileIntLiteral (toInteger n)
    LRat  n -> return $ compileRatLiteral (toRational n)
    LUnit   -> compilerDeveloperError "Should not have non-concrete Unit literals"
    LBool _ -> compilerDeveloperError "Should not have non-concrete Bool literals"
  _                                  -> compilerDeveloperError $
    "unexpected literal" <+> squotes (pretty $ show e) <+>
    "found during compilation to Agda"
-- App (Literal (LNat 3))
--   (Arg Implicit (Var "_x2") :| [Arg Instance (PrimDict (Var "_x3"))])

compileTypeClass :: MonadAgdaCompile m => Code -> OutputExpr -> m Code
compileTypeClass name arg = do
  arg' <- compileExpr arg
  return $ annotateApp [] name [arg']

compileIndexLiteral :: Integer -> Code
compileIndexLiteral i = annotateInfixOp1 [DataFin] 10 Nothing "#" [pretty i]

compileNatLiteral :: Integer -> Code
compileNatLiteral = pretty

compileIntLiteral :: Integer -> Code
compileIntLiteral i
  | i >= 0    = annotateInfixOp1 [DataInteger] 8 (Just intQualifier) "+" [pretty i]
  | otherwise = annotateInfixOp1 [DataInteger] 6 (Just intQualifier) "-" [compileIntLiteral (- i)]

compileRatLiteral :: Rational -> Code
compileRatLiteral r = annotateInfixOp2 [DataRat] 7 id
  (Just ratQualifier) "/"
  [ compileIntLiteral (numerator r)
  , compileNatLiteral (denominator r)
  ]

-- |Compiling sequences. No sequences in Agda so have to go via cons.
compileSeq :: MonadAgdaCompile m => Provenance -> OutputExpr -> [OutputExpr] -> m Code
compileSeq _ t = go
  where
    go :: MonadAgdaCompile m => [OutputExpr] -> m Code
    go []       = do
      dep <- case t of
        ListType{}   -> return DataList
        TensorType{} -> return DataTensor
        _            -> unexpectedTypeError t [List, Tensor]
      return $ annotateConstant [dep] "[]"
    go (x : xs) = do
      cx  <- compileExpr x
      cxs <- go xs
      return $ annotateInfixOp2 [] 5 id Nothing "∷" [cx , cxs]

compileCons :: MonadCompile m => OutputExpr -> [Code] -> m Code
compileCons t args = do
  (qualifier, dep) <- case t of
    ListType{}   -> return (listQualifier, DataList)
    TensorType{} -> return (tensorQualifier, DataTensor)
    _            -> unexpectedTypeError t [List, Tensor]

  return $ annotateInfixOp2 [dep] 5 id (Just qualifier) "∷" args

-- |Compiling boolean constants
compileBoolOp0 :: MonadAgdaCompile m => Bool -> m Code
compileBoolOp0 value = do
  boolLevel <- getBoolLevel
  let (deps, code) = case (value, boolLevel) of
        (True,  BoolLevel) -> ([DataBool],  "true")
        (True,  TypeLevel) -> ([DataUnit],  "⊤")
        (False, BoolLevel) -> ([DataBool],  "false")
        (False, TypeLevel) -> ([DataEmpty], "⊥")
  return $ annotateConstant deps code

-- |Compiling boolean negation
compileNot :: MonadAgdaCompile m => [Code] -> m Code
compileNot args = do
  boolLevel <- getBoolLevel
  return $ case boolLevel of
    BoolLevel -> annotateApp      [DataBool] "not" args
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
        BoolLevel -> ("∨", 5,  [DataBool])
        TypeLevel -> ("⊎", 1,  [DataSum])
  return $ annotateInfixOp2 dependencies precedence id Nothing opDoc args

compileImplies :: MonadAgdaCompile m => [Code] -> m Code
compileImplies args = do
  boolLevel <- getBoolLevel
  let (opDoc, precedence, dependencies) = case boolLevel of
        BoolLevel -> ("⇒", 4,  [VehicleUtils])
        TypeLevel -> (arrow, minPrecedence, [])
  return $ annotateInfixOp2 dependencies precedence id Nothing opDoc args

-- |Compiling numeric unary operations
compileNeg :: MonadAgdaCompile m => OutputExpr -> [Code] -> m Code
compileNeg t args = case t of
  NatType{} ->
    compilerDeveloperError "Negation is not supported for naturals"
  IntType{} ->
    return $ annotateInfixOp1 [DataInteger] 8 (Just intQualifier) "-" args
  RatType{} ->
    return $ annotateInfixOp1 [DataRat] 8 (Just ratQualifier) "-" args
  Var{} -> do
    let deps      = []
    let qualifier = Nothing
    return $ annotateInfixOp1 deps 8 qualifier "-" args
  _ ->
    compilerDeveloperError $ "Unexpected numeric op1 type" <+> prettyVerbose t

compileAdd :: MonadAgdaCompile m => OutputExpr -> [Code] -> m Code
compileAdd t args = do
  (qualifier, dependency) <- case t of
    NatType{} -> return (natQualifier, DataNat)
    IntType{} -> return (intQualifier, DataInteger)
    RatType{} -> return (ratQualifier, DataRat)
    _         -> unexpectedTypeError t [Nat, Int, Rat]

  return $ annotateInfixOp2 [dependency] 6 id (Just qualifier) "+" args

compileSub :: MonadAgdaCompile m => OutputExpr -> [Code] -> m Code
compileSub t args = do
  (qualifier, dependency) <- case t of
    IntType{} -> return (intQualifier, DataInteger)
    RatType{} -> return (ratQualifier, DataRat)
    _         -> unexpectedTypeError t [Int, Rat]

  return $ annotateInfixOp2 [dependency] 6 id (Just qualifier) "-" args

compileMul :: MonadAgdaCompile m => OutputExpr -> [Code] -> m Code
compileMul t args = do
  (qualifier, dependency) <- case t of
    NatType{} -> return (natQualifier, DataNat)
    IntType{} -> return (intQualifier, DataInteger)
    RatType{} -> return (ratQualifier, DataRat)
    _         -> unexpectedTypeError t [Nat, Int, Rat]

  return $ annotateInfixOp2 [dependency] 7 id (Just qualifier) "*" args

compileDiv :: MonadAgdaCompile m => OutputExpr -> [Code] -> m Code
compileDiv t args = do
  (qualifier, dependency) <- case t of
    RatType{} -> return (ratQualifier, DataRat)
    _         -> unexpectedTypeError t [Nat, Int, Rat]

  return $ annotateInfixOp2 [dependency] 7 id (Just qualifier) "÷" args

compileOrder :: MonadAgdaCompile m => Order -> OutputExpr -> [Code] -> m Code
compileOrder order elemType args = do
  boolLevel <- getBoolLevel

  (qualifier, elemDep) <- case elemType of
    IndexType{} -> return (finQualifier, DataFin)
    NatType{}   -> return (natQualifier, DataNat)
    IntType{}   -> return (intQualifier, DataInteger)
    RatType{}   -> return (ratQualifier, DataRat)
    _           -> unexpectedTypeError elemType [Nat, Int, Rat, Index]

  let (boolDecDoc, boolDeps, opBraces) = case boolLevel of
        BoolLevel -> ("?", [RelNullary], boolBraces)
        TypeLevel -> ("" , [], id)

  let orderDoc = case order of
        Le -> "≤"
        Lt -> "<"
        Ge -> "≥"
        Gt -> ">"

  let dependencies = [elemDep] <> boolDeps
  let opDoc        = orderDoc <> boolDecDoc
  return $ annotateInfixOp2 dependencies 4 opBraces (Just qualifier) opDoc args

compileAt :: MonadAgdaCompile m => Provenance -> [OutputExpr] -> m Code
compileAt _ [tensorExpr, indexExpr] =
  annotateApp [] <$> compileExpr tensorExpr <*> traverse compileExpr [indexExpr]
compileAt ann args =
  unexpectedArgsError (Builtin ann At) args ["tensor", "index"]

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
  n <+> ":" <+> align t <> line <>
  n <+> (if null ns then mempty else hsep ns <> " ") <> "=" <+> e

-- |Compile a `network` declaration
compilePostulate :: Code -> Code -> Code
compilePostulate name t =
  "postulate" <+> name <+> ":" <+> align t

compileProperty :: MonadAgdaCompile m => Code -> Code -> m Code
compileProperty propertyName propertyBody = do
  proofCache <- asks (proofCacheLocation . fst)
  return $
    case proofCache of
      Nothing  ->
        "postulate" <+> propertyName <+> ":" <+> align propertyBody
      Just loc ->
        scopeCode "abstract" $
          propertyName <+> ":" <+> align propertyBody          <> line <>
          propertyName <+> "= checkSpecification record"       <> line <>
            indentCode (
            "{ proofCache   =" <+> dquotes (pretty loc) <> line <>
            "}")



-- Calculates the dependencies needed for equality over the provided type
equalityDependencies :: MonadAgdaCompile m => OutputExpr -> m [Dependency]
equalityDependencies = \case
  NatType  _ -> return [DataNatInstances]
  IntType  _ -> return [DataIntegerInstances]
  BoolType _ -> return [DataBoolInstances]
  App _ (Builtin _ List)   [tElem] -> do
    deps <- equalityDependencies (argExpr tElem)
    return $ [DataListInstances] <> deps
  App _ (Builtin _ Tensor) [tElem, _tDims] -> do
    deps <- equalityDependencies (argExpr tElem)
    return $ [DataTensorInstances] <> deps
  Var ann n -> throwError $ UnsupportedPolymorphicEquality AgdaBackend (provenanceOf ann) n
  t         -> unexpectedTypeError t [Bool, Nat, Int, List, Tensor]

unexpectedTypeError :: MonadCompile m => OutputExpr -> [Builtin] -> m a
unexpectedTypeError actualType expectedTypes = compilerDeveloperError $
  "Unexpected type found." <+>
  "Was expecting one of" <+> pretty expectedTypes <+>
  "but found" <+> prettyFriendly actualType <+>
  "at" <+> pretty (provenanceOf actualType) <> "."

unexpectedArgsError :: MonadCompile m => OutputExpr -> [OutputExpr] -> [String] -> m a
unexpectedArgsError fun actualArgs expectedArgs = compilerDeveloperError $
  "The function" <+> prettyFriendly fun <+> "was expected to have arguments" <+>
  "of the following form" <+> squotes (pretty expectedArgs) <+> "but found" <+>
  "the following" <+> squotes (prettyFriendly actualArgs) <+>
  "at" <+> pretty (provenanceOf fun) <> "."

currentPhase :: Doc ()
currentPhase = "compilation to Agda"
