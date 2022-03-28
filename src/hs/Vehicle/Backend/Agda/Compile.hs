module Vehicle.Backend.Agda.Compile
  ( AgdaOptions(..)
  , compileProgToAgda
  ) where

import GHC.Real (numerator, denominator)
import Control.Monad.Except (MonadError(..))
import Control.Monad.Reader (MonadReader(..), runReaderT, asks)
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Text (Text)
import Data.Foldable (fold)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map as Map (Map)
import Data.List (sort)
import Prettyprinter hiding (hsep, vsep, hcat, vcat)

import Vehicle.Language.Print
import Vehicle.Language.Sugar
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.CapitaliseTypeNames (capitaliseTypeNames)
import Vehicle.Compile.SupplyNames (supplyDBNames)
import Vehicle.Compile.Descope (runDescopeProg)
import Vehicle.Backend.Prelude

compileProgToAgda :: MonadCompile m => AgdaOptions -> CheckedProg -> m (Doc a)
compileProgToAgda options prog1 = flip runReaderT options $ do
  let prog2 = capitaliseTypeNames prog1
  let prog3 = supplyDBNames prog2
  let prog4 = runDescopeProg prog3
  programDoc <- compileProg prog4
  let programStream = layoutPretty defaultLayoutOptions programDoc
  -- Collects dependencies by first discarding precedence info and then
  -- folding using Set Monoid
  let progamDependencies = fold (reAnnotateS fst programStream)
  return $ unAnnotate ((vsep2 :: [Code] -> Code)
    [ optionStatements ["allow-exec"]
    , importStatements progamDependencies
    , moduleHeader (moduleName options)
    , programDoc
    ])

--------------------------------------------------------------------------------
-- Agda-specific options

data AgdaOptions = AgdaOptions
  { proofCacheLocation  :: FilePath
  , moduleName          :: Text
  , vehicleUIDs         :: Map.Map Identifier Text
  }

type Precedence = Int

--------------------------------------------------------------------------------
-- Debug functions

logEntry :: MonadAgdaCompile m => OutputExpr -> m ()
logEntry e = do
  incrCallDepth
  logDebug $ "compile-entry" <+> prettySimple e

logExit :: MonadAgdaCompile m => Code -> m ()
logExit e = do
  logDebug $ "compile-exit " <+> e
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
  | DataReal
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
    DataTensorAll        -> "Vehicle.Data.Tensor.Relation.Unary.All as" <+> containerQualifier Tensor
    DataTensorAny        -> "Vehicle.Data.Tensor.Relation.Unary.Any as" <+> containerQualifier Tensor
    DataUnit             -> "Data.Unit"
    DataEmpty            -> "Data.Empty"
    DataProduct          -> "Data.Product"
    DataSum              -> "Data.Sum"
    DataNat              -> "Data.Nat as" <+> numericQualifier Nat <+> "using" <+> parens "ℕ"
    DataNatInstances     -> "Data.Nat.Instances"
    DataNatDivMod        -> "Data.Nat.DivMod as" <+> numericQualifier Nat
    DataInteger          -> "Data.Integer as" <+> numericQualifier Int <+> "using" <+> parens "ℤ"
    DataIntegerInstances -> "Data.Integer.Instances"
    DataIntegerDivMod    -> "Data.Int.DivMod as" <+> numericQualifier Int
    DataRat              -> "Data.Rational as" <+> numericQualifier Rat <+> "using" <+> parens "ℚ"
    DataRatInstances     -> "Data.Rational.Instances"
    -- HACK: At the moment redirect to rationals
    DataReal             -> "Data.Rational as" <+> numericQualifier Real <+> "using" <+> parens "" <+> "renaming (ℚ to ℝ)"
      -- "Data.Real as" <+> numericQualifier Real <+> "using" <+> parens "ℝ"
    DataBool             -> "Data.Bool as 𝔹" <+> "using" <+> parens "Bool; true; false; if_then_else_"
    DataBoolInstances    -> "Data.Bool.Instances"
    DataFin              -> "Data.Fin as Fin" <+> "using" <+> parens "#_"
    DataList             -> "Data.List"
    DataListInstances    -> "Data.List.Instances"
    DataListAll          -> "Data.List.Relation.Unary.All as" <+> containerQualifier List
    DataListAny          -> "Data.List.Relation.Unary.Any as" <+> containerQualifier List
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

numericQualifier :: NumericType -> Doc a
numericQualifier = \case
  Nat   -> "ℕ"
  Int   -> "ℤ"
  Rat   -> "ℚ"
  Real  -> "ℝ"

containerQualifier :: ContainerType -> Doc a
containerQualifier = pretty . show

numericDependencies :: NumericType -> [Dependency]
numericDependencies = \case
  Nat   -> [DataNat]
  Int   -> [DataInteger]
  Rat   -> [DataRat]
  Real  -> [DataReal]

indentCode :: Code -> Code
indentCode = indent 2

scopeCode :: Code -> Code -> Code
scopeCode keyword code = keyword <> line <> indentCode code

--------------------------------------------------------------------------------
-- Intermediate results of compilation

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

binderBrackets :: Visibility -> Code -> Code
binderBrackets Explicit = parens
binderBrackets Implicit = braces
binderBrackets Instance = braces . braces

boolBraces :: Code -> Code
boolBraces c = annotateConstant [RelNullaryDecidable] "⌊" <+> c <+> "⌋"

arrow :: Code
arrow = "→" -- <> softline'

--------------------------------------------------------------------------------
-- Program Compilation

type MonadAgdaCompile m =
  ( MonadCompile m
  , MonadReader AgdaOptions m
  )

compileProg :: MonadAgdaCompile m => OutputProg -> m Code
compileProg (Main ds) = vsep2 <$> traverse compileDecl ds

compileDecl :: MonadAgdaCompile m => OutputDecl -> m Code
compileDecl = \case
  DeclData ann ident _ ->
    throwError $ UnsupportedDecl AgdaBackend (provenanceOf ann) ident Dataset

  DeclNetw _ann n t -> compileNetwork (compileIdentifier n) <$> compileExpr t

  DefFun _ann n t e -> do
    let (binders, body) = foldLam e
    if isProperty t
      then compileProperty (compileIdentifier n) =<< compileExpr e
      else do
        let binders' = traverse (compileBinder True) binders
        compileFunDef (compileIdentifier n) <$> compileExpr t <*> binders' <*> compileExpr body

compileExpr :: MonadAgdaCompile m => OutputExpr -> m Code
compileExpr expr = do
  logEntry expr
  result <- case expr of
    Hole{}     -> developerError "Holes should have been removed during type-checking"
    Meta{}     -> developerError "Meta-variables should have been removed during type-checking"
    PrimDict{} -> developerError "Primitive dictionaries should never be compiled"

    Type l      -> return $ compileType l
    Var _ann n  -> return $ annotateConstant [] (pretty n)

    Pi ann binder result -> case foldPi ann binder result of
      Left (binders, body)  -> compileTypeLevelQuantifier All binders body
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
      cBinders <- traverse (compileBinder False) binders
      cBody    <- compileExpr body
      return $ annotate (mempty, minPrecedence) ("λ" <+> hsep cBinders <+> arrow <+> cBody)

    Builtin{} -> compileBuiltin expr
    Literal{} -> compileLiteral expr

    App _ fun args -> case fun of
      Builtin{}    -> compileBuiltin expr
      Literal{}    -> compileLiteral expr
      _            -> do
        cFun   <- compileExpr fun
        cArgs  <- traverse compileArg args
        return $ annotateApp [] cFun (NonEmpty.toList cArgs)

    LSeq ann dict xs -> compileSeq ann dict xs

  logExit result
  return result

compileLetBinder :: MonadAgdaCompile m
                 => LetBinder OutputBinding OutputVar OutputAnn
                 -> m Code
compileLetBinder (binder, expr) = do
  let binderName = pretty (nameOf binder :: OutputBinding)
  cExpr <- compileExpr expr
  return $ binderName <+> "=" <+> cExpr

compileArg :: MonadAgdaCompile m => OutputArg -> m Code
compileArg arg = argBrackets (visibilityOf arg) <$> compileExpr (argExpr arg)

compileBooleanType :: BooleanType -> Code
compileBooleanType t = case t of
    Prop -> compileType 0
    Bool -> annotateConstant [DataBool] "Bool"

compileNumericType :: NumericType -> Code
compileNumericType t = annotateConstant (numericDependencies t) (numericQualifier t)

compileIdentifier :: Identifier -> Code
compileIdentifier ident = pretty (nameOf ident :: Symbol)

compileType :: UniverseLevel -> Code
compileType 0 = "Set"
compileType l = annotateConstant [] ("Set" <> pretty l)

compileBinder :: MonadAgdaCompile m => Bool -> OutputBinder -> m Code
compileBinder topLevel binder = do
  let binderName = pretty (nameOf binder :: OutputBinding)
  if topLevel
    then return binderName
    else do
      binderType <- compileExpr (typeOf binder)
      let annBinder = annotateInfixOp2 [] minPrecedence id Nothing ":" [binderName, binderType]
      return $ binderBrackets (visibilityOf binder) annBinder

compileBuiltin :: MonadAgdaCompile m => OutputExpr -> m Code
compileBuiltin e = case e of
  BuiltinBooleanType _ t  -> return $ compileBooleanType t
  BuiltinNumericType _ t  -> return $ compileNumericType t

  ListType   _ tElem       -> annotateApp [DataList]   "List"   <$> traverse compileExpr [tElem]
  TensorType _ tElem tDims -> annotateApp [DataTensor] "Tensor" <$> traverse compileExpr [tElem, tDims]
  FinType    _ size        -> annotateApp [DataFin]    "Fin"    <$> traverse compileExpr [size]

  IfExpr _ _ [e1, e2, e3] -> do
    ce1 <- compileArg e1
    ce2 <- compileArg e2
    ce3 <- compileArg e3
    return $ annotate (Set.singleton DataBool, 0)
      ("if" <+> ce1 <+> "then" <+> ce2 <+> "else" <+> ce3)

  BooleanOp2Expr op2 _ t   args -> compileBoolOp2 op2 t <$> traverse compileArg args
  NotExpr            _ t   args -> compileNot         t <$> traverse compileArg args
  NumericOp2Expr op2 _ t _ args -> compileNumOp2  op2 t <$> traverse compileArg args
  NegExpr            _ t   args -> compileNeg         t <$> traverse compileArg args

  (QuantifierExpr   q _              binder body)      -> compileTypeLevelQuantifier q [binder] body
  (QuantifierInExpr q ann tCont tRes binder body cont) -> compileQuantIn tRes q tCont (Lam ann binder body) cont

  (OrderExpr order  _ t1 t2 args) -> compileNumOrder order (numericType t1) t2 <$> traverse compileArg args
  (EqualityExpr Eq  _ t1 t2 args) -> compileEquality   t1 t2 =<< traverse compileArg args
  (EqualityExpr Neq _ t1 t2 args) -> compileInequality t1 t2 =<< traverse compileArg args

  (ConsExpr _ tElem               args) -> compileCons tElem <$> traverse compileArg args
  (AtExpr ann _tElem _tDim _tDims args) -> compileAt ann (map argExpr args)

  MapExpr{}             -> throwError $ UnsupportedBuiltin AgdaBackend (provenanceOf e) Map
  FoldExpr{}            -> throwError $ UnsupportedBuiltin AgdaBackend (provenanceOf e) Fold
  BuiltinTypeClass _ tc -> throwError $ UnsupportedBuiltin AgdaBackend (provenanceOf e) (TypeClass tc)

  _ -> developerError $ "unexpected application of builtin found during compilation to Agda:" <+>
                        squotes (prettyVerbose e)

compileAnn :: Code -> Code -> Code
compileAnn e t = annotateInfixOp2 [FunctionBase] 0 id Nothing "∋" [t,e]

compileTypeLevelQuantifier :: MonadAgdaCompile m => Quantifier -> [OutputBinder] -> OutputExpr -> m Code
compileTypeLevelQuantifier q binders body = do
  cBinders  <- traverse (compileBinder False) binders
  cBody     <- compileExpr body
  let quant = if q == All then "∀" else annotateConstant [DataProduct] "∃ λ"
  return $ quant <+> hsep cBinders <+> arrow <+> cBody

compileContainerTypeLevelQuantifier :: MonadAgdaCompile m
                                    => Quantifier
                                    -> OutputExpr
                                    -> OutputExpr
                                    -> OutputExpr
                                    -> m Code
compileContainerTypeLevelQuantifier q tCont fn cont = do
  let contType = containerType tCont
  let deps     = containerQuantifierDependencies q (containerType tCont)
  let quant    = containerQualifier contType <> "." <> (if q == All then "All" else "Any")
  annotateApp deps quant <$> traverse compileExpr [fn, cont]

compileContainerExprLevelQuantifier :: MonadAgdaCompile m
                                    => Quantifier
                                    -> OutputExpr
                                    -> OutputExpr
                                    -> OutputExpr
                                    -> m Code
compileContainerExprLevelQuantifier q tCont fn cont = do
  let contType = containerType tCont
  let quant    = containerQualifier contType <> "." <> (if q == All then "all" else "any")
  let deps     = containerDependencies contType
  annotateApp deps quant <$> traverse compileExpr [fn, cont]

compileQuantIn :: MonadAgdaCompile m => BooleanType -> Quantifier -> OutputExpr -> OutputExpr -> OutputExpr -> m Code
compileQuantIn Bool = compileContainerExprLevelQuantifier
compileQuantIn Prop = compileContainerTypeLevelQuantifier

compileLiteral :: MonadAgdaCompile m => OutputExpr -> m Code
compileLiteral e = return $ case e of
  NatLiteralExpr  _ann FinType{}  n -> compileFinLiteral  (toInteger n)
  NatLiteralExpr  _ann NatType{}  n -> compileNatLiteral  (toInteger n)
  NatLiteralExpr  _ann IntType{}  n -> compileIntLiteral  (toInteger n)
  NatLiteralExpr  _ann RatType{}  n -> compileRatLiteral  (toRational n)
  NatLiteralExpr  _ann RealType{} n -> compileRealLiteral (toRational n)
  IntLiteralExpr  _ann Int  i -> compileIntLiteral  (toInteger i)
  IntLiteralExpr  _ann Rat  i -> compileRatLiteral  (toRational i)
  IntLiteralExpr  _ann Real i -> compileRealLiteral (toRational i)
  RatLiteralExpr  _ann Rat  p -> compileRatLiteral  p
  RatLiteralExpr  _ann Real p -> compileRealLiteral p
  BoolLiteralExpr _ann t b    -> compileBoolOp0 b t
  _                           -> developerError $
    "unexpected literal" <+> squotes (prettyVerbose e) <+>
    -- "of type" <+> squotes (pretty t) <+>
    "found during compilation to Agda"

compileFinLiteral :: Integer -> Code
compileFinLiteral i = annotateInfixOp1 [DataFin] 10 Nothing "#" [pretty i]

compileNatLiteral :: Integer -> Code
compileNatLiteral = pretty

compileIntLiteral :: Integer -> Code
compileIntLiteral i
  | i >= 0    = annotateInfixOp1 [DataInteger] 8 (Just (numericQualifier Int)) "+" [pretty i]
  | otherwise = annotateInfixOp1 [DataInteger] 6 (Just (numericQualifier Int)) "-" [compileIntLiteral (- i)]

compileRatLiteral :: Rational -> Code
compileRatLiteral r = annotateInfixOp2 [DataRat] 7 id
  (Just $ numericQualifier Rat) "/"
  [ compileIntLiteral (numerator r)
  , compileNatLiteral (denominator r)
  ]

compileRealLiteral :: Rational -> Code
compileRealLiteral r = annotateInfixOp2 [DataReal] 7 id
  (Just $ numericQualifier Real) "/"
  [ compileIntLiteral (numerator r)
  , compileNatLiteral (denominator r)
  ]

-- |Compiling sequences. No sequences in Agda so have to go via cons.
compileSeq :: MonadAgdaCompile m => OutputAnn -> OutputExpr -> [OutputExpr] -> m Code
compileSeq _ (PrimDict _ (IsContainerExpr _ _ tCont)) elems = go elems
  where
    go :: MonadAgdaCompile m => [OutputExpr] -> m Code
    go []       = return $ annotateConstant (containerDependencies (containerType tCont)) "[]"
    go (x : xs) = do
      cx  <- compileExpr x
      cxs <- go xs
      return $ annotateInfixOp2 [] 5 id Nothing "∷" [cx , cxs]
compileSeq ann dict elems = unexpectedArgsError (LSeq ann dict elems) elems ["tElem", "tCont", "tc"]


-- |Compiling cons operator
compileCons :: OutputExpr -> [Code] -> Code
compileCons tCont = annotateInfixOp2 deps 5 id (Just qualifier) "∷"
  where
    contType  = containerType tCont
    qualifier = containerQualifier contType
    deps      = containerDependencies contType

-- |Compiling boolean constants
compileBoolOp0 :: Bool -> BooleanType -> Code
compileBoolOp0 True  Bool = annotateConstant [DataBool]  "true"
compileBoolOp0 False Bool = annotateConstant [DataBool]  "false"
compileBoolOp0 True  Prop = annotateConstant [DataUnit]  "⊤"
compileBoolOp0 False Prop = annotateConstant [DataEmpty] "⊥"

-- |Compiling boolean negation
compileNot :: BooleanType -> [Code] -> Code
compileNot Bool = annotateApp      [DataBool] "not"
compileNot Prop = annotateInfixOp1 [RelNullary] 3 Nothing "¬"

-- |Compiling boolean binary operations
compileBoolOp2 :: BooleanOp2 -> BooleanType -> [Code] -> Code
compileBoolOp2 op2 t = annotateInfixOp2 dependencies precedence id Nothing opDoc
  where
    (opDoc, precedence, dependencies) = case (op2, t) of
      (And , Bool) -> ("∧", 6,  [DataBool])
      (Or  , Bool) -> ("∨", 5,  [DataBool])
      (Impl, Bool) -> ("⇒", 4,  [VehicleUtils])
      (And , Prop) -> ("×", 2,  [DataProduct])
      (Or  , Prop) -> ("⊎", 1,  [DataSum])
      (Impl, Prop) -> (arrow, minPrecedence, [])

-- |Compiling numeric unary operations
compileNeg :: NumericType -> [Code] -> Code
compileNeg Nat = developerError "Negation is not supported for naturals"
compileNeg t   = annotateInfixOp1 (numericDependencies t) 8 (Just (numericQualifier t)) "-"

-- |Compiling numeric binary operations
compileNumOp2 :: NumericOp2 -> NumericType -> [Code] -> Code
compileNumOp2 op2 t = annotateInfixOp2 dependencies precedence id qualifier opDoc
  where
    precedence = if op2 == Mul || op2 == Div then 7 else 6
    qualifier  = Just (numericQualifier t)
    (opDoc, dependencies) = case (op2, t) of
      (Add, _)     -> ("+", numericDependencies t)
      (Mul, _)     -> ("*", numericDependencies t)
      (Sub, Nat)   -> ("∸", numericDependencies t)
      (Sub, _)     -> ("-", numericDependencies t)
      (Div, Nat)   -> ("/", [DataNatDivMod])
      (Div, Int)   -> ("/", [DataIntegerDivMod])
      (Div, Rat)   -> ("÷", [DataRat])
      (Div, Real)  -> ("÷", [DataReal])

compileNumOrder :: Order -> NumericType -> BooleanType -> [Code] -> Code
compileNumOrder order nt bt = annotateInfixOp2 dependencies 4 opBraces qualifier opDoc
  where
    qualifier = Just $ numericQualifier nt
    numDeps   = numericDependencies nt
    (boolDecDoc, boolDeps) = booleanModifierDocAndDeps bt
    orderDoc = case order of
      Le -> "≤"
      Lt -> "<"
      Ge -> "≥"
      Gt -> ">"

    opBraces     = if bt == Bool then boolBraces else id
    dependencies = numDeps <> boolDeps
    opDoc        = orderDoc <> boolDecDoc

compileAt :: MonadAgdaCompile m => CheckedAnn -> [OutputExpr] -> m Code
compileAt _ [tensorExpr, indexExpr] =
  annotateApp [] <$> compileExpr tensorExpr <*> traverse compileExpr [indexExpr]
compileAt _ args =
  unexpectedArgsError (Builtin emptyUserAnn At) args ["tensor", "index"]

compileEquality :: MonadAgdaCompile m => OutputExpr -> BooleanType -> [Code] -> m Code
compileEquality _tElem Prop args = return $ annotateInfixOp2 [PropEquality] 4 id Nothing "≡" args
compileEquality tElem  Bool args = do
  -- Boolean function equality is more complicated as we need an actual decision procedure.
  -- We handle this using instance arguments
  instanceArgDependencies <- equalityDependencies tElem
  return $ annotateInfixOp2 ([RelNullary] <> instanceArgDependencies) 4 boolBraces Nothing "≟" args

compileInequality :: MonadAgdaCompile m => OutputExpr -> BooleanType -> [Code] -> m Code
compileInequality _tElem Prop args = return $ annotateInfixOp2 [PropEquality] 4 id Nothing "≢" args
compileInequality tElem  Bool args = do
  eq <- compileEquality tElem Bool args
  return $ compileNot (booleanType tElem) [eq]

compileFunDef :: Code -> Code -> [Code] -> Code -> Code
compileFunDef n t ns e =
  n <+> ":" <+> align t <> line <>
  n <+> (if null ns then mempty else hsep ns <> " ") <> "=" <+> e

-- |Compile a `network` declaration
compileNetwork :: Code -> Code -> Code
compileNetwork networkName networkType =
  "postulate" <+> networkName <+> ":" <+> align networkType

compileProperty :: MonadAgdaCompile m => Code -> Code -> m Code
compileProperty propertyName propertyBody = do
  proofCache <- asks proofCacheLocation
  return $
    scopeCode "abstract" $
      propertyName <+> ":" <+> align propertyBody          <> line <>
      propertyName <+> "= checkProperty record"            <> line <>
        indentCode (
        "{ proofCache   =" <+> dquotes (pretty proofCache) <> line <>
        "}")

containerDependencies :: ContainerType -> [Dependency]
containerDependencies = \case
  List   -> [DataList]
  Tensor -> [DataTensor]

containerQuantifierDependencies :: Quantifier -> ContainerType -> [Dependency]
containerQuantifierDependencies All List   = [DataListAll]
containerQuantifierDependencies Any List   = [DataListAny]
containerQuantifierDependencies All Tensor = [DataTensorAll]
containerQuantifierDependencies Any Tensor = [DataTensorAny]

booleanModifierDocAndDeps :: BooleanType -> (Code, [Dependency])
booleanModifierDocAndDeps = \case
  Bool -> ("?", [RelNullary])
  Prop -> ("" , [])

-- Calculates the dependencies needed for equality over the provided type
equalityDependencies :: MonadAgdaCompile m => OutputExpr -> m [Dependency]
equalityDependencies = \case
  BuiltinNumericType _ Nat  -> return [DataNatInstances]
  BuiltinNumericType _ Int  -> return [DataIntegerInstances]
  BuiltinNumericType _ Real -> return [DataRatInstances]
  BuiltinBooleanType _ Bool -> return [DataBoolInstances]
  App _ (BuiltinContainerType _ List)   [tElem] -> do
    deps <- equalityDependencies (argExpr tElem)
    return $ [DataListInstances] <> deps
  App _ (BuiltinContainerType _ Tensor) [tElem, _tDims] -> do
    deps <- equalityDependencies (argExpr tElem)
    return $ [DataTensorInstances] <> deps
  Var ann n -> throwError $ UnsupportedPolymorphicEquality AgdaBackend (provenanceOf ann) n
  t         -> unexpectedTypeError t ["Tensor", "Real", "Int", "List"]

numericType :: OutputExpr -> NumericType
numericType (Builtin _ (NumericType t)) = t
numericType t = unexpectedTypeError t (map show [Nat, Int, Rat, Real])

booleanType :: OutputExpr -> BooleanType
booleanType (Builtin _ (BooleanType t)) = t
booleanType t = unexpectedTypeError t (map show [Bool, Prop])

containerType :: OutputExpr -> ContainerType
containerType (App _ (Builtin _ (ContainerType t)) _) = t
containerType t = unexpectedTypeError t (map show [List, Tensor])

unexpectedTypeError :: OutputExpr -> [String] -> a
unexpectedTypeError actualType expectedTypes = developerError $
  "Unexpected type found." <+>
  "Was expecting one of" <+> pretty expectedTypes <+>
  "but found" <+> prettyFriendly actualType <+>
  "at" <+> pretty (provenanceOf actualType) <> "."

unexpectedArgsError :: OutputExpr -> [OutputExpr] -> [String] -> a
unexpectedArgsError fun actualArgs expectedArgs = developerError $
  "The function" <+> prettyFriendly fun <+> "was expected to have arguments" <+>
  "of the following form" <+> squotes (pretty expectedArgs) <+> "but found" <+>
  "the following" <+> squotes (prettyFriendly actualArgs) <+>
  "at" <+> pretty (provenanceOf fun) <> "."