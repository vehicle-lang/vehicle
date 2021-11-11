{-# LANGUAGE OverloadedLists #-}

module Vehicle.Backend.ITP.Agda
  ( AgdaOptions(..)
  , compileToAgda
  ) where

import GHC.Real (numerator, denominator)
import Control.Monad.Except (MonadError(..))
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Text (Text)
import Data.Foldable (fold)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map as Map (Map)
import Prettyprinter hiding (hsep, vsep, hcat, vcat)

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Language.Print
import Vehicle.Language.Sugar
import Vehicle.Backend.ITP.Core

compileToAgda :: (MonadLogger m, MonadError CompileError m)
              => AgdaOptions -> OutputProg -> m (Doc a)
compileToAgda options prog = runReaderT (compileProgramToAgda prog) options

--------------------------------------------------------------------------------
-- Agda-specific options

data AgdaOptions = AgdaOptions
  { modulePath  :: [Text]
  , vehicleUIDs :: Map.Map Identifier Text
  }

--------------------------------------------------------------------------------
-- Modules

-- |All possible Agda modules the program may depend on.
data Dependency
  -- AISEC agda library
  = AISECCore
  | AISECUtils
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
  | DataInt
  | DataIntInstances
  | DataIntDivMod
  | DataRat
  | DataRatInstances
  | DataReal
  | DataBool
  | DataBoolInstances
  | DataList
  | DataListInstances
  | DataListAll
  | DataListAny
  | PropEquality
  | RelNullary
  deriving (Eq, Ord)

instance Pretty Dependency where
  pretty = \case
    AISECCore           -> "AISEC.Core"
    AISECUtils          -> "AISEC.Utils"
    DataTensor          -> "AISEC.Data.Tensor"
    DataTensorInstances -> "AISEC.Data.Tensor.Instances"
    DataTensorAll       -> "AISEC.Data.Tensor.Relation.Unary.All as" <+> containerQualifier Tensor
    DataTensorAny       -> "AISEC.Data.Tensor.Relation.Unary.Any as" <+> containerQualifier Tensor
    DataUnit            -> "Data.Unit"
    DataEmpty           -> "Data.Empty"
    DataProduct         -> "Data.Product"
    DataSum             -> "Data.Sum"
    DataNat             -> "Data.Nat as" <+> numericQualifier Nat <+> "using" <+> parens "ℕ"
    DataNatInstances    -> "Data.Nat.Instances"
    DataNatDivMod       -> "Data.Nat.DivMod as" <+> numericQualifier Nat
    DataInt             -> "Data.Int as" <+> numericQualifier Int <+> "using" <+> parens "ℤ"
    DataIntInstances    -> "Data.Int.Instances"
    DataIntDivMod       -> "Data.Int.DivMod as" <+> numericQualifier Int
    DataRat             -> "Data.Rational as" <+> numericQualifier Rat <+> "using" <+> parens "ℚ"
    DataRatInstances    -> "Data.Rational.Instances"
    DataReal            -> "Data.Real as" <+> numericQualifier Real <+> "using" <+> parens "ℝ"
    DataBool            -> "Data.Bool as 𝔹" <+> "using" <+> parens "Bool; true; false"
    DataBoolInstances   -> "Data.Bool.Instances"
    DataList            -> "Data.List"
    DataListInstances   -> "Data.List.Instances"
    DataListAll         -> "Data.List.Relation.Unary.All as" <+> containerQualifier List
    DataListAny         -> "Data.List.Relation.Unary.Any as" <+> containerQualifier List
    PropEquality        -> "Relation.Binary.PropositionalEquality"
    RelNullary          -> "Relation.Nullary"

importStatement :: Dependency -> Doc a
importStatement dep = "open import" <+> pretty dep

importStatements :: Set Dependency -> Doc a
importStatements deps = vsep $ map importStatement $ Set.toList deps

type ModulePath = [Text]

moduleHeader :: ModulePath -> Doc a
moduleHeader path = "module" <+> concatWith (surround ".") (map pretty path) <+> "where"

numericQualifier :: NumericType -> Doc a
numericQualifier = \case
  Nat  -> "ℕ"
  Int  -> "ℤ"
  Rat  -> "ℚ"
  Real -> "ℝ"

containerQualifier :: ContainerType -> Doc a
containerQualifier = pretty . show

numericDependencies :: NumericType -> [Dependency]
numericDependencies = \case
  Nat  -> [DataNat]
  Int  -> [DataInt]
  Rat  -> [DataRat]
  Real -> [DataReal]

--------------------------------------------------------------------------------
-- Intermediate results of compilation

type Code = Doc (Set Dependency, Precedence)

minPrecedence :: Precedence
minPrecedence = -1000

maxPrecedence :: Precedence
maxPrecedence = 1000

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
bracketIfRequired parentPrecedence expr = case docAnn expr of
  Just (_ , exprPrecedence)
    | exprPrecedence <= parentPrecedence -> parens expr
  _                                      -> expr

boolBraces :: Code -> Code
boolBraces c = "⌊" <+> c <+> "⌋"

-----------------
-- Compilation --
-----------------

type MonadAgdaCompile m = MonadCompile AgdaOptions m

compileProgramToAgda :: MonadAgdaCompile m
                     => OutputProg
                     -> m (Doc a)
compileProgramToAgda program = do
  programDoc <- compile program
  let programStream = layoutPretty defaultLayoutOptions programDoc
  -- Collects dependencies by first discarding precedence info and then folding using Set Monoid
  let progamDependencies = fold (reAnnotateS fst programStream)
  options <- ask
  return $ unAnnotate ((vsep2 :: [Code] -> Code)
    [ importStatements progamDependencies
    , moduleHeader (modulePath options)
    , programDoc
    ])

class CompileToAgda a where
  compile
    :: MonadAgdaCompile m
    => a
    -> m Code

--------------------------------------------------------------------------------
-- Debug functions

logEntry :: MonadAgdaCompile m => OutputExpr -> m ()
logEntry e = do
  incrCallDepth
  logDebug $ "compile-entry" <+> prettyVerbose e

logExit :: MonadAgdaCompile m => Code -> m ()
logExit e = do
  logDebug $ "compile-exit " <+> e
  decrCallDepth

--------------------------------------------------------------------------------
-- Compilation of programs

instance CompileToAgda OutputProg where
  compile (Main ds) = do
    cds <- traverse compile ds
    return $ vsep2 cds

instance CompileToAgda OutputDecl where
  compile = \case
    DeclData ann _ _    -> throwError $ CompilationUnsupported (provenanceOf ann) "dataset"

    DeclNetw _ann n t   -> compileNetwork (pretty n) <$> compile t

    DefFun _ann n t e -> do
      let (binders, body) = foldLam e
      if isProperty t
        then do compileProperty (pretty n) <$> compile e
        else do compileFunDef   (pretty n) <$> compile t <*> traverse compile binders <*> compile body

instance CompileToAgda OutputExpr where
  compile expr = do
    logEntry expr
    result <- case expr of
      Hole{}     -> developerError "Holes should have been removed during type-checking"
      Meta{}     -> developerError "Meta-variables should have been removed during type-checking"
      PrimDict{} -> developerError "Primitive dictionaries should never be compiled"

      Var _ann n  -> return $ annotateConstant [] (pretty n)

      Type l -> return $ annotateApp [] "Set" [pretty l]

      Pi ann binder result -> case foldPi ann binder result of
        Left (binders, body)  -> compileTypeLevelQuantifier All binders body
        Right (input, output) ->
          annotateInfixOp2 [] minPrecedence id Nothing "→" <$> traverse compile [input, output]

      Ann _ann e t -> do
        annotateInfixOp2 [] minPrecedence id Nothing ":" <$> traverse compile [e, t]

      Let{} -> do
        let (boundExprs, body) = foldLet expr
        cBoundExprs <- traverse compile boundExprs
        cBody       <- compile body
        return $ "let" <+> vsep cBoundExprs <+> "in" <+> cBody

      Lam{} -> do
        let (binders, body) = foldLam expr
        cBinders <- traverse compile binders
        cBody    <- compile body
        return $ annotate (mempty, minPrecedence) ("λ" <+> hsep cBinders <+> "→" <+> cBody)

      Builtin ann op -> compileBuiltin ann op []
      Literal ann op -> compileLiteral ann op []

      App ann fun args -> case fun of
        Builtin _ op -> compileBuiltin ann op (fmap argExpr (NonEmpty.toList args))
        Literal _ op -> compileLiteral ann op (fmap argExpr (NonEmpty.toList args))
        Seq _     xs -> compileSeq     ann xs (fmap argExpr (NonEmpty.toList args))
        _            -> do
          cFun   <- compile fun
          cArgs  <- traverse compile args
          return $ cFun <+> hsep cArgs

      Seq _ _ -> developerError "when compiling to Agda, Seqs should have been caught in App case"

    logExit result
    return result

instance CompileToAgda (LetBinder OutputBinding OutputVar OutputAnn) where
  compile (binder, expr) = do
    let binderName = pretty (nameOf binder :: OutputBinding)
    cExpr <- compile expr
    return $ binderName <+> "=" <+> cExpr

instance CompileToAgda OutputBinder where
  compile binder = do
    binderType <- compile (typeOf binder)
    let binderName     = pretty (nameOf binder :: OutputBinding)
    let binderBrackets = visBrackets (visibilityOf binder)
    return $ binderBrackets (binderName <+> ":" <+> binderType)

instance CompileToAgda OutputArg where
  compile arg = visBrackets (visibilityOf arg) <$> compile (argExpr arg)

instance CompileToAgda BooleanType where
  compile t = return $ case t of
    Prop -> annotateConstant []         "Set"
    Bool -> annotateConstant [DataBool] "Bool"

instance CompileToAgda NumericType where
  compile t = return $ annotateConstant (numericDependencies t) (numericQualifier t)

compileBuiltin :: MonadAgdaCompile m => OutputAnn -> Builtin -> [OutputExpr] -> m Code
compileBuiltin ann op args = case (op, args) of
  (TypeClass  tc, []) -> throwError $ CompilationUnsupported (provenanceOf ann) (pretty tc)
  (BooleanType t, []) -> compile t
  (NumericType t, []) -> compile t

  (ContainerType List,   opArgs) -> annotateApp [DataList]   "List"   <$> traverse compile opArgs
  (ContainerType Tensor, opArgs) -> annotateApp [AISECUtils] "Tensor" <$> traverse compile opArgs

  (If, [_t, e1, e2, e3]) -> do
    ce1 <- compile e1
    ce2 <- compile e2
    ce3 <- compile e3
    return $ "if" <+> ce1 <+> "then" <+> ce2 <+> "else" <+> ce3

  (BooleanOp2 op2, t : _tc : opArgs) -> compileBoolOp2 op2 (booleanType t) <$> traverse compile opArgs
  (Not,            t : _tc : opArgs) -> compileNot         (booleanType t) <$> traverse compile opArgs
  (NumericOp2 op2, t : _tc : opArgs) -> compileNumOp2  op2 (numericType t) <$> traverse compile opArgs
  (Neg,            t : _tc : opArgs) -> compileNeg         (numericType t) <$> traverse compile opArgs

  (Quant   q, _tElem                      : opArgs) -> compileQuant   ann   q opArgs
  (QuantIn q, _tElem : tCont : tRes : _tc : opArgs) -> compileQuantIn tCont q (booleanType tRes) opArgs

  (Order order,  t1 : t2 : _tc : opArgs) -> compileNumOrder order (numericType t1) (booleanType t2) <$> traverse compile opArgs
  (Equality Eq,  t1 : t2 : _tc : opArgs) -> compileEquality   t1 (booleanType t2) =<< traverse compile opArgs
  (Equality Neq, t1 : t2 : _tc : opArgs) -> compileInequality t1 (booleanType t2) =<< traverse compile opArgs

  (Cons, tElem  : opArgs)         -> compileCons tElem <$> traverse compile opArgs
  (At  , _tElem : tDims : opArgs) -> compileAt tDims opArgs

  (Map , _) -> throwError $ CompilationUnsupported (provenanceOf ann) (pretty Map)
  (Fold, _) -> throwError $ CompilationUnsupported (provenanceOf ann) (pretty Fold)

  _ -> developerError $ "unexpected application of builtin found during compilation to Agda:" <+>
                        squotes (pretty op) <+> "applied to" <+> prettyFriendly args

compileTypeLevelQuantifier :: MonadAgdaCompile m => Quantifier -> [OutputBinder] -> OutputExpr -> m Code
compileTypeLevelQuantifier q binders body = do
  cBinders  <- traverse compile binders
  cBody     <- compile body
  let quant = if q == All then "∀" else "∃"
  return $ quant <+> hsep cBinders <+> "→" <+> cBody

compileContainerTypeLevelQuantifier :: MonadAgdaCompile m => OutputExpr -> Quantifier -> [OutputExpr] -> m Code
compileContainerTypeLevelQuantifier tCont q args = do
  let contType = containerType tCont
  let deps     = containerQuantifierDependencies q (containerType tCont)
  let quant    = containerQualifier contType <> "." <> (if q == All then "All" else "Any")
  annotateApp deps quant <$> traverse compile args

compileContainerExprLevelQuantifier :: MonadAgdaCompile m => OutputExpr -> Quantifier -> [OutputExpr] -> m Code
compileContainerExprLevelQuantifier tCont q args = do
  let contType = containerType tCont
  let quant    = containerQualifier contType <> "." <> (if q == All then "all" else "any")
  let deps     = containerDependencies contType
  annotateApp deps quant <$> traverse compile args

compileQuant :: MonadAgdaCompile m => OutputAnn -> Quantifier  -> [OutputExpr] -> m Code
compileQuant _   q [Lam _ binder body] = compileTypeLevelQuantifier q [binder] body
compileQuant _   _ args                = developerError $ "malformed quantifier args" <+> prettyFriendly args

compileQuantIn :: MonadAgdaCompile m => OutputExpr -> Quantifier -> BooleanType -> [OutputExpr] -> m Code
compileQuantIn tCont q Bool args = compileContainerExprLevelQuantifier tCont q args
compileQuantIn tCont q Prop args = compileContainerTypeLevelQuantifier tCont q args

compileLiteral :: MonadAgdaCompile m => OutputAnn -> Literal -> [OutputExpr] -> m Code
compileLiteral _ann lit args = return $ case (lit, args) of
  (LNat  n,     _)                             -> pretty n
  (LInt  i,     _)                             -> pretty i
  (LRat  p,    _)                              -> annotateInfixOp2 [DataRat] 7 id (Just $ numericQualifier Rat) "/"
                                                    [pretty (numerator p), pretty (denominator p) ]
  (LBool True,  [BuiltinBooleanType _ t, _tc]) -> compileBoolOp0 True t
  (LBool False, [BuiltinBooleanType _ t, _tc]) -> compileBoolOp0 False t
  _ -> developerError $ "unexpected application of literal found during compilation to Agda:" <+>
                        squotes (pretty lit) <+> "applied to" <+> prettyFriendly args

-- |Compiling sequences. No sequences in Agda so have to go via cons.
compileSeq :: MonadAgdaCompile m => OutputAnn -> [OutputExpr] -> [OutputExpr] -> m Code
compileSeq _ args [_, tCont, _] = go args
  where
    go :: MonadAgdaCompile m => [OutputExpr] -> m Code
    go []       = return $ annotateConstant (containerDependencies (containerType tCont)) "[]"
    go (x : xs) = do
      cx  <- compile x
      cxs <- go xs
      return $ annotateInfixOp2 [] 5 id Nothing "∷" [cx , cxs]
compileSeq ann xs args = unexpectedArgsError (Seq ann xs) args ["tElem", "tCont", "tc"]


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
      (Impl, Bool) -> ("⇒", 4,  [AISECUtils])
      (And , Bool) -> ("∧", 6,  [DataBool])
      (Or  , Bool) -> ("∨", 5,  [DataBool])
      (Impl, Prop) -> ("→", minPrecedence, [])
      (And , Prop) -> ("×", 2,  [DataProduct])
      (Or  , Prop) -> ("⊎", 1,  [DataSum])

-- |Compiling numeric unary operations
compileNeg :: NumericType -> [Code] -> Code
compileNeg Nat = developerError "Negation is not supported for naturals"
compileNeg t   = annotateInfixOp1 (numericDependencies t) 8 (Just (numericQualifier Int)) "-"

-- |Compiling numeric binary operations
compileNumOp2 :: NumericOp2 -> NumericType -> [Code] -> Code
compileNumOp2 op2 t = annotateInfixOp2 dependencies precedence id qualifier opDoc
  where
    precedence = if op2 == Mul || op2 == Div then 7 else 6
    qualifier  = Just (numericQualifier t)
    (opDoc, dependencies) = case (op2, t) of
      (Add, _)    -> ("+", numericDependencies t)
      (Mul, _)    -> ("*", numericDependencies t)
      (Sub, Nat)  -> ("∸", numericDependencies t)
      (Sub, _)    -> ("-", numericDependencies t)
      (Div, Nat)  -> ("/", [DataNatDivMod])
      (Div, Int)  -> ("/", [DataIntDivMod])
      (Div, Rat)  -> ("÷", [DataRat])
      (Div, Real) -> ("÷", [DataReal])

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

-- TODO implement this via proof by reflection
compileAt :: MonadAgdaCompile m => OutputExpr -> [OutputExpr] -> m Code
compileAt tDims args@[tensorExpr, indexExpr] = case exprHead indexExpr of
  Literal indexAnn (LNat index) -> case tensorSize tDims of
    Left err -> throwError $ ContainerDimensionError (provenanceOf indexAnn) err
    Right size ->
      if index >= size
        then throwError $ ContainerDimensionError (provenanceOf indexAnn) $
          TensorIndexOutOfBounds index size
        else do
          tensorDoc <- compile tensorExpr
          return $ tensorDoc <+> pretty index
  _ -> do
    let deps     = containerDependencies Tensor
    let modifier = containerQualifier Tensor
    annotateApp deps (modifier <> "." <> "lookup") <$> traverse compile args
compileAt _tDims args = unexpectedArgsError (Builtin emptyUserAnn At) args ["tensor", "index"]

compileEquality :: MonadAgdaCompile m => OutputExpr -> BooleanType -> [Code] -> m Code
compileEquality _tElem Prop args = return $ annotateInfixOp2 [PropEquality] 4 id Nothing "≡" args
compileEquality tElem  Bool args =
  -- Boolean function equality is more complicated as we need an actual decision procedure.
  -- We handle this using instance arguments
  case equalityDependencies tElem of
    Left  UnexpectedEqualityType ->
      unexpectedTypeError tElem ["Tensor", "Real", "Int", "List"]
    Left (PolymorphicEqualityType n) ->
      throwError $ CompilationUnsupported (provenanceOf tElem) ("polymorphic equality over" <+> squotes (pretty n) <> "'")
    Right dependencies ->
      return $ annotateInfixOp2 ([RelNullary] <> dependencies) 4 boolBraces Nothing "≟" args

compileInequality :: MonadAgdaCompile m => OutputExpr -> BooleanType -> [Code] -> m Code
compileInequality _tElem Prop args = return $ annotateInfixOp2 [PropEquality] 4 id Nothing "≢" args
compileInequality tElem  Bool args = do
  eq <- compileEquality tElem Bool args
  return $ compileNot (booleanType tElem) [eq]

compileFunDef :: Code -> Code -> [Code] -> Code -> Code
compileFunDef n t ns e =
  n <+> ":" <+> t <> hardline <>
  n <+> (if null ns then mempty else hsep ns <> " ") <> "=" <+> e

-- |Compile a `network` declaration
compileNetwork :: Code -> Code -> Code
compileNetwork networkName networkType =
  networkName <+> ":" <+> networkType       <> hardline <>
  networkName <+> "= evaluate record"       <> hardline <>
    indent 2 (
    "{ databasePath = DATABASE_PATH" <> hardline <>
    "; networkUUID  = NETWORK_UUID"  <> hardline <>
    "}")

compileProperty :: Code -> Code -> Code
compileProperty propertyName propertyBody =
  propertyName <+> ":" <+> propertyBody      <> hardline <>
  propertyName <+> "= checkProperty record"  <> hardline <>
    indent 2 (
    "{ databasePath = DATABASE_PATH" <> hardline <>
    "; propertyUUID = ????"          <> hardline <>
    "}")

containerDependencies :: ContainerType -> [Dependency]
containerDependencies = \case
  List   -> [DataList]
  Tensor -> [AISECUtils]

containerQuantifierDependencies :: Quantifier -> ContainerType -> [Dependency]
containerQuantifierDependencies All List   = [DataListAll]
containerQuantifierDependencies Any List   = [DataListAny]
containerQuantifierDependencies All Tensor = [DataTensorAll]
containerQuantifierDependencies Any Tensor = [DataTensorAny]

booleanModifierDocAndDeps :: BooleanType -> (Code, [Dependency])
booleanModifierDocAndDeps = \case
  Bool -> ("?", [RelNullary])
  Prop -> ("" , [])

data EqualityTypeError
  = UnexpectedEqualityType
  | PolymorphicEqualityType Symbol

-- Calculates the dependencies needed for equality over the provided type
equalityDependencies :: OutputExpr -> Either EqualityTypeError [Dependency]
equalityDependencies = \case
  BuiltinNumericType _ Nat  -> return [DataNatInstances]
  BuiltinNumericType _ Int  -> return [DataIntInstances]
  BuiltinNumericType _ Real -> return [DataRatInstances]
  BuiltinBooleanType _ Bool -> return [DataBoolInstances]
  App _ (BuiltinContainerType _ List)   [tElem] -> do
    deps <- equalityDependencies (argExpr tElem)
    return $ [DataListInstances] <> deps
  App _ (BuiltinContainerType _ Tensor) [tElem, _tDims] -> do
    deps <- equalityDependencies (argExpr tElem)
    return $ [DataTensorInstances] <> deps
  Var _ann n -> throwError $ PolymorphicEqualityType n
  _          -> throwError UnexpectedEqualityType