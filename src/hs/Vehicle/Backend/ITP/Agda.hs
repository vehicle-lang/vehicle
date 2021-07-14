module Vehicle.Backend.ITP.Agda
  (AgdaOptions(..), compileToAgda) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text (intercalate, unwords, null, pack, toUpper, splitAt)
import Data.Foldable (fold)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Control.Monad.Except (MonadError(..))
import Control.Monad.Reader (MonadReader(..), runReaderT)

import Prettyprinter hiding (hsep, vsep, hcat, vcat)
import Prettyprinter.Render.Text (renderStrict)

import Vehicle.Backend.ITP.Core
import Vehicle.Frontend.AST
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Agda-specific options

data AgdaOptions = AgdaOptions
  { useProp    :: Bool
  , modulePath :: [Text]
  }

--------------------------------------------------------------------------------
-- Modules

-- |The type of all possible Agda modules the program may depend on.
data Dependency
  -- AISEC agda library
  = AISECCore
  | AISECUtils
  | DataTensor
  | DataTensorInstances
  -- Standard library
  | DataUnit
  | DataEmpty
  | DataProduct
  | DataSum
  | DataInt
  | DataIntInstances
  | DataIntDivMod
  | DataRat
  | DataRatInstances
  | DataBool
  | DataBoolInstances
  | DataList
  | DataListInstances
  | PropEquality
  | RelNullary
  deriving (Eq, Ord)

instance Show Dependency where
  show AISECCore           = "AISEC.Core"
  show AISECUtils          = "AISEC.Utils"
  show DataTensor          = "AISEC.Data.Tensor"
  show DataTensorInstances = "AISEC.Data.Tensor.Instances"
  show DataUnit            = "Data.Unit"
  show DataEmpty           = "Data.Empty"
  show DataProduct         = "Data.Product"
  show DataSum             = "Data.Sum"
  show DataInt             = "Data.Int as ℤ"
  show DataIntInstances    = "Data.Int.Instances"
  show DataIntDivMod       = "Data.Int.DivMod as ℤ"
  show DataRat             = "Data.Rational as ℚ"
  show DataRatInstances    = "Data.Rational.Instances"
  show DataBool            = "Data.Bool as 𝔹"
  show DataBoolInstances   = "Data.Bool.Instances"
  show DataList            = "Data.List"
  show DataListInstances   = "Data.List.Instances"
  show PropEquality        = "Relation.Binary.PropositionalEquality"
  show RelNullary          = "Relation.Nullary"

importStatement :: Dependency -> Text
importStatement dep = "open import " <> Text.pack (show dep)

importStatements :: Set Dependency -> Text
importStatements deps = Text.unwords $ map importStatement $ Set.toList deps

type ModulePath = [Text]

moduleHeader :: ModulePath -> Text
moduleHeader path = "module " <> Text.intercalate "." path <> " where"


--------------------------------------------------------------------------------
-- Intermediate results of compilation

type Code = Doc (Set Dependency, Precedence)

annotateConstant :: Dependency -> Text -> Code
annotateConstant d t = annotate (Set.singleton d, maxBound :: Int) (pretty t)

annotateOp1 :: [Dependency] -> Precedence -> Symbol -> Code -> Code
annotateOp1 ds opPrecedence op arg =
  let bArg = bracketIfRequired opPrecedence arg in
  annotate (Set.fromList ds, opPrecedence) (pretty op <+> bArg)

annotateOp2 :: [Dependency]
            -> Bool
            -> Precedence
            -> (Code -> Code)
            -> Symbol
            -> Code -> Code -> Code
annotateOp2 ds isInfix opPrecedence opBraces op arg1 arg2 =
  let bArg1 = bracketIfRequired opPrecedence arg1 in
  let bArg2 = bracketIfRequired opPrecedence arg2 in
  annotate (Set.fromList ds, opPrecedence) (opBraces (if isInfix
    then bArg1 <+> pretty op <+> bArg2
    else pretty op <+> bArg1 <+> bArg2))

annotateInfixOp2 :: [Dependency]
                 -> Precedence
                 -> (Code -> Code)
                 -> Symbol
                 -> Code -> Code -> Code
annotateInfixOp2 ds = annotateOp2 ds True

annotateApp1 :: [Dependency] -> Symbol -> Code -> Code
annotateApp1 ds = annotateOp1 ds 20

annotateApp2 :: [Dependency] -> Symbol -> Code -> Code -> Code
annotateApp2 ds = annotateOp2 ds False 20 id

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

compileToAgda :: ITPOptions AgdaOptions -> OutputProg -> Either CompileError Text
compileToAgda options prog = runReaderT (compileProgramToAgda prog) options

type MonadAgdaCompile m = MonadCompile m AgdaOptions

compileProgramToAgda
  :: MonadAgdaCompile m
  => OutputProg
  -> m Text
compileProgramToAgda program = do
  programDoc <- compile program
  let programStream = layoutPretty defaultLayoutOptions programDoc
  -- Collects dependencies by first discarding precedence info and then folding using Set Monoid
  let progamDependencies = fold (reAnnotateS fst programStream)
  let programText = renderStrict programStream
  options <- ask
  return $ Text.intercalate "\n\n"
    [ fileHeader options "--"
    , importStatements progamDependencies
    , moduleHeader (modulePath (backendOpts options))
    , programText
    ]

class CompileToAgda (sort :: Sort) where
  compile
    :: MonadAgdaCompile m
    => OutputTree sort
    -> m Code

--------------------------------------------------------------------------------
-- Compilation of program tree

instance CompileToAgda 'KIND where
  compile _ = return mempty

instance CompileToAgda 'TYPE where
  compile = \case
    TForall _ann ns t  -> do
      cns <- traverse compile ns
      ct  <- compile t
      return $ "∀" <+> hsep cns <+> ct

    TVar    _ann n     -> return $ pretty n
    TFun    _ann t1 t2 -> annotateInfixOp2 [] 20 id "→" <$> compile t1 <*> compile t2
    TProp   _ann       -> compileProp
    TBool   _ann       -> return $ annotateConstant DataBool "Bool"
    TReal   _ann       -> return $ annotateConstant DataRat "ℚ"
    TInt    _ann       -> return $ annotateConstant DataInt "ℤ"
    TList   _ann t     -> annotateApp1 [DataList]   "List"   <$> compile t
    TTensor _ann t1 t2 -> annotateApp2 [AISECUtils] "Tensor" <$> compile t1 <*> compile t2
    TAdd    _ann t1 t2 -> compileNumOp2 Add Int <$> compile t1 <*> compile t2
    TCons   _ann t1 t2 -> compileCons           <$> compile t1 <*> compile t2

    TLitDim _ann i      -> return $ pretty i
    TLitDimList _ann ts -> compileLitSeq <$> traverse compile ts

instance CompileToAgda 'TARG where
  compile (TArg _ann n) = return $ pretty n

instance CompileToAgda 'EXPR where
  compile = \case
    EAnn _ann e t -> do
      ce <- compile e
      ct <- compile t
      return $ ce <+> ":" <+> ct

    ELet _ann ds e -> do
      cds <- traverse compile ds
      ce  <- compile e
      return $ "let" <+> vsep cds <+> "in" <+> ce

    ELam _ann ns e -> do
      cns <- traverse compile ns
      ce  <- compile e
      return $ "λ" <+> vsep cns <+> "→" <+> ce

    EApp _ann e1 e2 -> do
      ce1 <- compile e1
      ce2 <- compile e2
      return $ ce1 <+> parens ce2

    EVar _ann n ->
      return $ pretty n

    ETyApp _ann _e _t  ->
      return mempty

    ETyLam _ann _ns _e ->
      return mempty

    EIf _ann e1 e2 e3 -> do
      ce1 <- compile e1
      ce2 <- compile e2
      ce3 <- compile e3
      return $ "if" <+> ce1 <+> "then" <+> ce2 <+> "else" <+> ce3

    e@(EImpl    _ann e1 e2) -> compileBoolOp2 Impl  <$> booleanType e <*> compile e1 <*> compile e2
    e@(EAnd     _ann e1 e2) -> compileBoolOp2 And   <$> booleanType e <*> compile e1 <*> compile e2
    e@(EOr      _ann e1 e2) -> compileBoolOp2 Or    <$> booleanType e <*> compile e1 <*> compile e2
    e@(ENot     _ann e1)    -> compileBoolOp1 Not   <$> booleanType e <*> compile e1
    e@(ETrue    _ann)       -> compileBoolOp0 True  <$> booleanType e
    e@(EFalse   _ann)       -> compileBoolOp0 False <$> booleanType e

    EAll     _ann           -> return $ pretty ("∀" :: Text)
    EAny     _ann           -> return $ pretty ("∃" :: Text)
    e@(EEq   _ann e1 e2)    -> compileEquality e e1 e2
    e@(ENeq  _ann e1 e2)    -> compileBoolOp1 Not Prop <$> compileEquality e e1 e2

    e@(ELe   _ann e1 e2)    -> compileNumOrder' Leq e e1 e2
    e@(ELt   _ann e1 e2)    -> compileNumOrder' Lt  e e1 e2
    e@(EGe   _ann e1 e2)    -> compileNumOrder' Geq e e1 e2
    e@(EGt   _ann e1 e2)    -> compileNumOrder' Gt  e e1 e2

    e@(EMul  _ann e1 e2)    -> compileNumOp2 Mul <$> numericType e <*> compile e1 <*> compile e2
    e@(EDiv  _ann e1 e2)    -> compileNumOp2 Div <$> numericType e <*> compile e1 <*> compile e2
    e@(EAdd  _ann e1 e2)    -> compileNumOp2 Add <$> numericType e <*> compile e1 <*> compile e2
    e@(ESub  _ann e1 e2)    -> compileNumOp2 Sub <$> numericType e <*> compile e1 <*> compile e2
    e@(ENeg  _ann e1)       -> compileNumOp1 Neg <$> numericType e <*> compile e1

    ELitInt  _ann i         -> return $ pretty i
    ELitReal _ann d         -> return $ pretty d
    ECons    _ann e1 e2     -> compileCons <$> compile e1 <*> compile e2
    e@(EAt   _ann e1 e2)    -> compileLookup e e1 e2
    ELitSeq  _ann es        -> compileLitSeq <$> traverse compile es

instance CompileToAgda 'EARG where
    compile (EArg _ann n) = return $ pretty n

instance CompileToAgda 'DECL where
  compile = \case
    DeclData ann _n _t     -> throwError $ CompilationUnsupported (prov ann) "dataset"
    DeclNetw _ann n t      -> compileNetwork <$> compile n <*> compile t
    DefType  _ann n ns t   -> compileTypeDef <$> compile n <*> traverse compile ns <*> compile t
    DefFun   _ann n t ns e -> compileFunDef  <$> compile n <*> compile t <*> traverse compileArgs ns <*> compile e
      where
        compileArgs :: MonadAgdaCompile m => Either OutputTArg OutputEArg -> m Code
        compileArgs (Left  tArg) = do x <- compile tArg; return $ "{" <+> x <+> "}"
        compileArgs (Right eArg) = compile eArg

  -- Programs
instance CompileToAgda 'PROG where
  compile (Main _ann ds) = do
    cds <- traverse compile ds
    return $ vsep2 cds

-- |Compiling literal sequences
compileLitSeq :: NonEmpty Code -> Code
compileLitSeq ls = annotate ann (encloseSep "[ " " ]" " , " (NonEmpty.toList ls))
  where ann = (Set.singleton AISECUtils , maxBound :: Int)

-- |Compiling cons operator
compileCons :: Code -> Code -> Code
compileCons = annotateInfixOp2 [DataList] 5 id "∷"

-- |Compiling boolean constants
compileBoolOp0 :: Bool -> BoolType -> Code
compileBoolOp0 True  Bool = annotateConstant DataBool  "true"
compileBoolOp0 False Bool = annotateConstant DataBool  "false"
compileBoolOp0 True  Prop = annotateConstant DataUnit  "⊤"
compileBoolOp0 False Prop = annotateConstant DataEmpty "⊥"

-- |Compiling boolean unary operations
-- Only negation at the moment
compileBoolOp1 :: BooleanOp1 -> BoolType -> Code -> Code
compileBoolOp1 Not Bool = annotateOp1 [DataBool]   20 "not"
compileBoolOp1 Not Prop = annotateOp1 [RelNullary] 3  "¬"

-- |Compiling boolean binary operations
compileBoolOp2 :: BooleanOp2 -> BoolType -> Code -> Code -> Code
compileBoolOp2 Impl Bool = annotateInfixOp2 [AISECUtils]  4  id "⇒"
compileBoolOp2 And  Bool = annotateInfixOp2 [DataBool]    6  id "∧"
compileBoolOp2 Or   Bool = annotateInfixOp2 [DataBool]    5  id "∨"
compileBoolOp2 Impl Prop = annotateInfixOp2 mempty        20 id "→"
compileBoolOp2 And  Prop = annotateInfixOp2 [DataProduct] 2  id "×"
compileBoolOp2 Or   Prop = annotateInfixOp2 [DataSum]     1  id "⊎"

-- |Compiling numeric unary operations
-- Only negation at the moment
compileNumOp1 :: NumericOp1 -> NumericType -> Code -> Code
compileNumOp1 Neg Int  = annotateOp1 [DataInt] 8 "ℕ.-"
compileNumOp1 Neg Real = annotateOp1 [DataInt] 8 "ℚ.-"

-- |Compiling numeric binary operations
compileNumOp2 :: NumericOp2 -> NumericType -> Code -> Code -> Code
compileNumOp2 Mul Int  = annotateInfixOp2 [DataInt]       7 id "ℤ.*"
compileNumOp2 Div Int  = annotateInfixOp2 [DataIntDivMod] 7 id "ℤ./"
compileNumOp2 Add Int  = annotateInfixOp2 [DataInt]       6 id "ℤ.+"
compileNumOp2 Sub Int  = annotateInfixOp2 [DataInt]       6 id "ℤ.-"
compileNumOp2 Mul Real = annotateInfixOp2 [DataRat]       7 id "ℚ.*"
compileNumOp2 Div Real = annotateInfixOp2 [DataRat]       7 id "ℚ./"
compileNumOp2 Add Real = annotateInfixOp2 [DataRat]       6 id "ℚ.+"
compileNumOp2 Sub Real = annotateInfixOp2 [DataRat]       6 id "ℚ.-"

-- |Compiling numeric orders
compileNumOrder' :: MonadAgdaCompile m
                 => OrderType
                 -> OutputExpr
                 -> OutputExpr
                 -> OutputExpr
                 -> m Code
compileNumOrder' orderType e e1 e2 = do
  boolType <- booleanType e
  numType  <- numericType e1
  ce1      <- compile e1
  ce2      <- compile e2
  return $ compileNumOrder orderType numType boolType ce1 ce2

compileNumOrder :: OrderType -> NumericType -> BoolType -> Code -> Code -> Code
compileNumOrder Leq Int  Bool = annotateInfixOp2 [DataInt, RelNullary] 4 boolBraces "ℤ.≤?"
compileNumOrder Lt  Int  Bool = annotateInfixOp2 [DataInt, RelNullary] 4 boolBraces "ℤ.<?"
compileNumOrder Geq Int  Bool = annotateInfixOp2 [DataInt, RelNullary] 4 boolBraces "ℤ.≥?"
compileNumOrder Gt  Int  Bool = annotateInfixOp2 [DataInt, RelNullary] 4 boolBraces "ℤ.>?"
compileNumOrder Leq Int  Prop = annotateInfixOp2 [DataInt]             4 id         "ℤ.≤"
compileNumOrder Lt  Int  Prop = annotateInfixOp2 [DataInt]             4 id         "ℤ.≤"
compileNumOrder Geq Int  Prop = annotateInfixOp2 [DataInt]             4 id         "ℤ.≤"
compileNumOrder Gt  Int  Prop = annotateInfixOp2 [DataInt]             4 id         "ℤ.≤"
compileNumOrder Leq Real Bool = annotateInfixOp2 [DataRat, RelNullary] 4 boolBraces "ℚ.≤?"
compileNumOrder Lt  Real Bool = annotateInfixOp2 [DataRat, RelNullary] 4 boolBraces "ℚ.<?"
compileNumOrder Geq Real Bool = annotateInfixOp2 [DataRat, RelNullary] 4 boolBraces "ℚ.≥?"
compileNumOrder Gt  Real Bool = annotateInfixOp2 [DataRat, RelNullary] 4 boolBraces "ℚ.>?"
compileNumOrder Leq Real Prop = annotateInfixOp2 [DataRat]             4 id         "ℚ.≤"
compileNumOrder Lt  Real Prop = annotateInfixOp2 [DataRat]             4 id         "ℚ.<"
compileNumOrder Geq Real Prop = annotateInfixOp2 [DataRat]             4 id         "ℚ.≥"
compileNumOrder Gt  Real Prop = annotateInfixOp2 [DataRat]             4 id         "ℚ.>"

-- TODO implement this via proof by reflection
compileLookup :: MonadAgdaCompile m => OutputExpr -> OutputExpr -> OutputExpr -> m Code
compileLookup e cont (ELitInt indexAnn index) = do
  let ann = annotation e
  cType <- containerType e
  case cType of
    Tensor ns
      | index < NonEmpty.head ns -> do
        tensor <- compile cont
        return $ tensor <+> pretty index
      | otherwise            -> throwError $
        TensorIndexOutOfBounds (prov indexAnn) (annotatedType ann) index
    -- Tricky to support lists as we can't guarantee the index is within the length
    -- of the list.
    List      -> throwError $ CompilationUnsupported (prov ann) "Lookup in lists"

-- Tricky to support variable indices as we can't guarantee
-- they're bounded by the length of the list.
compileLookup ann _cont (EVar    _ _) = throwError $
  CompilationUnsupported (prov ann) "Lookup of variable indices"

-- Anything else is an error.
compileLookup ann _cont e             = throwError $
  UnexpectedExpr (prov ann) e

compileEquality :: MonadAgdaCompile m => OutputExpr -> OutputExpr -> OutputExpr -> m Code
compileEquality e e1 e2 = do
  equalityType <- booleanType e
  case equalityType of
    Prop -> compilePropEquality e1 e2
    Bool -> compileBoolEquality e1 e2

-- Equality in properties is simply compiled to propositional equality
compilePropEquality :: MonadAgdaCompile m => OutputExpr -> OutputExpr -> m Code
compilePropEquality e1 e2 = annotateInfixOp2 [PropEquality] 4 id "≡" <$> compile e1 <*> compile e2

data EqualityTypeError
  = UnexpectedEqualityType
  | PolymorphicEqualityType Symbol

-- Equality in boolean functions is more complicated as we need an actual decision procedure.
-- We handle this using instance arguments
compileBoolEquality :: MonadAgdaCompile m => OutputExpr -> OutputExpr -> m Code
compileBoolEquality e1 e2 = let t1 = annotatedType (annotation e1) in
  case equalityDependencies t1 of
    Left UnexpectedEqualityType      -> throwError $ UnexpectedType (prov e1) e1 t1 expectedTypes
      where expectedTypes = ["Tensor", "Real", "Int", "List"]
    Left (PolymorphicEqualityType n) -> throwError $ CompilationUnsupported (prov e1) ("Polymorphic equality over '" <> n <> "'")
    Right dependencies ->
      annotateInfixOp2 ([RelNullary] <> dependencies) 4 boolBraces "≟" <$> compile e1 <*> compile e2

-- Calculates the dependencies needed for equality over the provided type
equalityDependencies :: OutputType -> Either EqualityTypeError [Dependency]
equalityDependencies = \case
  TInt    _ann        -> return [DataIntInstances]
  TReal   _ann        -> return [DataRatInstances]
  TBool   _ann        -> return [DataBoolInstances]
  TList   _ann t1     -> do
    deps <- equalityDependencies t1
    return $ [DataListInstances] <> deps
  TTensor _ann t1 _t2 -> do
    deps <- equalityDependencies t1
    return $ [DataTensorInstances] <> deps
  TVar    _ann  n     -> throwError (PolymorphicEqualityType n)
  _                   -> throwError UnexpectedEqualityType

compileProp :: MonadAgdaCompile m => m Code
compileProp = do
  options <- ask
  return $ if useProp (backendOpts options)
    then "Prop"
    else "Set"

-- |Compile a declaration using the `network` keyword
compileNetwork :: Code -> Code -> Code
compileNetwork eArg eType =
  eArg <+> ":" <+> eType             <> hardline <>
  eArg <+> "= evaluate record"       <> hardline <>
    "{ databasePath = DATABASE_PATH" <> hardline <>
    "; networkUUID = NETWORK_UUID"   <> hardline <>
    "}"

compileTypeDef :: Code -> [Code] -> Code -> Code
compileTypeDef n args t = "pattern" <+> n <+> hsep args <+> "=" <+> t

compileFunDef :: Code -> Code -> [Code] -> Code -> Code
compileFunDef n t ns e =
  n <+> ":" <+> t <> hardline <>
  n <+> (if null ns then mempty else hsep ns <> " ") <> "=" <+> e

compileProperty :: Code -> Code -> Code
compileProperty eArg expr =
  eArg <+> ":" <+> expr              <> hardline <>
  eArg <+> "= checkProperty record"  <> hardline <>
    "{ databasePath = DATABASE_PATH" <> hardline <>
    "; networkUUID  = NETWORK_UUID"  <> hardline <>
    "; propertyUUID = ????"          <> hardline <>
    "}"

capitaliseTypeName :: Text -> Text
capitaliseTypeName name
  | Text.null name = name
  | otherwise =
    let (firstLetter, remainder) = Text.splitAt 1 name in
      Text.toUpper firstLetter <> remainder