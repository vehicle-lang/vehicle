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

import Vehicle.Backend.Core
import Vehicle.Language.AST
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
  | DataNat
  | DataNatInstances
  | DataNatDivMod
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
  show DataNat             = "Data.Nat as ℕ"
  show DataNatInstances    = "Data.Nat.Instances"
  show DataNatDivMod       = "Data.Nat.DivMod as ℕ"
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

class CompileToAgda a where
  compile
    :: MonadAgdaCompile m
    => a
    -> m Code

--------------------------------------------------------------------------------
-- Compilation of program tree

instance CompileToAgda OutputBinder where
  compile (Binder _p v n Nothing)  = return $ visBrackets v (pretty n)
  compile (Binder _p v n (Just t)) = do
    t' <- compile t
    return $ visBrackets v (pretty n <+> ":" <+> t')

instance CompileToAgda OutputArg where
  compile (Arg _p v e) = visBrackets v <$> compile e

instance CompileToAgda Double where
  compile d = _

instance CompileToAgda OutputExpr where
  compile = \case
    Hole{}        -> developerError "Holes should have been removed"

    Type l -> return $ annotateConstant ("Set" <+> pretty l)

    Forall _ann ns t  -> do
      cns <- traverse compile ns
      ct  <- compile t
      return $ "∀" <+> hsep cns <+> ct

    Fun _ann t1 t2 -> annotateInfixOp2 [] 20 id "→" <$> compile t1 <*> compile t2

    Ann _ann e t -> do
      ce <- compile e
      ct <- compile t
      return $ ce <+> ":" <+> ct

    Let _ann ds e -> do
      cds <- traverse compile ds
      ce  <- compile e
      return $ "let" <+> vsep cds <+> "in" <+> ce

    Lam _ann ns e -> do
      cns <- traverse compile ns
      ce  <- compile e
      return $ "λ" <+> vsep cns <+> "→" <+> ce

    App _ann e1 e2 -> do
      ce1 <- compile e1
      ce2 <- compile e2
      return $ ce1 <+> parens ce2

    Var     _ann n  -> return $ pretty n
    Seq     _ann es -> compileSeq <$> traverse compile es
    Literal _ann l  -> compile l
{-
    e@HasEq{}       -> throwError $ CompilationUnsupported (provenanceOf e) "HasEq"
    e@HasOrd{}      -> throwError $ CompilationUnsupported (provenanceOf e) "HasOrd"
    e@IsTruth{}     -> throwError $ CompilationUnsupported (provenanceOf e) "IsTruth"
    e@IsNatural{}   -> throwError $ CompilationUnsupported (provenanceOf e) "IsNatural"
    e@IsIntegral{}  -> throwError $ CompilationUnsupported (provenanceOf e) "IsIntegral"
    e@IsRational{}  -> throwError $ CompilationUnsupported (provenanceOf e) "IsRational"
    e@IsReal{}      -> throwError $ CompilationUnsupported (provenanceOf e) "IsReal"
    e@IsQuant{}     -> throwError $ CompilationUnsupported (provenanceOf e) "IsQuant"
    e@IsContainer{} -> throwError $ CompilationUnsupported (provenanceOf e) "IsContainer"

    Prop   _ann       -> compileProp
    Bool   _ann       -> return $ annotateConstant DataBool "Bool"
    Nat    _ann       -> return $ annotateConstant DataNat "ℕ"
    Int    _ann       -> return $ annotateConstant DataInt "ℤ"
    Real   _ann       -> return $ annotateConstant DataRat "ℚ"
    List   _ann t     -> annotateApp1 [DataList]   "List"   <$> compile t
    Tensor _ann t1 t2 -> annotateApp2 [AISECUtils] "Tensor" <$> compile t1 <*> compile t2

    If _ann e1 e2 e3 -> do
      ce1 <- compile e1
      ce2 <- compile e2
      ce3 <- compile e3
      return $ "if" <+> ce1 <+> "then" <+> ce2 <+> "else" <+> ce3

    e@(Impl    _ann e1 e2) -> compileBoolOp2 ImplOp (truthType e) <$> compile e1 <*> compile e2
    e@(And     _ann e1 e2) -> compileBoolOp2 AndOp  (truthType e) <$> compile e1 <*> compile e2
    e@(Or      _ann e1 e2) -> compileBoolOp2 OrOp   (truthType e) <$> compile e1 <*> compile e2
    e@(Not     _ann e1)    -> compileBoolOp1 NotOp  (truthType e) <$> compile e1

    All     _ann           -> return $ pretty ("∀" :: Text)
    Any     _ann           -> return $ pretty ("∃" :: Text)
    e@(Eq   _ann e1 e2)    -> compileEquality e e1 e2
    e@(Neq  _ann e1 e2)    -> compileBoolOp1 NotOp Prop <$> compileEquality e e1 e2

    e@(Le   _ann e1 e2)    -> compileNumOrder' LeqOp e e1 e2
    e@(Lt   _ann e1 e2)    -> compileNumOrder' LtOp  e e1 e2
    e@(Ge   _ann e1 e2)    -> compileNumOrder' GeqOp e e1 e2
    e@(Gt   _ann e1 e2)    -> compileNumOrder' GtOp  e e1 e2

    e@(Mul  _ann e1 e2)    -> compileNumOp2 MulOp (numericType e) <$> compile e1 <*> compile e2
    e@(Div  _ann e1 e2)    -> compileNumOp2 DivOp (numericType e) <$> compile e1 <*> compile e2
    e@(Add  _ann e1 e2)    -> compileNumOp2 AddOp (numericType e) <$> compile e1 <*> compile e2
    e@(Sub  _ann e1 e2)    -> compileNumOp2 SubOp (numericType e) <$> compile e1 <*> compile e2
    e@(Neg  _ann e1)       -> compileNumOp1 NegOp (numericType e) <$> compile e1

    Cons    _ann e1 e2     -> compileCons <$> compile e1 <*> compile e2
    e@(At   _ann e1 e2)    -> compileLookup e e1 e2


instance CompileToAgda OutputDecl where
  compile = \case
    DeclData ann _n _t     -> throwError $ CompilationUnsupported (provenanceOf ann) "dataset"
    DeclNetw _ann n t      -> compileNetwork <$> compile n <*> compile t
    DefType  _ann n ns t   -> compileTypeDef <$> compile n <*> traverse compile ns <*> compile t
    DefFun   _ann n t ns e -> compileFunDef  <$> compile n <*> compile t <*> traverse compile ns <*> compile e

  -- Programs
instance CompileToAgda OutputProg where
  compile (Main ds) = do
    cds <- traverse compile ds
    return $ vsep2 cds

-- |Compiling sequences
compileSeq :: [Code] -> Code
compileSeq ls = annotate ann (encloseSep "[ " " ]" " , " ls)
  where ann = (Set.singleton AISECUtils , maxBound :: Int)

-- |Compiling cons operator
compileCons :: Code -> Code -> Code
compileCons = annotateInfixOp2 [DataList] 5 id "∷"

instance CompileToAgda Literal where
  compile = \case
    LNat  n -> return $ pretty n
    LInt  i -> return $ pretty i
    LRat  r -> compile r
    LBool b -> _
    {-
    e@(True    _ann)       -> compileBoolOp0 True  <$> booleanType e
    e@(False   _ann)       -> compileBoolOp0 False <$> booleanType e
    LitInt  _ann i         -> return $ pretty i
    LitReal _ann d         -> return $ pretty d
    -}

-- |Compiling boolean constants
compileBoolOp0 :: Bool -> TruthType -> Code
compileBoolOp0 True  TBool = annotateConstant DataBool  "true"
compileBoolOp0 False TBool = annotateConstant DataBool  "false"
compileBoolOp0 True  TProp = annotateConstant DataUnit  "⊤"
compileBoolOp0 False TProp = annotateConstant DataEmpty "⊥"

-- |Compiling boolean unary operations
-- Only negation at the moment
compileBoolOp1 :: BooleanOp1 -> TruthType -> Code -> Code
compileBoolOp1 NotOp TBool = annotateOp1 [DataBool]   20 "not"
compileBoolOp1 NotOp TProp = annotateOp1 [RelNullary] 3  "¬"

-- |Compiling boolean binary operations
compileBoolOp2 :: BooleanOp2 -> TruthType -> Code -> Code -> Code
compileBoolOp2 ImplOp TBool = annotateInfixOp2 [AISECUtils]  4  id "⇒"
compileBoolOp2 AndOp  TBool = annotateInfixOp2 [DataBool]    6  id "∧"
compileBoolOp2 OrOp   TBool = annotateInfixOp2 [DataBool]    5  id "∨"
compileBoolOp2 ImplOp TProp = annotateInfixOp2 mempty        20 id "→"
compileBoolOp2 AndOp  TProp = annotateInfixOp2 [DataProduct] 2  id "×"
compileBoolOp2 OrOp   TProp = annotateInfixOp2 [DataSum]     1  id "⊎"

-- |Compiling numeric unary operations
-- Only negation at the moment
compileNumOp1 :: NumericOp1 -> NumericType -> Code -> Code
compileNumOp1 NegOp TNat  = developerError "Negation is not supported for naturals"
compileNumOp1 NegOp TInt  = annotateOp1 [DataInt] 8 "ℕ.-"
compileNumOp1 NegOp TReal = annotateOp1 [DataInt] 8 "ℚ.-"

-- |Compiling numeric binary operations
compileNumOp2 :: NumericOp2 -> NumericType -> Code -> Code -> Code
compileNumOp2 MulOp TNat  = annotateInfixOp2 [DataNat]       7 id "ℕ.*"
compileNumOp2 DivOp TNat  = annotateInfixOp2 [DataNatDivMod] 7 id "ℕ./"
compileNumOp2 AddOp TNat  = annotateInfixOp2 [DataNat]       6 id "ℕ.+"
compileNumOp2 SubOp TNat  = annotateInfixOp2 [DataNat]       6 id "ℕ.-"
compileNumOp2 MulOp TInt  = annotateInfixOp2 [DataInt]       7 id "ℤ.*"
compileNumOp2 DivOp TInt  = annotateInfixOp2 [DataIntDivMod] 7 id "ℤ./"
compileNumOp2 AddOp TInt  = annotateInfixOp2 [DataInt]       6 id "ℤ.+"
compileNumOp2 SubOp TInt  = annotateInfixOp2 [DataInt]       6 id "ℤ.-"
compileNumOp2 MulOp TReal = annotateInfixOp2 [DataRat]       7 id "ℚ.*"
compileNumOp2 DivOp TReal = annotateInfixOp2 [DataRat]       7 id "ℚ./"
compileNumOp2 AddOp TReal = annotateInfixOp2 [DataRat]       6 id "ℚ.+"
compileNumOp2 SubOp TReal = annotateInfixOp2 [DataRat]       6 id "ℚ.-"

-- |Compiling numeric orders
compileNumOrder' :: MonadAgdaCompile m
                 => OrderType
                 -> OutputExpr
                 -> OutputExpr
                 -> OutputExpr
                 -> m Code
compileNumOrder' orderType e e1 e2 =
  compileNumOrder orderType (numericType e1) (truthType e) <$> compile e1 <*> compile e2

compileNumOrder :: OrderType -> NumericType -> TruthType -> Code -> Code -> Code
compileNumOrder LeqOp TNat  TBool = annotateInfixOp2 [DataNat, RelNullary] 4 boolBraces "ℕ.≤?"
compileNumOrder LtOp  TNat  TBool = annotateInfixOp2 [DataNat, RelNullary] 4 boolBraces "ℕ.<?"
compileNumOrder GeqOp TNat  TBool = annotateInfixOp2 [DataNat, RelNullary] 4 boolBraces "ℕ.≥?"
compileNumOrder GtOp  TNat  TBool = annotateInfixOp2 [DataNat, RelNullary] 4 boolBraces "ℕ.>?"
compileNumOrder LeqOp TNat  TProp = annotateInfixOp2 [DataNat]             4 id         "ℕ.≤"
compileNumOrder LtOp  TNat  TProp = annotateInfixOp2 [DataNat]             4 id         "ℕ.≤"
compileNumOrder GeqOp TNat  TProp = annotateInfixOp2 [DataNat]             4 id         "ℕ.≤"
compileNumOrder GtOp  TNat  TProp = annotateInfixOp2 [DataNat]             4 id         "ℕ.≤"
compileNumOrder LeqOp TInt  TBool = annotateInfixOp2 [DataInt, RelNullary] 4 boolBraces "ℤ.≤?"
compileNumOrder LtOp  TInt  TBool = annotateInfixOp2 [DataInt, RelNullary] 4 boolBraces "ℤ.<?"
compileNumOrder GeqOp TInt  TBool = annotateInfixOp2 [DataInt, RelNullary] 4 boolBraces "ℤ.≥?"
compileNumOrder GtOp  TInt  TBool = annotateInfixOp2 [DataInt, RelNullary] 4 boolBraces "ℤ.>?"
compileNumOrder LeqOp TInt  TProp = annotateInfixOp2 [DataInt]             4 id         "ℤ.≤"
compileNumOrder LtOp  TInt  TProp = annotateInfixOp2 [DataInt]             4 id         "ℤ.≤"
compileNumOrder GeqOp TInt  TProp = annotateInfixOp2 [DataInt]             4 id         "ℤ.≤"
compileNumOrder GtOp  TInt  TProp = annotateInfixOp2 [DataInt]             4 id         "ℤ.≤"
compileNumOrder LeqOp TReal TBool = annotateInfixOp2 [DataRat, RelNullary] 4 boolBraces "ℚ.≤?"
compileNumOrder LtOp  TReal TBool = annotateInfixOp2 [DataRat, RelNullary] 4 boolBraces "ℚ.<?"
compileNumOrder GeqOp TReal TBool = annotateInfixOp2 [DataRat, RelNullary] 4 boolBraces "ℚ.≥?"
compileNumOrder GtOp  TReal TBool = annotateInfixOp2 [DataRat, RelNullary] 4 boolBraces "ℚ.>?"
compileNumOrder LeqOp TReal TProp = annotateInfixOp2 [DataRat]             4 id         "ℚ.≤"
compileNumOrder LtOp  TReal TProp = annotateInfixOp2 [DataRat]             4 id         "ℚ.<"
compileNumOrder GeqOp TReal TProp = annotateInfixOp2 [DataRat]             4 id         "ℚ.≥"
compileNumOrder GtOp  TReal TProp = annotateInfixOp2 [DataRat]             4 id         "ℚ.>"

-- TODO implement this via proof by reflection
compileLookup :: MonadAgdaCompile m => OutputExpr -> OutputExpr -> OutputExpr -> m Code
compileLookup e cont (Literal indexAnn (LNat index)) = do
  let ann = annotation e
  case containerType e of
    TTensor ns
      | index < NonEmpty.head ns -> do
        tensor <- compile cont
        return $ tensor <+> pretty index
      | otherwise            -> throwError $
        TensorIndexOutOfBounds (provenanceOf indexAnn) (getType e) (fromIntegral index)
    -- Tricky to support lists as we can't guarantee the index is within the length
    -- of the list.
    TList      -> throwError $ CompilationUnsupported (provenanceOf ann) "Lookup in lists"
-- Tricky to support variable indices as we can't guarantee
-- they're bounded by the length of the list.
compileLookup e _cont (Var    _ _) = throwError $
  CompilationUnsupported (provenanceOf e) "Lookup of variable indices"
-- Anything else is an error.
compileLookup e _cont index = unexpectedExprError (provenanceOf index) e ["Natural", "Var"]

compileEquality :: MonadAgdaCompile m => OutputExpr -> OutputExpr -> OutputExpr -> m Code
compileEquality e e1 e2 = case truthType e of
  TProp -> compilePropEquality e1 e2
  TBool -> compileBoolEquality e1 e2

-- Equality in properties is simply compiled to propositional equality
compilePropEquality :: MonadAgdaCompile m => OutputExpr -> OutputExpr -> m Code
compilePropEquality e1 e2 = annotateInfixOp2 [PropEquality] 4 id "≡" <$> compile e1 <*> compile e2

data EqualityTypeError
  = UnexpectedEqualityType
  | PolymorphicEqualityType Symbol

-- Equality in boolean functions is more complicated as we need an actual decision procedure.
-- We handle this using instance arguments
compileBoolEquality :: MonadAgdaCompile m => OutputExpr -> OutputExpr -> m Code
compileBoolEquality e1 e2 = let t1 = getType e1 in
  case equalityDependencies t1 of
    Left UnexpectedEqualityType      -> unexpectedTypeError e1 t1 expectedTypes
      where expectedTypes = ["Tensor", "Real", "Int", "List"]
    Left (PolymorphicEqualityType n) -> throwError $ CompilationUnsupported (provenanceOf e1) ("Polymorphic equality over '" <> n <> "'")
    Right dependencies ->
      annotateInfixOp2 ([RelNullary] <> dependencies) 4 boolBraces "≟" <$> compile e1 <*> compile e2

-- Calculates the dependencies needed for equality over the provided type
equalityDependencies :: OutputExpr -> Either EqualityTypeError [Dependency]
equalityDependencies = \case
  Nat    _ann        -> return [DataNatInstances]
  Int    _ann        -> return [DataIntInstances]
  Real   _ann        -> return [DataRatInstances]
  Bool   _ann        -> return [DataBoolInstances]
  List   _ann t1     -> do
    deps <- equalityDependencies t1
    return $ [DataListInstances] <> deps
  Tensor _ann t1 _t2 -> do
    deps <- equalityDependencies t1
    return $ [DataTensorInstances] <> deps
  Var    _ann  n     -> throwError (PolymorphicEqualityType n)
  _                  -> throwError UnexpectedEqualityType

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
-}