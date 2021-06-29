{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vehicle.Backend.ITP.Agda where

import           Data.List.NonEmpty (NonEmpty)
import           Data.Text as Text (Text, intercalate, unwords, pack, toUpper, null, splitAt)
import           Data.Foldable (fold)
import qualified Data.List.NonEmpty as List
import           Data.Set (Set)
import qualified Data.Set as Set
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.Reader (MonadReader(..))

import           Prettyprinter hiding (hsep, vsep, hcat, vcat)
import           Prettyprinter.Render.Text (renderStrict)

import           Vehicle.Backend.ITP.Core
import           Vehicle.Frontend.AST (Tree(..), annotation)
import           Vehicle.Prelude (Sort(..), Symbol)

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
importStatement dep = "open import " <> pack (show dep)

importStatements :: Set Dependency -> Text
importStatements deps = Text.unwords $ map importStatement $ Set.toList deps

type ModulePath = [Text]

moduleHeader :: ModulePath -> Text
moduleHeader path = "module " <> intercalate "." path <> " where"


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

type MonadAgdaCompile m = MonadCompile m AgdaOptions

compileProgramToAgda
  :: MonadAgdaCompile m
  => TProg
  -> m Text
compileProgramToAgda program = do
  programDoc <- compile program
  let programStream = layoutPretty defaultLayoutOptions programDoc
  -- Collects dependencies by first discarding precedence info and then folding using Set Monoid
  let progamDependencies = fold (reAnnotateS fst programStream)
  let programText = renderStrict programStream
  options <- ask
  return $ intercalate "\n\n"
    [ fileHeader options "--"
    , importStatements progamDependencies
    , moduleHeader (modulePath (backendOptions options))
    , programText
    ]

class CompileToAgda (sort :: Sort) where
  compile
    :: MonadAgdaCompile m
    => TTree sort
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

    TApp _ann t1 t2 -> do
      ct1 <- compile t1
      ct2 <- compile t2
      return $ ct1 <+> parens ct2

    TVar    _ann n     -> return $ pretty n
    TFun    _ann t1 t2 -> annotateInfixOp2 [] (maxBound :: Int) id "→" <$> compile t1 <*> compile t2
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

    EImpl    ann e1 e2    -> compileBoolOp2 Impl  <$> booleanType ann <*> compile e1 <*> compile e2
    EAnd     ann e1 e2    -> compileBoolOp2 And   <$> booleanType ann <*> compile e1 <*> compile e2
    EOr      ann e1 e2    -> compileBoolOp2 Or    <$> booleanType ann <*> compile e1 <*> compile e2
    ENot     ann e        -> compileBoolOp1 Not   <$> booleanType ann <*> compile e
    ETrue    ann          -> compileBoolOp0 True  <$> booleanType ann
    EFalse   ann          -> compileBoolOp0 False <$> booleanType ann

    EAll     _ann         -> return $ pretty ("∀" :: Text)
    EAny     _ann         -> return $ pretty ("∃" :: Text)
    EEq      ann e1 e2    -> compileEquality ann e1 e2
    ENeq     ann e1 e2    -> compileBoolOp1 Not Prop <$> compileEquality ann e1 e2

    ELe      ann e1 e2    -> compileNumOrder' Leq ann e1 e2
    ELt      ann e1 e2    -> compileNumOrder' Lt  ann e1 e2
    EGe      ann e1 e2    -> compileNumOrder' Geq ann e1 e2
    EGt      ann e1 e2    -> compileNumOrder' Gt  ann e1 e2

    EMul     ann e1 e2    -> compileNumOp2 Mul <$> numericType ann <*> compile e1 <*> compile e2
    EDiv     ann e1 e2    -> compileNumOp2 Div <$> numericType ann <*> compile e1 <*> compile e2
    EAdd     ann e1 e2    -> compileNumOp2 Add <$> numericType ann <*> compile e1 <*> compile e2
    ESub     ann e1 e2    -> compileNumOp2 Sub <$> numericType ann <*> compile e1 <*> compile e2
    ENeg     ann e        -> compileNumOp1 Neg <$> numericType ann <*> compile e

    ELitInt  _ann i       -> return $ pretty i
    ELitReal _ann d       -> return $ pretty d
    ECons    _ann e1 e2   -> compileCons   <$> compile e1 <*> compile e2
    EAt      ann  e1 e2   -> compileLookup ann e1 e2
    ELitSeq  _ann es      -> compileLitSeq <$> traverse compile es

instance CompileToAgda 'EARG where
    compile (EArg _ann n) = return $ pretty n

instance CompileToAgda 'DECL where
  compile = \case
    DeclData ann _n _t     -> throwError $ CompilationUnsupported "dataset" (annotatedProv ann)
    DeclNetw _ann n t      -> compileNetwork <$> compile n <*> compile t
    DefType  _ann n ns t   -> compileTypeDef <$> compile n <*> traverse compile ns <*> compile t
    DefFun   _ann n t ns e -> compileFunDef  <$> compile n <*> compile t <*> traverse compile ns <*> compile e

  -- Programs
instance CompileToAgda 'PROG where
  compile (Main _ann ds) = do
    cds <- traverse compile ds
    return $ vsep cds

-- |Compiling literal sequences
compileLitSeq :: NonEmpty Code -> Code
compileLitSeq ls = annotate ann (brackets (concatWith (\u v -> u <+> "," <+> v) ls))
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
compileNumOrder' :: MonadAgdaCompile m => OrderType -> TAnn 'EXPR -> TExpr -> TExpr -> m Code
compileNumOrder' orderType orderAnn e1 e2 = do
  numType  <- numericType (annotation e1)
  boolType <- booleanType orderAnn
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
compileLookup :: MonadAgdaCompile m => TAnn 'EXPR -> TExpr -> TExpr -> m Code
compileLookup ann cont (ELitInt indexAnn index) = do
  cType <- containerType ann
  case cType of
    Tensor ns
      | index < List.head ns -> do
        tensor <- compile cont
        return $ tensor <+> pretty index
      | otherwise            -> throwError $
        IndexOutOfBounds (annotatedType (annotation cont)) index (annotatedProv indexAnn)
    -- Tricky to support lists as we can't guarantee the index is within the length
    -- of the list.
    List      -> throwError $ CompilationUnsupported "Lookup in lists" (annotatedProv ann)

-- Tricky to support variable indices as we can't guarantee
-- they're bounded by the length of the list.
compileLookup ann _cont (EVar    _ _) = throwError $
  CompilationUnsupported "Lookup of variable indices" (annotatedProv ann)

-- Anything else is an error.
compileLookup ann _cont e             = throwError $
  UnexpectedExpr e (annotatedProv ann)

-- Calculates the dependencies needed for equality over the provided type
equalityDependencies :: MonadAgdaCompile m => TType -> m [Dependency]
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
  TVar    ann  n      -> throwError $ CompilationUnsupported ("Polymorphic equality over '" <> n <> "'") (annotatedProv ann)
  t                   -> throwError $ UnexpectedType t (annotatedProv (annotation t))

compileEquality :: MonadAgdaCompile m => TAnn 'EXPR -> TExpr -> TExpr -> m Code
compileEquality ann e1 e2 = do
  typ <- booleanType ann
  case typ of
    Prop -> annotateInfixOp2 [PropEquality] 4 id "≡" <$> compile e1 <*> compile e2
    Bool -> do
      dependencies <- equalityDependencies (annotatedType (annotation e1))
      -- Boolean equality is handled by instance arguments
      annotateInfixOp2 ([RelNullary] <> dependencies) 4 boolBraces "≟" <$> compile e1 <*> compile e2

compileProp :: MonadAgdaCompile m => m Code
compileProp = do
  options <- ask
  return $ if useProp (backendOptions options)
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
  n <+> hsep ns <+> "=" <+> e

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