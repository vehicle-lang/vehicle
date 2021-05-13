{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Vehicle.Backend.Agda where

import           Data.Text as Text (Text, intercalate, unwords, pack, toUpper, null, splitAt)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Control.Monad (liftM2)
import           Control.Monad.Except (MonadError)
import           Control.Monad.Error.Class (throwError)
import           Vehicle.Backend.Core
import           Vehicle.Core.AST (Tree(..), Expr, Type, Decl, Prog, EArg, TArg, Builtin(..), BuiltinOp(..))
import           Vehicle.Core.Abs (Name(..))
import           Vehicle.Prelude (K(..), Symbol)

commentToken :: Text
commentToken = "--"

nameSymbol :: Name -> Symbol
nameSymbol (Name (_ , name)) = name

eArgName :: EArg (K Name) builtin ann -> Symbol
eArgName (EArg _ (K name))= nameSymbol name

tArgName :: TArg (K Name) builtin ann -> Symbol
tArgName (TArg _ (K name))= nameSymbol name

-------------------------
-- Module dependencies --
-------------------------

data Dependency
  -- AISEC agda library
  = AISECCore
  | AISECUtils
  -- Standard library
  | DataUnit
  | DataEmpty
  | DataProduct
  | DataSum
  | DataNat
  | DataVec
  | DataRat
  | DataBool
  | DataList
  | PropEquality
  deriving (Eq, Ord)

instance Show Dependency where
  show AISECCore = "AISEC.Core"
  show AISECUtils = "AISEC.Utils"
  show DataUnit = "Data.Unit"
  show DataEmpty = "Data.Empty"
  show DataProduct = "Data.Product"
  show DataSum = "Data.Sum"
  show DataNat = "Data.Nat"
  show DataVec = "Data.Vec"
  show DataRat = "Data.Rational"
  show DataBool = "Data.Bool"
  show DataList = "Data.List"
  show PropEquality = "Relation.Binary.PropositionalEquality"

importStatement :: Dependency -> Text
importStatement dep = "open import " <> pack (show dep)

importStatements :: Set Dependency -> Text
importStatements deps = Text.unwords $ map importStatement $ Set.toList deps

-----------------
-- Module path --
-----------------

type ModulePath = [Text]

moduleHeader :: ModulePath -> Text
moduleHeader path = "module " <> intercalate "." path <> " where"

------------------------------------------
-- Intermediate results of compilation  --
------------------------------------------

data CompilationError
    = CompilationUnsupported Text

-- |Constraint for the monad stack used by the Compiler.
type MonadCompilation m = MonadError CompilationError m

data Code = Code
  { code :: Text
  , dependencies :: Set Dependency
  }

instance Semigroup Code where
  (<>) x y = Code (code x <> code y) (dependencies x <> dependencies y)

instance Monoid Code where
  mempty = Code "" Set.empty

comp :: MonadCompilation m => Text -> m Code
comp txt = return $ Code txt mempty

compWithDep :: MonadCompilation m => Text -> Dependency -> m Code
compWithDep txt dep = return $ Code txt (Set.singleton dep)

concatWith :: MonadCompilation m => Text -> [m Code] -> m Code
concatWith txt = foldr (liftM2 appendWith) (return mempty)
  where
  appendWith :: Code -> Code -> Code
  appendWith a b = Code (code a <> txt <> code b) (dependencies a <> dependencies b)

concatCode :: MonadCompilation m => [m Code] -> m Code
concatCode = concatWith " "

concatLines :: MonadCompilation m => [m Code] -> m Code
concatLines = concatWith "\n"

concatDecls :: MonadCompilation m => [m Code] -> m Code
concatDecls = concatWith "\n\n"

concatList :: MonadCompilation m => [m Code] -> m Code
concatList = concatWith " , "

-----------------
-- Compilation  --
-----------------

class Compile vf where
  compile
    :: MonadCompilation m
    => vf   -- ^ The tree to convert
    -> m Code

compileProgramToAgda
  :: MonadCompilation m
  => TransProg ann
  -> ModulePath
  -> m Text
compileProgramToAgda program modulePath = do
  progResult <- compile program
  return $ intercalate "\n\n"
    [ fileHeader commentToken
    , importStatements (dependencies progResult)
    , moduleHeader modulePath
    , code progResult
    ]

instance Compile (TransProg ann) where
  compile (Main _ decls) = concatDecls $ map compile decls

instance Compile (TransDecl ann) where
  compile (DefType _ann arg args typ) = do
    let typeName = tArgName arg
    let typeArgs = Text.unwords (map tArgName args)
    concatCode [ comp typeName , comp typeArgs , comp "=" , compile typ]

  compile (DefFun _ann arg typ expr) =
    if isVerificationCondition typ
      then compileVerificationCondition arg expr
      else let
        funName = comp $ eArgName arg
        line1 = concatCode [ funName, comp ":", compile typ ]
        line2 = concatCode [ funName, comp "=", compile expr ]
      in
        concatLines [ line1, line2]

  compile DeclData {} = throwError $ CompilationUnsupported "dataset"
  compile DeclNetw {} = throwError $ CompilationUnsupported "network"

instance Compile (TransType ann) where
  compile (TLitDim _ann d) = comp $ pack $ show d
  compile (TMeta _ann _i) = throwError $ CompilationUnsupported "TMeta"
  compile (TVar _ann (K name)) = comp $ nameSymbol name
  compile (TLitList _ann typs) = concatCode [ compWithDep "[" DataList, concatList $ map compile typs, comp "]"]

  compile (TForall _ann arg forallBody) = concatCode [ comp "∀" , comp $ tArgName arg,  comp "→" , compile forallBody ]

  compile (TOp0 TBool _ _) = compWithDep "Bool" DataBool
  compile (TOp0 TInt _ _) = compWithDep "ℕ" DataNat
  compile (TOp0 TReal _ _) = compWithDep "ℚ" DataRat
  compile (TOp1 TList t1 _ _ _) = concatCode [ compWithDep "List" DataList , compile t1 ]
  compile (TOp2 TTensor t1 t2 _ _ _ _) = concatCode [ compWithDep "Tensor" AISECUtils , compile t1 , compile t2 ]
  compile (TOp2 TAdd t1 t2 _ _ _ _) = concatCode [ compile t1 , compWithDep "+" DataNat , compile t2 ]
  compile (TOp2 TCons t1 t2 _ _ _ _) = concatCode [ compile t1 , compWithDep "∷" DataList , compile t2 ]
  compile (TOp2 TFun t1 t2 _ _ _ _) = concatCode [ compile t1 , comp "→" , compile t2 ]
  compile _typ = throwError $ CompilationUnsupported "Unknown type application"

instance Compile (TransExpr ann) where
  compile (ELitInt _ n) = comp $ pack $ show n
  compile (ELitReal _ _j) = throwError $ CompilationUnsupported "Real literals"
  compile (EVar _ (K name)) = comp $ nameSymbol name

  compile (ELitSeq _ exprs) = concatCode [ compWithDep "[" DataList, concatList $ map compile exprs, comp "]"]
  compile (EAnn _ expr typ) = concatCode [ comp "(", compile expr, comp ":", compile typ, comp ")"]
  compile (ELam _ arg expr) = concatCode [ comp "λ", comp $ eArgName arg, comp "→" , compile expr]
  compile (ETyApp _ann _expr _typ) = throwError $ CompilationUnsupported "ETyApp"
  compile (ETyLam _ann _targ _expr) = throwError $ CompilationUnsupported "ETyLam"
  compile (ELet _ arg value body) = concatCode [ comp "let", comp $ eArgName arg, comp "=", compile value, comp "in", compile body ]

  compile (EOp0 ETrue _ _) = compWithDep "⊤" DataUnit
  compile (EOp0 EFalse _ _) = compWithDep "⊥" DataEmpty
  compile (EOp1 ENot e1 _ _ _) = concatCode [ compWithDep "not" DataNat, compile e1 ]
  compile (EOp2 EImpl e1 e2 _ _ _ _) = concatCode [ compile e1, comp "→", compile e2 ]
  compile (EOp2 EAnd e1 e2 _ _ _ _) = concatCode [ compile e1, compWithDep "×" DataProduct, compile e2 ]
  compile (EOp2 EOr e1 e2 _ _ _ _) = concatCode [ compile e1, compWithDep "⊎" DataSum, compile e2 ]
  compile (EOp2 EEq e1 e2 _ _ _ _) = concatCode [ compile e1, compWithDep "≡" PropEquality, compile e2 ]
  compile (EOp2 ENeq e1 e2 _ _ _ _) = concatCode [ compile e1, compWithDep "≢" PropEquality, compile e2 ]
  compile (EOp3 EIf e1 e2 e3 _ _ _ _ _) = concatCode [ compWithDep "if" DataBool, compile e1, comp "then", compile e2, comp "else", compile e3]

  compile (EOp0 ENil _ _) = compWithDep "[]" DataList
  compile (EOp2 ECons e1 e2 _ _ _ _) = concatCode [ compile e1, compWithDep "::" DataList, compile e2 ]
  compile (EOp2 EAt   e1 e2 _ _ _ _) = concatCode [ compWithDep "lookup" DataList, compile e1, compile e2 ]

  -- TODO need typing information to distinguish between naturals and reals/rationals.
  compile (EOp1 ENeg e1 _ _ _) = concatCode [ compWithDep "-" DataRat , compile e1]
  compile (EOp2 ELe  e1 e2 _ _ _ _) = concatCode [ compile e1, compWithDep "≤" DataNat, compile e1, compile e2 ]
  compile (EOp2 ELt  e1 e2 _ _ _ _) = concatCode [ compile e1, compWithDep "<" DataNat, compile e1, compile e2 ]
  compile (EOp2 EGe  e1 e2 _ _ _ _) = concatCode [ compile e1, compWithDep "≥" DataNat, compile e1, compile e2 ]
  compile (EOp2 EGt  e1 e2 _ _ _ _) = concatCode [ compile e1, compWithDep ">" DataNat, compile e2 ]
  compile (EOp2 EMul e1 e2 _ _ _ _) = concatCode [ compile e1, compWithDep "*" DataNat, compile e2 ]
  compile (EOp2 EDiv e1 e2 _ _ _ _) = concatCode [ compile e1, compWithDep "/" DataRat , compile e1 , compile e2 ]
  compile (EOp2 EAdd e1 e2 _ _ _ _) = concatCode [ compile e1, compWithDep "+" DataNat, compile e1, compile e2 ]
  compile (EOp2 ESub e1 e2 _ _ _ _) = concatCode [ compile e1, compWithDep "∸" DataNat, compile e1, compile e2 ]

  compile (EOp2 EAll e1 e2 _ _ _ _) = concatCode [ comp "∀", compile e1, comp "→", compile e2 ]
  compile (EOp2 EAny e1 e2 _ _ _ _) = concatCode [ comp "∃", compile e1, comp "→", compile e2 ]

  compile _expr = throwError $ CompilationUnsupported "Unknown expression application"

isVerificationCondition :: TransType ann -> Bool
isVerificationCondition (TOp0 TBool _ _) = True
isVerificationCondition _                = False

compileVerificationCondition :: MonadCompilation m => TransEArg ann -> TransExpr ann -> m Code
compileVerificationCondition arg expr =
  let
    line1 = concatCode [ comp $ eArgName arg , comp ":", compile expr ]
    line2 = comp "?" -- TODO
  in
    concatLines [line1 , line2]

capitaliseTypeName :: Text -> Text
capitaliseTypeName name
  | Text.null name = name
  | otherwise =
    let (firstLetter, remainder) = Text.splitAt 1 name in
      Text.toUpper firstLetter <> remainder
