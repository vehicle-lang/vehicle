{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Frontend.Print
  ( prettyFrontend
  ) where

import Data.Functor.Foldable (Recursive(..))
import Prettyprinter (annotate, encloseSep, unAnnotate, hardline)

import Vehicle.Frontend.AST
import Vehicle.Prelude

prettyFrontend :: Compile a => a -> Doc b
prettyFrontend = unAnnotate . compile

--------------------------------------------------------------------------------
-- Intermediate results of compilation

type Precedence = Int
type Code = Doc Precedence

annotateOp1 :: Precedence -> Code -> Code -> Code
annotateOp1 opPrecedence op arg =
  let bArg = bracketIfRequired opPrecedence arg in
  annotate opPrecedence (op <+> bArg)

annotateOp2 :: Bool
            -> Precedence
            -> Code
            -> Code -> Code -> Code
annotateOp2 isInfix opPrecedence op arg1 arg2 =
  let bArg1 = bracketIfRequired opPrecedence arg1 in
  let bArg2 = bracketIfRequired opPrecedence arg2 in
  annotate opPrecedence (if isInfix
    then bArg1 <+> op <+> bArg2
    else op <+> bArg1 <+> bArg2)

bracketIfRequired :: Precedence -> Code -> Code
bracketIfRequired parentPrecedence expr = case docAnn expr of
  Just exprPrecedence
    | exprPrecedence <= parentPrecedence -> parens expr
  _                                      -> expr

compileApp1 :: Code -> Code -> Code
compileApp1 = annotateOp1 20

compileApp2 :: Code -> Code -> Code -> Code
compileApp2 = annotateOp2 False 20

compileConstant :: Symbol -> Code
compileConstant s = annotate 20 (pretty s)

compileInfixOp2 :: Precedence -> Symbol -> Code -> Code -> Code
compileInfixOp2 p symbol = annotateOp2 True p (pretty symbol)

-- |Compiling literal sequences
compileSeq :: [Code] -> Code
compileSeq ls = annotate 20 (encloseSep "[ " " ]" " , " ls)

--------------------------------------------------------------------------------
-- Compilation of program tree

class Compile a where
  compile :: a -> Code

instance Compile Literal where
  compile = \case
    LNat  v -> pretty v
    LInt  v -> pretty v
    LRat  v -> pretty v
    LBool v -> pretty v

instance Compile Quantifier  where
  compile Any = "some"
  compile All = "every"

instance Compile (WithProvenance Identifier) where
  compile (WithProvenance _p n) = pretty n

instance Compile (Binder ann) where
  compile (Binder _p v n typeAnn) =
    let typAnn = maybe "" (\t -> " :" <+> compile t) typeAnn
    in  visBrackets v (pretty n <> typAnn)

instance Compile (Arg ann) where
  compile (Arg _p Explicit  e) = compile e
  compile (Arg _p Implicit _e) = ""
  compile (Arg _p Instance _e) = ""

instance Compile (LetDecl ann) where
  compile (LetDecl _p n e) = compile n <+> compile e

instance Compile (Expr ann) where
  compile = cata $ \case
     -- Core
    TypeF l                -> "Type" <+> pretty l
    ForallF  _ann ns t     -> "forall" <+> hsep (fmap compile ns) <+> t
    FunF     _ann t1 t2    -> compileInfixOp2 2 "->"  t1 t2
    AnnF     _ann e t      -> parens (e <+> ":" <+> t)
    LiteralF _ann l        -> compile l
    LetF     _ann ds e     -> "let" <+> vsep (fmap compile ds) <+> "in" <+> e
    LamF     _ann ns e     -> "\\"  <+> hsep (fmap compile ns) <+> "->" <+> e
    AppF     _ann e1 e2    -> e1 <+> parens (compile e2)
    VarF     _ann n        -> pretty n
    HoleF    _ann n        -> pretty n
    PrimDictF tc           -> "primDict" <+> tc

    -- Types
    PropF    _ann          -> compileConstant "Prop"
    BoolF    _ann          -> compileConstant "Bool"
    NatF     _ann          -> compileConstant "Nat"
    IntF     _ann          -> compileConstant "Int"
    RealF    _ann          -> compileConstant "Real"
    ListF    _ann t        -> compileApp1 "List"     t
    TensorF  _ann t1 t2    -> compileApp2 "Tensor"   t1 t2

    -- Type classes
    HasEqF       _ann e1 e2 -> compileApp2 "HasEq"       e1 e2
    HasOrdF      _ann e1 e2 -> compileApp2 "HasOrd"      e1 e2
    IsContainerF _ann e1 e2 -> compileApp2 "IsContainer" e1 e2
    IsTruthF     _ann e     -> compileApp1 "IsTruth"     e
    IsQuantF     _ann e     -> compileApp1 "IsQuant"     e
    IsNaturalF   _ann e     -> compileApp1 "IsNatural"   e
    IsIntegralF  _ann e     -> compileApp1 "IsIntegral"  e
    IsRationalF  _ann e     -> compileApp1 "IsRational"  e
    IsRealF      _ann e     -> compileApp1 "IsReal"      e

    -- Terms
    IfF      _ann e1 e2 e3 -> "if" <+> e1 <+> "then" <+> e2 <+> "else" <+> e3
    ImplF    _ann e1 e2    -> compileInfixOp2 4 "implies" e1 e2
    AndF     _ann e1 e2    -> compileInfixOp2 5 "and" e1 e2
    OrF      _ann e1 e2    -> compileInfixOp2 6 "or"  e1 e2
    NotF     _ann e        -> compileApp1     "not" e
    QuantF   _ann q b e    -> compile q <+> compile b <+> "." <+> e
    QuantInF _ann q b e1 e2-> compile q <+> compile b <+> "in" <+> e1 <+> "." <+> e2
    EqF      _ann e1 e2    -> compileInfixOp2 8 "==" e1 e2
    NeqF     _ann e1 e2    -> compileInfixOp2 8 "!=" e1 e2
    LeF      _ann e1 e2    -> compileInfixOp2 8 "<=" e1 e2
    LtF      _ann e1 e2    -> compileInfixOp2 8 "<"  e1 e2
    GeF      _ann e1 e2    -> compileInfixOp2 8 ">=" e1 e2
    GtF      _ann e1 e2    -> compileInfixOp2 8 ">"  e1 e2
    MulF     _ann e1 e2    -> compileInfixOp2 8 "*"  e1 e2
    DivF     _ann e1 e2    -> compileInfixOp2 8 "/"  e1 e2
    AddF     _ann e1 e2    -> compileInfixOp2 9 "+"  e1 e2
    SubF     _ann e1 e2    -> compileInfixOp2 9 "-"  e1 e2
    NegF     _ann e        -> compileApp1 "-" e
    SeqF     _ann es       -> compileSeq es
    ConsF    _ann e1 e2    -> compileInfixOp2 3 "::" e1 e2
    AtF      _ann e1 e2    -> compileInfixOp2 11 "!" e1 e2
    MapF     _ann e1 e2    -> compileApp2 "map" e1 e2
    --FoldF    _ann e1 e2 e3 -> _ -- compileApp3 "fold" e1 e2 e3

instance Compile (Decl ann) where
  compile = \case
    DeclData _ann n t      -> "dataset" <+> compile n <+> ":" <+> compile t
    DeclNetw _ann n t      -> "network" <+> compile n <+> ":" <+> compile t
    DefType  _ann n ns t   -> "type"    <+> compile n <+> hsep (fmap compile ns) <+> "=" <+> compile t
    DefFun   _ann n t ns e ->
      compile n <+> ":" <+> compile t <> hardline <>
      compile n <+> hsep (fmap compile ns) <+> "=" <+> compile e

instance Compile (Prog ann) where
  compile (Main ds) = vsep2 (fmap compile ds)

instance Pretty (Expr ann) where
  pretty = unAnnotate . compile