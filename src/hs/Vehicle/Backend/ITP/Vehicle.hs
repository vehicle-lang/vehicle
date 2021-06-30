{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}


module Vehicle.Backend.ITP.Vehicle where

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import           Prettyprinter hiding (hsep, vsep, hcat, vcat)

import Vehicle.Backend.ITP.Core (Precedence, docAnn, hsep, vsep, vsep2)
import Vehicle.Frontend.AST (Tree)
import Vehicle.Frontend.AST.Recursive.Unsorted
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Intermediate results of compilation

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
compileLitSeq :: NonEmpty Code -> Code
compileLitSeq ls = annotate 20 (encloseSep "[ " " ]" " , " (NonEmpty.toList ls))

-----------------
-- Compilation --
-----------------
{-
compileProgramToVehicle :: TProg -> Text
compileProgramToVehicle program = let
  programDoc    = compile program
  programStream = layoutPretty defaultLayoutOptions programDoc
  programText   = renderStrict programStream
  options <- ask
  return $ intercalate "\n\n"
    [ fileHeader options "--"
    , programText
    ]
-}

--------------------------------------------------------------------------------
-- Compilation of program tree

compile :: forall sort ann. KnownSort sort => Tree ann sort -> Code
compile = foldTree compileF

compileF :: (forall sort. KnownSort sort => TreeF ann sort Code -> Code)
compileF (tree :: TreeF ann sort Code) = case sortSing :: SSort sort of
  SKIND -> mempty

  STYPE -> case tree of
    TForallF     _ann ns t  -> "forall" <+> hsep ns <+> t
    TAppF        _ann t1 t2 -> compileApp1 t1 t2
    TVarF        _ann n     -> pretty n
    TFunF        _ann t1 t2 -> compileInfixOp2 2 "->"  t1 t2
    TPropF       _ann       -> compileConstant "Prop"
    TBoolF       _ann       -> compileConstant "Bool"
    TRealF       _ann       -> compileConstant "ℚ"
    TIntF        _ann       -> compileConstant "ℤ"
    TListF       _ann t     -> compileApp1 "List"     t
    TTensorF     _ann t1 t2 -> compileApp2 "Tensor"   t1 t2
    TAddF        _ann t1 t2 -> compileInfixOp2 2 "+"  t1 t2
    TConsF       _ann t1 t2 -> compileInfixOp2 1 "::" t1 t2
    TLitDimF     _ann i     -> pretty i
    TLitDimListF _ann ts    -> compileLitSeq ts

  STARG -> case tree of
   TArgF _ann n -> pretty n

  SEXPR -> case tree of
    EAnnF     _ann e t      -> e <+> ":" <+> t
    ELetF     _ann ds e     -> "let" <+> vsep ds <+> "in" <+> e
    ELamF     _ann ns e     -> "λ" <+> vsep ns <+> "→" <+> e
    EAppF     _ann e1 e2    -> e1 <+> parens e2
    EVarF     _ann n        -> pretty n
    ETyAppF   _ann _e _t    -> mempty
    ETyLamF   _ann _ns _e   -> mempty
    EIfF      _ann e1 e2 e3 -> "if" <+> e1 <+> "then" <+> e2 <+> "else" <+> e3
    EImplF    _ann e1 e2    -> compileInfixOp2 4 "=>"  e1 e2
    EAndF     _ann e1 e2    -> compileInfixOp2 5 "and" e1 e2
    EOrF      _ann e1 e2    -> compileInfixOp2 6 "or"  e1 e2
    ENotF     _ann e        -> compileApp1     "not" e
    ETrueF    _ann          -> compileConstant "True"
    EFalseF   _ann          -> compileConstant "False"
    EAllF     _ann          -> "∀"
    EAnyF     _ann          -> "∃"
    EEqF      _ann e1 e2    -> compileInfixOp2 8 "==" e1 e2
    ENeqF     _ann e1 e2    -> compileInfixOp2 8 "!=" e1 e2
    ELeF      _ann e1 e2    -> compileInfixOp2 8 "<=" e1 e2
    ELtF      _ann e1 e2    -> compileInfixOp2 8 "<"  e1 e2
    EGeF      _ann e1 e2    -> compileInfixOp2 8 ">=" e1 e2
    EGtF      _ann e1 e2    -> compileInfixOp2 8 "<=" e1 e2
    EMulF     _ann e1 e2    -> compileInfixOp2 8 "*" e1 e2
    EDivF     _ann e1 e2    -> compileInfixOp2 8 "/" e1 e2
    EAddF     _ann e1 e2    -> compileInfixOp2 9 "+" e1 e2
    ESubF     _ann e1 e2    -> compileInfixOp2 9 "-" e1 e2
    ENegF     _ann e        -> compileApp1 "-" e
    ELitIntF  _ann i        -> pretty i
    ELitRealF _ann d        -> pretty d
    EConsF    _ann e1 e2    -> compileInfixOp2 3 "::" e1 e2
    EAtF      _ann  e1 e2   -> compileInfixOp2 11 "!"    e1 e2
    ELitSeqF  _ann es       -> compileLitSeq es

  SEARG -> case tree of
    EArgF _ann n -> pretty n

  SDECL -> case tree of
    DeclDataF _ann n t      -> "dataset" <+> n <+> ":" <+> t
    DeclNetwF _ann n t      -> "network" <+> n <+> ":" <+> t
    DefTypeF  _ann n ns t   -> "type"    <+> n <+> hsep ns <+> "=" <+> t
    DefFunF   _ann n t ns e -> n <+> ":" <+> t <> hardline <> n <+> hsep ns <+> "=" <+> e

  -- Programs
  SPROG -> case tree of
    MainF _ann ds -> vsep2 ds
