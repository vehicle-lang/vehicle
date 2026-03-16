{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Vehicle.Compile.Sugar.Core where

import Data.Text (Text, pack)
import Vehicle.Compile.Prelude (Provenance)
import Vehicle.Data.AST.Expr.Desugared (Expr)
import Vehicle.Syntax.External.Abs qualified as B
import Vehicle.Syntax.Token (mkToken)

class DesugarableBuiltin builtin where
  elabUnitLiteral :: Provenance -> Expr builtin
  elabBoolLiteral :: Provenance -> Bool -> Expr builtin
  elabNatLiteral :: Provenance -> Int -> Expr builtin
  elabDecimalLiteral :: Provenance -> Rational -> Expr builtin

{-
  foreachBuiltin :: Expr builtin
  forallBuiltin :: Expr builtin
  existsBuiltin :: Expr builtin
  forallInBuiltin :: Expr builtin
  existsInBuiltin :: Expr builtin

  addBuiltin :: Expr builtin
  subBuiltin :: Expr builtin
  mulBuiltin :: Expr builtin
  divBuiltin :: Expr builtin
  negBuiltin :: Expr builtin

  ifBuiltin :: Expr builtin
  implBuiltin :: Expr builtin
  andBuiltin :: Expr builtin
  orBuiltin :: Expr builtin
  notBuiltin :: Expr builtin
  neBuiltin :: Expr builtin
  compBuiltin :: ComparisonOp -> Expr builtin
  compPointBuiltin :: ComparisonOp -> Expr builtin

  vecLitBuiltin :: Expr builtin
  consBuiltin :: Expr builtin
  atBuiltin :: Expr builtin
  -}
pattern InferableOption :: Text
pattern InferableOption = "infer"

pattern DefaultOption :: Text
pattern DefaultOption = "default"

tokType :: Int -> B.Expr
tokType l = B.Type (mkToken B.TokType ("Type" <> pack (show l)))

builtinAnn = mkToken B.TokAnnotation "@builtin"

networkAnn = mkToken B.TokAnnotation "@network"

datasetAnn = mkToken B.TokAnnotation "@dataset"

parameterAnn = mkToken B.TokAnnotation "@parameter"

propertyAnn = mkToken B.TokAnnotation "@property"

differentiableLogicAnn = mkToken B.TokAnnotation "@differentiableLogic"

tensorAnn = mkToken B.TokAnnotation "@tensor"

instanceAnn = mkToken B.TokAnnotation "@instance"

typeClassAnn = mkToken B.TokAnnotation "@typeclass"

tokArrow = mkToken B.TokArrow "->"

tokForallT = mkToken B.TokForallT "forallT"

tokIf = mkToken B.TokIf "if"

tokThen = mkToken B.TokThen "then"

tokElse = mkToken B.TokElse "else"

tokLet = mkToken B.TokLet "let"

tokElemOf = mkToken B.TokElemOf ":"

tokLambda = mkToken B.TokLambda "\\"

tokVector = mkToken B.TokVector "Vector"

tokTensor = mkToken B.TokTensor "Tensor"

tokNonCastingTensor = mkToken B.TokNonCastingTensor "NonCastingTensor"

tokUnit = mkToken B.TokUnit "Unit"

tokBool = mkToken B.TokBool "Bool"

tokList = mkToken B.TokList "List"

tokReal = mkToken B.TokReal "Real"

tokNat = mkToken B.TokNat "Nat"

tokIndex = mkToken B.TokIndex "Index"

tokForall = mkToken B.TokForall "forall"

tokExists = mkToken B.TokExists "exists"

tokForeach = mkToken B.TokForeach "foreach"

tokImpl = mkToken B.TokImpl "=>"

tokAnd = mkToken B.TokAnd "and"

tokOr = mkToken B.TokOr "or"

tokNot = mkToken B.TokNot "not"

tokEq = mkToken B.TokEq "=="

tokNe = mkToken B.TokNe "!="

tokLe = mkToken B.TokLe "<="

tokLt = mkToken B.TokLt "<"

tokGe = mkToken B.TokGe ">="

tokGt = mkToken B.TokGt ">"

compareIndexEq = mkToken B.TokCompareIndexEq "compareIndexEq"

compareIndexNe = mkToken B.TokCompareIndexNe "compareIndexNe"

compareIndexLe = mkToken B.TokCompareIndexLe "compareIndexLe"

compareIndexLt = mkToken B.TokCompareIndexLe "compareIndexLt"

compareIndexGe = mkToken B.TokCompareIndexGe "compareIndexGe"

compareIndexGt = mkToken B.TokCompareIndexGt "compareIndexGt"

compareNatEq = mkToken B.TokCompareNatEq "compareNatEq"

compareNatNe = mkToken B.TokCompareNatNe "compareNatNe"

compareNatLe = mkToken B.TokCompareNatLe "compareNatLe"

compareNatLt = mkToken B.TokCompareNatLt "compareNatLt"

compareNatGe = mkToken B.TokCompareNatGe "compareNatGe"

compareNatGt = mkToken B.TokCompareNatGt "compareNatGt"

compareRatTensorPointwiseEq = mkToken B.TokCompareRatTensorPointwiseEq "compareRatTensorPointwiseEq"

compareRatTensorPointwiseNe = mkToken B.TokCompareRatTensorPointwiseNe "compareRatTensorPointwiseNe"

compareRatTensorPointwiseLe = mkToken B.TokCompareRatTensorPointwiseLe "compareRatTensorPointwiseLe"

compareRatTensorPointwiseLt = mkToken B.TokCompareRatTensorPointwiseLt "compareRatTensorPointwiseLt"

compareRatTensorPointwiseGe = mkToken B.TokCompareRatTensorPointwiseGe "compareRatTensorPointwiseGe"

compareRatTensorPointwisedGt = mkToken B.TokCompareRatTensorPointwiseGt "compareRatTensorPointwiseGt"

compareRatTensorReducedEq = mkToken B.TokCompareRatTensorReducedEq "compareRatTensorReducedEq"

compareRatTensorReducedNe = mkToken B.TokCompareRatTensorReducedNe "compareRatTensorReducedNe"

compareRatTensorReducedLe = mkToken B.TokCompareRatTensorReducedLe "compareRatTensorReducedLe"

compareRatTensorReducedLt = mkToken B.TokCompareRatTensorReducedLt "compareRatTensorReducedLt"

compareRatTensorReducedGe = mkToken B.TokCompareRatTensorReducedGe "compareRatTensorReducedGe"

compareRatTensorReducedGt = mkToken B.TokCompareRatTensorReducedGt "compareRatTensorReducedGt"

tokEqPoint = mkToken B.TokEqPoint "==."

tokNePoint = mkToken B.TokNePoint "!=."

tokLePoint = mkToken B.TokLePoint "<=."

tokLtPoint = mkToken B.TokLtPoint "<."

tokGePoint = mkToken B.TokGePoint ">=."

tokGtPoint = mkToken B.TokGtPoint ">."

tokAdd = mkToken B.TokAdd "+"

tokSub = mkToken B.TokSub "-"

tokMul = mkToken B.TokMul "*"

tokDiv = mkToken B.TokDiv "/"

tokMin = mkToken B.TokMin "min"

tokMax = mkToken B.TokMax "max"

tokAddNat = mkToken B.TokAddNat "addNat"

tokMulNat = mkToken B.TokAddNat "mulNat"

tokAddRealTensor = mkToken B.TokAddRealTensor "addRealTensor"

tokSubRealTensor = mkToken B.TokSubRealTensor "subRealTensor"

tokMulRealTensor = mkToken B.TokSubRealTensor "mulRealTensor"

tokDivRealTensor = mkToken B.TokSubRealTensor "divRealTensor"

tokSeqOpen = mkToken B.TokSeqOpen "["

tokSeqClose = mkToken B.TokSeqClose "]"

tokNil = mkToken B.TokNil "nil"

tokCons = mkToken B.TokCons "::"

tokAt = mkToken B.TokAt "!"

tokConst = mkToken B.TokConst "const"

tokMap = mkToken B.TokMap "map"

tokFold = mkToken B.TokFold "fold"

tokReduceAnd = mkToken B.TokReduceAnd "reduceAnd"

tokReduceOr = mkToken B.TokReduceOr "reduceOr"

tokReduceAdd = mkToken B.TokReduceAdd "reduceAdd"

tokReduceMul = mkToken B.TokReduceMul "reduceMul"

tokReduceMin = mkToken B.TokReduceMin "reduceMin"

tokReduceMax = mkToken B.TokReduceMax "reduceMax"

tokHasEq = mkToken B.TokHasEq "HasEq"

tokHasNotEq = mkToken B.TokHasNotEq "HasNotEq"

tokHasLeq = mkToken B.TokHasLeq "HasLeq"

tokHasMap = mkToken B.TokHasMap "HasMap"

tokHasFold = mkToken B.TokHasFold "HasFold"
