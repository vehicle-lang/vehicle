{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Vehicle.Compile.Sugar.Core where

import Data.Text (Text, pack)
import Vehicle.Syntax.External.Abs qualified as B
import Vehicle.Syntax.Token (mkToken)

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
