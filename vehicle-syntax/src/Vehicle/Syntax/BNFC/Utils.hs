{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Vehicle.Syntax.BNFC.Utils where

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader (..), asks)
import Data.Text (Text, pack)
import Vehicle.Syntax.AST.Name (ModulePath)
import Vehicle.Syntax.AST.Provenance
import Vehicle.Syntax.External.Abs qualified as B
import Vehicle.Syntax.Parse.Error (ParseError (..))
import Vehicle.Syntax.Parse.Token (IsToken, mkToken)

type MonadElab m =
  ( MonadError ParseError m,
    MonadReader ParseLocation m
  )

pattern InferableOption :: Text
pattern InferableOption = "infer"

type ParseLocation = (ModulePath, FilePath)

getModule :: (MonadElab m) => m ModulePath
getModule = asks fst

getFile :: (MonadElab m) => m FilePath
getFile = asks snd

mkProvenance :: (MonadElab m, IsToken tk) => tk -> m Provenance
mkProvenance tk = tkProvenance tk <$> getFile

tokType :: Int -> B.Expr
tokType l = B.Type (mkToken B.TokType ("Type" <> pack (show l)))

networkAnn = mkToken B.TokAnnotation "@network"

datasetAnn = mkToken B.TokAnnotation "@dataset"

parameterAnn = mkToken B.TokAnnotation "@parameter"

propertyAnn = mkToken B.TokAnnotation "@property"

tokPostulate = mkToken B.TokPostulate "@postulate"

tokArrow = mkToken B.TokArrow "->"

tokForallT = mkToken B.TokForallT "forallT"

tokIf = mkToken B.TokIf "if"

tokThen = mkToken B.TokThen "then"

tokElse = mkToken B.TokElse "else"

tokLet = mkToken B.TokLet "let"

tokDot = mkToken B.TokDot "."

tokElemOf = mkToken B.TokElemOf ":"

tokLambda = mkToken B.TokLambda "\\"

tokVector = mkToken B.TokVector "Vector"

tokUnit = mkToken B.TokUnit "Unit"

tokBool = mkToken B.TokBool "Bool"

tokList = mkToken B.TokList "List"

tokRat = mkToken B.TokRat "Rat"

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

tokNeq = mkToken B.TokNeq "!="

tokLe = mkToken B.TokLe "<="

tokLt = mkToken B.TokLt "<"

tokGe = mkToken B.TokGe ">="

tokGt = mkToken B.TokGt ">"

tokAdd = mkToken B.TokAdd "+"

tokSub = mkToken B.TokSub "-"

tokMul = mkToken B.TokMul "*"

tokDiv = mkToken B.TokDiv "/"

tokSeqOpen = mkToken B.TokSeqOpen "["

tokSeqClose = mkToken B.TokSeqClose "]"

tokNil = mkToken B.TokNil "nil"

tokCons = mkToken B.TokCons "::"

tokZipWith = mkToken B.TokZipWith "zipWith"

tokAt = mkToken B.TokAt "!"

tokMap = mkToken B.TokMap "map"

tokFold = mkToken B.TokFold "fold"

tokIndices = mkToken B.TokIndices "indices"

tokHasAdd = mkToken B.TokHasAdd "HasAdd"

tokHasSub = mkToken B.TokHasSub "HasSub"

tokHasMul = mkToken B.TokHasMul "HasMul"

tokHasEq = mkToken B.TokHasEq "HasEq"

tokHasNotEq = mkToken B.TokHasNotEq "HasNotEq"

tokHasLeq = mkToken B.TokHasLeq "HasLeq"

tokHasMap = mkToken B.TokHasMap "HasMap"

tokHasFold = mkToken B.TokHasFold "HasFold"
