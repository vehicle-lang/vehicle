{-# LANGUAGE TemplateHaskell #-}

module Vehicle.Language.AST.JSON where
import Vehicle.Prelude
import Vehicle.Language.AST qualified as V
import Data.Aeson

import GHC.Generics (Generic)
import Data.List.NonEmpty qualified as NonEmpty (map)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe ( fromMaybe )
import qualified Vehicle.Compile.Prelude as X
import qualified Vehicle.Core.Abs as X
import Data.ByteString.Lazy (ByteString)



--add aeson to vehicle.cabal, lib:vehicle 
--attach to JExpr, JBuiltin
--function Expr to JExpr then calls aeson (thats what will be exported from this module)
--vehicle.Prelude new OutputTarget (JSON)
--vehicle.compile hook all this up - new case in case split on output target



data JBuiltin
  -- Types
  = NumericType   V.NumericType
  | BooleanType
  -- Expressions
  | If
  | Not
  | BooleanOp2 V.BooleanOp2
  | Neg
  | NumericOp2 V.NumericOp2
  | Equality  V.Equality
  | Order     V.Order
  deriving (Eq, Ord, Generic)


data JExpr 
  = Builtin JBuiltin
  | Literal V.Literal
  | App JExpr (NonEmpty JExpr)
  | Network Symbol
  | Var V.DBIndex
  | Quant V.Quantifier Symbol JExpr JExpr
  deriving (Generic)

toJBuiltin :: V.Builtin -> JBuiltin
toJBuiltin e =
  case e of
    V.NumericType t    -> NumericType t
    V.BooleanType _    -> BooleanType
    V.If               -> If
    V.Not              -> Not
    V.BooleanOp2 t     -> BooleanOp2 t
    V.Neg              -> Neg
    V.NumericOp2 t     -> NumericOp2 t
    V.Equality t       -> Equality t
    V.Order t          -> Order t

    
    V.ContainerType _  -> developerError "Unexpected value ContainerType"
    V.Cons             -> developerError "Unexpected value Cons"
    V.At               -> developerError "Unexpected value At"
    V.Map              -> developerError "Unexpected value Map"
    V.Fold             -> developerError "Unexpected value Fold"
    V.TypeClass _      -> developerError "Unexpected value TypeClass"
    V.Quant _          -> developerError "Unexpected value Quant"  
    V.QuantIn _        -> developerError "Unexpected value QuantIn"



argToJExpr :: X.CheckedArg -> JExpr
argToJExpr (V.Arg _ _ t) = toJExpr t

binderToJExpr :: X.CheckedBinder -> JExpr
binderToJExpr (V.Binder _ _ _ t) = toJExpr t

binderToSymbol :: X.CheckedBinder -> Symbol
binderToSymbol (V.Binder _ _ b _) = fromMaybe (developerError "Complex type") b 

toJExpr :: X.CheckedExpr  -> JExpr
toJExpr e = 
  case e of
    V.Builtin _ t                     -> Builtin (toJBuiltin t)
    V.Literal _ t               -> Literal t
    V.App _ t p                       -> App (toJExpr t) (NonEmpty.map argToJExpr p)
    V.Var _ (V.Free (V.Identifier t)) -> Network t
    V.Var _ (V.Bound t)               -> Var t
    V.QuantifierExpr q _ binder body  -> Quant q (binderToSymbol binder) (binderToJExpr binder) (toJExpr body)
    
    V.Hole{}                          -> developerError "Hole"
    V.Meta{}                          -> developerError "Meta"
    V.Ann{}                           -> developerError "Ann"
    V.Let{}                           -> developerError "Let"
    V.Lam{}                           -> developerError "Lam"
    V.LSeq{}                           -> developerError "Seq"
    V.PrimDict{}                      -> developerError "PrimDict"
    V.Pi{}                            -> developerError "Pi"
    V.Type{}                          -> developerError "Type"


--newtype NewOrder = NewOrder X.Order
--  deriving (Eq, Ord, Generic)

instance FromJSON X.Order
instance ToJSON X.Order
instance FromJSON X.NumericOp2 
instance ToJSON X.NumericOp2
instance FromJSON X.NumericType 
instance ToJSON X.NumericType
instance FromJSON X.BooleanOp2 
instance ToJSON X.BooleanOp2
instance FromJSON X.Equality
instance ToJSON X.Equality

  
instance FromJSON JBuiltin
instance ToJSON JBuiltin where
  toEncoding = genericToEncoding defaultOptions


instance FromJSON X.Literal
instance ToJSON X.Literal
instance FromJSON X.Quantifier 
instance ToJSON X.Quantifier 

instance FromJSON JExpr
instance ToJSON JExpr where
  toEncoding = genericToEncoding defaultOptions

toJSON :: X.CheckedExpr -> ByteString
toJSON e = encode (toJExpr e)
    



    
