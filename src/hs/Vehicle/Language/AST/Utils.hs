module Vehicle.Language.AST.Utils where

import Control.Monad (void)
import Data.Functor.Foldable (Recursive(..))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Text (pack)

import Vehicle.Prelude
import Vehicle.Language.AST.Core
import Vehicle.Language.AST.DeBruijn
import Vehicle.Language.AST.BuiltinPatterns
import Vehicle.Language.AST.Name
import Vehicle.Language.AST.Visibility
import Vehicle.Language.AST.Builtin

--------------------------------------------------------------------------------
-- Utility functions

isHole :: Expr binder var ann -> Bool
isHole Hole{} = True
isHole _      = False

isType :: Expr binder var ann -> Bool
isType Type{} = True
isType _      = False

isMeta :: Expr binder var ann -> Bool
isMeta Meta{}           = True
isMeta (App _ Meta{} _) = True
isMeta _                = False

isProperty :: Expr binder var ann -> Bool
isProperty BoolType{}                  = True
isProperty (TensorType _ BoolType{} _) = True
isProperty _                           = False

isFinite :: Expr binder var ann -> Bool
isFinite BoolType{}             = True
isFinite IndexType{}            = True
isFinite (TensorType _ tElem _) = isFinite tElem
isFinite _                      = False

isAuxiliaryTypeClass :: Expr binder var ann -> Bool
isAuxiliaryTypeClass e = case exprHead e of
  Builtin _ PolarityTypeClass{} -> True
  _                             -> False

isAuxiliaryType :: Expr binder var ann -> Bool
isAuxiliaryType (Builtin _ AuxiliaryType) = True
isAuxiliaryType  _                        = False

--------------------------------------------------------------------------------
-- Enumeration functions

freeNames :: Expr binder DBVar ann -> [Identifier]
freeNames = cata $ \case
  TypeF{}                   -> []
  HoleF{}                   -> []
  PrimDictF{}               -> []
  MetaF{}                   -> []
  LiteralF{}                -> []
  BuiltinF{}                -> []
  AnnF  _ e t               -> e <> t
  AppF  _ fun args          -> fun <> concatMap (freeNames . argExpr) args
  PiF   _ binder result     -> freeNames (typeOf binder) <> result
  VarF  _ (Free ident)      -> [ident]
  VarF  _ (Bound _)         -> []
  LetF  _ bound binder body -> bound <> freeNames (typeOf binder) <> body
  LamF  _ binder body       -> freeNames (typeOf binder) <> body
  LSeqF _ _ xs              -> concat xs

freeMetas :: Expr binder var ann -> [Meta]
freeMetas = cata $ \case
  TypeF{}                   -> []
  HoleF{}                   -> []
  PrimDictF{}               -> []
  LiteralF{}                -> []
  BuiltinF{}                -> []
  VarF {}                   -> []
  MetaF _ m                 -> [m]
  AnnF  _ e t               -> e <> t
  AppF  _ fun args          -> fun <> concatMap (freeMetas . argExpr) args
  PiF   _ binder result     -> freeMetas (typeOf binder) <> result
  LetF  _ bound binder body -> bound <> freeMetas (typeOf binder) <> body
  LamF  _ binder body       -> freeMetas (typeOf binder) <> body
  LSeqF _ _ xs              -> concat xs

--------------------------------------------------------------------------------
-- Destruction functions

toHead :: Expr binder var ann -> (Expr binder var ann, [Arg binder var ann])
toHead (App _ann fun args ) = (fun, NonEmpty.toList args)
toHead e                    = (e, [])

exprHead :: Expr binder var ann -> Expr binder var ann
exprHead = fst . toHead

onlyExplicit :: NonEmpty (Arg binder var ann) -> [Expr binder var ann]
onlyExplicit args = argExpr <$> filter isExplicit (NonEmpty.toList args)

--------------------------------------------------------------------------------
-- Views

getBinderSymbol :: Binder DBBinding var ann -> Symbol
getBinderSymbol binder = case nameOf binder of
  Just symbol -> symbol
  Nothing     -> developerError "Binder unexpectedly does not appear to have a name"

getContainerElem :: Expr binder var ann -> Maybe (Expr binder var ann)
getContainerElem (ListType   _ t)   = Just t
getContainerElem (TensorType _ t _) = Just t
getContainerElem _                  = Nothing

getDimension :: Expr binder var ann -> Maybe Int
getDimension (NatLiteralExpr _ _ n) = return n
getDimension _                      = Nothing

getDimensions :: Expr binder var ann -> Maybe [Int]
getDimensions (SeqExpr _ _ _ es) = traverse getDimension es
getDimensions _                  = Nothing

getExplicitArg :: Arg binder var ann -> Maybe (Expr binder var ann)
getExplicitArg (ExplicitArg _ arg) = Just arg
getExplicitArg _                   = Nothing

getExplicitArgs :: Traversable t => t (Arg binder var ann) -> Maybe (t (Expr binder var ann))
getExplicitArgs = traverse getExplicitArg

--------------------------------------------------------------------------------
-- Construction functions

-- | Generates a name for a variable based on the indices, e.g. x [1,2,3] -> x_1_2_3
mkNameWithIndices :: Symbol -> [Int] -> Symbol
mkNameWithIndices n indices = mconcat (n : [pack (show index) | index <- indices])

removeAnnotations :: Functor (t binder var) => t binder var ann -> t binder var ()
removeAnnotations = void

mkHole :: ann -> Symbol -> Expr binder var ann
mkHole ann name = Hole ann ("_" <> name)

mkDoubleExpr :: ann -> Double -> Expr binder var ann
mkDoubleExpr ann v = LitRat ann (toRational v)

mkIndexType :: ann -> Int -> Expr binder var ann
mkIndexType ann n = IndexType ann (NatLiteralExpr ann (NatType ann) n)

mkIntExpr :: ann -> Int -> Expr binder var ann
mkIntExpr ann v
  | v >= 0    = LitNat ann v
  | otherwise = LitInt ann v

mkSeqExpr :: ann -> [Expr binder var ann] -> Expr binder var ann
mkSeqExpr ann = LSeq ann (Hole ann "_seqTC")

mkTensorDims :: ann
             -> [Int]
             -> Expr binder var ann
mkTensorDims ann dims =
  let listType = ListType ann (NatType ann) in
  let dimExprs = fmap (Literal ann . LNat) dims in
  let dimList  = SeqExpr ann (NatType ann) listType dimExprs in
  dimList

mkTensorType :: ann
             -> Expr binder var ann
             -> [Int]
             -> Expr binder var ann
mkTensorType ann tElem dims =
  let dimList = mkTensorDims ann dims in
  App ann (BuiltinContainerType ann Tensor) (fmap (ExplicitArg ann) [tElem, dimList])

mkQuantifierSeq :: Quantifier
                -> ann
                -> [binder]
                -> Expr binder var ann
                -> Expr binder var ann
                -> Expr binder var ann
mkQuantifierSeq q ann names t body =
  foldl (\e name -> QuantifierExpr q ann (ExplicitBinder ann name t) e) body names

mkList :: ann
       -> Expr binder var ann
       -> [Expr binder var ann]
       -> Expr binder var ann
mkList ann tElem = foldr cons (NilExpr ann tElem)
  where cons x xs = ConsExpr ann tElem $ fmap (ExplicitArg ann) [x, xs]

mkBooleanBigOp :: BooleanOp2
               -> ann
               -> Expr binder var ann
               -> Expr binder var ann
               -> Expr binder var ann
mkBooleanBigOp op ann containerType container =
  FoldExpr ann (BoolType ann) containerType (BoolType ann) $ fmap (ExplicitArg ann)
    [ Builtin ann (BooleanOp2 op)
    , BoolLiteralExpr ann unit
    , container
    ]
  where
    unit :: Bool
    unit = case op of
      And  -> True
      Or   -> False
      Impl -> True