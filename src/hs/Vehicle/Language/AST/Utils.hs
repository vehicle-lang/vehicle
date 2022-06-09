module Vehicle.Language.AST.Utils where

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
import Vehicle.Language.AST.Provenance

--------------------------------------------------------------------------------
-- Utility functions

isHole :: Expr binder var -> Bool
isHole Hole{} = True
isHole _      = False

isType :: Expr binder var -> Bool
isType Type{} = True
isType _      = False

isMeta :: Expr binder var -> Bool
isMeta Meta{}           = True
isMeta (App _ Meta{} _) = True
isMeta _                = False

isFinite :: Expr binder var -> Bool
isFinite BoolType{}             = True
isFinite IndexType{}            = True
isFinite (TensorType _ tElem _) = isFinite tElem
isFinite _                      = False

isAuxiliaryTypeClass :: Expr binder var -> Bool
isAuxiliaryTypeClass e = case exprHead e of
  Builtin _ PolarityTypeClass{} -> True
  _                             -> False

isAuxiliaryType :: Expr binder var -> Bool
isAuxiliaryType (Builtin _ AuxiliaryType) = True
isAuxiliaryType  _                        = False

--------------------------------------------------------------------------------
-- Enumeration functions

freeNames :: Expr binder DBVar -> [Identifier]
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

--------------------------------------------------------------------------------
-- Destruction functions

toHead :: Expr binder var -> (Expr binder var, [Arg binder var])
toHead (App _ann fun args ) = (fun, NonEmpty.toList args)
toHead e                    = (e, [])

exprHead :: Expr binder var -> Expr binder var
exprHead = fst . toHead

onlyExplicit :: NonEmpty (Arg binder var) -> [Expr binder var]
onlyExplicit args = argExpr <$> filter isExplicit (NonEmpty.toList args)

--------------------------------------------------------------------------------
-- Views

getBinderSymbol :: Binder DBBinding var -> Symbol
getBinderSymbol binder = case nameOf binder of
  Just symbol -> symbol
  Nothing     -> developerError "Binder unexpectedly does not appear to have a name"

getContainerElem :: Expr binder var -> Maybe (Expr binder var)
getContainerElem (ListType   _ t)   = Just t
getContainerElem (TensorType _ t _) = Just t
getContainerElem _                  = Nothing

getDimension :: Expr binder var -> Maybe Int
getDimension (NatLiteralExpr _ _ n) = return n
getDimension _                      = Nothing

getDimensions :: Expr binder var -> Maybe [Int]
getDimensions (SeqExpr _ _ _ es) = traverse getDimension es
getDimensions _                  = Nothing

getExplicitArg :: Arg binder var -> Maybe (Expr binder var)
getExplicitArg (ExplicitArg _ arg) = Just arg
getExplicitArg _                   = Nothing

getExplicitArgs :: Traversable t => t (Arg binder var) -> Maybe (t (Expr binder var))
getExplicitArgs = traverse getExplicitArg

filterOutNonExplicitArgs :: NonEmpty (Arg binder var) -> [Expr binder var]
filterOutNonExplicitArgs args = maybe [] NonEmpty.toList $ getExplicitArgs args

--------------------------------------------------------------------------------
-- Construction functions

-- | Generates a name for a variable based on the indices, e.g. x [1,2,3] -> x_1_2_3
mkNameWithIndices :: Symbol -> [Int] -> Symbol
mkNameWithIndices n indices = mconcat (n : [pack (show index) | index <- indices])

mkHole :: Provenance -> Symbol -> Expr binder var
mkHole ann name = Hole ann ("_" <> name)

mkDoubleExpr :: Provenance -> Double -> Expr binder var
mkDoubleExpr ann v = LitRat ann (toRational v)

mkIndexType :: Provenance -> Int -> Expr binder var
mkIndexType ann n = IndexType ann (NatLiteralExpr ann (NatType ann) n)

mkIntExpr :: Provenance -> Int -> Expr binder var
mkIntExpr ann v
  | v >= 0    = LitNat ann v
  | otherwise = LitInt ann v

mkSeqExpr :: Provenance -> [Expr binder var] -> Expr binder var
mkSeqExpr ann = LSeq ann (Hole ann "_seqTC")

mkTensorDims :: Provenance
             -> [Int]
             -> Expr binder var
mkTensorDims ann dims =
  let listType = ListType ann (NatType ann) in
  let dimExprs = fmap (Literal ann . LNat) dims in
  let dimList  = SeqExpr ann (NatType ann) listType dimExprs in
  dimList

mkTensorType :: Provenance
             -> Expr binder var
             -> [Int]
             -> Expr binder var
mkTensorType _   tElem []   = tElem
mkTensorType ann tElem dims =
  let dimList = mkTensorDims ann dims in
  App ann (BuiltinContainerType ann Tensor) (fmap (ExplicitArg ann) [tElem, dimList])

mkQuantifierSeq :: Quantifier
                -> Provenance
                -> [binder]
                -> Expr binder var
                -> Expr binder var
                -> Expr binder var
mkQuantifierSeq q ann names t body =
  foldl (\e name -> QuantifierExpr q ann (ExplicitBinder ann name t) e) body names

mkList :: Provenance
       -> Expr binder var
       -> [Expr binder var]
       -> Expr binder var
mkList ann tElem = foldr cons (NilExpr ann tElem)
  where cons x xs = ConsExpr ann tElem $ fmap (ExplicitArg ann) [x, xs]

mkTensor :: Provenance
         -> Expr binder var
         -> [Int]
         -> [Expr binder var]
         -> Expr binder var
mkTensor ann tBaseElem dims elems =
  let tensorType = mkTensorType ann tBaseElem dims in
  let elemType = mkTensorType ann tBaseElem (tail dims) in
  SeqExpr ann elemType tensorType elems

mkBooleanBigOp :: BooleanOp2
               -> Provenance
               -> Expr binder var
               -> Expr binder var
               -> Expr binder var
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