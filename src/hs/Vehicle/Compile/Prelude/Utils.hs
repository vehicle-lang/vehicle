module Vehicle.Compile.Prelude.Utils where

import Data.Functor.Foldable (Recursive(..))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Text (pack)

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Compile.Prelude.Patterns

--------------------------------------------------------------------------------
-- Utility functions

isTypeUniverse :: Expr binder var -> Bool
isTypeUniverse TypeUniverse{} = True
isTypeUniverse _              = False

isPolarityUniverse :: Expr binder var -> Bool
isPolarityUniverse PolarityUniverse{} = True
isPolarityUniverse _                  = False

isLinearityUniverse :: Expr binder var -> Bool
isLinearityUniverse LinearityUniverse{} = True
isLinearityUniverse _                   = False

isAuxiliaryUniverse :: Expr binder var -> Bool
isAuxiliaryUniverse e = isPolarityUniverse e || isLinearityUniverse e

isAnnBoolType :: DBExpr -> Bool
isAnnBoolType AnnBoolType{} = True
isAnnBoolType _             = False

isNatType :: DBExpr -> Bool
isNatType NatType{} = True
isNatType _         = False

isIntType :: DBExpr -> Bool
isIntType IntType{} = True
isIntType _         = False

isAnnRatType :: DBExpr -> Bool
isAnnRatType AnnRatType{} = True
isAnnRatType _            = False

isListType :: DBExpr -> Bool
isListType ListType{} = True
isListType _            = False

isTensorType :: DBExpr -> Bool
isTensorType TensorType{} = True
isTensorType _            = False

isIndexType :: DBExpr -> Bool
isIndexType IndexType{} = True
isIndexType _           = False

isMeta :: DBExpr -> Bool
isMeta Meta{}           = True
isMeta (App _ Meta{} _) = True
isMeta _                = False

isAuxiliaryTypeClass :: TypeClass -> Bool
isAuxiliaryTypeClass tc = case tc of
    MulLinearity                        -> True
    MaxLinearity                        -> True
    NegPolarity{}                       -> True
    AddPolarity{}                       -> True
    EqPolarity{}                        -> True
    ImplPolarity{}                      -> True
    MaxPolarity{}                       -> True
    -- TypesEqualModAuxiliaryAnnotations{} -> True
    _                                   -> False

--------------------------------------------------------------------------------
-- Enumeration functions

freeNamesIn :: Expr binder DBVar -> [Identifier]
freeNamesIn = cata $ \case
  VarF  _ (Free ident)      -> [ident]
  VarF  _ (Bound _)         -> []
  UniverseF{}               -> []
  HoleF{}                   -> []
  PrimDictF{}               -> []
  MetaF{}                   -> []
  LiteralF{}                -> []
  BuiltinF{}                -> []
  AnnF  _ e t               -> e <> t
  AppF  _ fun args          -> fun <> concatMap (freeNamesIn . argExpr) args
  PiF   _ binder result     -> freeNamesIn (typeOf binder) <> result
  LetF  _ bound binder body -> bound <> freeNamesIn (typeOf binder) <> body
  LamF  _ binder body       -> freeNamesIn (typeOf binder) <> body
  LSeqF _ xs                -> concat xs

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

getContainerElem :: DBExpr -> Maybe DBExpr
getContainerElem (ListType   _ t)      = Just t
getContainerElem (TensorType p t dims) = case getDimensions dims of
  Just [_]      -> Just t
  Just (_ : ds) -> Just (TensorType p t (mkTensorDims p ds))
  _             -> Nothing
getContainerElem _                     = Nothing

getDimension :: DBExpr -> Maybe Int
getDimension (NatLiteralExpr _ _ n) = return n
getDimension _                      = Nothing

getDimensions :: DBExpr -> Maybe [Int]
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

mkDoubleExpr :: Provenance -> Double -> DBExpr
mkDoubleExpr ann v = LitRat ann (toRational v)

mkIndexType :: Provenance -> Int -> DBExpr
mkIndexType ann n = IndexType ann (NatLiteralExpr ann (NatType ann) n)

mkIntExpr :: Provenance -> Int -> DBExpr
mkIntExpr ann v
  | v >= 0    = LitNat ann v
  | otherwise = LitInt ann v

mkTensorDims :: Provenance
             -> [Int]
             -> DBExpr
mkTensorDims ann dims =
  let dimExprs = fmap (NatLiteralExpr ann (NatType ann)) dims in
  mkList ann (NatType ann) dimExprs

mkTensorType :: Provenance
             -> DBExpr
             -> [DBExpr]
             -> DBExpr
mkTensorType _   tElem []   = tElem
mkTensorType ann tElem dims =
  let dimList = mkList ann (NatType ann) dims in
  App ann (BuiltinContainerType ann Tensor) (fmap (ExplicitArg ann) [tElem, dimList])

mkQuantifierSeq :: Quantifier
                -> Provenance
                -> [DBBinding]
                -> DBExpr
                -> DBExpr
                -> DBExpr
mkQuantifierSeq q ann names t body =
  foldl (\e name -> QuantifierExpr q ann (ExplicitBinder ann name t) e) body names

mkList :: Provenance
       -> DBExpr
       -> [DBExpr]
       -> DBExpr
mkList ann elemType = SeqExpr ann elemType (ListType ann elemType)

mkTensor :: Provenance
         -> DBExpr
         -> [DBExpr]
         -> [DBExpr]
         -> DBExpr
mkTensor ann tBaseElem dims =
  let elemType   = mkTensorType ann tBaseElem (tail dims) in
  let tensorType = mkTensorType ann tBaseElem dims in
  SeqExpr ann elemType tensorType

mkBooleanBigOp :: BooleanOp2
               -> Provenance
               -> DBExpr
               -> DBExpr
               -> DBExpr
mkBooleanBigOp op ann containerType container =
  let (unit, opExpr) = case op of
        And  -> (True, AndExpr ann [])
        Or   -> (False, OrExpr ann [])
        Impl -> developerError "Cannot bigOp '=>'"
  in
  FoldExpr ann (BoolType ann) containerType (BoolType ann) $ fmap (ExplicitArg ann)
    [ opExpr
    , BoolLiteralExpr ann unit
    , container
    ]
