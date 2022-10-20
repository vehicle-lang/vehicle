module Vehicle.Compile.Prelude.Utils where

import Data.Functor.Foldable (Recursive(..))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Text (pack)
import Data.Maybe (mapMaybe)

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

isBoundVar :: DBExpr -> Bool
isBoundVar BoundVar{} = True
isBoundVar _          = False

isAnnBoolType :: DBExpr -> Bool
isAnnBoolType AnnBoolType{} = True
isAnnBoolType _             = False

isNatType :: DBExpr -> Bool
isNatType NatType{} = True
isNatType _         = False

isIntType :: DBExpr -> Bool
isIntType IntType{} = True
isIntType _         = False

isAnnRatType :: Type binder var -> Bool
isAnnRatType AnnRatType{} = True
isAnnRatType _            = False

isRatType :: Type binder var -> Bool
isRatType RatType{} = True
isRatType _         = False

isListType :: DBExpr -> Bool
isListType ListType{} = True
isListType _            = False

isVectorType :: DBExpr -> Bool
isVectorType VectorType{} = True
isVectorType _            = False

isIndexType :: DBExpr -> Bool
isIndexType IndexType{} = True
isIndexType _           = False

isMeta :: DBExpr -> Bool
isMeta Meta{}           = True
isMeta (App _ Meta{} _) = True
isMeta _                = False

isTypeSynonym :: Expr binder var -> Bool
isTypeSynonym = \case
  TypeUniverse{} -> True
  Pi _ _ res     -> isTypeSynonym res
  _              -> False

--------------------------------------------------------------------------------
-- Enumeration functions

freeNamesIn :: Expr binder DBVar -> [Identifier]
freeNamesIn = cata $ \case
  VarF  _ (Free ident)      -> [ident]
  VarF  _ (Bound _)         -> []
  UniverseF{}               -> []
  HoleF{}                   -> []
  MetaF{}                   -> []
  LiteralF{}                -> []
  BuiltinF{}                -> []
  AnnF  _ e t               -> e <> t
  AppF  _ fun args          -> fun <> concatMap argExpr args
  PiF   _ binder result     -> binderType binder <> result
  LetF  _ bound binder body -> bound <> binderType binder <> body
  LamF  _ binder body       -> binderType binder <> body
  LVecF _ xs                -> concat xs

--------------------------------------------------------------------------------
-- Destruction functions

toHead :: Expr binder var -> (Expr binder var, [Arg binder var])
toHead (App _ fun args) = (fun, NonEmpty.toList args)
toHead e                = (e, [])

exprHead :: Expr binder var -> Expr binder var
exprHead = fst . toHead

onlyExplicit :: NonEmpty (Arg binder var) -> [Expr binder var]
onlyExplicit args = argExpr <$> filter isExplicit (NonEmpty.toList args)

--------------------------------------------------------------------------------
-- Views

getFreeVar :: DBExpr -> Maybe Identifier
getFreeVar = \case
  FreeVar _ ident -> Just ident
  _               -> Nothing

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
getDimension (NatLiteral _ n) = return n
getDimension _            = Nothing

getDimensions :: DBExpr -> Maybe [Int]
getDimensions (NilExpr _ _)          = Just []
getDimensions (ConsExpr _ _ [x, xs]) = do
  d  <- getDimension  (argExpr x)
  ds <- getDimensions (argExpr xs)
  return $ d : ds
getDimensions _                      = Nothing

getExplicitArg :: Arg binder var -> Maybe (Expr binder var)
getExplicitArg (ExplicitArg _ arg) = Just arg
getExplicitArg _                   = Nothing

getImplicitArg :: Arg binder var -> Maybe (Expr binder var)
getImplicitArg (ImplicitArg _ arg) = Just arg
getImplicitArg _                   = Nothing

getConcreteList :: Expr binder var -> [Expr binder var]
getConcreteList = \case
  NilExpr{}            -> []
  AppConsExpr _ _ x xs -> x : getConcreteList xs
  _                    -> developerError "Malformed concrete list"

filterOutNonExplicitArgs :: NonEmpty (Arg binder var) -> [Expr binder var]
filterOutNonExplicitArgs args = mapMaybe getExplicitArg (NonEmpty.toList args)

findInstanceArg :: [Arg binder var] -> (Expr binder var, [Arg binder var])
findInstanceArg (InstanceArg _ inst : xs) = (inst, xs)
findInstanceArg (_ : xs) = findInstanceArg xs
findInstanceArg []       = developerError "Malformed type class operation"

--------------------------------------------------------------------------------
-- Construction functions

-- | Generates a name for a variable based on the indices, e.g. x [1,2,3] -> x_1_2_3
mkNameWithIndices :: Symbol -> Int -> Symbol
mkNameWithIndices n index = n <> pack (show index)
  --mconcat (n : [pack (show index) | index <- indices])

mkHole :: Provenance -> Symbol -> Expr binder var
mkHole ann name = Hole ann ("_" <> name)

mkDoubleExpr :: Provenance -> Double -> DBExpr
mkDoubleExpr ann v = RatLiteral ann (toRational v)

mkIndexType :: Provenance -> Int -> DBExpr
mkIndexType ann n = IndexType ann (NatLiteral ann n)

mkIntExpr :: Provenance -> Int -> DBExpr
mkIntExpr ann v
  | v >= 0    = NatLiteral ann v
  | otherwise = IntLiteral ann v

mkTensorDims :: Provenance
             -> [Int]
             -> DBExpr
mkTensorDims ann dims =
  mkList ann (NatType ann) (fmap (NatLiteral ann) dims)

mkTensorType :: Provenance
             -> DBExpr
             -> [DBExpr]
             -> DBExpr
mkTensorType _   tElem []   = tElem
mkTensorType ann tElem dims =
  let dimList = mkList ann (NatType ann) dims in
  App ann (Builtin ann Tensor) (fmap (ExplicitArg ann) [tElem, dimList])

mkList :: Provenance
       -> Expr var binder
       -> [Expr var binder]
       -> Expr var binder
mkList p elemType = foldr cons (NilExpr p elemType)
  where cons x xs = ConsExpr p elemType [ExplicitArg p x, ExplicitArg p xs]