module Vehicle.Compile.Prelude.Utils where

import Data.Functor.Foldable (Recursive (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Maybe (mapMaybe)
import Data.Text (pack)
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Patterns
import Vehicle.Libraries.StandardLibrary (pattern TensorIdent)
import Vehicle.Prelude
import Vehicle.Syntax.AST

--------------------------------------------------------------------------------
-- Utility functions

isTypeUniverse :: Expr binder var builtin -> Bool
isTypeUniverse TypeUniverse {} = True
isTypeUniverse _ = False

isPolarityUniverse :: Expr binder var builtin -> Bool
isPolarityUniverse PolarityUniverse {} = True
isPolarityUniverse _ = False

isLinearityUniverse :: Expr binder var builtin -> Bool
isLinearityUniverse LinearityUniverse {} = True
isLinearityUniverse _ = False

isAuxiliaryUniverse :: Expr binder var builtin -> Bool
isAuxiliaryUniverse e = isPolarityUniverse e || isLinearityUniverse e

isBoundVar :: DBExpr Builtin -> Bool
isBoundVar BoundVar {} = True
isBoundVar _ = False

isAnnBoolType :: DBExpr Builtin -> Bool
isAnnBoolType AnnBoolType {} = True
isAnnBoolType _ = False

--------------------------------------------------------------------------------
-- Enumeration functions

freeNamesIn :: Expr binder DBIndexVar builtin -> [Identifier]
freeNamesIn = cata $ \case
  VarF _ (Free ident) -> [ident]
  VarF _ (Bound _) -> []
  UniverseF {} -> []
  HoleF {} -> []
  MetaF {} -> []
  LiteralF {} -> []
  BuiltinF {} -> []
  AnnF _ e t -> e <> t
  AppF _ fun args -> fun <> concatMap argExpr args
  PiF _ binder result -> binderType binder <> result
  LetF _ bound binder body -> bound <> binderType binder <> body
  LamF _ binder body -> binderType binder <> body
  LVecF _ xs -> concat xs

--------------------------------------------------------------------------------
-- Destruction functions

toHead :: Expr binder var builtin -> (Expr binder var builtin, [Arg binder var builtin])
toHead (App _ fun args) = (fun, NonEmpty.toList args)
toHead e = (e, [])

exprHead :: Expr binder var builtin -> Expr binder var builtin
exprHead = fst . toHead

onlyExplicit :: NonEmpty (GenericArg expr) -> [expr]
onlyExplicit args = argExpr <$> filter isExplicit (NonEmpty.toList args)

--------------------------------------------------------------------------------
-- Views

getMetaID :: Expr binder var builtin -> Maybe MetaID
getMetaID e = case exprHead e of
  Meta _ m -> Just m
  _ -> Nothing

getFreeVar :: DBExpr Builtin -> Maybe Identifier
getFreeVar = \case
  FreeVar _ ident -> Just ident
  _ -> Nothing

getBinderName :: GenericBinder DBBinding expr -> Name
getBinderName binder = case binderNamingForm binder of
  NameAndType name -> name
  OnlyName name -> name
  OnlyType -> developerError "Binder unexpectedly does not appear to have a name"

getContainerElem :: DBExpr Builtin -> Maybe (DBExpr Builtin)
getContainerElem (ListType _ t) = Just t
getContainerElem (TensorType p t dims) = case getDimensions dims of
  Just [_] -> Just t
  Just (_ : ds) -> Just (mkTensorType p t (mkTensorDims p ds))
  _ -> Nothing
getContainerElem _ = Nothing

getDimension :: DBExpr Builtin -> Maybe Int
getDimension (NatLiteral _ n) = return n
getDimension _ = Nothing

getDimensions :: DBExpr Builtin -> Maybe [Int]
getDimensions NilExpr {} = Just []
getDimensions (ConsExpr _ _ [x, xs]) = do
  d <- getDimension (argExpr x)
  ds <- getDimensions (argExpr xs)
  return $ d : ds
getDimensions _ = Nothing

getExplicitArg :: Arg binder var builtin -> Maybe (Expr binder var builtin)
getExplicitArg (ExplicitArg _ arg) = Just arg
getExplicitArg _ = Nothing

getImplicitArg :: Arg binder var builtin -> Maybe (Expr binder var builtin)
getImplicitArg (ImplicitArg _ arg) = Just arg
getImplicitArg _ = Nothing

filterOutNonExplicitArgs :: NonEmpty (Arg binder var builtin) -> [Expr binder var builtin]
filterOutNonExplicitArgs args = mapMaybe getExplicitArg (NonEmpty.toList args)

findInstanceArg :: [GenericArg a] -> (a, [GenericArg a])
findInstanceArg (InstanceArg _ inst : xs) = (inst, xs)
findInstanceArg (_ : xs) = findInstanceArg xs
findInstanceArg [] = developerError "Malformed type class operation"

--------------------------------------------------------------------------------
-- Construction functions

-- | Generates a name for a variable based on the indices, e.g. x [1,2,3] -> x_1_2_3
mkNameWithIndices :: Name -> Int -> Name
mkNameWithIndices n index = n <> pack (show index)

-- mconcat (n : [pack (show index) | index <- indices])

mkDoubleExpr :: Provenance -> Double -> DBExpr Builtin
mkDoubleExpr p v = RatLiteral p (toRational v)

mkIndexType :: Provenance -> Int -> DBExpr Builtin
mkIndexType p n =
  ConstructorExpr
    p
    Index
    [ ExplicitArg p (NatLiteral p n)
    ]

mkIntExpr :: Provenance -> Int -> DBExpr Builtin
mkIntExpr p v
  | v >= 0 = NatLiteral p v
  | otherwise = IntLiteral p v

mkTensorDims ::
  Provenance ->
  [Int] ->
  [DBExpr Builtin]
mkTensorDims p = fmap (NatLiteral p)

mkTensorType ::
  Provenance ->
  DBExpr Builtin ->
  [DBExpr Builtin] ->
  DBExpr Builtin
mkTensorType _ tElem [] = tElem
mkTensorType p tElem dims =
  let dimList = mkList p (NatType p) dims
   in App p (FreeVar p TensorIdent) (ExplicitArg p <$> [tElem, dimList])

mkList ::
  Provenance ->
  Expr var binder Builtin ->
  [Expr var binder Builtin] ->
  Expr var binder Builtin
mkList p elemType = foldr cons nil
  where
    nil = ConstructorExpr p Nil [ImplicitArg p elemType]
    cons x xs =
      ConstructorExpr
        p
        Cons
        ( ImplicitArg p elemType
            :| [ ExplicitArg p x,
                 ExplicitArg p xs
               ]
        )

mkVec ::
  Provenance ->
  Expr var binder builtin ->
  [Expr var binder builtin] ->
  Expr var binder builtin
mkVec p tElem xs =
  App
    p
    (LVec p xs)
    [ ImplicitArg p tElem
    ]
