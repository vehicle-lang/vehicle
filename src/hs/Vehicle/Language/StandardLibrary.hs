
module Vehicle.Language.StandardLibrary
  ( prelude
  ) where

import Data.Map (Map)
import Data.Map qualified as Map

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Language.DSL

prelude :: Map Identifier DBDecl
prelude = standardPostulates <> typeClassSolutions

{-
equalsBool :: Bool -> Bool -> Bool
equalsBool x y = (x and y) or (not x and not y)

notEqualsBool :: Bool -> Bool -> Bool
notEqualsBool x y = not (equalsBool x y)

vecEquals :: {{HasEquals a}} -> Vec a -> Vec a -> Bool
vecEquals xs ys = bigAnd (foreach i . xs ! i == ys ! i)
-}

{-
bigAnd :: {{HasFold Bool b}} -> b -> Bool
bigAnd xs = fold (\a b -> a and b) True xs

bigOr :: {{HasFold Bool b}} -> b -> Bool
bigOr xs = fold (\a b -> a or b) False xs
-}

{-
  equalsTensor : {{HasEq a b c}} -> Tensor a (d : ds)-> Tensor (d : ds) -> c
  equalsTensor xs ys = bigAnd (foreach i )
-}
--------------------------------------------------------------------------------
-- Types-classes
{-
function :: Symbol -> DSLExpr -> DSLExpr -> DBDecl
function name typ body =
  DefFunction mempty Nothing
    (Identifier name) (fromDSL mempty typ) (fromDSL mempty body)
-}
typeClassSolutions :: Map Identifier DBDecl
typeClassSolutions = fromDeclList
  []
{-
   function StdHasQuantifierBool
      ((tBool ~> tBool) ~> tBool)
      _
      {-
        let tCont = ListType ann t
        let cont  = mkList ann t [FalseExpr ann, TrueExpr ann]
        let e = QuantifierInExpr q ann tCont binder body cont
        Just $ nf e
      -}

  -- If we're quantifying over a finite index type then expand out to a list
  -- of indices up to the max value.
  , function StdHasQuantifierIndex
      (forall tNat $ \n -> (tIndex n ~> tBool) ~> tBool)
      _

    , function StdHasForallIn _ _
    , function StdHasExistsIn _ _
  ]
-}
--------------------------------------------------------------------------------
-- Postulates

standardPostulates :: Map Identifier DBDecl
standardPostulates = quantifiers

postulate :: Name -> DSLExpr -> DBDecl
postulate name t = DefPostulate mempty (Identifier name) (fromDSL mempty t)

quantifiers :: Map Identifier DBDecl
quantifiers = fromDeclList
  [ postulate "forallNat" $ (tNat ~> tBool) ~> tBool
  , postulate "forallInt" $ (tInt ~> tBool) ~> tBool
  , postulate "forallRat" $ quantifierType Forall

  , postulate "existsNat" $ (tNat ~> tBool) ~> tBool
  , postulate "existsInt" $ (tInt ~> tBool) ~> tBool
  , postulate "existsRat" $ quantifierType Exists
  ]

quantifierType :: Quantifier -> DSLExpr
quantifierType q =
  forall tLin $ \l1 -> forall tPol $ \p1 ->
    forall tLin $ \l2 -> forall tPol $ \p2 ->
      addPolarity q p1 p2 ~~~>
        (tAnnRat linear ~> tAnnBool l1 p2) ~> tAnnBool l2 p2

fromDeclList :: [DBDecl] -> Map Identifier DBDecl
fromDeclList = Map.fromList . fmap (\d -> (identifierOf d, d))