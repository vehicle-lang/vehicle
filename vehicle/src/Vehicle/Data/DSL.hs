{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Vehicle.Data.DSL
  ( DSL (..),
    DSLExpr (..),
    fromDSL,
    toDSL,
    type0,
    (~>),
    (.~>),
    (~~>),
    (~~~>),
    (.~~~>),
    (@@),
    (.@@),
    (@@@),
    (.@@@),
    (@@@@),
    (.@@@@),
    explLam,
    implLam,
    instLam,
    naryFunc,
    forAllExpl,
    forAll,
    forAllInstance,
    forAllIrrelevant,
    forAllTypes,
    forAllTypePairs,
    forAllTypeTriples,
    implTypeTripleLam,
    builtin,
    tHole,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Vehicle.Data.Code.Expr
import Vehicle.Data.DeBruijn
import Vehicle.Data.Universe
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction)
import Vehicle.Prelude
import Prelude hiding (pi)

--------------------------------------------------------------------------------
-- Definition
--------------------------------------------------------------------------------

class DSL expr where
  infixl 4 `app`

  hole :: expr
  app :: expr -> NonEmpty (Visibility, Relevance, expr) -> expr
  pi :: Maybe Name -> Visibility -> Relevance -> expr -> (expr -> expr) -> expr
  lam :: Name -> Visibility -> Relevance -> expr -> (expr -> expr) -> expr
  free :: StdLibFunction -> expr

newtype DSLExpr builtin = DSL
  { unDSL :: Provenance -> Lv -> Expr builtin
  }

fromDSL :: Provenance -> DSLExpr builtin -> Expr builtin
fromDSL p e = unDSL e p 0

toDSL :: Expr builtin -> DSLExpr builtin
toDSL e = DSL $ \_p l ->
  if l > 0
    then liftDBIndices l e
    else e

boundVar :: Lv -> DSLExpr builtin
boundVar i = DSL $ \p j -> BoundVar p (dbLevelToIndex j i)

approxPiForm :: Maybe Name -> Visibility -> BinderDisplayForm
approxPiForm name = \case
  Explicit {} -> BinderDisplayForm OnlyType False
  Implicit {} -> BinderDisplayForm (OnlyName $ fromMaybe "_" name) True
  Instance {} -> BinderDisplayForm OnlyType False

instance DSL (DSLExpr builtin) where
  hole = DSL $ \p _i ->
    Hole p "_"

  pi name v r binderType bodyFn = DSL $ \p i ->
    let varType = unDSL binderType p i
        var = boundVar i
        form = approxPiForm name v
        binder = Binder p form v r varType
        body = unDSL (bodyFn var) p (i + 1)
     in Pi p binder body

  lam name v r binderType bodyFn = DSL $ \p i ->
    let varType = unDSL binderType p i
        var = boundVar i
        binder = Binder p (BinderDisplayForm (OnlyName name) True) v r varType
        body = unDSL (bodyFn var) p (i + 1)
     in Lam p binder body

  app fun args = DSL $ \p i ->
    let fun' = unDSL fun p i
        args' = fmap (\(v, r, e) -> Arg p v r (unDSL e p i)) args
     in App fun' args'

  free stdlibFn = DSL $ \p _i ->
    FreeVar p (identifierOf stdlibFn)

--------------------------------------------------------------------------------
-- AST
--------------------------------------------------------------------------------

-- | Explicit function type
infixr 4 ~>

(~>) :: DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
x ~> y = pi Nothing Explicit Relevant x (const y)

-- | Irrelevant explicit function type
infixr 4 .~>

(.~>) :: DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
x .~> y = pi Nothing Explicit Irrelevant x (const y)

-- | Implicit function type
infixr 4 ~~>

(~~>) :: DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
x ~~> y = pi Nothing (Implicit False) Relevant x (const y)

-- | Instance function type
infixr 4 ~~~>

(~~~>) :: DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
x ~~~> y = pi Nothing (Instance False) Relevant x (const y)

-- | Irrelevant instance function type
infixr 4 .~~~>

(.~~~>) :: DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
x .~~~> y = pi Nothing (Instance False) Irrelevant x (const y)

explLam :: Name -> DSLExpr builtin -> (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
explLam n = lam n Explicit Relevant

implLam :: Name -> DSLExpr builtin -> (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
implLam n = lam n (Implicit False) Relevant

instLam :: Name -> DSLExpr builtin -> (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
instLam n = lam n (Instance False) Relevant

implTypeTripleLam :: (DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
implTypeTripleLam f =
  implLam "t1" type0 $ \t1 ->
    implLam "t2" type0 $ \t2 ->
      implLam "t3" type0 $ \t3 ->
        f t1 t2 t3

infixl 6 @@

(@@) :: DSLExpr builtin -> NonEmpty (DSLExpr builtin) -> DSLExpr builtin
(@@) f args = app f (fmap (Explicit,Relevant,) args)

infixl 6 .@@

(.@@) :: DSLExpr builtin -> NonEmpty (DSLExpr builtin) -> DSLExpr builtin
(.@@) f args = app f (fmap (Explicit,Irrelevant,) args)

infixl 6 @@@

(@@@) :: DSLExpr builtin -> NonEmpty (DSLExpr builtin) -> DSLExpr builtin
(@@@) f args = app f (fmap (Implicit True,Relevant,) args)

infixl 6 .@@@

(.@@@) :: DSLExpr builtin -> NonEmpty (DSLExpr builtin) -> DSLExpr builtin
(.@@@) f args = app f (fmap (Implicit True,Irrelevant,) args)

infixl 6 @@@@

(@@@@) :: DSLExpr builtin -> NonEmpty (DSLExpr builtin) -> DSLExpr builtin
(@@@@) f args = app f (fmap (Instance True,Relevant,) args)

infixl 6 .@@@@

(.@@@@) :: DSLExpr builtin -> NonEmpty (DSLExpr builtin) -> DSLExpr builtin
(.@@@@) f args = app f (fmap (Instance True,Irrelevant,) args)

naryFunc :: Int -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
naryFunc n a b = foldr (\_ r -> a ~> r) b ([0 .. n - 1] :: [Int])

forAllExpl :: Name -> DSLExpr builtin -> (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
forAllExpl name = pi (Just name) Explicit Relevant

forAll :: Name -> DSLExpr builtin -> (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
forAll name = pi (Just name) (Implicit False) Relevant

forAllIrrelevant :: Name -> DSLExpr builtin -> (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
forAllIrrelevant name = pi (Just name) (Implicit False) Irrelevant

forAllInstance :: Name -> DSLExpr builtin -> (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
forAllInstance name = pi (Just name) (Instance False) Relevant

universe :: UniverseLevel -> DSLExpr builtin
universe u = DSL $ \p _ -> Universe p u

type0 :: DSLExpr builtin
type0 = universe $ UniverseLevel 0

forAllTypes :: (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
forAllTypes f =
  forAll "t" type0 $ \t ->
    f t

forAllTypePairs :: (DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
forAllTypePairs f =
  forAll "t1" type0 $ \t1 ->
    forAll "t2" type0 $ \t2 ->
      f t1 t2

forAllTypeTriples :: (DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
forAllTypeTriples f =
  forAll "t1" type0 $ \t1 ->
    forAll "t2" type0 $ \t2 ->
      forAll "t3" type0 $ \t3 ->
        f t1 t2 t3

builtin :: builtin -> DSLExpr builtin
builtin b = DSL $ \p _ -> Builtin p b

tHole :: Name -> DSLExpr builtin
tHole name = DSL $ \p _ -> Hole p name
