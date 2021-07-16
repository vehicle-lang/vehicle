module Vehicle.Prelude.Types where

-- TODO swap out for lists?

-- | Type-level indexed product.
data (:*:) (f :: k -> *) (g :: k -> *) (x :: k) = (:*:) { ifst :: f x, isnd :: g x }
  deriving (Eq, Ord, Show)

ifirst :: (f x -> g x) -> (f :*: h) x -> (g :*: h) x
ifirst f (x :*: y) = f x :*: y

isecond :: (f x -> g x) -> (h :*: f) x -> (h :*: g) x
isecond f (x :*: y) = x :*: f y

instance (Semigroup (f x), Semigroup (g x)) => Semigroup ((f :*: g) x) where
  (x1 :*: y1) <> (x2 :*: y2) = (x1 <> x2) :*: (y1 <> y2)

instance (Monoid (f x), Monoid (g x)) => Monoid ((f :*: g) x) where
  mempty = mempty :*: mempty

-- |Type-level decidable membership
type family DecIn (x :: k) (xs :: [k]) :: Bool where
  DecIn x '[]       = 'False
  DecIn x (x ': xs) = 'True
  DecIn x (y ': xs) = DecIn x xs

-- |Type-level membership procedure
type In (x :: k) (xs :: [k]) = DecIn x xs ~ 'True

-- |Type-level constant function.
newtype K (a :: *) (b :: k) = K { unK :: a }
  deriving (Eq, Ord, Show)

instance Semigroup a => Semigroup (K a sort) where
  K x <> K y = K (x <> y)

instance Monoid a => Monoid (K a sort) where
  mempty = K mempty

-- |Type-level function composition.
newtype O (g :: * -> *) (f :: k -> *) (a :: k) = O { unO :: g (f a) }