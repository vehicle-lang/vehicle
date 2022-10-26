{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ApplicativeDo        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE EmptyDataDeriving    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Vehicle.Test.CompileMode.Generative.Generator where

import Control.Enumerable
import Control.Monad.Reader (Reader, ask, runReader, withReader)
import Control.Search
import Data.Bifunctor
import Data.Bifunctor.TH
import Data.Coolean
import Data.Maybe
import Data.Proxy
import Test.Feat.Access (valuesWith)

data Z                -- Z has 0 inhabitants
data S n = FZ | FS n  -- S n has n + 1 inhabitants

deriving instance Eq Z
deriving instance Read Z
deriving instance Show Z
deriving instance Typeable Z
deriving instance Eq n => Eq (S n)
deriving instance Functor S
deriving instance Read n => Read (S n)
deriving instance Typeable n => Typeable (S n)

-- Deriving Show via Int:
class Enumerable n => Fin n where
  toInt  :: n -> Int
  sing   :: forall n . Int

instance Fin Z where
  toInt  = fromZ
  sing   = 0

instance Fin n => Fin (S n) where
  toInt = fromS 0 (succ . toInt)
  sing  = sing @n + 1

instance Fin (S n) => Show (S n) where
  show = show . toInt

fromZ :: Z -> a
fromZ n = case n of {} -- No inhabitants, no cases.

fromS :: a -> (n -> a) -> (S n -> a)
fromS fz fs FZ     = fz
fromS fz fs (FS n) = fs n

instance Enumerable Z where
  enumerate = share . aconcat $ []

instance Enumerable n => Enumerable (S n) where
  enumerate = share . aconcat $ [c0 FZ, c1 FS]

enumFin :: Fin n => [n]
enumFin = concat (snd <$> valuesWith global)

infixr 7 :=>

data  Kind
   =  Star
   |  Kind :=> Kind

deriving instance Eq Kind
deriving instance Read Kind
deriving instance Show Kind
deriving instance Typeable Kind
deriveEnumerable ''Kind

infixr 7 :->

data Type ty
   = TyNat
   | (Type ty) :-> (Type ty)
   | TyForall Kind (Type (S ty))
   | TyVar ty
   | TyLam (Type (S ty))
   | TyApp (Type ty) (Type ty) Kind

deriving instance Eq ty => Eq (Type ty)
deriving instance Functor Type
deriving instance Read ty => Read (Type ty)
deriving instance (Show ty, Fin ty) => Show (Type ty)
deriving instance Typeable ty => Typeable (Type ty)
deriveEnumerable ''Type

newtype Normal   a = Normal   { unNormal   :: a }
newtype Neutral  a = Neutral  { unNeutral  :: a }

instance  Enumerable ty =>
          Enumerable (Normal (Type ty)) where
  enumerate = share . aconcat $
    [        c1  $ \(Neutral  a) -> Normal a
    , pay .  c1  $ \(Normal   a) -> Normal (TyLam a)
    ]

instance  Enumerable ty =>
          Enumerable (Neutral (Type ty)) where
  enumerate = share . aconcat . fmap pay $
    [  c0  $                                   Neutral TyNat
    ,  c2  $ \(Neutral a)  (Neutral  b)    ->  Neutral (a :-> b)
    ,  c2  $ \k            (Neutral  a)    ->  Neutral (TyForall k a)
    ,  c1  $ \i                            ->  Neutral (TyVar i)
    ,  c3  $ \(Neutral a)  (Normal   b) k  ->  Neutral (TyApp a b k)
    ]

deriving instance Eq a => Eq (Neutral a)
deriving instance Functor Neutral
deriving instance Typeable a => Typeable (Neutral a)

instance Show a => Show (Neutral a) where
  showsPrec d a = showsPrec d (unNeutral a)

deriving instance Eq a => Eq (Normal a)
deriving instance Functor Normal
deriving instance Typeable a => Typeable (Normal a)

instance Show a => Show (Normal a) where
  showsPrec d a = showsPrec d (unNormal a)

enumNeutralTy :: Enumerable ty => Int -> [Neutral (Type ty)]
enumNeutralTy depth = concat (snd <$> take depth (valuesWith global))

type KindEnv ty = ty -> Kind

extKindEnv :: Kind -> KindEnv ty -> KindEnv (S ty)
extKindEnv = fromS

checkTy :: Kind -> Type ty -> Reader (KindEnv ty) Cool
checkTy Star TyNat = return true
checkTy Star (a :-> b) = do
  aOk <- checkTy Star a
  bOk <- checkTy Star b
  return (aOk &&& bOk)
checkTy Star (TyForall k a) =
  withReader (extKindEnv k) $ checkTy Star a
checkTy k (TyVar i) = do
  kindOf <- ask
  return (toCool (k == kindOf i))
checkTy (k :=> k') (TyLam a) =
  withReader (extKindEnv k) $ checkTy k' a
checkTy k' (TyApp a b k) = do
  aOk <- checkTy (k :=> k') a
  bOk <- checkTy k' b
  return (aOk &&& bOk)
checkTy _ _ = return false

type TySub ty ty' = ty -> Type ty'

extTySub :: TySub ty ty' -> TySub (S ty) (S ty')
extTySub s = fromS (TyVar FZ) (fmap FS . s)

appTySub :: TySub ty ty' -> Type ty -> Type ty'
appTySub s TyNat          = TyNat
appTySub s (a :-> b)      = appTySub s a :-> appTySub s b
appTySub s (TyForall k a) = TyForall k (appTySub (extTySub s) a)
appTySub s (TyVar i)      = s i
appTySub s (TyLam a)      = TyLam (appTySub (extTySub s) a)
appTySub s (TyApp a b k)  = TyApp (appTySub s a) (appTySub s b) k

pattern TyRed a b k = TyApp (TyLam a) b k

stepTy :: Type ty -> Maybe (Type ty)
stepTy TyNat                 = empty
stepTy (a :-> b)              =
  ((:->) <$> stepTy a <*> pure b) <|>
  ((:->) <$> pure a <*> stepTy b)
stepTy (TyForall k a)         = TyForall <$> pure k <*> stepTy a
stepTy (TyLam a)              = TyLam <$> stepTy a
stepTy (TyVar i)              = empty
stepTy (TyRed a b k)          = pure (appTySub (fromS b TyVar) a)
stepTy (TyApp a b k)          =
  (TyApp <$> stepTy a <*> pure b <*> pure k) <|>
  (TyApp <$> pure a <*> stepTy b <*> pure k)

normTy :: Type ty -> Type ty
normTy a = maybe a normTy (stepTy a)

data  Term ty tm
   =  TmVar  tm                      -- deBruijn index
   |  TmLam  (Term ty (S tm))        -- \lambda-abstraction body
   |  TmApp  (Term ty tm)            -- function
             (Term ty tm)            -- argument
             (Normal (Type ty))      -- argument type
   |  TMLAM  (Term (S ty) tm)        -- \Lambda-abstraction body
   |  TMAPP  (Term ty tm)            -- polymorphic term
             (Normal (Type (S ty)))  -- codomain of \forall
             (Normal (Type ty))      -- concrete type
             Kind                    -- concrete type kind

   deriving (Typeable, Eq, Show)

deriveBifunctor   ''Term
deriveEnumerable  ''Term

type TyEnv ty tm = tm -> Type ty

extTyEnv :: Type ty -> TyEnv ty tm -> TyEnv ty (S tm)
extTyEnv a typeOf = fromS a typeOf

raiseTyEnv :: TyEnv ty tm -> TyEnv (S ty) tm
raiseTyEnv typeOf n = FS <$> typeOf n

checkTm ::  Eq ty => Type ty -> Term ty tm ->
            Reader (KindEnv ty, TyEnv ty tm) Cool
checkTm a (TmVar n) = do
  (kindOf, typeOf) <- ask
  return (toCool (a == typeOf n))
checkTm (a :-> b) (TmLam x) =
  withReader (second (extTyEnv a)) $
    checkTm b x
checkTm b (TmApp x y (Normal a)) = do
  xOk <- checkTm (a :-> b) x
  yOk <- checkTm a y
  return (xOk &&& yOk)
checkTm (TyForall k a) (TMLAM x) =
  withReader (bimap (extKindEnv k) raiseTyEnv) $
    checkTm a x
checkTm c (TMAPP x (Normal a) (Normal b) k) = do
  xOk <- checkTm (TyForall k a) x
  bOk <- withReader fst (checkTy k b)
  let cOk = normTy (TyRed a b k) == c
  return (xOk &&& bOk &&& cOk)
checkTm _ _ = return false

checkClosedTm :: Type Z -> Term Z Z -> Cool
checkClosedTm a x =
  runReader (checkTm a x) (fromZ, fromZ)

enumClosedTm :: Int -> Type Z -> IO [Term Z Z]
enumClosedTm depth a = search depth (checkClosedTm a)
