{-# LANGUAGE CPP                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Expr.DeBruijn
  ( LocallyNamelessVar(..)
  , DBBinding
  , DBIndex(..)
  , DBIndexVar
  , DBLevel(..)
  , DBLevelVar
  , DBBinder
  , DBArg
  , DBExpr
  , DBDecl
  , DBProg
  , BindingDepth
  , Substitution
  , substituteDB
  , substDBInto
  , substDBIntoAtLevel
  , substDBAll
  , liftDBIndices
  , underDBBinder
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.Reader (MonadReader (..), local, runReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (Bifunctor (..))
import Data.Hashable (Hashable (..))
import GHC.Generics (Generic)

import Vehicle.Prelude
import Vehicle.Syntax.AST

#if nothunks
import NoThunks.Class (NoThunks)
#endif

--------------------------------------------------------------------------------
-- Definitions

-- | The type of data DeBruijn indices store at name sites
data LocallyNamelessVar a
  = Free Identifier
  | Bound a
  deriving (Eq, Ord, Show, Generic)

#if nothunks
instance NoThunks a => NoThunks (LocallyNamelessVar a)
#endif

instance NFData   a => NFData   (LocallyNamelessVar a)
instance Hashable a => Hashable (LocallyNamelessVar a)
instance ToJSON   a => ToJSON   (LocallyNamelessVar a)
instance FromJSON a => FromJSON (LocallyNamelessVar a)

-- | A DeBruijn index pointing to the binder that the variable refers to,
-- counting from the variable position upwards.
newtype DBIndex = DBIndex
  { dbIndex :: Int
  } deriving (Eq, Ord, Num, Show, Generic)

#if nothunks
instance NoThunks DBIndex
#endif

instance NFData   DBIndex
instance Hashable DBIndex
instance ToJSON   DBIndex
instance FromJSON DBIndex

instance Pretty DBIndex where
  pretty i = "i" <> pretty i

-- | A DeBruijn index pointing to the binder that the variable refers to,
-- counting from the top of the expression downwards.
newtype DBLevel = DBLevel
  { dbLevel :: Int
  } deriving (Eq, Ord, Num, Show, Generic)

#if nothunks
instance NoThunks DBLevel
#endif

instance NFData   DBLevel
instance Hashable DBLevel
instance ToJSON   DBLevel
instance FromJSON DBLevel

instance Pretty DBLevel where
  pretty l = "l" <> pretty l

-- |The type of data DeBruijn indices store at name sites
type DBIndexVar = LocallyNamelessVar DBIndex

-- |The type of data DeBruijn levels store at name sites
type DBLevelVar = LocallyNamelessVar DBLevel

-- | The type of the data DeBruijn notation stores at binding sites.
type DBBinding = Maybe Name

-- | Used to track the number of binders we're underneath during a traversal of
-- an expression
type BindingDepth = Int

--------------------------------------------------------------------------------
-- Expressions

-- An expression that uses DeBruijn index scheme for both binders and variables.

type DBBinder = Binder DBBinding DBIndexVar
type DBArg    = Arg    DBBinding DBIndexVar
type DBExpr   = Expr   DBBinding DBIndexVar
type DBDecl   = Decl   DBBinding DBIndexVar
type DBProg   = Prog   DBBinding DBIndexVar

--------------------------------------------------------------------------------
-- Substitution

type Substitution value = DBIndex -> Either DBIndex value

class Substitutable value target | target -> value where
  subst :: MonadReader (BindingDepth, Substitution value) m => target -> m target

instance Substitutable expr expr => Substitutable expr (GenericArg expr)  where
  subst = traverse subst

instance Substitutable expr expr => Substitutable expr (GenericBinder binder expr) where
  subst = traverse subst

instance Substitutable DBExpr DBExpr where
  subst = \case
    Var p (Bound i) -> do
      (d, s) <- ask
      return $ if i < DBIndex d then
        Var p (Bound i)
      else case s (i - DBIndex d) of
        Left i' -> Var p (Bound (i' + DBIndex d))
        Right v -> if d > 0 then liftDBIndices d v else v

    Universe p l        -> return $ Universe p l
    Meta     p m        -> return $ Meta p m
    Hole     p name     -> return $ Hole p name
    Builtin  p op       -> return $ Builtin p op
    Literal  p l        -> return $ Literal p l
    Var      p (Free i) -> return $ Var p (Free i)

    LVec p es           -> LVec    p <$> traverse subst es
    Ann  p term typ     -> Ann     p <$> subst   term   <*> subst typ
    App  p fun args     -> normApp p <$> subst   fun    <*> traverse subst args
    Pi   p binder res   -> Pi      p <$> traverse subst binder <*> underDBBinder (subst res)
    Let  p e1 binder e2 -> Let     p <$> subst e1 <*> traverse subst binder <*> underDBBinder (subst e2)
    Lam  p binder e     -> Lam     p <$> traverse subst binder <*> underDBBinder (subst e)

-- Temporarily go under a binder, increasing the binding depth by one
-- and shifting the current state.
underDBBinder :: MonadReader (BindingDepth, c) m => m a -> m a
underDBBinder = local (first (+ 1))

--------------------------------------------------------------------------------
-- Concrete operations

substituteDB :: BindingDepth -> Substitution DBExpr -> DBExpr -> DBExpr
substituteDB depth sub e = runReader (subst e) (depth, sub)

-- | Lift all DeBruijn indices that refer to environment variables by the
-- provided depth.
liftDBIndices :: Int                            -- ^ amount to lift by
              -> DBExpr                         -- ^ target term to lift
              -> DBExpr                         -- ^ lifted term
liftDBIndices d = substituteDB 0 (\v -> Left (v + DBIndex d))

-- | De Bruijn aware substitution of one expression into another
substDBIntoAtLevel :: DBIndex      -- ^ The index of the variable of which to substitute
                   -> DBExpr       -- ^ expression to substitute
                   -> DBExpr       -- ^ term to substitute into
                   -> DBExpr       -- ^ the result of the substitution
substDBIntoAtLevel level value = substituteDB 0 substVar
  where
    substVar :: DBIndex -> Either DBIndex DBExpr
    substVar v
      | v == level = Right value
      | v > level = Left (v - 1)
      | otherwise = Left v

-- | De Bruijn aware substitution of one expression into another
substDBInto :: DBExpr  -- ^ expression to substitute
            -> DBExpr -- ^ term to substitute into
            -> DBExpr -- ^ the result of the substitution
substDBInto = substDBIntoAtLevel 0

substDBAll :: BindingDepth
         -> (DBIndex -> Maybe DBIndex)
         -> DBExpr
         -> DBExpr
substDBAll depth sub = substituteDB depth (\v -> maybe (Left v) Left (sub v))
