{-# LANGUAGE PartialTypeSignatures #-}

module Vehicle.Expr.DeBruijn
  ( DBVar(..)
  , DBBinding
  , DBIndex
  , DBBinder
  , DBArg
  , DBExpr
  , DBDecl
  , DBProg
  , BindingDepth
  , Substitution
  , Substitutable(substituteDB)
  , substDBInto
  , substDBIntoAtLevel
  , substDBAll
  , liftDBIndices
  , underDBBinder
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.Reader (MonadReader (..), local, runReader)
import Data.Bifunctor (Bifunctor (..))
import Data.Hashable (Hashable (..))
import GHC.Generics (Generic)

import Vehicle.Syntax.AST

--------------------------------------------------------------------------------
-- Definitions

-- | A DeBruijn index pointing to the binder that the variable refers to.
type DBIndex = Int

-- |The type of data DeBruijn indices store at name sites
data DBVar
  = Free Identifier
  | Bound DBIndex
  deriving (Eq, Ord, Show, Generic)

instance NFData DBVar

instance Hashable DBVar

-- |The type of the data DeBruijn notation stores at binding sites.
type DBBinding = Maybe Name

-- | Used to track the number of binders we're underneath during a traversal of
-- an expression
type BindingDepth = Int

--------------------------------------------------------------------------------
-- Expressions

-- An expression that uses DeBruijn index scheme for both binders and variables.
type DBBinder = Binder DBBinding DBVar
type DBArg    = Arg    DBBinding DBVar
type DBExpr   = Expr   DBBinding DBVar
type DBDecl   = Decl   DBBinding DBVar
type DBProg   = Prog   DBBinding DBVar

--------------------------------------------------------------------------------
-- A framework for writing generic operations on DeBruijn variables

type Substitution value = DBIndex -> Either DBIndex value

class Substitutable value target | target -> value where
  subst :: MonadReader (BindingDepth, Substitution value) m => target -> m target

  substituteDB :: BindingDepth -> Substitution value -> target -> target
  substituteDB depth sub e = runReader (subst e) (depth, sub)

instance Substitutable expr expr => Substitutable expr (GenericArg expr)  where
  subst = traverse subst

instance Substitutable expr expr => Substitutable expr (GenericBinder binder expr) where
  subst = traverse subst

-- Temporarily go under a binder, increasing the binding depth by one
-- and shifting the current state.
underDBBinder :: MonadReader (BindingDepth, c) m => m a -> m a
underDBBinder = local (first (+1))

--------------------------------------------------------------------------------
-- Concrete operations

-- | Lift all DeBruijn indices that refer to environment variables by the
-- provided depth.
liftDBIndices :: Substitutable value target
              => Int                            -- ^ amount to lift by
              -> target                         -- ^ target term to lift
              -> target                         -- ^ lifted term
liftDBIndices d = substituteDB 0 (\v -> Left (v+d))

-- | De Bruijn aware substitution of one expression into another
substDBIntoAtLevel :: forall value target . Substitutable value target
                 => DBIndex      -- ^ The index of the variable of which to substitute
                 -> value        -- ^ expression to substitute
                 -> target       -- ^ term to substitute into
                 -> target       -- ^ the result of the substitution
substDBIntoAtLevel level value = substituteDB 0 substVar
  where
    substVar :: DBIndex -> Either DBIndex value
    substVar v
      | v == level = Right value
      | v > level  = Left (v - 1)
      | otherwise  = Left v

-- | De Bruijn aware substitution of one expression into another
substDBInto :: Substitutable value target
          => value  -- ^ expression to substitute
          -> target -- ^ term to substitute into
          -> target -- ^ the result of the substitution
substDBInto = substDBIntoAtLevel 0

substDBAll :: Substitutable value target
         => BindingDepth
         -> (DBIndex -> Maybe DBIndex)
         -> target
         -> target
substDBAll depth sub = substituteDB depth (\v -> maybe (Left v) Left (sub v))


--------------------------------------------------------------------------------
-- DeBruijn substitution

instance Substitutable DBExpr DBExpr where
  subst = \case

    Var p (Bound i) -> do
      (d, s) <- ask
      return $ if i < d then
        Var p (Bound i)
      else case s (i - d) of
        Left i' -> Var p (Bound (i' + d))
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
