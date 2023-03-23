{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Expr.DeBruijn
  ( DBBinding,
    DBIndex (..),
    DBLevel (..),
    DBBinder,
    DBArg,
    DBType,
    DBExpr,
    DBDecl,
    DBProg,
    DBTelescope,
    Substitution,
    substituteDB,
    substDBInto,
    substDBIntoAtLevel,
    substDBAll,
    liftDBIndices,
    underDBBinder,
    dbLevelToIndex,
    shiftDBIndex,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad.Reader (MonadReader (..), local, runReader)
import Data.Aeson (ToJSON)
import Data.Bifunctor (Bifunctor (..))
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Vehicle.Prelude
import Vehicle.Syntax.AST

--------------------------------------------------------------------------------
-- Definitions

-- | A DeBruijn index pointing to the binder that the variable refers to,
-- counting from the variable position upwards.
newtype DBIndex = DBIndex
  { unIndex :: Int
  }
  deriving (Eq, Ord, Num, Show, Generic)

instance NFData DBIndex

instance Hashable DBIndex

instance ToJSON DBIndex

instance Serialize DBIndex

instance Pretty DBIndex where
  pretty i = "𝓲" <> pretty (unIndex i)

-- | DeBruijn level - represents how many binders deep we currently are.
-- (e.g. \f . f (\x . x)) the variable `f` is at level 0 and the variable `x`
-- is at level 1.
-- When used as a variable refers to the binder at that level.
newtype DBLevel = DBLevel
  { unLevel :: Int
  }
  deriving (Eq, Ord, Num, Enum, Show, Generic)

instance NFData DBLevel

instance Hashable DBLevel

instance ToJSON DBLevel

instance Serialize DBLevel

instance Pretty DBLevel where
  pretty l = "𝓵" <> pretty (unLevel l)

-- | The type of the data DeBruijn notation stores at binding sites.
type DBBinding = ()

-- | Converts a `DBLevel` x to a `DBIndex` given that we're currently at
-- level `l`.
dbLevelToIndex :: DBLevel -> DBLevel -> DBIndex
dbLevelToIndex l x = DBIndex (unLevel l - unLevel x - 1)

shiftDBIndex :: DBIndex -> DBLevel -> DBIndex
shiftDBIndex i l = DBIndex (unIndex i + unLevel l)

--------------------------------------------------------------------------------
-- Expressions

-- An expression that uses DeBruijn index scheme for both binders and variables.
type DBBinder builtin = Binder DBBinding DBIndex builtin

type DBArg builtin = Arg DBBinding DBIndex builtin

type DBType builtin = DBExpr builtin

type DBExpr builtin = Expr DBBinding DBIndex builtin

type DBDecl builtin = Decl DBBinding DBIndex builtin

type DBProg builtin = Prog DBBinding DBIndex builtin

type DBTelescope builtin = [DBBinder builtin]

--------------------------------------------------------------------------------
-- Substitution

type Substitution value = DBIndex -> Either DBIndex value

class Substitutable value target | target -> value where
  subst :: MonadReader (DBLevel, Substitution value) m => target -> m target

instance Substitutable expr expr => Substitutable expr (GenericArg expr) where
  subst = traverse subst

instance Substitutable expr expr => Substitutable expr (GenericBinder binder expr) where
  subst = traverse subst

instance Substitutable (DBExpr builtin) (DBExpr builtin) where
  subst expr = case expr of
    BoundVar p i -> do
      (d, s) <- ask
      return $
        if unIndex i < unLevel d
          then BoundVar p i
          else case s (shiftDBIndex i (-d)) of
            Left i' -> BoundVar p (shiftDBIndex i' d)
            Right v -> if d > 0 then liftDBIndices d v else v
    Universe {} -> return expr
    Meta {} -> return expr
    Hole {} -> return expr
    Builtin {} -> return expr
    FreeVar {} -> return expr
    Ann p term typ -> Ann p <$> subst term <*> subst typ
    App p fun args -> normApp p <$> subst fun <*> traverse subst args
    Pi p binder res -> Pi p <$> traverse subst binder <*> underDBBinder (subst res)
    Let p e1 binder e2 -> Let p <$> subst e1 <*> traverse subst binder <*> underDBBinder (subst e2)
    Lam p binder e -> Lam p <$> traverse subst binder <*> underDBBinder (subst e)

-- Temporarily go under a binder, increasing the binding depth by one
-- and shifting the current state.
underDBBinder :: MonadReader (DBLevel, c) m => m a -> m a
underDBBinder = local (first (+ 1))

--------------------------------------------------------------------------------
-- Concrete operations

substituteDB :: DBLevel -> Substitution (DBExpr builtin) -> DBExpr builtin -> DBExpr builtin
substituteDB depth sub e = runReader (subst e) (depth, sub)

-- | Lift all DeBruijn indices that refer to environment variables by the
-- provided depth.
liftDBIndices ::
  -- | number of levels to lift by
  DBLevel ->
  -- | target term to lift
  DBExpr builtin ->
  -- | lifted term
  DBExpr builtin
liftDBIndices l = substituteDB 0 (\i -> Left (shiftDBIndex i l))

-- | De Bruijn aware substitution of one expression into another
substDBIntoAtLevel ::
  forall builtin.
  -- | The index of the variable of which to substitute
  DBIndex ->
  -- | expression to substitute
  DBExpr builtin ->
  -- | term to substitute into
  DBExpr builtin ->
  -- | the result of the substitution
  DBExpr builtin
substDBIntoAtLevel level value = substituteDB 0 substVar
  where
    substVar :: DBIndex -> Either DBIndex (DBExpr builtin)
    substVar v
      | v == level = Right value
      | v > level = Left (v - 1)
      | otherwise = Left v

-- | De Bruijn aware substitution of one expression into another
substDBInto ::
  -- | expression to substitute
  DBExpr builtin ->
  -- | term to substitute into
  DBExpr builtin ->
  -- | the result of the substitution
  DBExpr builtin
substDBInto = substDBIntoAtLevel 0

substDBAll ::
  DBLevel ->
  (DBIndex -> Maybe DBIndex) ->
  DBExpr builtin ->
  DBExpr builtin
substDBAll depth sub = substituteDB depth (\v -> maybe (Left v) Left (sub v))
