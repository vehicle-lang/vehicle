
module Vehicle.Compile.Simplify
  ( Simplify(..)
  , SimplifyOptions(..)
  ) where

import Control.Monad.Reader (MonadReader(..), runReader)
import Data.Default (Default (..))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Maybe (catMaybes)
import Data.IntMap ( IntMap )
import Data.Text ( Text )

import Vehicle.Language.AST


data SimplifyOptions = SimplifyOptions
  { removeImplicits   :: Bool
  , removeInstances   :: Bool
  , removeNonUserCode :: Bool
  }

instance Default SimplifyOptions where
  def = SimplifyOptions
      { removeImplicits   = True
      , removeInstances   = True
      , removeNonUserCode = True
      }

type MonadSimplify m = MonadReader SimplifyOptions m
type WellFormedAnn ann = (HasOwner ann, Semigroup ann)

class Simplify a where

  -- | Simplifies the code with the default options.
  simplify :: Simplify a => a -> a
  simplify = simplifyWith def

  -- | Simplifies the code according to the options provided.
  --   Note that this can be seen as undoing parts of the type-checking,
  --   and therefore the resulting code is not guaranteed to be well-typed.
  simplifyWith :: Simplify a => SimplifyOptions -> a -> a
  simplifyWith options x = runReader (simplifyReader x) options

  simplifyReader :: MonadSimplify m => a -> m a

instance WellFormedAnn ann => Simplify (Prog binder var ann) where
  simplifyReader (Main ds) = Main <$> traverse simplifyReader ds

instance WellFormedAnn ann => Simplify (Decl binder var ann) where
  simplifyReader = \case
    DeclNetw ann n t -> DeclNetw ann n <$> simplifyReader t
    DeclData ann n t -> DeclData ann n <$> simplifyReader t
    DefFun ann n t e -> DefFun ann n <$> simplifyReader t <*> simplifyReader e

instance WellFormedAnn ann => Simplify (Expr binder var ann) where
  simplifyReader expr = case expr of
    Type{}    -> return expr
    Hole{}    -> return expr
    Meta{}    -> return expr
    Builtin{} -> return expr
    Literal{} -> return expr
    Var{}     -> return expr

    App ann fun args          -> normAppList ann <$> simplifyReader fun <*> simplifyReaderArgs args
    LSeq ann dict xs          -> LSeq ann <$> simplifyReader dict <*> traverse simplifyReader xs
    Ann ann e t               -> Ann ann <$> simplifyReader e <*> simplifyReader t
    Pi ann binder result      -> Pi  ann <$> simplifyReader binder <*> simplifyReader result
    Let ann bound binder body -> Let ann <$> simplifyReader bound  <*> simplifyReader binder <*> simplifyReader body
    Lam ann binder body       -> Lam ann <$> simplifyReader binder <*> simplifyReader body
    PrimDict ann tc           -> PrimDict ann <$> simplifyReader tc

instance WellFormedAnn ann => Simplify (Binder binder var ann) where
  simplifyReader = traverseBinderType simplifyReader

instance WellFormedAnn ann => Simplify (Arg binder var ann) where
  simplifyReader = traverseArgExpr simplifyReader

simplifyReaderArgs
  :: (WellFormedAnn ann, MonadSimplify m)
  => NonEmpty (Arg binder var ann)
  -> m [Arg binder var ann]
simplifyReaderArgs args = catMaybes <$> traverse prettyArg (NonEmpty.toList args)
  where
    prettyArg :: (WellFormedAnn ann, MonadSimplify m)
              => Arg binder var ann
              -> m (Maybe (Arg binder var ann))
    prettyArg arg = do
      SimplifyOptions{..} <- ask

      let removeArg =
            (removeNonUserCode && ownerOf arg == TheMachine) ||
            (visibilityOf arg == Implicit && removeImplicits) ||
            (visibilityOf arg == Instance && removeInstances)

      if removeArg
        then return Nothing
        else Just <$> simplifyReader arg

--------------------------------------------------------------------------------
-- Other instances

instance Simplify a => Simplify [a] where
  simplifyReader = traverse simplifyReader

instance (Simplify a, Simplify b) => Simplify (a, b) where
  simplifyReader (x, y) = do
    x' <- simplifyReader x;
    y' <- simplifyReader y;
    return (x', y')

instance (Simplify a, Simplify b, Simplify c) => Simplify (a, b, c) where
  simplifyReader (x, y, z) = do
    x' <- simplifyReader x;
    y' <- simplifyReader y;
    z' <- simplifyReader z;
    return (x', y', z')

instance Simplify a => Simplify (IntMap a) where
  simplifyReader = traverse simplifyReader

instance Simplify PositionTree where
  simplifyReader = return

instance Simplify PositionsInExpr where
  simplifyReader (PositionsInExpr e t) = return $ PositionsInExpr e t
    --PositionsInExpr <$> simplifyReader e <*> simplifyReader t

instance Simplify Text where
  simplifyReader = return

instance Simplify Int where
  simplifyReader = return