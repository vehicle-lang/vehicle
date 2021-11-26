
module Vehicle.Compile.Simplify
  ( runSimplify
  , Simplify(..)
  , SimplifyOptions(..)
  ) where

import Control.Monad.Reader (MonadReader(..), runReader)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Maybe (catMaybes)

import Vehicle.Language.AST

-- | Simplifies the code according to the options provided! Note that
-- this can be seen as undoing parts of the type-checking, and therefore the
-- resulting code is not guaranteed to be well-typed.
runSimplify :: Simplify a => SimplifyOptions -> a -> a
runSimplify options x = runReader (simplify x) options

data SimplifyOptions = SimplifyOptions
  { removeImplicits   :: Bool
  , removeInstances   :: Bool
  , removeNonUserCode :: Bool
  }

type MonadSimplify m = MonadReader SimplifyOptions m
type WellFormedAnn ann = (HasOwner ann, Semigroup ann)

class Simplify a where
  simplify :: MonadSimplify m => a -> m a

instance WellFormedAnn ann => Simplify (Prog binder var ann) where
  simplify (Main ds) = Main <$> traverse simplify ds

instance WellFormedAnn ann => Simplify (Decl binder var ann) where
  simplify = \case
    DeclNetw ann n t -> DeclNetw ann n <$> simplify t
    DeclData ann n t -> DeclData ann n <$> simplify t
    DefFun ann n t e -> DefFun ann n <$> simplify t <*> simplify e

instance WellFormedAnn ann => Simplify (Expr binder var ann) where
  simplify expr = case expr of
    Type{}    -> return expr
    Hole{}    -> return expr
    Meta{}    -> return expr
    Builtin{} -> return expr
    Literal{} -> return expr
    Var{}     -> return expr

    App ann fun args          -> normAppList ann <$> simplify fun <*> simplifyArgs args
    Seq ann xs                -> Seq ann <$> traverse simplify xs
    Ann ann e t               -> Ann ann <$> simplify e <*> simplify t
    Pi ann binder result      -> Pi  ann <$> simplify binder <*> simplify result
    Let ann bound binder body -> Let ann <$> simplify bound  <*> simplify binder <*> simplify body
    Lam ann binder body       -> Lam ann <$> simplify binder <*> simplify body
    PrimDict tc               -> PrimDict <$> simplify tc

instance WellFormedAnn ann => Simplify (Binder binder var ann) where
  simplify = traverseBinderType simplify

instance WellFormedAnn ann => Simplify (Arg binder var ann) where
  simplify = traverseArgExpr simplify

simplifyArgs :: (WellFormedAnn ann, MonadSimplify m)
           => NonEmpty (Arg binder var ann)
           -> m [Arg binder var ann]
simplifyArgs args = catMaybes <$> traverse prettyArg (NonEmpty.toList args)
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
        else Just <$> simplify arg

--------------------------------------------------------------------------------
-- Other instances

instance Simplify a => Simplify [a] where
  simplify = traverse simplify

instance Simplify b => Simplify (a, b) where
  simplify (x, y) = do y' <- simplify y; return (x, y')