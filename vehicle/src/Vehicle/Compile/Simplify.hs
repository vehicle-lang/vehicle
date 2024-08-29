module Vehicle.Compile.Simplify
  ( Simplify (..),
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (filter)
import Data.Text qualified as Text
import Vehicle.Data.Builtin.Standard.Core ()
import Vehicle.Data.Code.Expr (PrintableBuiltin (..))
import Vehicle.Syntax.AST.Arg
import Vehicle.Syntax.AST.Expr
import Vehicle.Syntax.AST.Relevance (Relevance (..))
import Vehicle.Syntax.AST.Visibility (Visibility (..), visibilityOf)
import Vehicle.Syntax.Builtin

-- | Note that these operations can be seen as undoing parts of the type-checking,
-- and therefore the resulting code is not guaranteed to be well-typed.
class Simplify a where
  -- | Removes automatically inserted arguments and binders.
  uninsert :: a -> a

  -- | Shortens vectors
  shortenVec :: a -> a

instance Simplify Prog where
  uninsert = fmap uninsert
  shortenVec = fmap shortenVec

instance Simplify Decl where
  uninsert = fmap uninsert
  shortenVec = fmap shortenVec

instance Simplify Expr where
  uninsert = mapApp $ \fun args -> do
    let fun' = uninsert fun
    let args' = simplifyArgs args
    -- Remove automatically inserted cast functions
    if isLiteralCast fun' && not (null args')
      then argExpr $ last args'
      else normAppList fun' args'

  shortenVec = mapApp $ \fun args ->
    case fun of
      Builtin p (BuiltinConstructor LVec {}) -> case getHeadMidTail args of
        Just (firstArg, numberOfMiddleAgs, lastArg)
          | numberOfMiddleAgs > 3 ->
              normAppList
                fun
                [ firstArg,
                  Arg p Explicit Relevant (Var p ("<" <> n2 <> " more>")),
                  lastArg
                ]
          where
            n2 = Text.pack $ show $ length args - 2
        _ -> App fun args
      _ -> App fun args
    where
      getHeadMidTail :: forall a. NonEmpty a -> Maybe (a, Int, a)
      getHeadMidTail (x :| xs) = go 0 xs
        where
          go :: Int -> [a] -> Maybe (a, Int, a)
          go _ [] = Nothing
          go l [e] = Just (x, l, e)
          go l (_ : ys) = go (l + 1) ys

instance Simplify Binder where
  uninsert = fmap uninsert
  shortenVec = fmap shortenVec

instance Simplify Arg where
  uninsert = fmap uninsert
  shortenVec = fmap shortenVec

mapApp :: (Expr -> NonEmpty Arg -> Expr) -> Expr -> Expr
mapApp f expr = case expr of
  Universe {} -> expr
  Hole {} -> expr
  Builtin {} -> expr
  Var {} -> expr
  Pi p binder result -> Pi p (fmap (mapApp f) binder) (mapApp f result)
  Let p bound binder body -> Let p (mapApp f bound) (fmap (mapApp f) binder) (mapApp f body)
  Lam p binder body -> Lam p (fmap (mapApp f) binder) (mapApp f body)
  App fun args -> f fun args

simplifyArgs :: NonEmpty Arg -> [Arg]
simplifyArgs = fmap uninsert . NonEmpty.filter (not . wasInserted)

wasInserted :: Arg -> Bool
wasInserted arg = case visibilityOf arg of
  Implicit True -> True
  Instance True -> True
  _ -> False

isLiteralCast :: Expr -> Bool
isLiteralCast = \case
  Builtin _ b -> isCoercion b
  _ -> False
