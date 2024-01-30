module Vehicle.Compile.Simplify
  ( Simplify (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (filter)
import Data.Text qualified as Text
import Vehicle.Compile.Prelude
import Vehicle.Data.BuiltinInterface (PrintableBuiltin (isCoercion))
import Vehicle.Syntax.Builtin

-- | Note that these operations can be seen as undoing parts of the type-checking,
-- and therefore the resulting code is not guaranteed to be well-typed.
class Simplify a where
  -- | Removes automatically inserted arguments and binders.
  uninsert :: a -> a

  -- | Shortens vectors
  shortenVec :: a -> a

instance Simplify (Prog Name Builtin) where
  uninsert = fmap uninsert
  shortenVec = fmap shortenVec

instance Simplify (Decl Name Builtin) where
  uninsert = fmap uninsert
  shortenVec = fmap shortenVec

instance Simplify (Expr Name Builtin) where
  uninsert expr = case expr of
    Universe {} -> expr
    Hole {} -> expr
    Meta {} -> expr
    Builtin {} -> expr
    BoundVar {} -> expr
    FreeVar {} -> expr
    Pi p binder result -> Pi p (uninsert binder) (uninsert result)
    Let p bound binder body -> Let p (uninsert bound) (uninsert binder) (uninsert body)
    Lam p binder body -> Lam p (uninsert binder) (uninsert body)
    App p fun args -> do
      let fun' = uninsert fun
      let args' = simplifyArgs args
      -- Remove automatically inserted cast functions
      if isLiteralCast fun' && not (null args')
        then argExpr $ last args'
        else normAppList p fun' args'

  shortenVec = mapBuiltins $ \p1 p2 b args ->
    case b of
      BuiltinConstructor (LVec n) -> case getHeadMidTail args of
        Just (firstArg, numberOfMiddleAgs, lastArg)
          | numberOfMiddleAgs > 3 ->
              normAppList
                p1
                (Builtin p2 (BuiltinConstructor (LVec n)))
                [ firstArg,
                  Arg p2 Explicit Relevant (FreeVar p2 (Identifier StdLib ("<" <> n2 <> " more>"))),
                  lastArg
                ]
          where
            n2 = Text.pack $ show $ length args - 2
        _ -> normAppList p1 (Builtin p2 b) args
      _ -> normAppList p1 (Builtin p2 b) args
    where
      getHeadMidTail :: forall a. [a] -> Maybe (a, Int, a)
      getHeadMidTail [] = Nothing
      getHeadMidTail (x : xs) = go 0 xs
        where
          go :: Int -> [a] -> Maybe (a, Int, a)
          go _ [] = Nothing
          go l [e] = Just (x, l, e)
          go l (_ : ys) = go (l + 1) ys

instance Simplify (Binder Name Builtin) where
  uninsert = fmap uninsert
  shortenVec = fmap shortenVec

instance Simplify (Arg Name Builtin) where
  uninsert = fmap uninsert
  shortenVec = fmap shortenVec

simplifyArgs :: NonEmpty (Arg Name Builtin) -> [Arg Name Builtin]
simplifyArgs = fmap uninsert . NonEmpty.filter (not . wasInserted)

wasInserted :: Arg var builtin -> Bool
wasInserted arg = case visibilityOf arg of
  Implicit True -> True
  Instance True -> True
  _ -> False

isLiteralCast :: Expr var Builtin -> Bool
isLiteralCast = \case
  Builtin _ b -> isCoercion b
  _ -> False
