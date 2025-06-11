module Vehicle.Compile.Simplify
  ( Simplify (..),
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (filter, reverse, toList)
import Data.Text qualified as Text
import Vehicle.Data.Builtin.Core
import Vehicle.Syntax.AST.Arg
import Vehicle.Syntax.AST.Expr
import Vehicle.Syntax.AST.Relevance (Relevance (..), setRelevance)
import Vehicle.Syntax.AST.Visibility (Visibility (..), visibilityOf)

-- | Note that these operations can be seen as undoing parts of the type-checking,
-- and therefore the resulting code is not guaranteed to be well-typed.
class Simplify a where
  -- | Removes automatically inserted arguments, binders and modalities for display to users.
  clean :: a -> a

  -- | Shortens vectors
  shortenVec :: a -> a

instance Simplify Prog where
  clean = fmap clean
  shortenVec = fmap shortenVec

instance Simplify Decl where
  clean = fmap clean
  shortenVec = fmap shortenVec

instance Simplify Expr where
  clean = mapApp $ \fun args -> do
    let fun' = clean fun
    -- Remove automatically inserted cast functions
    removeInsertedCasts fun' args

  shortenVec = mapApp $ \fun args ->
    case (fun, args) of
      (Builtin p (BuiltinFunction StackTensor), (argExpr -> (Builtin _ (BuiltinConstructor (NatLiteral n)))) :| _) ->
        case getHeadMidTail (drop (length args - n) $ NonEmpty.toList args) of
          Just (firstArg, numberOfMiddleArgs, lastArg)
            | numberOfMiddleArgs > 3 ->
                normAppList
                  fun
                  [ firstArg,
                    Arg p Explicit Relevant (Var p ("<" <> n2 <> " more>")),
                    lastArg
                  ]
            where
              n2 = Text.pack $ show numberOfMiddleArgs
          _ -> App fun args
      _ -> App fun args
    where
      getHeadMidTail :: forall a. [a] -> Maybe (a, Int, a)
      getHeadMidTail [] = Nothing
      getHeadMidTail (x : xs) = go 0 xs
        where
          go :: Int -> [a] -> Maybe (a, Int, a)
          go _ [] = Nothing
          go l [e] = Just (x, l, e)
          go l (_ : ys) = go (l + 1) ys

instance Simplify Binder where
  clean = fmap clean . setRelevance Relevant
  shortenVec = fmap shortenVec

instance Simplify Arg where
  clean = fmap clean . setRelevance Relevant
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

removeInsertedCasts :: Expr -> NonEmpty Arg -> Expr
removeInsertedCasts fun args
  | null args = fun
  | otherwise = case fun of
      Builtin p b -> case b of
        BuiltinCast FromNat {} -> simplifyAndGetLastArg fun args
        BuiltinCast FromRat {} -> simplifyAndGetLastArg fun args
        TypeClassOp FromNatTC {} -> simplifyAndGetLastArg fun args
        TypeClassOp FromRatTC {} -> simplifyAndGetLastArg fun args
        TypeClassOp TensorTypeTC -> normAppList (Builtin p (BuiltinType TensorType)) $ simplifyArgs args
        BuiltinFunction StackTensor -> normAppList (Builtin p (TypeClassOp VecLiteralTC)) $ simplifyArgs args
        BuiltinConstructor Cons -> delabList fun args
        BuiltinType TensorType -> delabTensorType $ simplifyArgs args
        _ -> normAppList fun $ simplifyArgs args
      _ -> normAppList fun $ simplifyArgs args

simplifyAndGetLastArg :: Expr -> NonEmpty Arg -> Expr
simplifyAndGetLastArg fun args = case simplifyArgs args of
  [] -> fun
  res -> argExpr $ last res

simplifyArgs :: NonEmpty Arg -> [Arg]
simplifyArgs = fmap clean . NonEmpty.filter (not . wasInserted)

wasInserted :: Arg -> Bool
wasInserted arg = case visibilityOf arg of
  Implicit True -> True
  Instance True -> True
  _ -> False

delabTensorType :: [Arg] -> Expr
delabTensorType = \case
  [Arg _ _ _ tElem, Arg _ _ _ dim] | isNil dim -> tElem
  args -> normAppList (Builtin mempty (BuiltinType TensorType)) args
  where
    isNil :: Expr -> Bool
    isNil = \case
      Builtin _ (BuiltinConstructor Nil) -> True
      App (Builtin _ (BuiltinConstructor Nil)) _ -> True
      _ -> False

delabList :: Expr -> NonEmpty Arg -> Expr
delabList fun args = case go (App fun args) of
  Nothing -> normAppList fun $ simplifyArgs args
  Just xs -> normAppList (Builtin mempty (TypeClassOp VecLiteralTC)) $ fmap clean xs
  where
    go :: Expr -> Maybe [Arg]
    go = \case
      Builtin _ (BuiltinConstructor Nil) -> Just []
      App (Builtin _ (BuiltinConstructor Nil)) _ -> Just []
      App (Builtin _ (BuiltinConstructor Cons)) (NonEmpty.reverse -> xs :| x : _) ->
        (x :) <$> go (argExpr xs)
      _ -> Nothing
