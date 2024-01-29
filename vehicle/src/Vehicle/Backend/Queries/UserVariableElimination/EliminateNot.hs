module Vehicle.Backend.Queries.UserVariableElimination.EliminateNot
  ( eliminateNot,
  )
where

import Control.Monad.Writer
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Backend.Queries.UserVariableElimination.Unblocking (tryUnblockBool)
import Vehicle.Compile.Error (compilerDeveloperError)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.BuiltinInterface.Value
import Vehicle.Data.NormalisedExpr
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (StdNotBoolOp2))

--------------------------------------------------------------------------------
-- Not elimination

-- | Tries to push in a `Not` function as far as possible. Note that it will not
-- push it past a `Fold` application, and therefore these need to be tested for
-- and eliminated before applying this function.
eliminateNot ::
  forall m.
  (MonadQueryStructure m, MonadWriter [WHNFValue QueryBuiltin] m) =>
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
eliminateNot = go
  where
    go :: WHNFValue QueryBuiltin -> m (WHNFValue QueryBuiltin)
    go = \case
      ----------------
      -- Base cases --
      ----------------
      VNot x -> return x
      VBoolLiteral b -> return $ VBoolLiteral (not b)
      VOrder dom eq x y -> return $ VOrder dom (neg eq) x y
      VEqual dom x y -> return $ VNotEqual dom x y
      VNotEqual dom x y -> return $ VEqual dom x y
      -- We can't actually lower the `not` through the body of the quantifier as
      -- it is not yet unnormalised. However, it's fine to stop here as we'll
      -- simply continue to normalise it once we re-encounter it again after
      -- normalising the quantifier.
      VForall args binder env body -> return $ VExists args binder env (negExpr body)
      VExists args binder env body -> return $ VForall args binder env (negExpr body)
      -- It's not enough simply to negate the free variable, we also need
      -- to negate the instance argument solution for the function
      -- which is a function accepting two arguments and returning a bool.
      VVectorEqualFull spine -> VVectorNotEqualFull <$> negVectorEqSpine spine
      VVectorNotEqualFull spine -> VVectorEqualFull <$> negVectorEqSpine spine
      ---------------------
      -- Inductive cases --
      ---------------------
      VOr x y -> VAnd <$> go x <*> go y
      VAnd x y -> VOr <$> go x <*> go y
      VIf t c x y -> VIf t c <$> go x <*> go y
      expr -> tryUnblockBool expr go (throwUnexpectedlyBlockedError expr)

negVectorEqSpine :: (MonadQueryStructure m) => WHNFSpine QueryBuiltin -> m (WHNFSpine QueryBuiltin)
negVectorEqSpine (VVecEqSpine a b n fn x y) = do
  fn' <- appStdlibDef StdNotBoolOp2 [a, b, Arg mempty Explicit Relevant fn]
  return $ VVecEqSpine a b n fn' x y
negVectorEqSpine spine = compilerDeveloperError $ "Malformed equality spine" <+> prettyVerbose spine

throwUnexpectedlyBlockedError :: (MonadPropertyStructure m) => WHNFValue QueryBuiltin -> m a
throwUnexpectedlyBlockedError expr = do
  let exprDoc = prettyVerbose expr
  compilerDeveloperError ("Could not unblock blocked `not` on" <+> squotes exprDoc)