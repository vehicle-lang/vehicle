module Vehicle.Backend.Queries.UserVariableElimination.EliminateNot
  ( eliminateNot,
  )
where

import Control.Monad.Writer
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Backend.Queries.UserVariableElimination.Unblocking (unblockBoolExpr)
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Error (compilerDeveloperError)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.BuiltinInterface.Expr
import Vehicle.Data.BuiltinInterface.Value
import Vehicle.Data.NormalisedExpr
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (StdNotBoolOp2))
import Vehicle.Syntax.Builtin

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
      VBuiltinFunction (Order dom op) spine -> return $ VBuiltinFunction (Order dom (neg op)) spine
      VBuiltinFunction (Equals dom op) spine -> return $ VBuiltinFunction (Equals dom (neg op)) spine
      -- We can't actually lower the `not` through the body of the quantifier as
      -- it is not yet unnormalised. However, it's fine to stop here as we'll
      -- simply continue to normalise it once we re-encounter it again after
      -- normalising the quantifier.
      VForall args binder (WHNFBody env body) -> return $ VExists args binder (WHNFBody env (negExpr body))
      VExists args binder (WHNFBody env body) -> return $ VForall args binder (WHNFBody env (negExpr body))
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
      expr -> go =<< unblockBoolExpr expr

negVectorEqSpine :: (MonadQueryStructure m) => WHNFSpine QueryBuiltin -> m (WHNFSpine QueryBuiltin)
negVectorEqSpine (VVecEqSpine a b n fn x y) = do
  fn' <- appHiddenStdlibDef StdNotBoolOp2 [a, b, fn]
  return $ VVecEqSpine a b n (Arg mempty Explicit Relevant fn') x y
negVectorEqSpine spine = compilerDeveloperError $ "Malformed equality spine" <+> prettyVerbose spine
