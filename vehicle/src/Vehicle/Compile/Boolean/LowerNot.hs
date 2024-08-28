module Vehicle.Compile.Boolean.LowerNot
  ( lowerNot,
  )
where

import Vehicle.Compile.Context.Free
import Vehicle.Compile.Error (MonadCompile, compilerDeveloperError)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.Builtin.Standard ()
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (StdNotBoolOp2))
import Vehicle.Syntax.Builtin

--------------------------------------------------------------------------------
-- Not elimination

type MonadDropNot m =
  ( MonadCompile m,
    MonadFreeContext Builtin m
  )

-- | Tries to push in a `Not` as far as possible into a boolean expression.
-- If it is not possible to push it all the way through, it calls the continuation.
lowerNot ::
  forall m.
  (MonadDropNot m) =>
  (WHNFValue Builtin -> m (WHNFValue Builtin)) ->
  WHNFValue Builtin ->
  m (WHNFValue Builtin)
lowerNot whenBlocked = go
  where
    go :: WHNFValue Builtin -> m (WHNFValue Builtin)
    go = \case
      ----------------
      -- Base cases --
      ----------------
      INot x -> return x
      IBoolLiteral p b -> return $ IBoolLiteral p (not b)
      VBuiltinFunction (Order dom op) spine -> return $ VBuiltinFunction (Order dom (neg op)) spine
      VBuiltinFunction (Equals dom op) spine -> return $ VBuiltinFunction (Equals dom (neg op)) spine
      -- We can't actually lower the `not` through the body of the quantifier as
      -- it is not yet unnormalised. However, it's fine to stop here as we'll
      -- simply continue to normalise it once we re-encounter it again after
      -- normalising the quantifier.
      IForall args (VLam binder (WHNFClosure env body)) -> return $ IExists args (VLam binder (WHNFClosure env (INot body)))
      IExists args (VLam binder (WHNFClosure env body)) -> return $ IForall args (VLam binder (WHNFClosure env (INot body)))
      -- It's not enough simply to negate the free variable, we also need
      -- to negate the instance argument solution for the function
      -- which is a function accepting two arguments and returning a bool.
      IVectorEqualFull spine -> IVectorNotEqualFull <$> negVectorEqSpine spine
      IVectorNotEqualFull spine -> IVectorEqualFull <$> negVectorEqSpine spine
      ---------------------
      -- Inductive cases --
      ---------------------
      IOr x y -> IAnd <$> go x <*> go y
      IAnd x y -> IOr <$> go x <*> go y
      IIf t c x y -> IIf t c <$> go x <*> go y
      expr -> whenBlocked expr

negVectorEqSpine :: (MonadDropNot m) => WHNFSpine Builtin -> m (WHNFSpine Builtin)
negVectorEqSpine (IVecEqSpine a b n fn x y) = do
  fn' <- appHiddenStdlibDef StdNotBoolOp2 [a, b, fn]
  return $ IVecEqSpine a b n (Arg mempty Explicit Relevant fn') x y
negVectorEqSpine spine = compilerDeveloperError $ "Malformed equality spine" <+> prettyVerbose spine
