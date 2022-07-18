module Vehicle.Compile.Elaborate.Internal
  ( elab
  ) where

import Control.Monad.Except (MonadError(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (unpack)

import Vehicle.Prelude
import Vehicle.Internal.Abs as B
import Vehicle.Compile.Prelude qualified as V
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error

--------------------------------------------------------------------------------
-- Conversion from BNFC AST
--
-- We convert from the simple AST generated automatically by BNFC to our
-- more complicated internal version of the AST which allows us to annotate
-- terms with sort-dependent types.
--
-- While doing this, we
--
--   1) extract the positions from the tokens generated by BNFC and convert them
--   into `Provenance` annotations.
--
--   2) convert the builtin strings into `Builtin`s

class Elab vf vc where
  elab :: MonadCompile m => vf -> m vc

instance Elab B.Prog V.InputProg where
  elab (B.Main ds) = V.Main <$> traverse elab ds

instance Elab B.Decl V.InputDecl where
  elab = \case
    B.DeclNetw  n t   -> V.DefResource  (tkProvenance n) Network   <$> elab n <*> elab t
    B.DeclData  n t   -> V.DefResource  (tkProvenance n) Dataset   <$> elab n <*> elab t
    B.DeclParam n t   -> V.DefResource  (tkProvenance n) Parameter <$> elab n <*> elab t
    B.DefFun    n t e -> V.DefFunction  (tkProvenance n) Nothing   <$> elab n <*> elab t <*> elab e
    B.DeclPost  n t   -> V.DefPostulate (tkProvenance n)           <$> elab n <*> elab t

instance Elab B.Expr V.InputExpr where
  elab = \case
    B.Type l           -> return $ convType l
    B.Hole name        -> return $ V.Hole (tkProvenance name) (tkSymbol name)
    B.Ann term typ     -> op2 V.Ann  <$> elab term <*> elab typ
    B.Pi  binder expr  -> op2 V.Pi   <$> elab binder <*> elab expr;
    B.Lam binder e     -> op2 V.Lam  <$> elab binder <*> elab e
    B.Let binder e1 e2 -> op3 V.Let  <$> elab e1 <*> elab binder <*>  elab e2
    B.LSeq es          -> op1 V.LSeq <$> traverse elab es
    B.Builtin c        -> V.Builtin (mkAnn c) <$> lookupBuiltin c
    B.Var n            -> return $ V.Var (mkAnn n) (tkSymbol n)
    B.Literal v        -> V.Literal mempty <$> elab v

    B.App fun arg -> do
      fun' <- elab fun
      arg' <- elab arg
      let p = fillInProvenance [provenanceOf fun', provenanceOf arg']
      return $ normApp p fun' (arg' :| [])

instance Elab B.Binder V.InputBinder where
  elab = \case
    B.ExplicitBinder n e -> mkBinder n Explicit  e
    B.ImplicitBinder n e -> mkBinder n Implicit  e
    B.InstanceBinder n e -> mkBinder n Instance  e
    where
      mkBinder :: MonadCompile m => B.NameToken -> Visibility -> B.Expr -> m V.InputBinder
      mkBinder n v e = V.Binder (mkAnn n) v (Just (tkSymbol n)) <$> elab e

instance Elab B.Arg V.InputArg where
  elab = \case
    B.ExplicitArg e -> mkArg Explicit <$> elab e
    B.ImplicitArg e -> mkArg Implicit <$> elab e
    B.InstanceArg e -> mkArg Instance <$> elab e
    where
      mkArg :: Visibility -> V.InputExpr -> V.InputArg
      mkArg v e = V.Arg (expandByArgVisibility v (provenanceOf e)) v e

instance Elab B.Lit Literal where
  elab = \case
    B.LitBool b -> return $ LBool (read (unpack $ tkSymbol b))
    B.LitRat  r -> return $ LRat  (readRat (tkSymbol r))
    B.LitNat  n -> return $ LNat  (readNat (tkSymbol n))

instance Elab B.NameToken Identifier where
  elab n = return $ Identifier $ tkSymbol n

lookupBuiltin :: MonadCompile m => B.BuiltinToken -> m V.Builtin
lookupBuiltin (BuiltinToken tk) = case builtinFromSymbol (tkSymbol tk) of
    Nothing -> throwError $ UnknownBuiltin $ toToken tk
    Just v  -> return v

mkAnn :: IsToken a => a -> V.Provenance
mkAnn = tkProvenance

op1 :: (HasProvenance a)
    => (V.Provenance -> a -> b)
    -> a -> b
op1 mk t = mk (provenanceOf t) t

op2 :: (HasProvenance a, HasProvenance b)
    => (V.Provenance -> a -> b -> c)
    -> a -> b -> c
op2 mk t1 t2 = mk (provenanceOf t1 <> provenanceOf t2) t1 t2

op3 :: (HasProvenance a, HasProvenance b, HasProvenance c)
    => (V.Provenance -> a -> b -> c -> d)
    -> a -> b -> c -> d
op3 mk t1 t2 t3 = mk (provenanceOf t1 <> provenanceOf t2 <> provenanceOf t3) t1 t2 t3

-- | Elabs the type token into a Type expression.
-- Doesn't run in the monad as if something goes wrong with this, we've got
-- the grammar wrong.
convType :: TypeToken -> V.InputExpr
convType tk = case unpack (tkSymbol tk) of
  ('T':'y':'p':'e':l) -> V.TypeUniverse (mkAnn tk) (read l)
  t                   -> developerError $ "Malformed type token" <+> pretty t