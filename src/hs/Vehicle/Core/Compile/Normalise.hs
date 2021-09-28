
module Vehicle.Core.Compile.Normalise
  ( NormError
  , normalise
  ) where

import Control.Monad (when)
import Control.Monad.State (MonadState(..), evalStateT, gets, modify)
import Control.Monad.Except (MonadError, ExceptT)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)

import Vehicle.Prelude
import Vehicle.Core.AST
import Vehicle.Core.Print (prettySimple)

-- |Run a function in 'MonadNorm'.
normalise :: Norm a => a -> ExceptT NormError Logger a
normalise x = evalStateT (nf x) mempty

--------------------------------------------------------------------------------
-- Setup

type DeclCtx = M.Map Identifier CheckedExpr

-- |Constraint for the monad stack used by the normaliser.
type MonadNorm m =
  ( MonadError NormError m
  , MonadLogger m
  , MonadState DeclCtx m
  )

-- |Errors thrown during normalisation
newtype NormError
  = EmptyQuantifierDomain Provenance

instance MeaningfulError NormError where
  details (EmptyQuantifierDomain p) = UError $ UserError
    { problem    = "Quantifying over an empty domain"
    , provenance = p
    , fix        = "Check your definition of the domain"
    }

pattern ETrue :: ann -> Expr var ann
pattern ETrue ann = Literal ann (LBool True)

pattern EFalse :: ann -> Expr var ann
pattern EFalse ann = Literal ann (LBool False)

pattern EInt :: ann -> Int -> Expr var ann
pattern EInt ann i = Literal ann (LInt i)

pattern EReal :: ann -> Double -> Expr var ann
pattern EReal ann d = Literal ann (LRat d)

--------------------------------------------------------------------------------
-- Debug functions

showEntry :: MonadNorm m => CheckedExpr -> m CheckedExpr
showEntry e = do
  logDebug ("norm-entry " <> prettySimple e)
  incrCallDepth
  return e

showExit :: MonadNorm m => CheckedExpr -> m CheckedExpr -> m CheckedExpr
showExit old mNew = do
  new <- mNew
  decrCallDepth
  when (old /= new) $
    logDebug ("normalising" <+> prettySimple old)
  logDebug ("norm-exit" <+> prettySimple new)
  return new

--------------------------------------------------------------------------------
-- Normalisation algorithms

-- |Class for the various normalisation functions.
-- Invariant is that everything in the context is fully normalised
class Norm vf where
  nf :: MonadNorm m => vf -> m vf

instance Norm CheckedExpr where
  nf e = showExit e $ do
    e' <- showEntry e
    case e' of
      Type{}      -> return e
      Hole{}      -> return e
      Literal{}   -> return e
      Builtin{}   -> return e
      Meta{}      -> return e
      PrimDict tc -> nf tc

      Seq ann exprs       -> Seq ann <$> traverse nf exprs
      Lam ann binder expr -> Lam ann binder <$> nf expr
      Pi ann binder body  -> Pi ann binder <$> nf body

      Ann _ann expr _typ  -> nf expr

      Var _ (Bound _)     -> return e
      Var _ (Free ident)  -> gets (fromMaybe e . M.lookup ident)

      Let _ letValue _ letBody -> do
        normalisedLetValue <- nf letValue
        let letBodyWithSubstitution = substInto normalisedLetValue letBody
        nf letBodyWithSubstitution

      App ann fn arg -> do
        normalisedArg <- nf arg
        normalisedFn  <- nf fn
        normApp (App ann normalisedFn normalisedArg)

instance Norm CheckedArg where
  nf (Arg p Explicit e) = Arg p Explicit <$> nf e
  nf arg@Arg{}          = return arg

instance Norm CheckedDecl where
  nf = \case
    DeclNetw ann arg typ    -> DeclNetw ann arg <$> nf typ
    DeclData ann arg typ    -> DeclData ann arg <$> nf typ
    DefFun ann ident typ expr -> do
      expr' <- nf expr
      modify (M.insert (deProv ident) expr')
      return $ DefFun ann ident typ expr'

instance Norm CheckedProg where
  nf (Main decls)= Main <$> traverse nf decls

normApp :: MonadNorm m => CheckedExpr -> m CheckedExpr
normApp e = case decomposeExplicitApp e of
  (Lam _ _ funcBody, Arg _ _ arg : _) -> nf (substInto arg funcBody)
  (Builtin _ op, args) -> let ann = annotation e in case (op, map argExpr args) of
    -- Equality
    (Eq, [ETrue  _, e2])         -> return e2
    --(Eq, [EFalse _, e2])         -> normApp $ _ --Op1 ENot e2 ann ann1 pos
    (Eq, [EInt  _ i, EInt  _ j]) -> return $ mkBool (i == j) ann
    (Eq, [EReal _ x, EReal _ y]) -> return $ mkBool (x == y) ann
    -- TODO implement reflexive rules?

    -- Not
    (Not, [ETrue  _]) -> return $ mkBool False ann
    (Not, [EFalse _]) -> return $ mkBool True ann
    -- TODO implement idempotence rules?

    -- And
    (And, [ETrue  _  , e2])     -> return e2
    (And, [e1,       ETrue  _]) -> return e1
    (And, [EFalse _, _])        -> return $ EFalse ann
    (And, [_,        EFalse _]) -> return $ EFalse ann
    -- TODO implement associativity rules?

    -- Or
    (Or, [ETrue  _, _])        -> return $ ETrue ann
    (Or, [EFalse _,   e2])     -> return e2
    (Or, [_,        ETrue  _]) -> return $ ETrue ann
    (Or, [e1,       EFalse _]) -> return e1
    -- See https://github.com/wenkokke/vehicle/issues/2

    -- If
    (If, [ETrue  _, e2, _]) -> return e2
    (If, [EFalse _, _, e3]) -> return e3

    -- Le
    (Le, [EInt  _ i, EInt  _ j]) -> return $ mkBool (i <= j) ann
    (Le, [EReal _ x, EReal _ y]) -> return $ mkBool (x <= y) ann

    -- Lt
    (Lt, [EInt  _ i, EInt  _ j]) -> return $ mkBool (i < j) ann
    (Lt, [EReal _ x, EReal _ y]) -> return $ mkBool (x < y) ann

    -- Addition
    (Add, [EInt  _ i, EInt  _ j]) -> return $ EInt  ann (i + j)
    (Add, [EReal _ x, EReal _ y]) -> return $ EReal ann (x + y)
    -- TODO implement identity/associativity rules?

    -- Subtraction
    (Sub, [EInt  _ i, EInt  _ j]) -> return $ EInt  ann (i - j)
    (Sub, [EReal _ x, EReal _ y]) -> return $ EReal ann (x - y)
    -- TODO implement identity/associativity rules?

    -- Multiplication
    (Mul, [EInt  _ i, EInt  _ j]) -> return $ EInt  ann (i * j)
    (Mul, [EReal _ x, EReal _ y]) -> return $ EReal ann (x * y)
    -- TODO implement zero/identity/associativity rules?

    -- Division
    (Div, [EReal _ x, EReal _ y]) -> return $ EReal ann (x / y)

    -- Negation
    (Neg, [EReal _ x]) -> return $ EReal ann (- x)

    -- Cons
    (Cons, [x, Seq _ xs]) -> return $ Seq ann (x : xs)

    -- Lookup
    (At, [Seq _ es, EInt _ i]) -> return $ es !! fromIntegral i
    (At, [xs, i]) -> case (decomposeExplicitApp xs, i) of
      ((Builtin _ Cons, [x, _]), EInt _ 0) -> return $ argExpr x
      --((Builtin _ Cons, [_, _, _, x, xs']), EInt _ i) -> Op2 EAt es (EInt ann3 (i - 1)) ann ann1 ann2 pos
      _                                     -> return e

    -- Map
    (Map, [f , Seq _ xs]) ->
      Seq ann <$> traverse (nf . App ann f . Arg ann Explicit) xs

    -- Fold
    (Fold, [fop, bc, Seq _ xs]) ->
      nf $ foldr (\x body -> App ann (App ann fop (Arg ann Explicit x)) (Arg ann Explicit body)) bc xs

    -- Quantifier builtins
    --(Quant q, [_, _, e1, e2]) -> normQuantifier q e1 e2 ann ann1 ann2 pos >>= norm

    -- Fall-through case
    _ -> return e
  _ -> return e

mkBool :: Bool -> CheckedAnn -> CheckedExpr
mkBool b ann = Literal ann (LBool b)
  --App ann (App ann (Literal ann (LBool b)) (Arg ann Implicit _)) (Arg ann Constraint _)