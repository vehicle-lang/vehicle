
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

pattern ENat :: ann -> Int -> Expr var ann
pattern ENat ann i = Literal ann (LNat i)

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
  logDebug ("norm-exit " <+> prettySimple new)
  return new

--------------------------------------------------------------------------------
-- Normalisation algorithms

-- |Class for the various normalisation functions.
-- Invariant is that everything in the context is fully normalised
class Norm vf where
  nf :: MonadNorm m => vf -> m vf

instance Norm CheckedProg where
  nf (Main decls)= Main <$> traverse nf decls

instance Norm CheckedDecl where
  nf = \case
    DeclNetw ann arg   typ      -> DeclNetw ann arg <$> nf typ
    DeclData ann arg   typ      -> DeclData ann arg <$> nf typ
    DefFun   ann ident typ expr -> do
      expr' <- nf expr
      modify (M.insert (deProv ident) expr')
      return $ DefFun ann ident typ expr'

instance Norm CheckedExpr where
  nf e = showExit e $ do
    e' <- showEntry e
    case e' of
      Type{}      -> return e
      Hole{}      -> return e
      Literal{}   -> return e
      Builtin{}   -> return e
      Meta{}      -> developerError "All metas should have been solved before normalisation"

      PrimDict tc         -> nf tc
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

      eApp@(App ann _ _) -> let (fun, args) = decomposeApp eApp in do
        nFun  <- nf fun
        nArgs <- traverse (\arg -> if vis arg == Explicit then nf arg else return arg) args
        normApp ann nFun nArgs

instance Norm CheckedArg where
  nf (Arg p Explicit e) = Arg p Explicit <$> nf e
  nf arg@Arg{}          = return arg

argHead :: CheckedArg -> CheckedExpr
argHead = fst . decomposeApp . argExpr

normApp :: MonadNorm m => CheckedAnn -> CheckedExpr -> [CheckedArg] -> m CheckedExpr
normApp _ann (Lam _ _ funcBody) (Arg _ _ arg : _) = nf (substInto arg funcBody)
normApp ann  fun@(Builtin _ op) args              = do
  let e = composeApp ann fun args
  case (op, args) of
    -- Equality
    (Eq, [_tElem, _tRes, _tc, arg1, arg2]) -> case (argHead arg1, argHead arg2) of
      --(EFalse _,  _)         -> normApp $ composeApp ann (Builtin ann op, [tElem, _, e2])
      (ENat  _ m, EInt  _ n) -> return  $ mkBool (m == n) ann
      (EInt  _ i, EInt  _ j) -> return  $ mkBool (i == j) ann
      (EReal _ x, EReal _ y) -> return  $ mkBool (x == y) ann
      _                      -> return e
    -- TODO implement reflexive rules?

    -- Inequality
    (Neq, [_t1, _t2, _tc, arg1, arg2]) -> case (argHead arg1, argHead arg2) of
      --(ETrue  _, _)          -> normApp $ composeApp ann (Builtin ann Not) [_t2, _, arg2]
      (EFalse _, _)          -> return $ argExpr arg2
      (ENat  _ m, ENat  _ n) -> return $ mkBool (m /= n) ann
      (EInt  _ i, EInt  _ j) -> return $ mkBool (i /= j) ann
      (EReal _ x, EReal _ y) -> return $ mkBool (x /= y) ann
      _                      -> return e

    -- Not
    (Not, [_t, _tc, arg1]) -> case argHead arg1 of
      ETrue  _ -> return $ mkBool False ann
      EFalse _ -> return $ mkBool True ann
      _        -> return e
    -- TODO implement idempotence rules?

    -- And
    (And, [_t, _tc, arg1, arg2]) -> case (argHead arg1, argHead arg2) of
      (ETrue  _, _)        -> return $ argExpr arg2
      (_,        ETrue  _) -> return $ argExpr arg1
      (EFalse _, _)        -> return $ mkBool False ann
      (_,        EFalse _) -> return $ mkBool False ann
      _        -> return e
    -- TODO implement associativity rules?

    -- Or
    (Or, [_t, _tc, arg1, arg2]) -> case (argHead arg1, argHead arg2) of
      (ETrue  _, _)        -> return $ mkBool True ann
      (EFalse _, _)        -> return $ argExpr arg2
      (_,        ETrue  _) -> return $ mkBool True ann
      (_,        EFalse _) -> return $ argExpr arg1
      _                    -> return e
    -- See https://github.com/wenkokke/vehicle/issues/2

    -- Implication
    (Impl, [t, tc, arg1, arg2]) -> case (argHead arg1, argHead arg2) of
      (ETrue  _, _)        -> return $ argExpr arg2
      (EFalse _, _)        -> return $ mkBool True ann
      (_,        ETrue  _) -> return $ mkBool True ann
      (_,        EFalse _) -> return $ composeApp ann (Builtin ann Not) [t, tc, arg2]
      _                    -> return e

    -- If
    (If, [_tRes, arg1, arg2, arg3]) -> case argHead arg1 of
      ETrue  _ -> return $ argExpr arg2
      EFalse _ -> return $ argExpr arg3
      _        -> return e

    -- Le
    (Le, [_t1, _t2, _tc, arg1, arg2]) -> case (argHead arg1, argHead arg2) of
      (EInt  _ i, EInt  _ j) -> return $ mkBool (i <= j) ann
      (EReal _ x, EReal _ y) -> return $ mkBool (x <= y) ann
      _                      -> return e

    -- Lt
    (Lt, [_t1, _t2, _tc, arg1, arg2]) -> case (argHead arg1, argHead arg2) of
      (EInt  _ i, EInt  _ j) -> return $ mkBool (i < j) ann
      (EReal _ x, EReal _ y) -> return $ mkBool (x < y) ann
      _                      -> return e

    -- Addition
    (Add, [_t, _tc, arg1, arg2]) -> case (argHead arg1, argHead arg2) of
      (ENat  _ m, ENat  _ n) -> return $ EInt  ann (m + n)
      (EInt  _ i, EInt  _ j) -> return $ EInt  ann (i + j)
      (EReal _ x, EReal _ y) -> return $ EReal ann (x + y)
      _                      -> return e
    -- TODO implement identity/associativity rules?

    -- Subtraction
    (Sub, [_t, _tc, arg1, arg2]) -> case (argHead arg1, argHead arg2) of
      (EInt  _ i, EInt  _ j) -> return $ EInt  ann (i - j)
      (EReal _ x, EReal _ y) -> return $ EReal ann (x - y)
      _                      -> return e
    -- TODO implement identity/associativity rules?

    -- Multiplication
    (Mul, [_t, _tc, arg1, arg2]) -> case (argHead arg1, argHead arg2) of
      (EInt  _ i, EInt  _ j) -> return $ EInt  ann (i * j)
      (EReal _ x, EReal _ y) -> return $ EReal ann (x * y)
      _                      -> return e
    -- TODO implement zero/identity/associativity rules?

    -- Division
    (Div, [_t, _tc, arg1, arg2]) -> case (argHead arg1, argHead arg2) of
      (EReal _ x, EReal _ y) -> return $ EReal ann (x / y)
      _                      -> return e

    -- Negation
    (Neg, [_t, _tc, arg]) -> case argHead arg of
      (EInt  _ x) -> return $ EInt  ann (- x)
      (EReal _ x) -> return $ EReal ann (- x)
      _           -> return e

    -- Cons
    (Cons, [_tCont, _tElem, _tc, item, cont]) -> case argHead cont of
      Seq _ xs -> return $ Seq ann (argExpr item : xs)
      _        -> return e

    -- Lookup
    (At, [_tCont, _tElem, _tc, cont, index]) -> case (argHead cont, argHead index) of
      (Seq _ es, EInt _ i) -> return $ es !! fromIntegral i
      _ -> return e --(xs      , i) -> case (decomposeExplicitApp xs, i) of
        --((Builtin _ Cons, [x, _]), EInt _ 0) -> return $ argExpr x
        --((Builtin _ Cons, [_, _, _, x, xs']), EInt _ i) -> Op2 EAt es (EInt ann3 (i - 1)) ann ann1 ann2 pos
        --_                                     -> return e

    -- Map
    (Map, [_tCont, _tElem, fn, cont]) -> case argHead cont of
        Seq _ xs -> Seq ann <$> traverse (nf . App ann (argExpr fn) . Arg ann Explicit) xs
        _        -> return e

    -- Fold
    (Fold, [_tCont, _tElem, _tRes, _tc, foldOp, unit, cont]) -> case argHead cont of
      Seq _ xs -> nf $ foldr (\x body -> App ann (App ann (argExpr foldOp) (Arg ann Explicit x)) (Arg ann Explicit body)) (argExpr unit) xs
      _        -> return e

    -- Quantifier builtins
    --(Quant q, [_, _, e1, e2]) -> normQuantifier q e1 e2 ann ann1 ann2 pos >>= norm

    -- Fall-through case
    _ -> return e

normApp ann fun args = return $ composeApp ann fun args

mkBool :: Bool -> CheckedAnn -> CheckedExpr
mkBool b ann = Literal ann (LBool b)
  --App ann (App ann (Literal ann (LBool b)) (Arg ann Implicit _)) (Arg ann Instance _)