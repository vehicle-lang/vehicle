
module Vehicle.Core.Compile.Normalise where

import Control.Monad.Except (MonadError, ExceptT)
import Control.Monad.State (MonadState(..))
import Data.IntMap qualified as IntMap

import Vehicle.Prelude
import Vehicle.Core.AST

-- |Run a function in 'MonadNorm'.
runNorm :: a -> ExceptT NormError Logger a
runNorm = nf

--------------------------------------------------------------------------------
-- Setup

-- |Constraint for the monad stack used by the normaliser.
type MonadNorm m = (MonadError NormError m, MonadLogger m)

-- |Errors thrown during normalisation
newtype NormError
  = EmptyQuantifierDomain Provenance

pattern ETrue :: ann -> Expr var ann
pattern ETrue ann = Literal ann (LBool True)

pattern EFalse :: ann -> Expr var ann
pattern EFalse ann = Literal ann (LBool False)

pattern EInt :: ann -> Int -> Expr var ann
pattern EInt ann i = Literal ann (LInt i)

pattern EReal :: ann -> Double -> Expr var ann
pattern EReal ann d = Literal ann (LRat d)

--------------------------------------------------------------------------------
-- Normalisation algorithms

-- |Class for the various normalisation functions.
-- Invariant is that everything in the context is fully normalised
class Norm vf where
  nf :: MonadNorm m => vf -> m vf

instance Norm CheckedExpr where
  nf e = case e of
    Type{}     -> return e
    Hole{}     -> return e
    Literal{}  -> return e
    Builtin{}  -> return e
    Var{}      -> return e
    Meta{}     -> return e
    PrimDict{} -> return e

    Seq ann exprs     -> Seq ann <$> traverse nf exprs
    Ann ann expr typ  -> Ann ann <$> nf expr <*> nf typ
    Lam ann arg expr  -> Lam ann arg <$> nf expr

    Pi ann arg body -> Pi ann arg <$> nf body

    Let _ letValue _ letBody -> do
      normalisedLetValue <- nf letValue
      let letBodyWithSubstitution = substInto normalisedLetValue letBody
      nf letBodyWithSubstitution

    App ann fn arg -> do
      normalisedArg <- nf arg
      normalisedFn  <- nf fn
      normApp (App ann normalisedFn normalisedArg)

instance Norm CheckedDecl where
  nf = \case
    DeclNetw ann arg typ    -> DeclNetw ann arg <$> nf typ
    DeclData ann arg typ    -> DeclData ann arg <$> nf typ
    DefFun ann arg typ expr -> DefFun   ann arg <$> nf typ <*> nf expr

instance Norm CheckedProg where
  nf (Main decls)= Main <$> traverse nf decls

normApp :: MonadNorm m => CheckedExpr -> m CheckedExpr
normApp e = case decomposeApp e of
  (Lam _ _ funcBody, Arg _ _ arg : _) -> nf (substInto arg funcBody)
  (Builtin _ op, args) -> let ann = annotation e in case (op, map argExpr args) of
    -- Equality
    (Eq, (_, _, _, ETrue  _, e2))         -> return e2
    (Eq, (_, _, _, EFalse _, e2))         -> normApp $ _ --Op1 ENot e2 ann ann1 pos
    (Eq, (_, _, _, EInt  _ i, EInt  _ j)) -> return $ mkBool (i == j) ann
    (Eq, (_, _, _, EReal _ x, EReal _ y)) -> return $ mkBool (x == y) ann
    -- TODO implement reflexive rules?

    -- Not
    (Not, (_, _, ETrue  _)) -> return $ EFalse ann
    (Not, (_, _, EFalse _)) -> return $ ETrue  ann
    -- TODO implement idempotence rules?

    -- And
    (And, (_, _, ETrue  _  , e2))     -> return e2
    (And, (_, _, e1,       ETrue  _)) -> return e1
    (And, (_, _, EFalse _, _))        -> return $ EFalse ann
    (And, (_, _, _,        EFalse _)) -> return $ EFalse ann
    -- TODO implement associativity rules?

    -- Or
    (Or, (_, _, ETrue  _, _))        -> return $ ETrue ann
    (Or, (_, _, EFalse _,   e2))     -> return e2
    (Or, (_, _, _,        ETrue  _)) -> return $ ETrue ann
    (Or, (_, _, e1,       EFalse _)) -> return e1
    -- See https://github.com/wenkokke/vehicle/issues/2

    -- If
    (If, (_, _, ETrue  _, e2, _)) -> return e2
    (If, (_, _, EFalse _, _, e3)) -> return e3

    -- Le
    (Le, (_, _, _, EInt  _ i, EInt  _ j)) -> return $ mkBool (i <= j) ann
    (Le, (_, _, _, EReal _ x, EReal _ y)) -> return $ mkBool (x <= y) ann

    -- Lt
    (Lt, (_, _, _, EInt  _ i, EInt  _ j)) -> return $ mkBool (i < j) ann
    (Lt, (_, _, _, EReal _ x, EReal _ y)) -> return $ mkBool (x < y) ann

    -- Addition
    (Add, (_, _, EInt  _ i, EInt  _ j)) -> return $ EInt  ann (i + j)
    (Add, (_, _, EReal _ x, EReal _ y)) -> return $ EReal ann (x + y)
    -- TODO implement identity/associativity rules?

    -- Subtraction
    (Sub, (_, _, EInt  _ i, EInt  _ j)) -> return $ EInt  ann (i - j)
    (Sub, (_, _, EReal _ x, EReal _ y)) -> return $ EReal ann (x - y)
    -- TODO implement identity/associativity rules?

    -- Multiplication
    (Mul, (_, _, EInt  _ i, EInt  _ j)) -> return $ EInt  ann (i * j)
    (Mul, (_, _, EReal _ x, EReal _ y)) -> return $ EReal ann (x * y)
    -- TODO implement zero/identity/associativity rules?

    -- Division
    (Div, (_, _, EReal _ x, EReal _ y)) -> return $ EReal ann (x / y)

    -- Negation
    (Neg, (_, _, EReal _ x)) -> return $ EReal ann (- x)

    -- Cons
    (Cons, (_, _, _, e, Seq _ es)) -> return $ Seq ann (e : es)

    -- Lookup
    (At, (_, _, _, Seq _ es, EInt _ i)) -> return $ es !! fromIntegral i
    (At, (_, _, _, xs, i)) -> case (xs, i) of
      (Cons, (_, _, _, x, xs'), EInt _ 0) -> xs'
      (Cons, (_, _, _, x, xs'), EInt _ i) -> Op2 EAt es (EInt ann3 (i - 1)) ann ann1 ann2 pos
      _                                   -> return e

    -- Quantifier builtins
    (Quant q, (_, _, e1, e2)) -> normQuantifier q e1 e2 ann ann1 ann2 pos >>= norm

    -- Fall-through case
    _ -> return e
  _ -> return e

mkBool :: Bool -> CheckedAnn -> CheckedExpr
mkBool b ann = Literal ann (LBool b)