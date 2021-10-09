{-# LANGUAGE OverloadedLists #-}

module Vehicle.Core.Compile.Normalise
  ( NormError
  , normalise
  ) where

import Control.Exception (assert)
import Control.Monad (when)
import Control.Monad.State (MonadState(..), evalStateT, gets, modify)
import Control.Monad.Except (MonadError, ExceptT)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.List.Split (chunksOf)

import Vehicle.Prelude
import Vehicle.Core.AST
import Vehicle.Core.Print

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

exprHead :: CheckedExpr -> CheckedExpr
exprHead = fst . toHead

argHead :: CheckedArg -> CheckedExpr
argHead = exprHead . argExpr


mkBool :: Bool -> CheckedAnn -> CheckedExpr
mkBool b ann = Literal ann (LBool b)
  -- TODO deeply dodgy, doesn't recreate implicit and instance arguments
  --App ann (App ann (Literal ann (LBool b)) (Arg ann Implicit _)) (Arg ann Instance _)

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
-- Normalisation algorithm

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

      eApp@(App ann _ _) -> let (fun, args) = toHead eApp in do
        nFun  <- nf fun
        -- TODO temporary hack please remove in future once we actually have computational
        -- behaviour in implicit/instance arguments
        nArgs <- traverse (\arg -> if vis arg == Explicit then nf arg else return arg) args
        nfApp ann nFun nArgs

instance Norm CheckedArg where
  nf (Arg p Explicit e) = Arg p Explicit <$> nf e
  nf arg@Arg{}          = return arg

--------------------------------------------------------------------------------
-- Application

nfApp :: MonadNorm m => CheckedAnn -> CheckedExpr -> [CheckedArg] -> m CheckedExpr
nfApp _ann (Lam _ _ funcBody) (Arg _ _ arg : _) = nf (substInto arg funcBody)
nfApp ann  fun@(Builtin _ op) args              = do
  let e = normAppList ann fun args
  case (op, args) of
    -- Equality
    (Eq, [_tElem, _tRes, _tc, arg1, arg2]) -> case (argHead arg1, argHead arg2) of
      --(EFalse _,  _)         -> normApp $ composeApp ann (Builtin ann op, [tElem, _, e2])
      (ENat  _ m, EInt  _ n) -> return $ mkBool (m == n) ann
      (EInt  _ i, EInt  _ j) -> return $ mkBool (i == j) ann
      (EReal _ x, EReal _ y) -> return $ mkBool (x == y) ann
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
      _                    -> return e
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
      (_,        EFalse _) -> return $ App ann (Builtin ann Not) [t, tc, arg2]
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
        Seq _ xs -> Seq ann <$> traverse (\x -> nf $ App ann (argExpr fn) [Arg ann Explicit x]) xs
        _        -> return e
    -- TODO distribute over cons

    -- Fold
    (Fold, [_tCont, _tElem, _tRes, _tc, foldOp, unit, cont]) -> case argHead cont of
      Seq _ xs -> nf $ foldr (\x body -> App ann (argExpr foldOp) [Arg ann Explicit x, Arg ann Explicit body]) (argExpr unit) xs
      _        -> return e
    -- TODO distribute over cons

    -- Quantifiers
    (Quant q, [_tElem, lam]) -> return $ fromMaybe e (nfQuantifier ann q lam)

    -- Fall-through case
    _ -> return e

nfApp ann fun args = return $ normAppList ann fun args

--------------------------------------------------------------------------------
-- Normalising quantification over types

nfQuantifier :: CheckedAnn
             -> Quantifier
             -> CheckedArg -- The quantified lambda expression
             -> Maybe CheckedExpr
nfQuantifier ann q lam = case argHead lam of
  Lam _ann (Binder _p _ n t) body -> case toHead t of
    -- If we have a tensor instead quantify over each individual element, and then substitute
    -- in a Seq construct with those elements in.
    (Builtin _ Tensor, [tElemArg, tDimsArg]) -> do
      let tElem = argExpr tElemArg
      let tDims = argHead tDimsArg

      -- Calculate the dimensions of the tensor
      dims <- getDimensions tDims
      let tensorSize = product dims

      -- Use the list monad to create a nested list of all possible indices into the tensor
      let allIndices = traverse (\dim -> [0..dim-1]) dims
      -- Generate the corresponding names from the indices
      let allNames   = map (makeNameWithIndices n) allIndices

      -- Generate a list of variables, one for each index
      let allExprs   = map (\i -> Var ann (Bound i)) (reverse [0..tensorSize-1])
      -- Construct the corresponding nested tensor expression
      let tensor     = makeTensor ann dims allExprs
      -- We're introducing `tensorSize` new binder so lift the indices in the body accordingly
      let body1      = liftDBIndices tensorSize body
      -- Substitute throught the tensor expression for the old top-level binder
      let body2      = substIntoAtLevel tensorSize tensor body1

      -- Generate a expression with `tensorSize` quantifiers
      return $ foldl (makeQuantifier ann q tElem) body2 allNames
    _ -> Nothing
  _ -> Nothing

makeQuantifier :: CheckedAnn -> Quantifier -> CheckedExpr -> CheckedExpr -> Name -> CheckedExpr
makeQuantifier ann q tElem body name =
  App ann (Builtin ann (Quant q))
    [Arg ann Explicit (Lam ann (Binder ann Explicit name tElem) body)]

makeTensor :: CheckedAnn -> [Int] -> [CheckedExpr] -> CheckedExpr
makeTensor ann dims exprs = assert (product dims == length exprs) (go dims exprs)
  where
    go []       [] = Seq ann []
    go [_]      es = Seq ann es
    go (_ : ds) es = Seq ann (map (go ds) (chunksOf (product ds) es))
    go []  (_ : _) = developerError "Found inhabitants of the empty dimension! Woo!"

-- | Generates a name for a variable based on the indices, e.g. x [1,2,3] -> x_1_2_3
makeNameWithIndices :: Name -> [Int] -> Name
makeNameWithIndices Machine  _       = Machine
makeNameWithIndices (User n) indices = User $
  mconcat (n : ["_" <> pack (show index) | index <- indices])

getDimensions :: CheckedExpr -> Maybe [Int]
getDimensions (Seq _ xs) = traverse getDimension xs
getDimensions _          = Nothing

getDimension :: CheckedExpr -> Maybe Int
getDimension e = case exprHead e of
  (Literal _ (LNat i)) -> Just i
  _                    -> Nothing

{-
-- |Elaborate quantification over the members of a container type.
-- Expands e.g. `every x in list . y` to `fold and true (map (\x -> y) list)`
quantIn :: MonadElab m => VC.InputAnn -> Quantifier -> VF.InputBinder -> VF.InputExpr -> VF.InputExpr -> m VC.InputExpr
quantIn ann quantifier n container body = do
  let (bop, unit) = quantImplementation quantifier
  let lam = VF.Lam ann (n :| []) body
  mappedContainer <- op VC.Map ann [lam, container]
  return $ opC VC.Fold ann [VC.Builtin ann bop, VC.Literal ann unit, mappedContainer]

quantImplementation :: Quantifier -> (VC.Builtin, Literal)
quantImplementation All = (VC.And, LBool True)
quantImplementation Any = (VC.Or,  LBool False)
-}