

module Vehicle.Compile.Normalise.DNF
  ( convertToDNF
  , splitConjunctions
  , splitDisjunctions
  , applyNotAndNormalise
  ) where

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Language.Print

--------------------------------------------------------------------------------
-- Or operations

-- | Converts an expression to disjunctive normal form.
-- Currently assumes all implications and negations have
-- been previously normalised out.
convertToDNF :: MonadCompile m => CheckedExpr -> m CheckedExpr
convertToDNF expr =
  logCompilerPass MinDetail "conversion to disjunctive normal form" $ do
    result <- dnf expr
    logCompilerPassOutput (prettyFriendly result)
    return result

dnf :: MonadCompile m => CheckedExpr -> m CheckedExpr
dnf expr = do
  showEntry expr
  result <- case expr of
    Literal{}   -> return expr
    Builtin{}   -> return expr
    Var{}       -> return expr

    LSeq{}      -> normalisationError currentPass "LSeq"
    Ann{}       -> normalisationError currentPass "Ann"
    Let{}       -> normalisationError currentPass "Let"
    Universe{}  -> typeError          currentPass "Universe"
    Pi{}        -> typeError          currentPass "Pi"
    PrimDict{}  -> visibilityError    currentPass "PrimDict"
    Hole{}      -> visibilityError    currentPass "Hole"
    Meta{}      -> resolutionError    currentPass "Meta"
    Lam{}       -> caseError          currentPass "Lam" ["QuantifierExpr"]

    -- Some sanity checks
    NotExpr{}     -> normalisationError currentPass "Not"
    ImpliesExpr{} -> normalisationError currentPass "Implies"

    QuantifierExpr ann t binder body -> do
      body' <- dnf body
      return $ liftOr (QuantifierExpr ann t binder) body'

    AndExpr ann [ExplicitArg ann1 e1, ExplicitArg ann2 e2] -> do
      e1' <- dnf e1
      e2' <- dnf e2
      return $
        liftOr (\e1'' ->
          liftOr (\e2'' ->
            AndExpr ann [ExplicitArg ann1 e1'', ExplicitArg ann2 e2'']) e2') e1'

    OrExpr ann [e1, e2] -> do
      e1' <- traverseArgExpr dnf e1
      e2' <- traverseArgExpr dnf e2
      return $ OrExpr ann [e1', e2']

    App{} -> return expr

  showExit result
  return result

liftOr :: (CheckedExpr -> CheckedExpr) -> CheckedExpr -> CheckedExpr
liftOr f (OrExpr ann [e1, e2]) = OrExpr ann (mapArgExpr (liftOr f) <$> [e1, e2])
liftOr f e                     = f e

splitConjunctions :: Expr binder var -> [Expr binder var]
splitConjunctions (AndExpr _ann [e1, e2]) =
  splitConjunctions (argExpr e1) <> splitConjunctions (argExpr e2)
splitConjunctions e = [e]

splitDisjunctions :: Expr binder var -> [Expr binder var]
splitDisjunctions (OrExpr _ann [e1, e2]) =
  splitDisjunctions (argExpr e1) <> splitDisjunctions (argExpr e2)
splitDisjunctions e = [e]

currentPass :: Doc a
currentPass = "conversion to DNF"

showEntry :: MonadLogger m => CheckedExpr -> m ()
showEntry e = do
  logDebug MaxDetail $ "dnf-entry" <+> prettySimple e
  incrCallDepth

showExit :: MonadLogger m => CheckedExpr -> m ()
showExit e = do
  decrCallDepth
  logDebug MaxDetail $ "dnf-exit " <+> prettySimple e


--------------------------------------------------------------------------------
-- Not operations

applyNotAndNormalise :: CheckedArg -> CheckedExpr
applyNotAndNormalise x = do
  let ann = provenanceOf x
  case nfNot ann x of
    Just r  -> r
    Nothing -> NotExpr ann [x]

notArg :: CheckedArg -> CheckedArg
notArg x = ExplicitArg (provenanceOf x) $ applyNotAndNormalise x

notExpr :: CheckedExpr -> CheckedExpr
notExpr x = applyNotAndNormalise (ExplicitArg (provenanceOf x) x)

nfNot :: Provenance
      -> CheckedArg
      -> Maybe CheckedExpr
nfNot p arg = case argExpr arg of
  BoolLiteralExpr    _ b           -> Just $ BoolLiteralExpr p (not b)
  OrderExpr      _ ord tElem args  -> Just $ OrderExpr p (neg ord) tElem args
  EqualityExpr   _ eq  tElem args  -> Just $ EqualityExpr p (neg eq)  tElem args
  ForallExpr _ binder body         -> Just $ ExistsExpr p binder $ notExpr body
  ExistsExpr _ binder body         -> Just $ ForallExpr p binder $ notExpr body
  ImpliesExpr        _ [e1, e2]    -> Just $ AndExpr p [e1, notArg e2]
  OrExpr             _ [e1, e2]    -> Just $ AndExpr p (notArg <$> [e1, e2])
  AndExpr            _ [e1, e2]    -> Just $ OrExpr p (notArg <$> [e1, e2])
  IfExpr _ tRes [c, e1, e2]        -> Just $ IfExpr p tRes [c, notArg e1, notArg e2]
  _ -> Nothing