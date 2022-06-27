{-# LANGUAGE ViewPatterns #-}

module Vehicle.Compile.Type.TypeClass
  ( solveTypeClassConstraint
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (catMaybes, mapMaybe)
import Control.Monad (unless, MonadPlus(..), forM)
import Control.Monad.Except ( throwError )

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
import Vehicle.Language.Print (prettyVerbose)
import Data.Foldable (Foldable(fold))

--------------------------------------------------------------------------------
-- Public interface

solveTypeClassConstraint :: MonadMeta m
                         => ConstraintContext
                         -> TypeClassConstraint
                         -> m ConstraintProgress
solveTypeClassConstraint ctx c@(m `Has` tcExpr@(BuiltinTypeClass ann tc args)) = do
  progress <- solve tc (TC ctx c) (onlyExplicit args)

  unless (isStuck progress) $ do
    metaSolved m (PrimDict ann tcExpr)

  return progress

solveTypeClassConstraint _ (_ `Has` e) =
  compilerDeveloperError $ "Unknown type-class application" <+> squotes (prettyVerbose e)


solve :: MonadMeta m
      => TypeClass
      -> Constraint
      -> [CheckedExpr]
      -> m ConstraintProgress
solve = \case
  HasEq eq            -> solveHasEq eq
  HasOrd ord          -> solveHasOrd ord
  HasNot              -> solveHasNot
  HasAnd              -> solveHasAnd
  HasOr               -> solveHasOr
  HasImpl             -> solveHasImpl
  HasQuantifier q     -> solveHasQuantifier q
  HasNeg              -> solveHasNeg
  HasAdd              -> solveHasAdd
  HasSub              -> solveHasSub
  HasMul              -> solveHasMul
  HasDiv              -> solveHasDiv
  HasFold             -> solveHasFold
  HasQuantifierIn q   -> solveHasQuantifierIn q

  HasNatLitsUpTo n    -> solveHasNatLits n
  HasIntLits          -> solveHasIntLits
  HasRatLits          -> solveHasRatLits
  HasConLitsOfSize n  -> solveHasConLits n

  MaxLinearity -> solveMaxLinearity
  MulLinearity -> solveMulLinearity

  NegPolarity   -> solveNegPolarity
  AddPolarity q -> solveAddPolarity q
  EqPolarity eq -> solveEqPolarity eq
  ImplPolarity  -> solveImplPolarity
  MaxPolarity   -> solveMaxPolarity

  TypesEqualModAuxiliaryAnnotations -> solveTypesEqual

--------------------------------------------------------------------------------
-- HasEq

solveHasEq :: MonadMeta m
           => Equality
           -> Constraint
           -> [CheckedExpr]
           -> m ConstraintProgress
solveHasEq eq c [arg1, arg2, res]
  | allOf [arg1, arg2] isMeta =
    extractAndBlockOnMetas [arg1, arg2]

  | anyOf [arg1, arg2] isNatType   ||
    anyOf [arg1, arg2] isIntType   ||
    anyOf [arg1, arg2] isIndexType = do
      let resConst = unify c res (boolType p Constant Unquantified)
      let argsEq   = unify c arg1 arg2
      return $ argsEq <> resConst

  | anyOf [arg1, arg2] isAnnRatType = do
    (arg1Eq, arg1Lin)       <- unifyWithAnnRatType  c arg1
    (arg2Eq, arg2Lin)       <- unifyWithAnnRatType  c arg2
    (resEq, resLin, resPol) <- unifyWithAnnBoolType c res

    -- The resulting linearity is the max of the linearity of the arguments.
    linTC <- createTC c MaxLinearity [arg1Lin, arg2Lin, resLin]
    -- The polarity is unquantified.
    let polEq = unify c resPol (Pol p Unquantified)

    return $ arg1Eq <> arg2Eq <> resEq <> linTC <> polEq

  | anyOf [arg1, arg2] isAnnBoolType =
    checkBoolTypesEqual MaxLinearity (EqPolarity eq) c res [arg1, arg2]

  | anyOf [arg1, arg2] isListType  = do
    (arg1Eq, tElem1) <- unifyWithListType c arg1
    (arg2Eq, tElem2) <- unifyWithListType c arg2

    -- Recursively check that the element types have equality.
    recEq <- createTC c (HasEq eq) [tElem1, tElem2, res]

    return $ arg1Eq <> arg2Eq <> recEq

  | anyOf [arg1, arg2] isTensorType = do
    dims <- freshDimsMeta c
    (arg1Eq, tElem1) <- unifyWithTensorType c dims arg1
    (arg2Eq, tElem2) <- unifyWithTensorType c dims arg2

    -- Recursively check that the element types have equality.
    recEq <- createTC c (HasEq eq) [tElem1, tElem2, res]

    return $ arg1Eq <> arg2Eq <> recEq

  | otherwise = throwError $ head $
    unless2 (isMeta arg1)
      (FailedBuiltinConstraintArgument ctx (Equality eq) arg1 allowedTypes 1 2) <>
    unless2 (isMeta arg2)
      (FailedBuiltinConstraintArgument ctx (Equality eq) arg2 allowedTypes 2 2)
  where
    p = provenanceOf c
    ctx = constraintContext c
    allowedTypes = allowed $
      Bool : [Index] <>
      (NumericType <$> [Nat, Int, Rat]) <>
      (ContainerType <$> [List, Tensor])

solveHasEq _ c _ = malformedConstraint c

--------------------------------------------------------------------------------
-- HasOrd

solveHasOrd :: MonadMeta m
            => Order
            -> Constraint
            -> [CheckedExpr]
            -> m ConstraintProgress
solveHasOrd ord c [arg1, arg2, res]
  | allOf [arg1, arg2] isMeta =
    extractAndBlockOnMetas [arg1, arg2]

  | anyOf [arg1, arg2] isNatType   ||
    anyOf [arg1, arg2] isIntType   ||
    anyOf [arg1, arg2] isIndexType = do
      let resConst = unify c res (boolType p Constant Unquantified)
      let argsEq   = unify c arg1 arg2
      return $ argsEq <> resConst

  | anyOf [arg1, arg2] isAnnRatType = do
    (arg1Eq, arg1Lin) <- unifyWithAnnRatType c arg1
    (arg2Eq, arg2Lin) <- unifyWithAnnRatType c arg2
    (resEq, resLin, resPol) <- unifyWithAnnBoolType c res

    -- The resulting linearity is the max of the linearity of the arguments.
    linTC <- createTC c MaxLinearity [arg1Lin, arg2Lin, resLin]
    -- The resulting polarity is unquantified.
    let polTC = unify c resPol (Pol p Unquantified)

    return $ arg1Eq <> arg2Eq <> resEq <> linTC <> polTC

  | let dims = getTensorTypeDimensions [arg1, arg2],
    anyOf [arg1, arg2] isTensorType && anyOf dims isMeta = do
    extractAndBlockOnMetas dims

  | otherwise = throwError $ head $
    unless2 (isMeta arg1)
      (FailedBuiltinConstraintArgument ctx (Order ord) arg1 allowedTypes 1 2) <>
    unless2 (isMeta arg2)
      (FailedBuiltinConstraintArgument ctx (Order ord) arg2 allowedTypes 2 2)
  where
    ctx = constraintContext c
    p = provenanceOf ctx
    allowedTypes = allowed (Index : (NumericType <$> [Nat, Int, Rat]))

solveHasOrd _ c _ = malformedConstraint c

--------------------------------------------------------------------------------
-- HasNot

solveHasNot :: MonadMeta m
            => Constraint
            -> [CheckedExpr]
            -> m ConstraintProgress
solveHasNot c [arg, res] = do
  (argEq, argLin, argPol) <- unifyWithAnnBoolType c arg
  (resEq, resLin, resPol) <- unifyWithAnnBoolType c res

  let linEq = unify c argLin resLin
  polTC <- createTC c NegPolarity [argPol, resPol]

  return $ argEq <> resEq <> linEq <> polTC

solveHasNot c _ = malformedConstraint c

--------------------------------------------------------------------------------
-- HasAndOr

solveHasBoolOp2 :: MonadMeta m
                => BooleanOp2
                -> TypeClass
                -> Constraint
                -> [CheckedExpr]
                -> m ConstraintProgress
solveHasBoolOp2 _op2 polConstraint c [arg1, arg2, res] =
  checkBoolTypesEqual MaxLinearity polConstraint c res [arg1, arg2]
solveHasBoolOp2 _ _ c _ = malformedConstraint c

solveHasAnd :: MonadMeta m
            => Constraint
            -> [CheckedExpr]
            -> m ConstraintProgress
solveHasAnd = solveHasBoolOp2 And MaxPolarity

solveHasOr :: MonadMeta m
            => Constraint
            -> [CheckedExpr]
            -> m ConstraintProgress
solveHasOr = solveHasBoolOp2 Or MaxPolarity

solveHasImpl :: MonadMeta m
             => Constraint
             -> [CheckedExpr]
             -> m ConstraintProgress
solveHasImpl = solveHasBoolOp2 Impl ImplPolarity

--------------------------------------------------------------------------------
-- HasQuantifier

solveHasQuantifier :: forall m . MonadMeta m
                   => Quantifier
                   -> Constraint
                   -> [CheckedExpr]
                   -> m ConstraintProgress
solveHasQuantifier q c [domain, body, res]
  | isMeta domain =
    extractAndBlockOnMetas [domain]

  | isIndexType domain || isIntType domain || isNatType domain = do
    -- The body and the result types are unified with the same polarity
    resPol <- freshPolarityMeta p
    unifyBodyAndResTypes resPol resPol

  | otherwise = case domain of

    AnnRatType _ domainLin -> do
      -- The rational being quantified over is, by definition, linear
      let linProv = QuantifiedVariableProvenance p
      let domainLinEq = unify c domainLin (Lin p (Linear linProv))

      -- The quantifier is added to the polarity
      bodyPol <- freshPolarityMeta p
      resPol  <- freshPolarityMeta p
      polTC <- createTC c (AddPolarity q) [bodyPol, resPol]

      -- The body and the result types are unified (modulo the polarity)
      resEq <- unifyBodyAndResTypes bodyPol resPol

      return $ domainLinEq <> polTC <> resEq

    AnnBoolType _ domainLin domainPol -> do
      -- If we're quantifying over a Bool then it itself is linear and unquantified
      let linEq = unify c domainLin (Lin p Constant)
      let polEq = unify c domainPol (Pol p Unquantified)

      -- The body and the result types are unified with the same polarity
      resPol <- freshPolarityMeta p
      resEq <- unifyBodyAndResTypes resPol resPol

      return $ linEq <> polEq <> resEq


    TensorType _ tElem _ -> do
      -- Then apply the same constraint to the element type.
      recTC <- createTC c (HasQuantifier q) [tElem, body, res]
      return recTC

    _ -> throwError $ FailedQuantifierConstraintDomain (constraintContext c) domain q
  where
  p = provenanceOf c

  unifyBodyAndResTypes :: CheckedExpr -> CheckedExpr -> m ConstraintProgress
  unifyBodyAndResTypes bodyPol resPol = do
    -- The linearity is unchanged.
    lin <- freshLinearityMeta p
    let bodyEq = unify c body (AnnBoolType p lin bodyPol)
    let resEq  = unify c res  (AnnBoolType p lin resPol)
    return $ bodyEq <> resEq

solveHasQuantifier _ c _ = malformedConstraint c

--------------------------------------------------------------------------------
-- HasNeg

solveHasNeg :: MonadMeta m
            => Constraint
            -> [CheckedExpr]
            -> m ConstraintProgress
solveHasNeg c [arg, res]
  | isMeta arg && isMeta res =
    extractAndBlockOnMetas [arg, res]

  | anyOf [arg, res] isIntType || anyOf [arg, res] isAnnRatType =
    return $ unify c res arg

  | let dims = getTensorTypeDimensions [arg],
    anyOf [arg] isTensorType && anyOf dims isMeta = do
    extractAndBlockOnMetas dims

  | otherwise = throwError $ head $
    unless2 (isMeta arg)
      (FailedBuiltinConstraintArgument ctx Neg arg allowedTypes 1 1) <>
    unless2 (isMeta res)
      (FailedBuiltinConstraintResult   ctx Neg res allowedTypes)
  where
    ctx = constraintContext c
    allowedTypes = allowed (NumericType <$> [Int, Rat])

solveHasNeg c _ = malformedConstraint c

--------------------------------------------------------------------------------
-- HasAdd

solveHasAdd :: MonadMeta m
            => Constraint
            -> [CheckedExpr]
            -> m ConstraintProgress
solveHasAdd c [arg1, arg2, res]
  | allOf [arg1, arg2, res] isMeta =
    extractAndBlockOnMetas [arg1, arg2, res]

  | anyOf [arg1, arg2, res] isNatType =
    return $ checkOp2SimpleTypesEqual c arg1 arg2 res

  | anyOf [arg1, arg2, res] isIntType =
    return $ checkOp2SimpleTypesEqual c arg1 arg2 res

  | anyOf [arg1, arg2, res] isAnnRatType =
    checkRatTypesEqual MaxLinearity c res [arg1, arg2]

  | let dims = getTensorTypeDimensions [arg1, arg2],
    anyOf [arg1, arg2] isTensorType && anyOf dims isMeta = do
    extractAndBlockOnMetas dims

  | otherwise = throwError $ head $
    unless2 (isMeta arg1)
      (FailedBuiltinConstraintArgument ctx (NumericOp2 Add) arg1 allowedTypes 1 2) <>
    unless2 (isMeta arg2)
      (FailedBuiltinConstraintArgument ctx (NumericOp2 Add) arg2 allowedTypes 2 2) <>
    unless2 (isMeta res)
      (FailedBuiltinConstraintResult   ctx (NumericOp2 Add) res  allowedTypes)
  where
    ctx = constraintContext c
    allowedTypes = allowed (NumericType <$> [Nat, Int, Rat])

solveHasAdd c _ = malformedConstraint c

--------------------------------------------------------------------------------
-- HasSub

solveHasSub :: MonadMeta m
            => Constraint
            -> [CheckedExpr]
            -> m ConstraintProgress
solveHasSub c [arg1, arg2, res]
  | allOf [arg1, arg2, res] isMeta =
    extractAndBlockOnMetas [arg1, arg2, res]

  | anyOf [arg1, arg2, res] isIntType =
    return $ checkOp2SimpleTypesEqual c arg1 arg2 res

  | anyOf [arg1, arg2, res] isAnnRatType =
    checkRatTypesEqual MaxLinearity c res [arg1, arg2]

  | let dims = getTensorTypeDimensions [arg1, arg2],
    anyOf [arg1, arg2] isTensorType && anyOf dims isMeta = do
    extractAndBlockOnMetas dims

  | otherwise = throwError $ head $
    unless2 (isMeta arg1)
      (FailedBuiltinConstraintArgument ctx (NumericOp2 Sub) arg1 allowedTypes 1 2) <>
    unless2 (isMeta arg2)
      (FailedBuiltinConstraintArgument ctx (NumericOp2 Sub) arg2 allowedTypes 2 2) <>
    unless2 (isMeta res)
      (FailedBuiltinConstraintResult   ctx (NumericOp2 Sub) res  allowedTypes)
  where
    ctx = constraintContext c
    allowedTypes = allowed (NumericType <$> [Int, Rat])

solveHasSub c _ = malformedConstraint c

--------------------------------------------------------------------------------
-- HasMul

solveHasMul :: MonadMeta m
            => Constraint
            -> [CheckedExpr]
            -> m ConstraintProgress
solveHasMul c [arg1, arg2, res]
  | allOf [arg1, arg2, res] isMeta =
    extractAndBlockOnMetas [arg1, arg2, res]

  | anyOf [arg1, arg2, res] isNatType =
    return $ checkOp2SimpleTypesEqual c arg1 arg2 res

  | anyOf [arg1, arg2, res] isIntType =
    return $ checkOp2SimpleTypesEqual c arg1 arg2 res

  | anyOf [arg1, arg2, res] isAnnRatType =
    checkRatTypesEqual MulLinearity c res [arg1, arg2]

  | let dims = getTensorTypeDimensions [arg1, arg2],
    anyOf [arg1, arg2] isTensorType && anyOf dims isMeta = do
    extractAndBlockOnMetas dims

  | otherwise = throwError $ head $
    unless2 (isMeta arg1)
      (FailedBuiltinConstraintArgument ctx (NumericOp2 Mul) arg1 allowedTypes 1 2) <>
    unless2 (isMeta arg2)
      (FailedBuiltinConstraintArgument ctx (NumericOp2 Mul) arg2 allowedTypes 2 2) <>
    unless2 (isMeta res)
      (FailedBuiltinConstraintResult   ctx (NumericOp2 Mul) res  allowedTypes)
  where
    ctx = constraintContext c
    allowedTypes = allowed (NumericType <$> [Nat, Int, Rat])
solveHasMul c _ = malformedConstraint c

--------------------------------------------------------------------------------
-- HasDiv

solveHasDiv :: MonadMeta m
            => Constraint
            -> [CheckedExpr]
            -> m ConstraintProgress
solveHasDiv c [arg1, arg2, res]
  | allOf [arg1, arg2, res] isMeta =
    extractAndBlockOnMetas [arg1, arg2, res]

  | anyOf [arg1, arg2, res] isAnnRatType =
    checkRatTypesEqual MulLinearity c res [arg1, arg2]

  | let dims = getTensorTypeDimensions [arg1, arg2],
    anyOf [arg1, arg2] isTensorType && anyOf dims isMeta = do
    extractAndBlockOnMetas dims

  | otherwise = throwError $ head $
    unless2 (isMeta arg1)
      (FailedBuiltinConstraintArgument ctx (NumericOp2 Div) arg1 allowedTypes 1 2) <>
    unless2 (isMeta arg2)
      (FailedBuiltinConstraintArgument ctx (NumericOp2 Div) arg2 allowedTypes 2 2) <>
    unless2 (isMeta res)
      (FailedBuiltinConstraintResult   ctx (NumericOp2 Div) res  allowedTypes)
  where
    ctx = constraintContext c
    allowedTypes = allowed [NumericType Rat]

solveHasDiv c _ = malformedConstraint c

--------------------------------------------------------------------------------
-- HasFold

solveHasFold :: MonadMeta m
             => Constraint
             -> [CheckedExpr]
             -> m ConstraintProgress
solveHasFold c [tElem, tCont] = case tCont of
  (exprHead -> Meta _ m) ->
    blockOnMetas [m]

  ListType _ tElem2 -> do
    return $ unify c tElem tElem2

  TensorType _ tBaseElem tDims -> case tDims of
    (exprHead -> Meta _ m) ->
      blockOnMetas [m]

    (ConsExpr ann _ [_d, ds]) ->
      return $ unify c tElem (TensorType ann tBaseElem (argExpr ds))

    (SeqExpr ann tNat tNatList (_d : ds)) ->
      return $ unify c tElem (TensorType ann tBaseElem (SeqExpr ann tNat tNatList ds))

    _ -> malformedConstraint c

  _ -> throwError $ FailedFoldConstraintContainer (constraintContext c) tCont

solveHasFold c _ = malformedConstraint c

--------------------------------------------------------------------------------
-- HasQuantifierIn

solveHasQuantifierIn :: MonadMeta m
                     => Quantifier
                     -> Constraint
                     -> [CheckedExpr]
                     -> m ConstraintProgress
solveHasQuantifierIn q c [tElem, tCont, tRes] = case tCont of
  (exprHead -> Meta _ m) ->
    blockOnMetas [m]

  ListType _ tElem2 -> do
    return $ unify c tElem tElem2

  TensorType _ tBaseElem tDims -> case tDims of
    (exprHead -> Meta _ m) ->
      blockOnMetas [m]

    (ConsExpr p _ [_d, ds]) -> do
      let elemEq = unify c tElem (TensorType p tBaseElem (argExpr ds))
      (resEq, _, _) <- unifyWithAnnBoolType c tRes
      return $ elemEq <> resEq

    (SeqExpr p tNat tNatList (_d : ds)) -> do
      let elemEq = unify c tElem (TensorType p tBaseElem (SeqExpr p tNat tNatList ds))
      (resEq, _, _) <- unifyWithAnnBoolType c tRes
      return $ elemEq <> resEq

    _ -> malformedConstraint c

  _ -> throwError $ FailedQuantInConstraintContainer (constraintContext c) tCont q

solveHasQuantifierIn _ c _ = malformedConstraint c

--------------------------------------------------------------------------------
-- HasNatLits

solveHasNatLits :: MonadMeta m
                => Int
                -> Constraint
                -> [CheckedExpr]
                -> m ConstraintProgress
solveHasNatLits n c [arg] = do
  case arg of
    (exprHead -> Meta _ m) ->
      blockOnMetas [m]

    IndexType _ d -> case d of
      (exprHead -> Meta _ m) ->
        blockOnMetas [m]

      (App _ (Literal _ (LNat v)) _)
      -- ^ Can't pattern match on NatLitExpr here as the
      -- PrimDict may not yet have been solved.
        | n < v     -> return triviallySolved
        | otherwise -> throwError $ FailedNatLitConstraintTooBig (constraintContext c) n v

      _ -> throwError $ FailedNatLitConstraintUnknown (constraintContext c) n d

    NatType{} ->
      return triviallySolved

    IntType{} ->
      return triviallySolved

    AnnRatType _ lin ->
      return $ unify c lin (Lin (provenanceOf c) Constant)

    TensorType _ _ dims -> case dims of
      (exprHead -> Meta _ m) ->
        blockOnMetas [m]

      _ -> throwError $ FailedNatLitConstraint (constraintContext c) n arg

    _ -> throwError $ FailedNatLitConstraint (constraintContext c) n arg

solveHasNatLits _ c _ = malformedConstraint c

--------------------------------------------------------------------------------
-- HasIntLits

solveHasIntLits :: MonadMeta m
               => Constraint
               -> [CheckedExpr]
               -> m ConstraintProgress
solveHasIntLits c [arg] = case arg of
  (exprHead -> Meta _ m) ->
    blockOnMetas [m]

  IntType{} ->
    return triviallySolved

  AnnRatType _ lin ->
    return $ unify c lin (Lin (provenanceOf c) Constant)

  TensorType _ _ dims -> case dims of
    (exprHead -> Meta _ m) ->
      blockOnMetas [m]

    _ -> throwError $ FailedIntLitConstraint (constraintContext c) arg

  _ -> throwError $ FailedIntLitConstraint (constraintContext c) arg

solveHasIntLits c _ = malformedConstraint c

--------------------------------------------------------------------------------
-- HasRatLits

solveHasRatLits :: MonadMeta m
               => Constraint
               -> [CheckedExpr]
               -> m ConstraintProgress
solveHasRatLits c [arg] = case arg of
  (exprHead -> Meta _ m) ->
    blockOnMetas [m]

  AnnRatType _ lin ->
    return $ unify c lin (Lin (provenanceOf c) Constant)

  TensorType _ _ dims -> case dims of
    (exprHead -> Meta _ m) ->
      blockOnMetas [m]

    _ -> throwError $ FailedRatLitConstraint (constraintContext c) arg

  _ -> throwError $ FailedRatLitConstraint (constraintContext c) arg

solveHasRatLits c _ = malformedConstraint c

--------------------------------------------------------------------------------
-- HasConLits

solveHasConLits :: MonadMeta m
                => Int
                -> Constraint
                -> [CheckedExpr]
                -> m ConstraintProgress
solveHasConLits n c [tElem, tCont] = case tCont of
  (exprHead -> Meta _ m) ->
    blockOnMetas [m]

  ListType _ tResElem ->
    return $ unify c tElem tResElem

  TensorType _ tBaseElem dims -> case dims of
    (exprHead -> Meta _ m) ->
      blockOnMetas [m]

    (ConsExpr ann tNat [d, ds]) -> do
      let resElemType = TensorType p tBaseElem (argExpr ds)
      let elemEq = unify c tElem resElemType
      let dimEq = unify c (NatLiteralExpr ann tNat n) (argExpr d)
      return $ elemEq <> dimEq

    (SeqExpr ann tNat tNatList (d : ds)) -> do
      let resElemType = TensorType p tBaseElem (SeqExpr ann tNat tNatList ds)
      let elemEq = unify c tElem resElemType
      let dimEq = unify c (NatLiteralExpr ann (NatType ann) n) d
      return $ elemEq <> dimEq

    _ -> do
      logDebug MaxDetail (prettyVerbose dims)
      malformedConstraint c
    where p = provenanceOf c

  _ -> throwError $ FailedConLitConstraint (constraintContext c) tCont

solveHasConLits _ c _ = malformedConstraint c


-- [2] {?2} {List Nat} {{PrimDict ((HasConLitsOfSize 1 ?2) (List Nat)) }} {{?5 }}

--------------------------------------------------------------------------------
-- TypesEqualModAuxiliaryAnnotations

solveTypesEqual :: MonadMeta m
                => Constraint
                -> [CheckedExpr]
                -> m ConstraintProgress
solveTypesEqual c [targetType, SeqExpr _ _ _ subTypes]
  | allOf types isMeta =
    extractAndBlockOnMetas types

  | anyOf types isAnnRatType = do
    checkRatTypesEqual MaxLinearity c targetType subTypes

  | anyOf types isAnnBoolType = do
    checkBoolTypesEqual MaxLinearity MaxPolarity c targetType subTypes

  | anyOf types isListType = do
    (targetEqConstraint, targetElemType) <- unifyWithListType c targetType
    (subEqConstraints, subElemTypes) <- unzip <$> forM subTypes (unifyWithListType c)
    let subElemTypeSeq = mkList p (TypeUniverse p 0) subElemTypes
    let tc = TypesEqualModAuxiliaryAnnotations
    recEq <- createTC c tc [targetElemType, subElemTypeSeq]
    return $ targetEqConstraint <> fold subEqConstraints <> recEq

  | anyOf types isTensorType = do
    dims <- freshDimsMeta c
    (targetEqConstraint, targetElemType) <- unifyWithTensorType c dims targetType
    (subEqConstraints, subElemTypes) <- unzip <$> forM subTypes (unifyWithTensorType c dims)
    let subElemTypeSeq = mkList p (TypeUniverse p 0) subElemTypes
    let tc = TypesEqualModAuxiliaryAnnotations
    recEq <- createTC c tc [targetElemType, subElemTypeSeq]
    return $ targetEqConstraint <> fold subEqConstraints <> recEq

  | otherwise = do
    let adjacentPairs = zip types $ tail types
    let unifications = map (uncurry (unify c)) adjacentPairs
    return $ foldr (<>) triviallySolved unifications

  where p = provenanceOf c; types = targetType : subTypes

solveTypesEqual c _ = malformedConstraint c

--------------------------------------------------------------------------------
-- MaxLinearity

solveMaxLinearity :: MonadMeta m
                  => Constraint
                  -> [CheckedExpr]
                  -> m ConstraintProgress
solveMaxLinearity c [lin1, lin2, res] =
  case (lin1, lin2) of
    (exprHead -> Meta _ m1, _) -> blockOnMetas [m1]
    (_, exprHead -> Meta _ m2) -> blockOnMetas [m2]

    (Lin p l1, Lin _ l2) -> do
      let linRes = Lin p $ mulLinearity l1 l2
      return $ unify c res linRes

    _ -> malformedConstraint c

solveMaxLinearity c _ = malformedConstraint c

--------------------------------------------------------------------------------
-- MulLinearity

solveMulLinearity :: MonadMeta m
                  => Constraint
                  -> [CheckedExpr]
                  -> m ConstraintProgress
solveMulLinearity c [lin1, lin2, res] =
  case (lin1, lin2) of
    (exprHead -> Meta _ m1, _) -> blockOnMetas [m1]
    (_, exprHead -> Meta _ m2) -> blockOnMetas [m2]

    (Lin p l1, Lin _ l2) -> do
      let linRes = Lin p $ mulLinearity l1 l2
      return $ unify c res linRes

    _ -> malformedConstraint c

solveMulLinearity c _ = malformedConstraint c

--------------------------------------------------------------------------------
-- NegatePolarity

solveNegPolarity :: MonadMeta m
                 => Constraint
                 -> [CheckedExpr]
                 -> m ConstraintProgress
solveNegPolarity c [arg1, res] = case arg1 of
  (exprHead -> Meta _ m) -> blockOnMetas [m]

  Pol p pol -> do
    let resPol = Pol p $ negatePolarity (provenanceOf c) pol
    return $ unify c res resPol

  _ -> malformedConstraint c

solveNegPolarity c _ = malformedConstraint c

--------------------------------------------------------------------------------
-- AddPolarity

solveAddPolarity :: MonadMeta m
                  => Quantifier
                  -> Constraint
                  -> [CheckedExpr]
                  -> m ConstraintProgress
solveAddPolarity q c [arg1, res] = case arg1 of
  (exprHead -> Meta _ m) -> blockOnMetas [m]

  Pol _ pol -> do
    let p = provenanceOf c
    let resPol = Pol p $ addPolarity p q pol
    return $ unify c res resPol

  _ -> malformedConstraint c

solveAddPolarity _ c _ = malformedConstraint c

--------------------------------------------------------------------------------
-- Solve binary polarity constraints

solvePolarityOp2 :: MonadMeta m
                 => (Provenance -> Polarity -> Polarity -> Polarity)
                 -> Constraint
                 -> [CheckedExpr]
                 -> m ConstraintProgress
solvePolarityOp2 op2 c [arg1, arg2, res] = case (arg1, arg2) of
  (exprHead -> Meta _ m1, _) -> blockOnMetas [m1]
  (_, exprHead -> Meta _ m2) -> blockOnMetas [m2]

  (Pol p pol1, Pol _ pol2) -> do
    let pol3 = Pol p $ op2 (provenanceOf c) pol1 pol2
    return $ unify c res pol3

  _ -> malformedConstraint c

solvePolarityOp2 _ c _ = malformedConstraint c

solveMaxPolarity :: MonadMeta m
                 => Constraint
                 -> [CheckedExpr]
                 -> m ConstraintProgress
solveMaxPolarity = solvePolarityOp2 (const maxPolarity)

solveEqPolarity :: MonadMeta m
                 => Equality
                 -> Constraint
                 -> [CheckedExpr]
                 -> m ConstraintProgress
solveEqPolarity eq = solvePolarityOp2 (eqPolarity eq)

solveImplPolarity :: MonadMeta m
                 => Constraint
                 -> [CheckedExpr]
                 -> m ConstraintProgress
solveImplPolarity = solvePolarityOp2 implPolarity

--------------------------------------------------------------------------------
-- Utilities

pattern Pol :: Provenance -> Polarity -> Expr binder var
pattern Pol p pol = Builtin p (Polarity pol)

pattern Lin :: Provenance -> Linearity -> Expr binder var
pattern Lin p lin = Builtin p (Linearity lin)

boolType :: Provenance -> Linearity -> Polarity -> CheckedExpr
boolType p lin pol = AnnBoolType p (Lin p lin) (Pol p pol)

createTC :: MonadMeta m
         => Constraint
         -> TypeClass
         -> NonEmpty CheckedExpr
         -> m ConstraintProgress
createTC c tc args = do
  let p = provenanceOf c
  let ctx = constraintContext c
  let tcExpr = BuiltinTypeClass p tc (ExplicitArg p <$> args)
  m <- freshTypeClassPlacementMeta p tcExpr

  return Progress
    { newConstraints = [TC ctx (m `Has` tcExpr) ]
    , solvedMetas    = mempty
    }

unify :: Constraint
      -> CheckedExpr
      -> CheckedExpr
      -> ConstraintProgress
unify c e1 e2 = Progress
  { newConstraints = [UC ctx $ Unify (e1, e2)]
  , solvedMetas    = mempty
  } where ctx = (constraintContext c) { blockedBy = mempty }

unifyWithAnnRatType :: MonadMeta m
                => Constraint
                -> CheckedExpr
                -> m (ConstraintProgress, CheckedExpr)
unifyWithAnnRatType c t = do
  let p = provenanceOf c
  lin <- freshLinearityMeta p
  let eq = unify c t (AnnRatType p lin)
  return (eq, lin)

unifyWithAnnBoolType :: MonadMeta m
                 => Constraint
                 -> CheckedExpr
                 -> m (ConstraintProgress, CheckedExpr, CheckedExpr)
unifyWithAnnBoolType c t = do
  let p = provenanceOf c
  lin <- freshLinearityMeta p
  pol <- freshPolarityMeta p
  let eq = unify c t (AnnBoolType p lin pol)
  return (eq, lin, pol)

unifyWithListType :: MonadMeta m
                 => Constraint
                 -> CheckedExpr
                 -> m (ConstraintProgress, CheckedExpr)
unifyWithListType c t = do
  let p = provenanceOf c
  elemType <- freshExprMeta p (TypeUniverse p 0) (boundContext c)
  let eq = unify c t (ListType p elemType)
  return (eq, elemType)

unifyWithTensorType :: MonadMeta m
                    => Constraint
                    -> CheckedExpr
                    -> CheckedExpr
                    -> m (ConstraintProgress, CheckedExpr)
unifyWithTensorType c dims t = do
  let p = provenanceOf c
  elemType <- freshExprMeta p (TypeUniverse p 0) (boundContext c)
  let eq = unify c t (TensorType p elemType dims)
  return (eq, elemType)

freshDimsMeta :: MonadMeta m => Constraint -> m CheckedExpr
freshDimsMeta c = do
  let p = provenanceOf c
  freshExprMeta p (ListType p (NatType p)) (boundContext c)

getMeta :: CheckedExpr -> Maybe Meta
getMeta e = case exprHead e of
  Meta _ m -> Just m
  _        -> Nothing

getTensorTypeDimensions :: [CheckedExpr] -> [CheckedExpr]
getTensorTypeDimensions = mapMaybe $ \case
  TensorType _ _ dims -> Just dims
  _                   -> Nothing

blockOnMetas :: MonadMeta m
             => [Meta]
             -> m ConstraintProgress
blockOnMetas metas = do
  logDebug MaxDetail $ "stuck-on metas" <+> pretty metas
  return Stuck

extractAndBlockOnMetas :: MonadMeta m
                       => [CheckedExpr]
                       -> m ConstraintProgress
extractAndBlockOnMetas args = blockOnMetas $ catMaybes (getMeta <$> args)

checkOp2SimpleTypesEqual :: Constraint
                         -> CheckedExpr -> CheckedExpr -> CheckedExpr
                         -> ConstraintProgress
checkOp2SimpleTypesEqual c arg1 arg2 res = do
  let argsEq = unify c arg1 arg2
  let resEq  = unify c arg1 res
  argsEq <> resEq

checkRatTypesEqual :: MonadMeta m
                      => TypeClass
                      -> Constraint
                      -> CheckedExpr
                      -> [CheckedExpr]
                      -> m ConstraintProgress
checkRatTypesEqual linTC c targetType subTypes = do
  (targetEqConstraint, targetLin) <-
    unifyWithAnnRatType c targetType

  (subEqConstraints, subLins) <-
    unzip <$> forM subTypes (unifyWithAnnRatType c)

  linTCConstraints <- combineAuxiliaryConstraints linTC freshLinearityMeta c targetLin subLins

  return $ targetEqConstraint <> fold subEqConstraints <> linTCConstraints

checkBoolTypesEqual :: MonadMeta m
                    => TypeClass
                    -> TypeClass
                    -> Constraint
                    -> CheckedExpr
                    -> [CheckedExpr]
                    -> m ConstraintProgress
checkBoolTypesEqual linTC polTC c targetType subTypes = do
  (targetEqConstraint, targetLin, targetPol) <-
    unifyWithAnnBoolType c targetType

  (subEqConstraints, subLins, subPols) <-
    unzip3 <$> forM subTypes (unifyWithAnnBoolType c)

  linTCConstraints <- combineAuxiliaryConstraints linTC freshLinearityMeta c targetLin subLins
  polTCConstraints <- combineAuxiliaryConstraints polTC freshPolarityMeta  c targetPol subPols

  return $ targetEqConstraint <> fold subEqConstraints <> linTCConstraints <> polTCConstraints

combineAuxiliaryConstraints :: forall m . MonadMeta m
                            => TypeClass
                            -> (Provenance -> m CheckedExpr)
                            -> Constraint
                            -> CheckedExpr
                            -> [CheckedExpr]
                            -> m ConstraintProgress
combineAuxiliaryConstraints tc makeMeta c result auxs = do
  (res, tcConstraints) <- foldPairs auxs
  let resEq = unify c res result
  return $ resEq <> tcConstraints
  where
    foldPairs :: [CheckedExpr] -> m (CheckedExpr, ConstraintProgress)
    foldPairs []       = compilerDeveloperError "Empty list of auxiliary expressions"
    foldPairs [a]      = return (a, Stuck)
    foldPairs (a : cs) = do
      (b, constraints) <- foldPairs cs
      res <- makeMeta (provenanceOf c)
      tc1 <- createTC c tc [a, b, res]
      return (res, tc1 <> constraints)

malformedConstraint :: MonadMeta m => Constraint -> m a
malformedConstraint c = compilerDeveloperError $
  "Malformed constraint:" <+> prettyVerbose c

anyOf :: [a] -> (a -> Bool) ->  Bool
anyOf = flip any

allOf :: [a] -> (a -> Bool) ->  Bool
allOf = flip all

unless2 :: MonadPlus m => Bool -> a -> m a
unless2 p a = if not p then return a else mzero