{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Standard.Print () where

import Vehicle.Data.AST.Expr.Scoped (Arg, Expr (..), normAppList, pattern App)
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.Interface (pattern ICons, pattern INatLiteral, pattern ITensorType, pattern IVectorType)
import Vehicle.Prelude

instance PrintableBuiltin Builtin where
  coercionArgs b = case b of
    BuiltinCast FromNat {} -> Just $ \args -> argExpr $ last args
    BuiltinCast FromRat {} -> Just $ \args -> argExpr $ last args
    TypeClassOp FromNatTC {} -> Just $ \args -> argExpr $ last args
    TypeClassOp FromRatTC {} -> Just $ \args -> argExpr $ last args
    TypeClassOp VecLiteralTC {} -> Just $ \args -> normAppList (Builtin mempty b) args
    _ -> Nothing

  isDerivedBuiltin b = case b of
    DerivedFunction f -> Just $ identifierOf f
    _ -> Nothing

  explainInstanceFailure = explainStandardInstanceFailure

explainStandardInstanceFailure ::
  (Expr Builtin -> Doc a) ->
  Doc a ->
  Either Identifier Builtin ->
  [Arg Builtin] ->
  Maybe (Doc a)
explainStandardInstanceFailure prettyExpr opDoc typeClass args = case (typeClass, explicitArgExprs args) of
  -- A number literal used where a vector or tensor is expected.
  (Right (TypeClass tc), [expectedType])
    | isNumberLiteralClass tc,
      Just container <- containerDescription expectedType ->
        Just $
          opDoc
            <+> "is a single number, but it is used where"
            <+> container
            <+> "of type"
            <+> squotes (prettyExpr expectedType)
            <+> "is expected."
  -- A vector literal with the wrong number of elements.
  (Right (TypeClass HasVecLits), [expectedType, _elemType, INatLiteral actualLength])
    | Just expectedLength <- expectedVectorLength expectedType,
      expectedLength /= actualLength ->
        Just $
          opDoc
            <+> "has"
            <+> numberOf actualLength "element"
            <> ", but its expected type"
              <+> squotes (prettyExpr expectedType)
              <+> "requires"
              <+> pretty expectedLength
            <> "."
  -- A vector literal whose elements have the wrong type.
  (Right (TypeClass HasVecLits), [expectedType, actualElemType, _length])
    | Just requiredElemType <- requiredElementType expectedType,
      isKnown actualElemType,
      requiredElemType /= actualElemType ->
        Just $
          opDoc
            <+> "has elements of type"
            <+> squotes (prettyExpr actualElemType)
            <> ", but its expected type"
              <+> squotes (prettyExpr expectedType)
              <+> "requires elements of type"
              <+> squotes (prettyExpr requiredElemType)
            <> "."
  _ -> Nothing
  where
    containerDescription :: Expr Builtin -> Maybe (Doc a)
    containerDescription = \case
      IVectorType {} -> Just "a vector"
      ITensorType _ ICons {} -> Just "a tensor"
      _ -> Nothing

    -- The number of elements that a vector literal must have to have the
    -- type, if it is known.
    expectedVectorLength :: Expr Builtin -> Maybe Int
    expectedVectorLength = \case
      IVectorType _ (INatLiteral n) -> Just n
      ITensorType _ (ICons _ (INatLiteral n) _) -> Just n
      _ -> Nothing

    -- The type that the elements of a vector literal must have to have the
    -- type, if it is known.
    requiredElementType :: Expr Builtin -> Maybe (Expr Builtin)
    requiredElementType = \case
      IVectorType tElem _ -> Just tElem
      ITensorType tElem (ICons _ _ dims) -> Just (ITensorType tElem dims)
      _ -> Nothing

    -- Whether the type has been worked out, i.e. is not an unsolved
    -- meta-variable.
    isKnown :: Expr Builtin -> Bool
    isKnown = \case
      Meta {} -> False
      App (Meta {}) _ -> False
      _ -> True

    isNumberLiteralClass :: TypeClass -> Bool
    isNumberLiteralClass = \case
      HasNatLits -> True
      HasRatLits -> True
      _ -> False
