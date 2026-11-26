module Vehicle.Compile.Print.Error.Typing
  ( typingErrorDetails,
    prettyIdentName,
    unsupportedAnnotationTypeDescription,
    githubIssues,
    implementationLimitation,
  )
where

import Control.Applicative ((<|>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo (..))
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Core
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Meta.Substitution qualified as MetaSubstitution
import Vehicle.Data.Builtin.Core (BuiltinType (..))
import Vehicle.Data.Builtin.Core.StdlibTypeClassOp (lookupStdlibTypeClassOp)
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.ForcedValue qualified as Forced
import Vehicle.Data.DSL
import Vehicle.Data.Variable.Bound.Context.Generic
import Vehicle.Data.Variable.Free.Context (runFreeContext)
import Prelude hiding (pi)

typingErrorDetails ::
  forall builtin.
  (Eq builtin, PrintableBuiltin builtin, NormalisableBuiltin builtin) =>
  TypingError builtin ->
  VehicleError
typingErrorDetails = \case
  MissingExplicitArg err -> missingExplicitArgError err
  FunctionTypeMismatch err -> functionTypeMismatchError err
  FailedUnificationConstraints err -> failedUnificationConstraintsError err
  FailedInstanceConstraint err -> failedInstanceConstraintError err
  RelevantUseOfIrrelevantVariable err -> relevantUseOfIrrelevantVariableError err
  FailedIndexConstraintTooBig ctx v n ->
    VehicleUserError
      { provenance = Just $ provenanceOf ctx,
        problem =
          "the value"
            <+> squotes (pretty v)
            <+> "is too big to"
            <+> "be used as an index of size"
            <+> squotes (pretty n)
            <> ".",
        fix = Nothing
      }
  FailedIndexConstraintUnknown ctx v t ->
    VehicleUserError
      { provenance = Just $ provenanceOf ctx,
        problem =
          "unable to determine if"
            <+> squotes (prettyFriendly (WithContext v (namedBoundCtxOf ctx)))
            <+> "is a valid index of size"
            <+> squotes (prettyFriendly (WithContext t (namedBoundCtxOf ctx)))
            <> ".",
        fix = Nothing
      }
  UnsolvedConstraints cs ->
    VehicleUserError
      { provenance = Just $ provenanceOf ctx,
        problem = constraintOriginMessage,
        fix = Just "try adding more type annotations"
      }
    where
      WithContext constraint ctx = NonEmpty.head cs

      constraintOriginMessage = case constraint of
        UnificationConstraint (Unify origin _ _) -> case origin of
          CheckingExprType CheckingExpr {..} -> do
            let originNameCtx = toNamedBoundCtx checkedExprCtx
            "expected"
              <+> ( case checkedExpr of
                      Left binder -> "variable" <+> quotePretty binder
                      Right expr -> squotes (prettyFriendly $ WithContext expr originNameCtx)
                  )
              <+> "to be of type"
              <+> squotes (prettyFriendly $ WithContext checkedExprExpectedType originNameCtx)
              <+> "but was unable to prove it."
          CheckingInstanceType instanceOrigin ->
            instanceOriginConstraintMessage instanceOrigin
        InstanceConstraint (Resolve instanceOrigin _ _ _ _) -> instanceOriginConstraintMessage instanceOrigin
        -- AuxiliaryConstraint (Auxiliary auxiliaryOrigin _ _) -> auxiliaryOriginConstraintMessage auxiliaryOrigin
        ApplicationConstraint {} ->
          "unsolved application constraint: " <+> prettyFriendly (WithContext constraint ctx)

      instanceOriginConstraintMessage = \case
        InstanceArgOrigin ArgOrigin {..} ->
          "insufficient information to find a valid type for the overloaded expression"
            <+> squotes (prettyTypeClassConstraintOriginExpr ctx checkedInstanceOp checkedInstanceOpArgs)
        InstanceTypeRestrictionOrigin {} -> developerError "Unexpected type-restriction error"
  UnsolvedMetas _ ms ->
    VehicleUserError
      { provenance = Just p,
        problem = "Unable to infer type of bound variable",
        fix = Just "add more type annotations"
      }
    where
      (_, p) = NonEmpty.head ms
  InvalidInstanceHead (ident, p) expr ->
    VehicleUserError
      { provenance = Just p,
        problem =
          "Cannot add"
            <+> prettyIdentName ident
            <+> "as an instance"
            <+> "as"
            <+> prettyFriendlyEmptyCtx expr
            <+> "is not a valid shape.",
        fix = Just "add more type annotations"
      }
  NonTypeClassInstanceHead _ (ident, p) typeClassIdent ->
    VehicleUserError
      { provenance = Just p,
        problem =
          "Cannot add"
            <+> prettyIdentName ident
            <+> "as an instance"
            <+> "as"
            <+> prettyIdentName typeClassIdent
            <+> "is not annotated as a"
            <+> pretty AnnTypeClass
            <+> ".",
        fix = Just "add more type annotations"
      }
  QuantifiedIfCondition ctx ->
    VehicleUserError
      { provenance = Just $ provenanceOf ctx,
        problem = "cannot currently use quantifiers in `if` conditions.",
        fix = Just $ implementationLimitation Nothing
      }

implementationLimitation :: Maybe Int -> Doc a
implementationLimitation issue =
  "This is a limitation of the current implementation rather than a fundamental problem."
    <+> case issue of
      Nothing -> "If you would like this to be fixed, please open an issue at" <+> squotes githubIssues
      Just issueNumber ->
        "If you would like this to be fixed, please comment at"
          <+> squotes (githubIssues <+> pretty issueNumber)
          <> "."

--------------------------------------------------------------------------------
-- Individual errors
--------------------------------------------------------------------------------
-- MissingExplicitArgError

missingExplicitArgError ::
  (PrintableBuiltin builtin) =>
  MissingExplicitArgError builtin ->
  VehicleError
missingExplicitArgError (MissingExplicitArgError ctx explicitBinder nonExplicitArg) = do
  let argTypeDoc = prettyFriendly $ WithContext (typeOf explicitBinder) ctx
  let argDoc = prettyFriendly $ WithContext (argExpr nonExplicitArg) ctx
  VehicleUserError
    { provenance = Just $ provenanceOf nonExplicitArg,
      problem =
        "expected an"
          <+> pretty Explicit
          <+> "argument of type"
          <+> argTypeDoc
          <+> "but instead found"
          <+> pretty (visibilityOf nonExplicitArg)
          <+> "argument"
          <+> squotes argDoc,
      fix = Just $ "try inserting an argument of type" <+> argTypeDoc
    }

--------------------------------------------------------------------------------
-- FunctionTypeMismatchError

functionTypeMismatchError ::
  forall builtin.
  (PrintableBuiltin builtin) =>
  FunctionTypeMismatchError builtin ->
  VehicleError
functionTypeMismatchError (FunctionTypeMismatchError ctx fun nonPiType args) = do
  VehicleUserError
    { provenance = Just $ provenanceOf fun,
      problem =
        "expected"
          <+> squotes (prettyFriendly $ WithContext fun ctx)
          <+> "to have something of type"
          <+> squotes (prettyFriendly $ WithContext expectedType ctx)
          <+> "but inferred type"
          <+> squotes (prettyFriendly $ WithContext nonPiType ctx),
      fix = Nothing
    }
  where
    mkRes :: [Endo (DSLExpr builtin)]
    mkRes =
      [ Endo $ \tRes -> pi Nothing (visibilityOf arg) (relevanceOf arg) (tHole ("arg" <> pack (show i))) (const tRes)
        | (i, arg) <- zip [0 :: Int ..] args
      ]

    expectedType :: Expr builtin
    expectedType = fromDSL mempty (appEndo (mconcat mkRes) (tHole "res"))

--------------------------------------------------------------------------------
-- RelevantUseOfIrrelevantVariableError

relevantUseOfIrrelevantVariableError ::
  RelevantUseOfIrrelevantVariableError builtin ->
  VehicleError
relevantUseOfIrrelevantVariableError (RelevantUseOfIrrelevantVariableError _ p name) =
  VehicleUserError
    { provenance = Just p,
      problem = "cannot use irrelevant variable" <+> quotePretty name <+> "in an relevant context",
      fix = Nothing
    }

--------------------------------------------------------------------------------
-- FailedUnificationConstraintsError

failedUnificationConstraintsError ::
  forall builtin.
  (PrintableBuiltin builtin, NormalisableBuiltin builtin) =>
  FailedUnificationConstraintsError builtin ->
  VehicleError
failedUnificationConstraintsError (FailedUnificationConstraintsError freeCtx (WithContext (Unify origin e1 e2) ctx :| _)) =
  case origin of
    CheckingExprType checkingExpr -> typeMismatchError freeCtx ctx checkingExpr e1 e2
    CheckingInstanceType instanceOrigin -> instanceTypeMismatchError ctx instanceOrigin e1 e2

-- | The error for when an expression (or a binder) does not have the type
-- expected of it. The whole expected and actual types are shown first. Only
-- then, if unification failed on some smaller part of them, is that part
-- pointed out.
typeMismatchError ::
  forall builtin.
  (PrintableBuiltin builtin, NormalisableBuiltin builtin) =>
  FreeCtx builtin ->
  ConstraintContext builtin ->
  CheckingExprType builtin ->
  ThunkWithMetas builtin ->
  ThunkWithMetas builtin ->
  VehicleError
typeMismatchError freeCtx ctx CheckingExpr {..} e1 e2 =
  VehicleUserError
    { provenance = Just $ provenanceOf ctx,
      problem =
        subject
          <+> "does not have the expected type."
          <> line
          <> indent 2 (typeComparisonBlock [("Expected", expectedDoc), ("Actual", actualDoc)])
          <> differenceNote,
      fix = Nothing
    }
  where
    -- The expression and its types live in the context in which it was
    -- checked, whereas the sub-terms on which unification failed live in the
    -- context of the failed constraint, which may contain extra binders.
    originEnv = boundContextToEnv checkedExprCtx
    expectedDoc = prettyDisplayType freeCtx checkedExprCtx (Unforced originEnv checkedExprExpectedType)
    actualDoc = prettyDisplayType freeCtx checkedExprCtx (Unforced originEnv checkedExprActualType)

    failedCtx = boundContextOf ctx
    expectedPartDoc = prettyDisplayType freeCtx failedCtx e1
    actualPartDoc = prettyDisplayType freeCtx failedCtx e2

    subject = case checkedExpr of
      Left (Just name) -> "variable" <+> quotePretty name
      Left Nothing -> "this variable"
      Right expr -> squotes (prettyFriendly (WithContext expr (toNamedBoundCtx checkedExprCtx)))

    differenceNote
      | sameDoc expectedPartDoc expectedDoc && sameDoc actualPartDoc actualDoc = ""
      | otherwise = line <> typeDifferenceNote ("expected", expectedPartDoc) ("actual", actualPartDoc)

sameDoc :: UnAnnDoc -> UnAnnDoc -> Bool
sameDoc d1 d2 = layoutAsString d1 == layoutAsString d2

-- | Lays out labelled types one above the other so that they can be easily
-- compared, e.g.
--
--   Expected: Vector Real 2
--   Actual:   Vector Real 3
typeComparisonBlock :: [(Text, Doc a)] -> Doc a
typeComparisonBlock rows = vsep (fmap row rows)
  where
    labelWidth = maximum (0 : fmap (Text.length . fst) rows) + 1
    row (label, typ) = pretty (Text.justifyLeft labelWidth ' ' (label <> ":")) <+> align typ

-- | Points out the parts of two types, labelled as in `typeComparisonBlock`,
-- that differ.
typeDifferenceNote :: (Doc a, Doc a) -> (Doc a, Doc a) -> Doc a
typeDifferenceNote (label1, part1) (label2, part2) =
  "They differ in that the"
    <+> label1
    <+> "type has"
    <+> squotes part1
    <+> "where the"
    <+> label2
    <+> "type has"
    <+> squotes part2
    <> "."

-- | The error for when a unification constraint arising from an instance
-- constraint fails.
instanceTypeMismatchError ::
  forall builtin.
  (PrintableBuiltin builtin) =>
  ConstraintContext builtin ->
  InstanceConstraintOrigin builtin ->
  ThunkWithMetas builtin ->
  ThunkWithMetas builtin ->
  VehicleError
instanceTypeMismatchError ctx origin e1 e2 = do
  let namedBoundCtx = toNamedBoundCtx (boundContextOf ctx)
  let originMessage = case origin of
        InstanceArgOrigin ArgOrigin {..} ->
          "unable to find a consistent type for the overloaded expression"
            <+> squotes (prettyTypeClassConstraintOriginExpr ctx checkedInstanceOp checkedInstanceOpArgs)
        InstanceTypeRestrictionOrigin (TypeRestrictionOrigin _ _ (Right sort) _) ->
          "All fields of record declarations annotated with" <+> quotePretty sort <+> "must have the same type"
        InstanceTypeRestrictionOrigin {} ->
          ""

  let problemDescription = case origin of
        InstanceTypeRestrictionOrigin (TypeRestrictionOrigin _ (ident, _) (Right (FieldTypesMatch f1 f2)) _) ->
          "."
            <> line
            <> "In the declaration of"
              <+> quotePretty (nameOf ident)
              <+> "field"
              <+> quotePretty (nameOf f1)
              <+> "has type:"
            <> line
            <> indent 2 (squotes (prettyFriendly (WithContext e1 namedBoundCtx)))
            <> line
            <> "which is not the same as the type of field"
              <+> quotePretty (nameOf f2)
            <> line
            <> indent 2 (squotes (prettyFriendly (WithContext e2 namedBoundCtx)))
        _ ->
          "."
            <+> "In particular"
            <+> squotes (prettyFriendly (WithContext e1 namedBoundCtx))
            <+> "is not equal to"
            <+> squotes (prettyFriendly (WithContext e2 namedBoundCtx))
            <> "."

  let fixDescription = case origin of
        InstanceTypeRestrictionOrigin (TypeRestrictionOrigin _ _ (Right (FieldTypesMatch f1 f2)) _) ->
          "either remove the @tensor annotation or ensure"
            <+> quotePretty (nameOf f1)
            <+> "and"
            <+> quotePretty (nameOf f2)
            <+> "have the same type"
        _ ->
          "check your types"

  VehicleUserError
    { provenance = Just $ provenanceOf ctx,
      problem = originMessage <> problemDescription,
      fix = Just fixDescription
    }

--------------------------------------------------------------------------------
-- FailedInstanceConstraintError

failedInstanceConstraintError ::
  forall builtin.
  (Eq builtin, NormalisableBuiltin builtin, PrintableBuiltin builtin) =>
  FailedInstanceConstraintError builtin ->
  VehicleError
failedInstanceConstraintError (FailedInstanceConstraintError freeEnv (WithContext constraint ctx) candidates) =
  case instanceOrigin constraint of
    InstanceTypeRestrictionOrigin t -> typeRestrictionError ctx t candidates
    InstanceArgOrigin t -> instanceArgOriginError freeEnv ctx t candidates

typeRestrictionError ::
  (Eq builtin, NormalisableBuiltin builtin) =>
  ConstraintContext builtin ->
  InstanceTypeRestrictionOrigin builtin ->
  [FailedInstanceCandidate builtin] ->
  VehicleError
typeRestrictionError _ctx (TypeRestrictionOrigin _freeEnv (ident, p) sort typ) _candidates = do
  VehicleUserError
    { provenance = Just p,
      problem = problemDescription,
      fix =
        Just $
          "change the type of"
            <+> fixIdent
            <+> "to a supported type"
    }
  where
    gluedType = Glued @MetaID typ (Unforced emptyBoundEnv typ)

    fixIdent = case sort of
      Right (FieldTypeIsAllowed f) -> quotePretty (nameOf f)
      _ -> prettyIdentName ident

    problemDescription = case sort of
      Right (FieldTypeIsAllowed f) ->
        "The type of"
          <+> quotePretty (nameOf f)
          <+> "in record declaration"
          <+> quotePretty (nameOf ident :: Text)
          <> ":"
          <> line
          <> indent 2 (prettyFriendlyEmptyCtx $ unnormalised gluedType)
          <> line
          <> "is not supported."
            <+> "All fields of a record declaration annotated with"
            <+> quotePretty sort
            <+> "must have the same type and must be either:"
          <> line
          <> indent 2 (prettyAllowedTypes supportedTypes)
      _ -> do
        let Glued expr normExpr = gluedType
        let fnormExpr = Forced.Unforced Forced.emptyBoundEnv $ unnormalise 0 normExpr
        let gluedType' = Forced.Glued expr fnormExpr
        unsupportedAnnotationTypeDescription (pretty sort) ident gluedType'
          <> "."
            <+> "The possible valid types for"
            <+> quotePretty sort
            <+> "annotated declarations are:"
          <> line
          <> indent 2 (prettyAllowedTypes supportedTypes)

    supportedTypes = case sort of
      Left RestrictedProperty -> ["Bool", "Vector Bool n", "Tensor Bool ns"]
      Left (RestrictedParameter Inferable) -> [pretty NatType]
      Left (RestrictedParameter NonInferable) -> map pretty [BoolType, IndexType, NatType, RatType]
      Left RestrictedDataset -> ["List A    " <+> datasetElementTypes, "Vector A n" <+> datasetElementTypes]
      Left RestrictedNetwork -> ["S -> T, where S and T are either of the form Tensor Rat [a_1, ..., a_n] (and a_1 through a_n are compile-time constants), or a record whose fields are such tensors."]
      Right _ -> ["The element of a tensor (e.g. Real), or", "A tensor (e.g. Tensor Real [...])"]

    datasetElementTypes = "(where A is either `Index n`, `Nat`, `Rat`, `List A`, `Vector A n`)"

    prettyAllowedTypes :: [Doc a] -> Doc a
    prettyAllowedTypes ts = vsep ((\(t, no) -> pretty no <> "." <+> t) <$> zip ts [1 :: Int ..])

instanceArgOriginError ::
  forall builtin.
  (Eq builtin, PrintableBuiltin builtin, NormalisableBuiltin builtin) =>
  FreeCtx builtin ->
  ConstraintContext builtin ->
  InstanceArgOrigin builtin ->
  [FailedInstanceCandidate builtin] ->
  VehicleError
instanceArgOriginError freeCtx ctx (ArgOrigin tcOp tcOpArgs tcOpType tc) candidates =
  VehicleUserError
    { provenance = Just $ provenanceOf ctx,
      problem =
        fromMaybe genericExplanation typeClassExplanation
          <> line
          <> "It can only be used at the following types:"
          <> line
          <> indent 2 (vsep (fmap candidateOpType (zip [1 ..] candidates))),
      fix = Nothing
    }
  where
    originExpr :: Doc a
    originExpr = squotes (prettyTypeClassConstraintOriginExpr ctx tcOp tcOpArgs)

    isCoercion = isCoercionExpr tcOp
    opApplication = (boundContextOf ctx, tcOpArgs)

    -- The type of the overloaded expression at the point it is used, i.e.
    -- its type instantiated with the arguments of the type class constraint
    -- that could not be satisfied.
    goalTypeArgs = (boundContextOf ctx, 0, typeClassArgsOf tc)

    genericExplanation :: UnAnnDoc
    genericExplanation =
      originExpr
        <+> "cannot be used as something of type"
        <+> squotes usedAtType
        <> "."

    -- An explanation specific to the type class, if the builtins provide one.
    typeClassExplanation :: Maybe UnAnnDoc
    typeClassExplanation = do
      typeClass <- case tc of
        App (Builtin _ b) _ -> Just (Right b)
        App (FreeVar _ ident) _ -> Just (Left ident)
        _ -> Nothing
      let namedCtx = toNamedBoundCtx (boundContextOf ctx)
      let normalise e =
            fst $
              runFreeContext freeCtx $
                MetaSubstitution.deepForceWithUnsolvedMetas namedCtx (Unforced (boundContextToEnv (boundContextOf ctx)) e)
      let normArgs = fmap (fmap normalise) (typeClassArgsOf tc)
      let prettyExpr e = prettyFriendly (WithContext e namedCtx)
      explainInstanceFailure prettyExpr originExpr typeClass normArgs

    usedAtType :: Doc a
    usedAtType = calculateInstanceDisplayType freeCtx isCoercion tcOpType goalTypeArgs opApplication

    candidateDisplayType :: FailedInstanceCandidate builtin -> UnAnnDoc
    candidateDisplayType (candidate, _) =
      calculateInstanceDisplayType freeCtx isCoercion tcOpType (calculateInstanceCandidateTypeArgs candidate) opApplication

    candidateOpType :: (Int, FailedInstanceCandidate builtin) -> UnAnnDoc
    candidateOpType (no, failedCandidate@(_, err)) =
      pretty no
        <> "." <+> candidateDisplayType failedCandidate
        <> line
        <> indent 2 ("- rejected:" <+> candidateRejectionReason (provenanceOf tcOp) err)

-- | The arguments of a type class constraint, e.g. `[Nat, Nat, Nat]` for
-- `HasAdd Nat Nat Nat`.
typeClassArgsOf :: Type builtin -> [Arg builtin]
typeClassArgsOf = \case
  App _ args -> NonEmpty.toList args
  _ -> []

-- | Describes why an instance candidate for an overloaded expression was
-- rejected. If the rejection was caused by a failure to unify the types of
-- the same overloaded expression then the description of the expression is
-- omitted, as it has already been given in the enclosing error.
candidateRejectionReason ::
  forall builtin.
  (Eq builtin, PrintableBuiltin builtin, NormalisableBuiltin builtin) =>
  Provenance ->
  TypingError builtin ->
  UnAnnDoc
candidateRejectionReason opProvenance = \case
  FailedUnificationConstraints (FailedUnificationConstraintsError _ (WithContext (Unify origin e1 e2) ctx :| _))
    | isOriginOf opProvenance origin -> do
        let namedBoundCtx = toNamedBoundCtx (boundContextOf ctx)
        squotes (prettyFriendly (WithContext e1 namedBoundCtx))
          <+> "is not equal to"
          <+> squotes (prettyFriendly (WithContext e2 namedBoundCtx))
          <> "."
  err -> problem (typingErrorDetails err)

-- | Whether the unification constraint arose from checking the type of the
-- overloaded expression at the provided location.
isOriginOf :: Provenance -> UnificationConstraintOrigin builtin -> Bool
isOriginOf opProvenance = \case
  CheckingInstanceType (InstanceArgOrigin ArgOrigin {..}) ->
    sameSourceRange (provenanceOf checkedInstanceOp) opProvenance
  _ -> False
  where
    -- NOTE: the `Eq` instance for `Provenance` deliberately equates all
    -- provenances, so we have to compare the underlying ranges.
    sameSourceRange :: Provenance -> Provenance -> Bool
    sameSourceRange (Provenance r1 f1) (Provenance r2 f2) = r1 == r2 && f1 == f2

calculateInstanceCandidateTypeArgs ::
  forall builtin.
  (PrintableBuiltin builtin) =>
  WithContext (InstanceCandidate builtin) ->
  (BoundCtx (Expr builtin), Int, [Arg builtin])
calculateInstanceCandidateTypeArgs (WithContext candidate typingCtx) =
  calculateCandidateType typingCtx 0 (candidateExpr candidate)
  where
    -- Returns the arguments of the head of the candidate, the context they
    -- live in, and the number of pattern variables of the candidate.
    calculateCandidateType :: BoundCtx (Expr builtin) -> Int -> Expr builtin -> (BoundCtx (Expr builtin), Int, [Arg builtin])
    calculateCandidateType dbCtx numberOfPatternVars = \case
      Builtin _ _tc -> (dbCtx, numberOfPatternVars, [])
      FreeVar _ _ident -> (dbCtx, numberOfPatternVars, [])
      App (Builtin _ _tc) args ->
        (dbCtx, numberOfPatternVars, NonEmpty.toList args)
      App (FreeVar _ _ident) args ->
        (dbCtx, numberOfPatternVars, NonEmpty.toList args)
      Pi _ binder result ->
        calculateCandidateType (binder : dbCtx) (numberOfPatternVars + 1) result
      t -> developerError $ "UNSUPPORTED PRINTING" <+> prettyVerbose t

-- | Calculates the type of an overloaded operation at a particular instance
-- of its type class, e.g. the type of `+` at the instance
-- `HasAdd Nat Nat Nat` is `Nat -> Nat -> Nat`.
--
-- The type of an overloaded operation consists of the parameters of the type
-- class, followed by the instance argument, followed by the rest of the type,
-- e.g. `addTC : {t1 t2 t3} -> {{HasAdd t1 t2 t3}} -> t1 -> t2 -> t3`.
-- There are two sources of information about how to instantiate it:
--
--   1. the arguments of the type class (see `typeClassParameterArgs`), and
--   2. the arguments that the operation is applied to in the source, e.g. the
--      length of a vector literal and its elements.
--
-- Where both are available, the argument of the type class is preferred,
-- unless it is uninformative, i.e. a pattern variable of the instance or an
-- unsolved meta-variable, in which case the argument of the application is
-- used if it is informative.
calculateInstanceDisplayType ::
  forall builtin a.
  (NormalisableBuiltin builtin, PrintableBuiltin builtin) =>
  FreeCtx builtin ->
  -- | Whether the overloaded operation is a coercion.
  Bool ->
  -- | The type of the overloaded operation.
  Type builtin ->
  -- | The arguments of the type class, along with the context they live in
  -- and the number of pattern variables of the instance at the head of the
  -- context.
  (BoundCtx (Type builtin), Int, [Arg builtin]) ->
  -- | The arguments that the operation is applied to, along with the context
  -- that they live in. The context of the type class arguments must be an
  -- extension of this context.
  (BoundCtx (Type builtin), [Arg builtin]) ->
  Doc a
calculateInstanceDisplayType freeEnv isCoercion opType (argsCtx, numberOfPatternVars, typeClassArgs) (appCtx, appArgs) = do
  let opTypeValue = Unforced (boundContextToEnv argsCtx) opType
  instantiateOpTypeForDisplay argsCtx opTypeValue (typeClassParameterArgs opType typeClassArgs) appArgs
  where
    -- The value to instantiate a parameter with, given the corresponding
    -- arguments of the type class and of the application (if any).
    parameterValue :: Maybe (Arg builtin) -> Maybe (Arg builtin) -> Maybe (UnforcedTypeWithMetas builtin)
    parameterValue typeClassArg appArg =
      (typeClassArg >>= informativeTypeClassArgValue)
        <|> (appArg >>= informativeValue appCtx . argExpr)
        <|> fmap (Unforced (boundContextToEnv argsCtx) . argExpr) typeClassArg

    informativeTypeClassArgValue :: Arg builtin -> Maybe (UnforcedTypeWithMetas builtin)
    informativeTypeClassArgValue arg = case argExpr arg of
      BoundVar _ ix | unIx ix < numberOfPatternVars -> Nothing
      e -> informativeValue argsCtx e

    informativeValue :: BoundCtx (Type builtin) -> Expr builtin -> Maybe (UnforcedTypeWithMetas builtin)
    informativeValue ctx = \case
      Hole {} -> Nothing
      Meta {} -> Nothing
      App (Meta {}) _ -> Nothing
      e -> Just (Unforced (boundContextToEnv ctx) e)

    -- Instantiates the leading binders of the type of the operation. Each
    -- value lives in the context it came from, both of which `ctx` extends,
    -- and `ctx` is only extended by the binders that are not instantiated.
    instantiateOpTypeForDisplay ::
      BoundCtx (Type builtin) ->
      UnforcedTypeWithMetas builtin ->
      [Maybe (Arg builtin)] ->
      [Arg builtin] ->
      Doc a
    instantiateOpTypeForDisplay ctx typ typeClassParamArgs remainingAppArgs = do
      let forcedType =
            runFreeContext (abstractUserDefinitions freeEnv) $
              MetaSubstitution.forceThunkWithUnsolvedMetas (toNamedBoundCtx ctx) typ
      case forcedType of
        -- Coercions are inserted by the compiler and are invisible to the
        -- user, who only sees the expression being coerced (e.g. the
        -- literal `0`). Therefore the type shown is the result type of the
        -- coercion, rather than the type of the coercion function itself.
        VPi binder closure | not (isExplicit binder) || isCoercion -> do
          let (typeClassArg, typeClassParamArgs') = case typeClassParamArgs of
                [] -> (Nothing, [])
                a : as -> (a, as)
          let (appArg, remainingAppArgs') = case remainingAppArgs of
                arg : args | visibilityMatches arg binder -> (Just arg, args)
                _ -> (Nothing, remainingAppArgs)
          let continueWith newCtx body =
                instantiateOpTypeForDisplay newCtx body typeClassParamArgs' remainingAppArgs'
          case parameterValue typeClassArg appArg of
            Just value -> continueWith ctx (extendClosure closure binder value)
            Nothing -> do
              let unnormBinder = unnormalise (boundCtxLv ctx) binder
              continueWith (unnormBinder : ctx) (extendClosureWithBound closure binder (boundCtxLv ctx))
        _ -> prettyDisplayType freeEnv ctx typ

-- | Matches the type of an overloaded operation against the arguments of its
-- type class, returning for each of the leading parameters of the type of the
-- operation the argument of the type class that it should be instantiated
-- with (if any).
--
-- For example, if the type of the operation is
-- `{t1 t2 t3} -> {{HasAdd t1 t2 t3}} -> t1 -> t2 -> t3` and the arguments of
-- the type class are `[Nat, Nat, Nat]` then the parameters `t1`, `t2` and
-- `t3` are each instantiated with `Nat`.
typeClassParameterArgs :: forall builtin. Type builtin -> [Arg builtin] -> [Maybe (Arg builtin)]
typeClassParameterArgs opType typeClassArgs = go 0 opType
  where
    go :: Int -> Type builtin -> [Maybe (Arg builtin)]
    go numberOfParameters = \case
      Pi _ binder body
        | isInstance binder -> do
            let instanceTypeArgs = case typeOf binder of
                  App _ args -> NonEmpty.toList args
                  _ -> []
            -- The parameter that each argument of the instance binder's type
            -- refers to, where parameters are numbered from the start of the
            -- type of the operation.
            let referencedParameter instanceTypeArg = case argExpr instanceTypeArg of
                  BoundVar _ ix | unIx ix < numberOfParameters -> Just (numberOfParameters - 1 - unIx ix)
                  _ -> Nothing
            let assignment =
                  [ (parameter, typeClassArg)
                    | (instanceTypeArg, typeClassArg) <- zip instanceTypeArgs typeClassArgs,
                      Just parameter <- [referencedParameter instanceTypeArg]
                  ]
            [lookup parameter assignment | parameter <- [0 .. numberOfParameters - 1]]
        | not (isExplicit binder) -> go (numberOfParameters + 1) body
      _ -> []

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

prettyTypeClassConstraintOriginExpr ::
  (PrintableBuiltin builtin) =>
  ConstraintContext builtin ->
  Expr builtin ->
  [Arg builtin] ->
  Doc a
prettyTypeClassConstraintOriginExpr ctx fun args = case fun of
  -- Overloaded operators are desugared into stdlib type-class operations
  -- (e.g. `-` into `negTC`), so we print the operator that the user wrote.
  FreeVar _ ident
    | isStdlibIdent ident,
      Just op <- lookupStdlibTypeClassOp (nameOf ident) ->
        pretty op
  -- We don't want to print out the actual coercion functions as the user is
  -- oblivious to them. Instead we want to print out what they are applied to.
  Builtin _ b
    | Just f <- coercionArgs b -> prettyExpr (f args)
  _ -> prettyExpr fun
  where
    prettyExpr expr = prettyFriendly $ WithContext expr (namedBoundCtxOf ctx)

-- | Prints a type for display to the user. Types may contain unevaluated
-- internal computation (e.g. from the typing rules of the builtins), and
-- therefore they are normalised first. However, the user's own definitions
-- are not unfolded, so that the type is displayed in terms the user wrote.
--
-- If normalisation gets stuck on an unsolved meta-variable then the
-- partially normalised type may expose internals (e.g. the literal `2` with
-- an unsolved coercion instance would become `?2 2`), and so the type is
-- instead displayed unnormalised.
prettyDisplayType ::
  (PrintableBuiltin builtin, NormalisableBuiltin builtin) =>
  FreeCtx builtin ->
  BoundCtx (Type builtin) ->
  ThunkWithMetas builtin ->
  Doc a
prettyDisplayType freeCtx ctx typ = do
  let namedCtx = toNamedBoundCtx ctx
  let (normType, blockingMetas) =
        runFreeContext (abstractUserDefinitions freeCtx) $
          MetaSubstitution.deepForceWithUnsolvedMetas namedCtx typ
  if MetaSet.null blockingMetas
    then prettyFriendly (WithContext normType namedCtx)
    else prettyFriendly (WithContext typ namedCtx)

-- | Normalisation unfolds function definitions, but the user's definitions
-- should be displayed as they wrote them. Therefore they are replaced with
-- abstract definitions, which normalisation does not unfold.
abstractUserDefinitions :: FreeCtx builtin -> FreeCtx builtin
abstractUserDefinitions = fmap $ \case
  decl@(DefFunction p ident _ typ _) | isUserCode decl -> DefAbstract p ident BuiltinDef typ
  decl -> decl

unsupportedAnnotationTypeDescription ::
  forall builtin a.
  (Eq builtin, PrintableBuiltin builtin) =>
  Doc a ->
  Identifier ->
  Forced.GluedType builtin ->
  Doc a
unsupportedAnnotationTypeDescription annotation ident resourceType = do
  let unreducedResourceType = Forced.unnormalised resourceType
  let reducedResourceType = (unnormalise 0 (Forced.normalised resourceType) :: Expr builtin)
  let reducedResourceTypeDoc = prettyFriendlyEmptyCtx reducedResourceType
  let unreducedResourceTypeDoc = prettyFriendlyEmptyCtx unreducedResourceType

  "The type of"
    <+> annotation
    <+> quotePretty (nameOf ident :: Text)
    <> ":"
    <> line
    <> indent 2 unreducedResourceTypeDoc
    <> line
    <> ( if layoutAsString reducedResourceTypeDoc == layoutAsString unreducedResourceTypeDoc
           then ""
           else
             "which reduces to:"
               <> line
               <> indent 2 reducedResourceTypeDoc
               <> line
       )
    <> "is not supported"

prettyIdentName :: Identifier -> Doc a
prettyIdentName ident = quotePretty (nameOf ident :: Name)
