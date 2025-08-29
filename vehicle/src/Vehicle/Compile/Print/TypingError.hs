module Vehicle.Compile.Print.TypingError
  ( typingErrorDetails,
    prettyIdentName,
    unsupportedAnnotationTypeDescription,
  )
where

import Control.Monad.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Monoid (Endo (..))
import Data.Text (Text, pack)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (eval)
import Vehicle.Compile.Normalise.Quote (Quote (..), unnormalise)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Core
import Vehicle.Data.Builtin.Interface.Normalise (NormalisableBuiltin)
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Code.Value
import Vehicle.Data.DSL
import Vehicle.Prelude.Logging (SilentLoggerT, runSilentLoggerT)
import Vehicle.Syntax.Builtin (BuiltinType (..))
import Prelude hiding (pi)

typingErrorDetails ::
  forall builtin.
  (Eq builtin, PrintableBuiltin builtin, NormalisableBuiltin builtin) =>
  TypingError builtin ->
  UserError
typingErrorDetails = \case
  MissingExplicitArg err -> missingExplicitArgError err
  FunctionTypeMismatch err -> functionTypeMismatchError err
  FailedUnificationConstraints err -> failedUnificationConstraintsError err
  FailedInstanceConstraint err -> failedInstanceConstraintError err
  RelevantUseOfIrrelevantVariable err -> relevantUseOfIrrelevantVariableError err
  FailedIndexConstraintTooBig ctx v n ->
    UserError
      { provenance = provenanceOf ctx,
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
    UserError
      { provenance = provenanceOf ctx,
        problem =
          "unable to determine if"
            <+> squotes (prettyFriendly (WithContext v (namedBoundCtxOf ctx)))
            <+> "is a valid index of size"
            <+> squotes (prettyFriendly (WithContext t (namedBoundCtxOf ctx)))
            <> ".",
        fix = Nothing
      }
  UnsolvedConstraints cs ->
    UserError
      { provenance = provenanceOf ctx,
        problem = constraintOriginMessage,
        fix = Just "try adding more type annotations"
      }
    where
      WithContext constraint ctx = NonEmpty.head cs
      nameCtx = namedBoundCtxOf ctx

      constraintOriginMessage = case constraint of
        UnificationConstraint (Unify origin _ _) -> case origin of
          CheckingExprType CheckingExpr {..} ->
            "expected"
              <+> ( case checkedExpr of
                      Left binder -> "variable" <+> quotePretty binder
                      Right expr -> squotes (prettyUnificationConstraintOriginExpr ctx expr)
                  )
              <+> "to be of type"
              <+> squotes (prettyFriendly $ WithContext checkedExprExpectedType nameCtx)
              <+> "but was unable to prove it."
          CheckingInstanceType instanceOrigin ->
            instanceOriginConstraintMessage instanceOrigin
        InstanceConstraint (Resolve instanceOrigin _ _ _) -> instanceOriginConstraintMessage instanceOrigin
        -- AuxiliaryConstraint (Auxiliary auxiliaryOrigin _ _) -> auxiliaryOriginConstraintMessage auxiliaryOrigin
        ApplicationConstraint {} ->
          "unsolved application constraint: " <+> prettyFriendly (WithContext constraint ctx)

      instanceOriginConstraintMessage = \case
        InstanceArgOrigin ArgOrigin {..} ->
          "insufficient information to find a valid type for the overloaded expression"
            <+> squotes (prettyTypeClassConstraintOriginExpr ctx checkedInstanceOp checkedInstanceOpArgs)
        InstanceTypeRestrictionOrigin {} -> developerError "Unexpected type-restriction error"
  UnsolvedMetas _ ms ->
    UserError
      { provenance = p,
        problem = "Unable to infer type of bound variable",
        fix = Just "add more type annotations"
      }
    where
      (_, p) = NonEmpty.head ms

--------------------------------------------------------------------------------
-- Individual errors
--------------------------------------------------------------------------------
-- MissingExplicitArgError

missingExplicitArgError ::
  (PrintableBuiltin builtin) =>
  MissingExplicitArgError builtin ->
  UserError
missingExplicitArgError (MissingExplicitArgError ctx explicitBinder nonExplicitArg) = do
  let argTypeDoc = prettyFriendly $ WithContext (typeOf explicitBinder) ctx
  let argDoc = prettyFriendly $ WithContext (argExpr nonExplicitArg) ctx
  UserError
    { provenance = provenanceOf nonExplicitArg,
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
  UserError
functionTypeMismatchError (FunctionTypeMismatchError ctx fun nonPiType args) = do
  UserError
    { provenance = provenanceOf fun,
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
  UserError
relevantUseOfIrrelevantVariableError (RelevantUseOfIrrelevantVariableError _ p name) =
  UserError
    { provenance = p,
      problem = "cannot use irrelevant variable" <+> quotePretty name <+> "in an relevant context",
      fix = Nothing
    }

--------------------------------------------------------------------------------
-- FailedUnificationConstraintsError

failedUnificationConstraintsError ::
  forall builtin.
  (PrintableBuiltin builtin, NormalisableBuiltin builtin) =>
  FailedUnificationConstraintsError builtin ->
  UserError
failedUnificationConstraintsError (FailedUnificationConstraintsError freeEnv (err :| _)) = failedConstraintMessage err
  where
    failedConstraintMessage :: WithContext (UnificationConstraint builtin) -> UserError
    failedConstraintMessage (WithContext (Unify origin e1 e2) ctx) = do
      let boundCtx = boundContextOf ctx
      let namedBoundCtx = toNamedBoundCtx boundCtx
      let originMessage = case origin of
            CheckingExprType CheckingExpr {..} -> do
              let normActualType = runNorm $ eval freeEnv namedBoundCtx (boundContextToEnv boundCtx) checkedExprActualType
              "expected"
                <+> ( case checkedExpr of
                        Left binder -> "variable" <+> quotePretty binder
                        Right expr -> squotes (prettyUnificationConstraintOriginExpr ctx expr)
                    )
                <+> "to be of type"
                <+> squotes (prettyFriendly (WithContext checkedExprExpectedType namedBoundCtx))
                <+> "but was found to be of type"
                <+> squotes (prettyFriendly (WithContext normActualType namedBoundCtx))
            CheckingInstanceType (InstanceArgOrigin ArgOrigin {..}) ->
              "unable to find a consistent type for the overloaded expression"
                <+> squotes (prettyTypeClassConstraintOriginExpr ctx checkedInstanceOp checkedInstanceOpArgs)
            CheckingInstanceType (InstanceTypeRestrictionOrigin {}) ->
              ""
      UserError
        { provenance = provenanceOf ctx,
          problem =
            originMessage
              <> "."
                <+> "In particular"
                <+> squotes (prettyFriendly (WithContext e1 namedBoundCtx))
                <+> "is not equal to"
                <+> squotes (prettyFriendly (WithContext e2 namedBoundCtx))
              <> ".",
          fix = Just "check your types"
        }

--------------------------------------------------------------------------------
-- FailedInstanceConstraintError

failedInstanceConstraintError ::
  forall builtin.
  (Eq builtin, NormalisableBuiltin builtin, PrintableBuiltin builtin) =>
  FailedInstanceConstraintError builtin ->
  UserError
failedInstanceConstraintError (FailedInstanceConstraintError freeEnv (WithContext constraint ctx) candidates) =
  case instanceOrigin constraint of
    InstanceTypeRestrictionOrigin t -> typeRestrictionError ctx t candidates
    InstanceArgOrigin t -> instanceArgOriginError freeEnv ctx t candidates

typeRestrictionError ::
  (Eq builtin, NormalisableBuiltin builtin) =>
  ConstraintContext builtin ->
  InstanceTypeRestrictionOrigin builtin ->
  [(WithContext (InstanceCandidate builtin), UnAnnDoc)] ->
  UserError
typeRestrictionError ctx (TypeRestrictionOrigin freeEnv (ident, p) sort typ) _candidates = do
  let gluedType = Glued typ (runNorm $ eval freeEnv (namedBoundCtxOf ctx) emptyBoundEnv typ)
  UserError
    { provenance = p,
      problem =
        unsupportedAnnotationTypeDescription (pretty sort) ident gluedType
          <> "."
            <+> "The possible valid types for"
            <+> quotePretty sort
            <+> "annotated declarations are:"
          <> line
          <> indent 2 (prettyAllowedTypes supportedTypes),
      fix =
        Just $
          "change the type of"
            <+> prettyIdentName ident
            <+> "to a supported type"
    }
  where
    supportedTypes = case sort of
      RestrictedProperty -> ["Bool", "Vector Bool n", "Tensor Bool ns"]
      RestrictedParameter Inferable -> [pretty NatType]
      RestrictedParameter NonInferable -> map pretty [BoolType, IndexType, NatType, RatType]
      RestrictedDataset -> ["List A    " <+> datasetElementTypes, "Vector A n" <+> datasetElementTypes]
      RestrictedNetwork -> ["Tensor Rat [a_1, ..., a_n] -> Tensor Rat [b_1, ..., b_n]  (where 'a_i' and 'b_i' are all constants at compile time)"]

    datasetElementTypes = "(where A is either `Index n`, `Nat`, `Rat`, `List A`, `Vector A n`)"

    prettyAllowedTypes :: [Doc a] -> Doc a
    prettyAllowedTypes ts = vsep ((\(t, no) -> pretty no <> "." <+> t) <$> zip ts [1 :: Int ..])

instanceArgOriginError ::
  forall builtin.
  (PrintableBuiltin builtin, NormalisableBuiltin builtin) =>
  FreeEnv builtin ->
  ConstraintContext builtin ->
  InstanceArgOrigin builtin ->
  [(WithContext (InstanceCandidate builtin), UnAnnDoc)] ->
  UserError
instanceArgOriginError freeEnv ctx (ArgOrigin tcOp tcOpArgs tcOpType _tc) candidates =
  UserError
    { provenance = provenanceOf ctx,
      problem =
        "unable to work out a valid type for the overloaded expression"
          <+> originExpr
          <> "."
          <> line
          <> "The possible options explored were:"
          <> line
          <> indent 2 (vsep (fmap candidateOpType (zip [1 ..] candidates))),
      fix = Nothing
    }
  where
    originExpr :: Doc a
    originExpr = squotes (prettyTypeClassConstraintOriginExpr ctx tcOp tcOpArgs)

    actualArgs = if isCoercionExpr tcOp then tcOpArgs else []

    -- This assumes that the parameters of the type-class instance are the first arguments of the type-class operation.
    -- e.g. if `HasAdd t1 t2 t3` then `add` has type `forall {t1 t2 t3} . X`. If this is not the case then this function
    -- will not work.
    candidateOpType :: (Int, (WithContext (InstanceCandidate builtin), UnAnnDoc)) -> UnAnnDoc
    candidateOpType (no, (candidate, err)) = do
      let (candidateTypeArgs, solutionCtx) = calculateInstanceCandidateTypeArgs candidate
      let finalTypeDoc = calculateInstanceDisplayType freeEnv solutionCtx tcOpType candidateTypeArgs actualArgs
      pretty no
        <> "." <+> finalTypeDoc
        <> line
        <> indent 2 ("- rejected:" <+> err)

calculateInstanceCandidateTypeArgs ::
  forall builtin.
  (PrintableBuiltin builtin) =>
  WithContext (InstanceCandidate builtin) ->
  ([Arg builtin], BoundCtx (Expr builtin))
calculateInstanceCandidateTypeArgs (WithContext candidate typingCtx) =
  calculateCandidateType typingCtx (candidateExpr candidate)
  where
    calculateCandidateType :: BoundCtx (Expr builtin) -> Expr builtin -> ([Arg builtin], BoundCtx (Expr builtin))
    calculateCandidateType dbCtx = \case
      Builtin _ _tc -> ([], dbCtx)
      App (Builtin _ _tc) args ->
        (NonEmpty.toList args, dbCtx)
      Pi _ binder result ->
        calculateCandidateType (binder : dbCtx) result
      t -> developerError $ "UNSUPPORTED PRINTING" <+> prettyVerbose t

calculateInstanceDisplayType ::
  forall builtin a.
  (NormalisableBuiltin builtin, PrintableBuiltin builtin) =>
  FreeEnv builtin ->
  BoundCtx (Type builtin) ->
  Type builtin ->
  [Arg builtin] ->
  [Arg builtin] ->
  Doc a
calculateInstanceDisplayType freeEnv boundCtx fullType actualArgs typingArgs = do
  let normFullType = runNorm $ eval freeEnv (toNamedBoundCtx boundCtx) (boundContextToEnv boundCtx) fullType
  let opArgs = mergeArgs actualArgs typingArgs
  instantiateTelescope boundCtx normFullType opArgs
  where
    -- This is a complete hack
    mergeArgs :: [Arg builtin] -> [Arg builtin] -> [(Arg builtin, Bool)]
    mergeArgs args1 [] = fmap (,True) args1
    mergeArgs [] args2 = fmap (,False) args2
    mergeArgs (arg1 : args1) (arg2 : args2) = do
      let arg
            | isAbstract arg2 = (arg1, True)
            | isAbstract arg1 = (arg2, False)
            | otherwise = (arg1, True)
      arg : mergeArgs args1 args2
      where
        isAbstract :: Arg builtin -> Bool
        isAbstract (argExpr -> e) = case e of
          Hole {} -> True
          BoundVar {} -> True
          _ -> False

    instantiateTelescope :: BoundCtx (Type builtin) -> VType builtin -> [(Arg builtin, Bool)] -> Doc a
    instantiateTelescope ctx typ arguments = case (typ, arguments) of
      (VPi binder _, [])
        | isExplicit binder ->
            prettyFriendly (WithContext typ (toNamedBoundCtx ctx))
      (VPi binder (Closure env body), args) -> do
        let (alterEnv, remainingArgs) = findRemainingArgs ctx binder args
        let recType = runNorm $ eval freeEnv (toNamedBoundCtx ctx) (alterEnv env) body
        let unnormBinder = quote mempty (boundCtxLv ctx) binder
        instantiateTelescope (unnormBinder : ctx) recType remainingArgs
      (_, []) -> prettyFriendly (WithContext typ (toNamedBoundCtx ctx))
      _ -> "Malformed type-class operation type" <+> prettyVerbose typ <+> "and args" <+> prettyVerbose (fmap fst arguments)

    findRemainingArgs :: BoundCtx (Type builtin) -> VBinder binder -> [(Arg builtin, Bool)] -> (BoundEnv builtin -> BoundEnv builtin, [(Arg builtin, Bool)])
    findRemainingArgs ctx binder args = case args of
      [] -> (extendEnvWithBound (boundCtxLv ctx) binder, [])
      ((arg, fromCandidate) : remainingArgs)
        | visibilityOf arg == visibilityOf binder || fromCandidate -> do
            let normArg = runNorm $ eval freeEnv (toNamedBoundCtx ctx) (boundContextToEnv ctx) (argExpr arg)
            (extendEnvWithDefined normArg binder, remainingArgs)
        | isExplicit binder -> developerError "Missing explicit argument when printing"
        | otherwise -> (extendEnvWithBound (boundCtxLv ctx) binder, args)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

prettyTypeClassConstraintOriginExpr ::
  (PrintableBuiltin builtin) =>
  ConstraintContext builtin ->
  Expr builtin ->
  [Arg builtin] ->
  Doc a
prettyTypeClassConstraintOriginExpr ctx fun args = do
  let expr = case fun of
        -- We don't want to print out the actual coercion functions as the user is
        -- oblivious to them. Instead we want to print out what they are applied to.
        Builtin _ b -> case coercionArgs b of
          Just f -> f args
          Nothing -> fun
        _ -> fun
  prettyFriendly $ WithContext expr (namedBoundCtxOf ctx)

prettyUnificationConstraintOriginExpr ::
  (PrintableBuiltin builtin) =>
  ConstraintContext builtin ->
  Expr builtin ->
  Doc a
prettyUnificationConstraintOriginExpr ctx expr =
  prettyFriendly $ WithContext expr (namedBoundCtxOf ctx)

runNorm :: SilentLoggerT Identity b -> b
runNorm = fst . runIdentity . runSilentLoggerT

unsupportedAnnotationTypeDescription ::
  forall builtin a.
  (Eq builtin, PrintableBuiltin builtin) =>
  Doc a ->
  Identifier ->
  GluedType builtin ->
  Doc a
unsupportedAnnotationTypeDescription annotation ident resourceType = do
  let unreducedResourceType = unnormalised resourceType
  let reducedResourceType = (unnormalise 0 (normalised resourceType) :: Expr builtin)
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
