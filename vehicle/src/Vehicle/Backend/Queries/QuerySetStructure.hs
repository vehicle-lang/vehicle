module Vehicle.Backend.Queries.QuerySetStructure
  ( SeriousPropertyError (..),
    PropertyError (..),
    UnreducedAssertion (..),
    compileQueryStructure,
    eliminateNot,
    vectorStructureOperations,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.HashSet qualified as HashSet (fromList, intersection, null)
import Data.Map qualified as Map (keysSet)
import Data.Set (Set)
import Data.Set qualified as Set (fromList, intersection, map, null)
import Vehicle.Backend.Queries.IfElimination (eliminateIfs, unfoldIf)
import Vehicle.Backend.Queries.LinearExpr (UnreducedAssertion (..), VectorEquality (..))
import Vehicle.Backend.Queries.UsedFunctions
import Vehicle.Backend.Queries.Variable
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Data.BooleanExpr
import Vehicle.Data.BuiltinInterface
import Vehicle.Data.BuiltinPatterns
import Vehicle.Data.NormalisedExpr
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (..), findStdLibFunction, fromFiniteQuantifier)
import Vehicle.Verify.Core

--------------------------------------------------------------------------------
-- Main function

-- | Attempts to compile an arbitrary expression of type `Bool` whose top-most
-- quantifier is an `exists` down to a set of quantifiers over propositions in
-- disjunctive normal form which are guaranteed to not contain if-statements.
-- Only reduces quantified variables of `Vector` type as much as is required
-- in order to be able to reach DNF.
compileQueryStructure ::
  (MonadCompile m, MonadFreeContext Builtin m) =>
  DeclProvenance ->
  UsedFunctionsCtx ->
  NetworkContext ->
  WHNFValue Builtin ->
  m (Either SeriousPropertyError (UserVariableCtx, MaybeTrivial (BooleanExpr UnreducedAssertion), VariableNormalisationSteps))
compileQueryStructure declProv queryFreeCtx networkCtx expr =
  logCompilerPass MinDetail "compilation of boolean structure" $ do
    result <- runReaderT (compileBoolExpr mempty expr) (declProv, queryFreeCtx, networkCtx)
    case result of
      Left (TemporaryError err) ->
        compilerDeveloperError $ "Something went wrong in query compilation:" <+> pretty err
      Left (SeriousError err) ->
        return $ Left err
      Right (ctx, boolExpr, steps) ->
        return $ Right (reverse ctx, boolExpr, steps)

--------------------------------------------------------------------------------
-- Data

-- | The set of variables that will be cumulatively in scope at the current
-- point in time once all existential quantifiers have been lifted to the
-- top level. Essentially the set of quantifiers that are before the current
-- point in the tree when traversed depth first. These are ordered in appearance
-- order and therefore are *not* a bound context.
type CumulativeCtx = UserVariableCtx

type RevGlobalCtx = [UserVariable]

cumulativeVarsToCtx :: CumulativeCtx -> BoundCtx Builtin
cumulativeVarsToCtx = variableCtxToBoundCtx

-- | The only time we should be throwing this error is because we haven't sufficiently
-- normalised a user quantified variable above us in the tree, e.g. in the following
--
--   forall (xs : Vector Rat 2) . fold and True (map (\x -> x >= 2) xs)
--
-- we can't normalise the `map` and hence the `fold` until we normalise the quantified
-- variable `xs` to two separate quantifiers.
data TemporaryPropertyError
  = CannotEliminateIfs (BoundCtx Builtin) (WHNFValue Builtin)
  | CannotEliminateNot (BoundCtx Builtin) (WHNFValue Builtin)
  | NonBooleanQueryStructure (BoundCtx Builtin) (WHNFValue Builtin)
  deriving (Show)

instance Pretty TemporaryPropertyError where
  pretty = \case
    CannotEliminateIfs ctx e -> "CannotEliminateIfs[" <+> prettyFriendly (WithContext e ctx) <+> "]"
    CannotEliminateNot ctx e -> "CannotEliminateNot[" <+> prettyFriendly (WithContext e ctx) <+> "]"
    NonBooleanQueryStructure ctx e -> "NonBooleanQueryStructure[" <+> prettyFriendly (WithContext e ctx) <+> "]"

-- | Serious errors that are fundemental problems with the specification
data SeriousPropertyError
  = AlternatingQuantifiers
  | NonLinearSpecification (WHNFValue Builtin)
  | UnsupportedQuantifierType (WHNFBinder Builtin) (WHNFType Builtin)

data PropertyError
  = TemporaryError TemporaryPropertyError
  | SeriousError SeriousPropertyError

type QueryStructureResult =
  Either PropertyError (RevGlobalCtx, MaybeTrivial (BooleanExpr UnreducedAssertion), VariableNormalisationSteps)

-- | Pattern matches on a vector equality in the standard library.
isVectorEquals ::
  WHNFValue Builtin ->
  Maybe (WHNFArg Builtin, WHNFArg Builtin, TensorDimensions, WHNFArg Builtin -> WHNFArg Builtin -> WHNFValue Builtin)
isVectorEquals = \case
  VFreeVar ident args
    | ident == identifierOf StdEqualsVector -> case args of
        [t1, t2, dim, sol, e1, e2] -> do
          d <- getNatLiteral $ argExpr dim
          ds <- getTensorDimensions $ argExpr t1
          let dims = d : ds
          let mkOp e1' e2' = VFreeVar ident [t1, t2, dim, sol, e1', e2']
          Just (e1, e2, dims, mkOp)
        _ -> Nothing
  _ -> Nothing

getTensorDimensions :: WHNFValue Builtin -> Maybe TensorDimensions
getTensorDimensions = \case
  VRatType -> Just []
  VVectorType tElem dim -> do
    dims <- getTensorDimensions tElem
    d <- getNatLiteral dim
    Just $ d : dims
  _ -> Nothing

--------------------------------------------------------------------------------
-- Algorithm

type MonadQueryStructure m =
  ( MonadCompile m,
    MonadReader (DeclProvenance, UsedFunctionsCtx, NetworkContext) m,
    MonadFreeContext Builtin m
  )

-- | The set of vector operations that we sometimes want to avoid normalising
-- out in the property for efficiency reasons.
vectorStructureOperations :: Set StdLibFunction
vectorStructureOperations =
  Set.fromList
    [ StdAddVector,
      StdSubVector,
      StdEqualsVector,
      StdNotEqualsVector
    ]

-- | The set of finite quantifier operations that we sometimes want to avoid normalising
-- out in the property for efficiency reasons.
finiteQuantifierFunctions :: Set StdLibFunction
finiteQuantifierFunctions =
  Set.fromList
    [ StdForallIndex,
      StdExistsIndex
    ]

queryStructureNBEOptions :: NBEOptions
queryStructureNBEOptions = mkNBEOptions (vectorStructureOperations <> finiteQuantifierFunctions)

compileBoolExpr ::
  forall m.
  (MonadQueryStructure m) =>
  CumulativeCtx ->
  WHNFValue Builtin ->
  m QueryStructureResult
compileBoolExpr = go False
  where
    -- \| Traverses an arbitrary expression of type `Bool`.
    go :: Bool -> CumulativeCtx -> WHNFValue Builtin -> m QueryStructureResult
    go alreadyLiftedIfs quantifiedVariables expr =
      case expr of
        ----------------
        -- Base cases --
        ----------------
        VBoolLiteral b ->
          return $ Right ([], Trivial b, [])
        VBuiltinFunction op@(Equals _ eq) (reverse -> e2 : e1 : args) -> do
          -- `reverse` pattern match above is needed as EqIndex has implicit arguments.
          -- and not matching them causes issue #712.
          let mkOp e1' e2' = VBuiltinFunction op (args <> [e1', e2'])
          compileEquality quantifiedVariables alreadyLiftedIfs e1 eq e2 [] mkOp
        VBuiltinFunction Order {} _ -> do
          compileOrder alreadyLiftedIfs quantifiedVariables expr
        (isVectorEquals -> Just (e1, e2, dims, mkOp)) -> do
          compileEquality quantifiedVariables alreadyLiftedIfs e1 Eq e2 dims mkOp

        ---------------------
        -- Recursive cases --
        ---------------------
        VBuiltinFunction And [e1, e2] ->
          compileOp2 (andTrivial Conjunct) alreadyLiftedIfs quantifiedVariables (argExpr e1) (argExpr e2)
        VBuiltinFunction Or [e1, e2] ->
          compileOp2 (orTrivial Disjunct) alreadyLiftedIfs quantifiedVariables (argExpr e1) (argExpr e2)
        VBuiltinFunction Not [e] ->
          -- As the expression is of type `Bool` we can try lowering the `not` down
          -- through the expression.
          case eliminateNot (argExpr e) of
            Just result -> go alreadyLiftedIfs quantifiedVariables result
            Nothing -> do
              let err = CannotEliminateNot (cumulativeVarsToCtx quantifiedVariables) expr
              return $ Left $ TemporaryError err
        VBuiltinFunction If [_, c, x, y] -> do
          -- As the expression is of type `Bool` we can immediately unfold the `if`.
          let unfoldedExpr = unfoldIf c (argExpr x) (argExpr y)
          go alreadyLiftedIfs quantifiedVariables unfoldedExpr
        VInfiniteQuantifier q _ binder env body -> case q of
          -- If we're at a `Forall` we know we must have alternating quantifiers.
          Forall -> return $ Left $ SeriousError AlternatingQuantifiers
          -- Otherwise try to compile away the quantifier.
          Exists -> compileInfiniteQuantifier quantifiedVariables binder env body
        VFiniteQuantifier q args binder env body ->
          compileFiniteQuantifier quantifiedVariables q args binder env body
        ------------
        -- Errors --
        ------------
        _ -> do
          let err = NonBooleanQueryStructure (cumulativeVarsToCtx quantifiedVariables) expr
          return $ Left $ TemporaryError err

    compileEquality ::
      CumulativeCtx ->
      Bool ->
      WHNFArg Builtin ->
      EqualityOp ->
      WHNFArg Builtin ->
      TensorDimensions ->
      (WHNFArg Builtin -> WHNFArg Builtin -> WHNFValue Builtin) ->
      m QueryStructureResult
    compileEquality quantifiedVariables alreadyLiftedIfs lhs eq rhs dims mkRel = do
      let expr = mkRel lhs rhs
      let ctx = cumulativeVarsToCtx quantifiedVariables
      logDebug MaxDetail $ "Identified (in)equality:" <+> prettyFriendly (WithContext expr ctx)

      maybeResult <- elimIfs alreadyLiftedIfs quantifiedVariables expr
      case maybeResult of
        Just result -> return result
        Nothing -> do
          let assertion =
                if eq == Eq && null dims
                  then VectorEqualityAssertion $ VectorEquality (argExpr lhs) (argExpr rhs) dims mkRel
                  else NonVectorEqualityAssertion expr
          return $ Right ([], NonTrivial $ Query assertion, [])

    compileOrder :: Bool -> CumulativeCtx -> WHNFValue Builtin -> m QueryStructureResult
    compileOrder alreadyLiftedIfs quantifiedVariables expr = do
      -- Even though we're sticking the result in a `NonVectorEqualityAssertion` we still need
      -- to lift and eliminate `if`s as they may be inside network applications.
      maybeResult <- elimIfs alreadyLiftedIfs quantifiedVariables expr
      case maybeResult of
        Just result -> return result
        Nothing -> do
          let assertion = NonVectorEqualityAssertion expr
          return $ Right ([], NonTrivial $ Query assertion, [])

    elimIfs :: Bool -> CumulativeCtx -> WHNFValue Builtin -> m (Maybe QueryStructureResult)
    elimIfs alreadyLiftedIfs quantifiedVariables expr
      | alreadyLiftedIfs = return Nothing
      | otherwise = do
          let ctx = cumulativeVarsToCtx quantifiedVariables
          incrCallDepth
          ifLessResult <- eliminateIfs expr
          result <- case ifLessResult of
            Nothing -> return Nothing
            Just Nothing -> do
              let err = CannotEliminateIfs ctx expr
              return $ Just $ Left $ TemporaryError err
            Just (Just exprWithoutIf) -> do
              logDebug MaxDetail $ "If-lifted to:" <+> prettyFriendly (WithContext exprWithoutIf ctx)
              let env = variableCtxToEnv quantifiedVariables
              normExprWithoutIf <- reeval defaultNBEOptions env exprWithoutIf
              logDebug MaxDetail $ "Normalised to:" <+> prettyFriendly (WithContext normExprWithoutIf ctx)
              Just <$> go True quantifiedVariables normExprWithoutIf
          decrCallDepth
          return result

    compileOp2 ::
      (forall a. MaybeTrivial (BooleanExpr a) -> MaybeTrivial (BooleanExpr a) -> MaybeTrivial (BooleanExpr a)) ->
      Bool ->
      CumulativeCtx ->
      WHNFValue Builtin ->
      WHNFValue Builtin ->
      m QueryStructureResult
    compileOp2 op processingLiftedIfs quantifiedVariables e1 e2 = do
      let lhsVariables = quantifiedVariables
      lhsResult <- go processingLiftedIfs lhsVariables e1
      case lhsResult of
        Left err -> return $ Left err
        Right (lhsUserVars, e1', lhsReconstruction) -> do
          let rhsVariables = lhsUserVars <> quantifiedVariables
          rhsResult <- go processingLiftedIfs rhsVariables e2
          case rhsResult of
            Left err -> return $ Left err
            Right (rhsUserVars, e2', rhsReconstruction) -> do
              let userVars = lhsUserVars <> rhsUserVars
              return $ Right (userVars, op e1' e2', lhsReconstruction <> rhsReconstruction)

--------------------------------------------------------------------------------
-- Not elimination

eliminateNot :: WHNFValue Builtin -> Maybe (WHNFValue Builtin)
eliminateNot arg = case arg of
  -- Base cases
  VBoolLiteral b -> Just $ VBoolLiteral (not b)
  VBuiltinFunction (Order dom ord) args -> Just $ VBuiltinFunction (Order dom (neg ord)) args
  VBuiltinFunction (Equals dom eq) args -> Just $ VBuiltinFunction (Equals dom (neg eq)) args
  VBuiltinFunction Not [e] -> Just $ argExpr e
  -- Inductive cases
  VBuiltinFunction Or args -> do
    args' <- traverse (traverse eliminateNot) args
    return $ VBuiltinFunction And args'
  VBuiltinFunction And args -> do
    args' <- traverse (traverse eliminateNot) args
    return $ VBuiltinFunction Or args'
  VBuiltinFunction If [t, c, e1, e2] -> do
    e1' <- traverse eliminateNot e1
    e2' <- traverse eliminateNot e2
    return $ VBuiltinFunction If [t, c, e1', e2']
  VFreeVar ident (a : b : n : t : args) -> case findStdLibFunction ident of
    Just StdEqualsVector -> do
      t' <- traverse eliminateNot t
      return $ VFreeVar (identifierOf StdNotEqualsVector) (a : b : n : t' : args)
    Just StdNotEqualsVector -> do
      t' <- traverse eliminateNot t
      return $ VFreeVar (identifierOf StdEqualsVector) (a : b : n : t' : args)
    _ -> Nothing
  -- Quantifier cases
  -- We can't actually lower the `not` throw the body of the quantifier as
  -- the body is not yet unnormalised. However, it's fine to stop here as we'll
  -- simply continue to normalise it once we re-encounter it again after
  -- normalising the quantifier.
  VInfiniteQuantifier q args binder env body -> do
    let p = mempty
    let negatedBody = NotExpr p [Arg p Explicit Relevant body]
    Just $ VInfiniteQuantifier (neg q) args binder env negatedBody
  VFiniteQuantifier q args binder env body -> do
    let p = mempty
    let negatedBody = NotExpr p [Arg p Explicit Relevant body]
    Just $ VFiniteQuantifier (neg q) args binder env negatedBody
  -- Errors
  _ -> Nothing

--------------------------------------------------------------------------------
-- Finite quantifier elimination

compileFiniteQuantifier ::
  (MonadQueryStructure m) =>
  CumulativeCtx ->
  Quantifier ->
  WHNFSpine Builtin ->
  WHNFBinder Builtin ->
  WHNFEnv Builtin ->
  Expr Ix Builtin ->
  m QueryStructureResult
compileFiniteQuantifier quantifiedVariables q quantSpine binder env body = do
  canLeaveUnexpanded <- canLeaveFiniteQuantifierUnexpanded env body
  if canLeaveUnexpanded
    then do
      logDebug MidDetail $ "Keeping folded finite quantifier:" <+> pretty q <+> prettyVerbose binder
      let foldedExpr = VFiniteQuantifier q quantSpine binder env body
      let unnormalisedAssertion = NonVectorEqualityAssertion foldedExpr
      return $ Right (mempty, NonTrivial $ Query unnormalisedAssertion, mempty)
    else do
      logDebug MaxDetail $ "Unfolding finite quantifier:" <+> pretty q <+> prettyVerbose binder
      quantImplementation <- lookupIdentValueInEnv defaultNBEOptions env (fromFiniteQuantifier q)
      let quantifierExpr = VFiniteQuantifierSpine quantSpine binder env body
      normResult <- evalApp queryStructureNBEOptions quantImplementation quantifierExpr
      compileBoolExpr quantifiedVariables normResult

-- | This is a sound, inexpensive, but incomplete, check for whether
-- we can leave the finite quantifier folded as it will only contain
-- conjunctions.
canLeaveFiniteQuantifierUnexpanded ::
  (MonadQueryStructure m) =>
  WHNFEnv Builtin ->
  Expr Ix Builtin ->
  m Bool
canLeaveFiniteQuantifierUnexpanded env expr = do
  (_, usedFunctionsCtx, networkCtx) <- ask

  let (usedFunctions, usedFreeVars) = getUsedFunctions usedFunctionsCtx (getUsedFunctionsCtx usedFunctionsCtx env) expr
  let forbiddenFunctions =
        HashSet.fromList
          [ Quantifier Exists,
            Quantifier Forall
          ]

  let doesNotHaveDisjunction = HashSet.null (HashSet.intersection usedFunctions forbiddenFunctions)
  let networks = Set.map (Identifier User) (Map.keysSet networkCtx)
  let doesNotHaveNetwork = Set.null (Set.intersection usedFreeVars networks)
  return $ doesNotHaveDisjunction && doesNotHaveNetwork

--------------------------------------------------------------------------------
-- Infinite quantifier elimination

compileInfiniteQuantifier ::
  (MonadQueryStructure m) =>
  CumulativeCtx ->
  WHNFBinder Builtin ->
  WHNFEnv Builtin ->
  Expr Ix Builtin ->
  m QueryStructureResult
compileInfiniteQuantifier quantifiedVariables binder env body = do
  let variableName = getBinderName binder
  let variableDoc = "variable" <+> quotePretty variableName
  logCompilerPass MidDetail ("compilation of quantified" <+> variableDoc) $ do
    let isDuplicateName = any (\v -> userVarName v == variableName) quantifiedVariables
    if isDuplicateName
      then do
        (declProv, _, _) <- ask
        throwError $ DuplicateQuantifierNames declProv variableName
      else do
        let currentLevel = Lv $ length quantifiedVariables
        case calculateVariableDimensions binder of
          Left err -> return $ Left err
          Right tensorDimensions -> do
            let unnormalisedVar = UserVariable variableName tensorDimensions variableName

            -- First of all optimistically try not to normalise the quantified variable.
            let optimisticEnv = extendEnvWithBound binder env
            optimisiticBody <- eval queryStructureNBEOptions optimisticEnv body

            let unreducedPassDoc = "Trying without reducing dimensions of" <+> variableDoc
            optimisticResult <-
              logCompilerSection MidDetail unreducedPassDoc $
                compileBoolExpr (unnormalisedVar : quantifiedVariables) optimisiticBody

            case optimisticResult of
              -- If we can compile the structure without normalising then great!
              Right (allQuantifiedVariables, substructure, variableReconstruction) -> do
                logDebug MidDetail $ "Succeeded without reducing dimensions of" <+> variableDoc
                return $ Right (unnormalisedVar : allQuantifiedVariables, substructure, variableReconstruction)

              -- If we hit a serious error then immediately return as further normalisation of
              -- the quantified variable won't help.
              Left err@SeriousError {} ->
                return $ Left err
              -- Otherwise we only a hit a temporary error so we try again normalising the
              -- user variable more aggressively.
              Left (TemporaryError e) -> do
                logDebug MidDetail $ "Failed to compile without reducing dimensions of" <+> variableDoc
                logDebug MidDetail $ indent 2 ("Reason:" <+> pretty e) <> line
                let (newQuantifiedVariables, envEntry) = reduceVariable currentLevel unnormalisedVar
                let newEnv = extendEnvWithDefined envEntry binder env

                let updatedQuantifiedVars = newQuantifiedVariables <> quantifiedVariables

                normBody <- eval queryStructureNBEOptions newEnv body

                let reducedPassDoc = "Compiling while reducing dimensions of" <+> quotePretty variableName
                normalisedResult <-
                  logCompilerSection MidDetail reducedPassDoc $
                    compileBoolExpr updatedQuantifiedVars normBody

                case normalisedResult of
                  Left err -> return $ Left err
                  Right (subQuantifiedVariables, substructure, varReconstruction) -> do
                    let normalisationStep = Reduce (UserVar unnormalisedVar)
                    let allQuantifiedVariables = newQuantifiedVariables <> subQuantifiedVariables
                    return $ Right (allQuantifiedVariables, substructure, normalisationStep : varReconstruction)

calculateVariableDimensions :: WHNFBinder Builtin -> Either PropertyError TensorDimensions
calculateVariableDimensions binder = go (typeOf binder)
  where
    go :: WHNFType Builtin -> Either PropertyError TensorDimensions
    go = \case
      VVectorType tElem (VNatLiteral dim) -> do
        dims <- go tElem
        return $ dim : dims
      t -> goBase t

    goBase :: WHNFType Builtin -> Either PropertyError TensorDimensions
    goBase = \case
      VRatType {} -> return []
      baseType -> Left $ SeriousError $ UnsupportedQuantifierType binder baseType
