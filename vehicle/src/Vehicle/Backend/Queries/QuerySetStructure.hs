module Vehicle.Backend.Queries.QuerySetStructure
  ( SeriousPropertyError (..),
    PropertyError (..),
    UnreducedAssertion (..),
    compileQueryStructure,
    eliminateNot,
    UsedFunctionsCtx,
    UsedFunctionsInfo,
    getUsedFunctions,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet (fromList, intersection, null, singleton)
import Data.Map qualified as Map (keysSet, lookup)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set (intersection, map, null, singleton)
import Vehicle.Backend.Queries.IfElimination (eliminateIfs, unfoldIf)
import Vehicle.Backend.Queries.LinearExpr (UnreducedAssertion (..), VectorEquality (..))
import Vehicle.Backend.Queries.Variable
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Expr.Boolean
import Vehicle.Expr.BuiltinInterface
import Vehicle.Expr.BuiltinPatterns
import Vehicle.Expr.DeBruijn (dbLevelToIndex)
import Vehicle.Expr.Normalised
import Vehicle.Libraries.StandardLibrary (StdLibFunction (..), findStdLibFunction, fromFiniteQuantifier)
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
  Value Builtin ->
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
  = CannotEliminateIfs (Value Builtin)
  | CannotEliminateNot (Value Builtin)
  | NonBooleanQueryStructure (Value Builtin)
  deriving (Show)

instance Pretty TemporaryPropertyError where
  pretty = \case
    CannotEliminateIfs e -> "CannotEliminateIfs[" <+> prettyVerbose e <+> "]"
    CannotEliminateNot e -> "CannotEliminateNot[" <+> prettyVerbose e <+> "]"
    NonBooleanQueryStructure e -> "NonBooleanQueryStructure[" <+> prettyVerbose e <+> "]"

-- | Serious errors that are fundemental problems with the specification
data SeriousPropertyError
  = AlternatingQuantifiers
  | NonLinearSpecification (Value Builtin)
  | UnsupportedQuantifierType (VBinder Builtin) (VType Builtin)
  | UnsupportedInequalityOp

data PropertyError
  = TemporaryError TemporaryPropertyError
  | SeriousError SeriousPropertyError

type QueryStructureResult =
  Either PropertyError (RevGlobalCtx, MaybeTrivial (BooleanExpr UnreducedAssertion), VariableNormalisationSteps)

-- | Pattern matches on a vector equality in the standard library.
isVectorEquals ::
  Value Builtin ->
  Maybe (VArg Builtin, VArg Builtin, TensorDimensions, VArg Builtin -> VArg Builtin -> Value Builtin)
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

getTensorDimensions :: Value Builtin -> Maybe TensorDimensions
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

-- | For efficiency reasons, we sometimes want to avoid expanding out
-- finite quantifiers
evalWhilePreservingFiniteQuantifiers ::
  (MonadQueryStructure m) =>
  Env Builtin ->
  Expr Ix Builtin ->
  m (Value Builtin)
evalWhilePreservingFiniteQuantifiers env body = do
  let evalOptions = NBEOptions {evalFiniteQuantifiers = False}
  eval evalOptions env body

compileBoolExpr ::
  forall m.
  (MonadQueryStructure m) =>
  CumulativeCtx ->
  Value Builtin ->
  m QueryStructureResult
compileBoolExpr = go False
  where
    -- \| Traverses an arbitrary expression of type `Bool`.
    go :: Bool -> CumulativeCtx -> Value Builtin -> m QueryStructureResult
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
          -- As the expression is of type `Not` we can try lowering the `not` down
          -- through the expression.
          case eliminateNot (argExpr e) of
            Nothing -> return $ Left $ TemporaryError $ CannotEliminateNot expr
            Just result -> go alreadyLiftedIfs quantifiedVariables result
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
        _ -> return $ Left $ TemporaryError $ NonBooleanQueryStructure expr

    compileEquality ::
      CumulativeCtx ->
      Bool ->
      VArg Builtin ->
      EqualityOp ->
      VArg Builtin ->
      TensorDimensions ->
      (VArg Builtin -> VArg Builtin -> Value Builtin) ->
      m QueryStructureResult
    compileEquality quantifiedVariables alreadyLiftedIfs lhs eq rhs dims mkRel = do
      let ctx = cumulativeVarsToCtx quantifiedVariables
      let expr = mkRel lhs rhs
      logDebug MaxDetail $ "Identified (in)equality:" <+> prettyFriendly (WithContext expr ctx)

      maybeResult <- elimIfs alreadyLiftedIfs quantifiedVariables expr
      case maybeResult of
        Just result -> return result
        Nothing -> case eq of
          Eq -> do
            let assertion
                  | null dims = VectorEqualityAssertion $ VectorEquality (argExpr lhs) (argExpr rhs) dims mkRel
                  | otherwise = NonVectorEqualityAssertion expr
            return $ Right ([], NonTrivial $ Query assertion, [])
          Neq ->
            return $ Left $ SeriousError UnsupportedInequalityOp

    compileOrder :: Bool -> CumulativeCtx -> Value Builtin -> m QueryStructureResult
    compileOrder alreadyLiftedIfs quantifiedVariables expr = do
      -- Even though we're sticking the result in a `NonVectorEqualityAssertion` we still need
      -- to lift and eliminate `if`s as they may be inside network applications.
      maybeResult <- elimIfs alreadyLiftedIfs quantifiedVariables expr
      case maybeResult of
        Just result -> return result
        Nothing -> do
          let assertion = NonVectorEqualityAssertion expr
          return $ Right ([], NonTrivial $ Query assertion, [])

    elimIfs :: Bool -> CumulativeCtx -> Value Builtin -> m (Maybe QueryStructureResult)
    elimIfs alreadyLiftedIfs quantifiedVariables expr
      | alreadyLiftedIfs = return Nothing
      | otherwise = do
          let ctx = cumulativeVarsToCtx quantifiedVariables
          incrCallDepth
          ifLessResult <- eliminateIfs expr
          result <- case ifLessResult of
            Nothing -> return Nothing
            Just Nothing -> return $ Just $ Left $ TemporaryError $ CannotEliminateIfs expr
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
      Value Builtin ->
      Value Builtin ->
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

eliminateNot :: Value Builtin -> Maybe (Value Builtin)
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
  Spine Builtin ->
  VBinder Builtin ->
  Env Builtin ->
  Expr Ix Builtin ->
  m QueryStructureResult
compileFiniteQuantifier quantifiedVariables q quantSpine binder env body = do
  let foldedResult = do
        let foldedExpr = VFiniteQuantifier q quantSpine binder env body
        let unnormalisedAssertion = NonVectorEqualityAssertion foldedExpr
        logDebug MidDetail $ "Keeping folded:" <+> pretty q <+> prettyVerbose binder
        return $ Right (mempty, NonTrivial $ Query unnormalisedAssertion, mempty)

  canLeaveUnexpanded <- canLeaveFiniteQuantifierUnexpanded env body
  if canLeaveUnexpanded
    then foldedResult
    else do
      quantImplementation <- lookupIdentValueInEnv defaultNBEOptions (fromFiniteQuantifier q)
      let quantifierExpr = VFiniteQuantifierSpine quantSpine binder env body
      normResult <- evalApp defaultNBEOptions quantImplementation quantifierExpr
      compileBoolExpr quantifiedVariables normResult

-- | This is a sound, inexpensive, but incomplete, check for whether
-- we can leave the finite quantifier folded as it will only contain
-- conjunctions.
canLeaveFiniteQuantifierUnexpanded ::
  (MonadQueryStructure m) =>
  Env Builtin ->
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
  VBinder Builtin ->
  Env Builtin ->
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
            let optimisticEnv = extendEnv binder (VBoundVar currentLevel []) env
            optimisiticBody <- evalWhilePreservingFiniteQuantifiers optimisticEnv body

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
                let newEnv = extendEnv binder envEntry env

                let updatedQuantifiedVars = newQuantifiedVariables <> quantifiedVariables

                normBody <- evalWhilePreservingFiniteQuantifiers newEnv body

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

calculateVariableDimensions :: VBinder Builtin -> Either PropertyError TensorDimensions
calculateVariableDimensions binder = go (typeOf binder)
  where
    go :: VType Builtin -> Either PropertyError TensorDimensions
    go = \case
      VVectorType tElem (VNatLiteral dim) -> do
        dims <- go tElem
        return $ dim : dims
      t -> goBase t

    goBase :: VType Builtin -> Either PropertyError TensorDimensions
    goBase = \case
      VRatType {} -> return []
      baseType -> Left $ SeriousError $ UnsupportedQuantifierType binder baseType

--------------------------------------------------------------------------------
-- Builtin and free variable tracking

type UsedFunctionsCtx = GenericFreeCtx UsedFunctionsInfo

type UsedFunctionsInfo = (HashSet BuiltinFunction, Set Identifier)

getUsedFunctions ::
  GenericFreeCtx UsedFunctionsInfo ->
  GenericBoundCtx UsedFunctionsInfo ->
  Expr Ix Builtin ->
  UsedFunctionsInfo
getUsedFunctions freeCtx boundCtx expr = case expr of
  Universe {} -> mempty
  Meta {} -> mempty
  Hole {} -> mempty
  Pi {} -> mempty
  BoundVar _ v -> getUsedVarsBoundVar boundCtx v
  Builtin _ b -> getUsedFunctionsBuiltin b
  FreeVar _ ident -> getUsedFunctionsFreeVar freeCtx ident
  App _ fun args -> foldr (<>) (getUsedFunctions freeCtx boundCtx fun) (fmap (getUsedFunctions freeCtx boundCtx . argExpr) args)
  Let _ e1 _binder e2 -> getUsedFunctions freeCtx boundCtx e1 <> getUsedFunctions freeCtx boundCtx e2
  Lam _ _binder e -> getUsedFunctions freeCtx (mempty : boundCtx) e

getUsedNormFunctions ::
  GenericFreeCtx UsedFunctionsInfo ->
  GenericBoundCtx UsedFunctionsInfo ->
  Value Builtin ->
  UsedFunctionsInfo
getUsedNormFunctions freeCtx boundCtx expr = case expr of
  VPi {} -> mempty
  VUniverse {} -> mempty
  VMeta {} -> mempty
  VLam _ env body -> do
    let envInfo = getUsedFunctionsEnv freeCtx boundCtx env
    let bodyInfo = getUsedFunctions freeCtx (mempty : boundCtx) body
    envInfo <> bodyInfo
  VBoundVar v spine -> do
    let varInfo = getUsedVarsBoundVar boundCtx (dbLevelToIndex (Lv (length boundCtx)) v)
    let spineInfo = getUsedFunctionsSpine freeCtx boundCtx spine
    varInfo <> spineInfo
  VFreeVar ident spine -> do
    let identInfo = getUsedFunctionsFreeVar freeCtx ident
    let spineInfo = getUsedFunctionsSpine freeCtx boundCtx spine
    identInfo <> spineInfo
  VBuiltin b spine -> do
    let builtinInfo = getUsedFunctionsBuiltin b
    let spineInfo = getUsedFunctionsSpine freeCtx boundCtx spine
    builtinInfo <> spineInfo

getUsedFunctionsBuiltin ::
  Builtin ->
  UsedFunctionsInfo
getUsedFunctionsBuiltin = \case
  BuiltinFunction f -> (HashSet.singleton f, mempty)
  _ -> mempty

getUsedFunctionsFreeVar ::
  GenericFreeCtx UsedFunctionsInfo ->
  Identifier ->
  UsedFunctionsInfo
getUsedFunctionsFreeVar freeCtx ident =
  (mempty, Set.singleton ident) <> fromMaybe mempty (Map.lookup ident freeCtx)

getUsedVarsBoundVar ::
  GenericBoundCtx UsedFunctionsInfo ->
  Ix ->
  UsedFunctionsInfo
getUsedVarsBoundVar boundCtx ix =
  fromMaybe mempty (lookupIx boundCtx ix)

getUsedFunctionsSpine ::
  GenericFreeCtx UsedFunctionsInfo ->
  GenericBoundCtx UsedFunctionsInfo ->
  Spine Builtin ->
  UsedFunctionsInfo
getUsedFunctionsSpine freeCtx boundCtx =
  foldMap (getUsedNormFunctions freeCtx boundCtx . argExpr)

getUsedFunctionsEnv ::
  GenericFreeCtx UsedFunctionsInfo ->
  GenericBoundCtx UsedFunctionsInfo ->
  Env Builtin ->
  UsedFunctionsInfo
getUsedFunctionsEnv freeCtx boundCtx =
  foldMap (getUsedNormFunctions freeCtx boundCtx . binderValue)

getUsedFunctionsCtx ::
  GenericFreeCtx UsedFunctionsInfo ->
  Env Builtin ->
  GenericBoundCtx UsedFunctionsInfo
getUsedFunctionsCtx freeCtx =
  foldr (\u v -> getUsedNormFunctions freeCtx v (binderValue u) : v) mempty
