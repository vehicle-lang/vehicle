module Vehicle.Compile.Queries.QuerySetStructure
  ( SeriousPropertyError (..),
    PropertyError (..),
    UnreducedAssertion (..),
    compileQueryStructure,
    eliminateNot,
    QueryDeclCtx,
    queryDeclCtxToInfoDeclCtx,
    queryDeclCtxToNormDeclCtx,
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
import Data.Set qualified as Set (difference, null, singleton)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Compile.Queries.IfElimination (eliminateIfs, unfoldIf)
import Vehicle.Compile.Queries.LinearExpr (UnreducedAssertion (..), VectorEquality (..))
import Vehicle.Compile.Queries.Variable
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Compile.Type.Subsystem.Standard.Patterns
import Vehicle.Expr.Boolean
import Vehicle.Expr.DeBruijn (Ix (..), Lv (..), dbLevelToIndex)
import Vehicle.Expr.Normalisable (NormalisableBuiltin (..))
import Vehicle.Expr.Normalised
import Vehicle.Libraries.StandardLibrary (StdLibFunction (StdEqualsVector), fromFiniteQuantifier)
import Vehicle.Verify.Core

--------------------------------------------------------------------------------
-- Main function

-- | Attempts to compile an arbitrary expression of type `Bool` whose top-most
-- quantifier is an `exists` down to a set of quantifiers over propositions in
-- disjunctive normal form which are guaranteed to not contain if-statements.
-- Only reduces quantified variables of `Vector` type as much as is required
-- in order to be able to reach DNF.
compileQueryStructure ::
  (MonadCompile m) =>
  DeclProvenance ->
  QueryDeclCtx ->
  StandardNormExpr ->
  m (Either SeriousPropertyError (BoundCtx UserVariable, MaybeTrivial (BooleanExpr UnreducedAssertion), VariableNormalisationSteps))
compileQueryStructure declProv queryDeclCtx expr =
  logCompilerPass MinDetail "compilation of boolean structure" $ do
    result <- runReaderT (compileBoolExpr mempty expr) (declProv, queryDeclCtx)
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
type CumulativeCtx = BoundCtx UserVariable

type RevGlobalCtx = [UserVariable]

cumulativeVarsToCtx :: CumulativeCtx -> BoundDBCtx
cumulativeVarsToCtx = fmap (Just . userVarName)

-- | The only time we should be throwing this error is because we haven't sufficiently
-- normalised a user quantified variable above us in the tree, e.g. in the following
--
--   forall (xs : Vector Rat 2) . fold and True (map (\x -> x >= 2) xs)
--
-- we can't normalise the `map` and hence the `fold` until we normalise the quantified
-- variable `xs` to two separate quantifiers.
data TemporaryPropertyError
  = CannotEliminateIfs StandardNormExpr
  | CannotEliminateNot StandardNormExpr
  | NonBooleanQueryStructure StandardNormExpr
  deriving (Show)

instance Pretty TemporaryPropertyError where
  pretty = \case
    CannotEliminateIfs e -> "CannotEliminateIfs[" <+> prettyVerbose e <+> "]"
    CannotEliminateNot e -> "CannotEliminateNot[" <+> prettyVerbose e <+> "]"
    NonBooleanQueryStructure e -> "NonBooleanQueryStructure[" <+> prettyVerbose e <+> "]"

-- | Serious errors that are fundemental problems with the specification
data SeriousPropertyError
  = AlternatingQuantifiers
  | NonLinearSpecification StandardNormExpr
  | UnsupportedQuantifierType StandardNormBinder StandardNormType
  | UnsupportedInequalityOp

data PropertyError
  = TemporaryError TemporaryPropertyError
  | SeriousError SeriousPropertyError

type QueryStructureResult =
  Either PropertyError (RevGlobalCtx, MaybeTrivial (BooleanExpr UnreducedAssertion), VariableNormalisationSteps)

-- | Pattern matches on a vector equality in the standard library.
isVectorEquals ::
  StandardNormExpr ->
  Maybe (StandardNormExpr, StandardNormExpr, TensorDimensions, StandardNormExpr -> StandardNormExpr -> StandardNormExpr)
isVectorEquals = \case
  VFreeVar ident args
    | ident == identifierOf StdEqualsVector -> case args of
        [t1, t2, dim, sol, ExplicitArg _ e1, ExplicitArg _ e2] -> do
          d <- getNatLiteral $ argExpr dim
          ds <- getTensorDimensions $ argExpr t1
          let dims = d : ds
          let mkOp e1' e2' = VFreeVar ident [t1, t2, dim, sol, ExplicitArg mempty e1', ExplicitArg mempty e2']
          Just (e1, e2, dims, mkOp)
        _ -> Nothing
  _ -> Nothing

getTensorDimensions :: StandardNormExpr -> Maybe TensorDimensions
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
    MonadReader (DeclProvenance, QueryDeclCtx) m
  )

-- | For efficiency reasons, we sometimes want to avoid expanding out
-- finite quantifiers
evalWhilePreservingFiniteQuantifiers ::
  (MonadQueryStructure m) =>
  StandardEnv ->
  TypeCheckedExpr ->
  m StandardNormExpr
evalWhilePreservingFiniteQuantifiers env body = do
  (_, queryDeclCtx) <- ask
  let evalOptions = EvalOptions {evalFiniteQuantifiers = False}
  let declCtx = queryDeclCtxToNormDeclCtx queryDeclCtx
  runNormT evalOptions declCtx mempty (eval env body)

compileBoolExpr ::
  forall m.
  (MonadQueryStructure m) =>
  CumulativeCtx ->
  StandardNormExpr ->
  m QueryStructureResult
compileBoolExpr = go False
  where
    -- \| Traverses an arbitrary expression of type `Bool`.
    go :: Bool -> CumulativeCtx -> StandardNormExpr -> m QueryStructureResult
    go alreadyLiftedIfs quantifiedVariables expr = case expr of
      ----------------
      -- Base cases --
      ----------------
      VBoolLiteral b ->
        return $ Right ([], Trivial b, [])
      VBuiltinFunction op@(Equals _ eq) [e1, e2] -> do
        let mkOp e1' e2' = VBuiltinFunction op [e1', e2']
        compileEquality quantifiedVariables alreadyLiftedIfs e1 eq e2 [] mkOp
      VBuiltinFunction Order {} [_, _] -> do
        compileOrder alreadyLiftedIfs quantifiedVariables expr
      (isVectorEquals -> Just (e1, e2, dims, mkOp)) -> do
        compileEquality quantifiedVariables alreadyLiftedIfs e1 Eq e2 dims mkOp

      ---------------------
      -- Recursive cases --
      ---------------------
      VBuiltinFunction And [e1, e2] ->
        compileOp2 (andTrivial Conjunct) alreadyLiftedIfs quantifiedVariables e1 e2
      VBuiltinFunction Or [e1, e2] ->
        compileOp2 (orTrivial Disjunct) alreadyLiftedIfs quantifiedVariables e1 e2
      VBuiltinFunction Not [e] ->
        -- As the expression is of type `Not` we can try lowering the `not` down
        -- through the expression.
        case eliminateNot e of
          Nothing -> return $ Left $ TemporaryError $ CannotEliminateNot expr
          Just result -> go alreadyLiftedIfs quantifiedVariables result
      VBuiltinFunction If [c, x, y] -> do
        -- As the expression is of type `Bool` we can immediately unfold the `if`.
        let unfoldedExpr = unfoldIf c x y
        go alreadyLiftedIfs quantifiedVariables unfoldedExpr
      VInfiniteQuantifier q dom _ binder env body -> case q of
        -- If we're at a `Forall` we know we must have alternating quantifiers.
        Forall -> return $ Left $ SeriousError AlternatingQuantifiers
        -- Otherwise try to compile away the quantifier.
        Exists -> compileInfiniteQuantifier quantifiedVariables dom binder env body
      VFiniteQuantifier q args binder env body ->
        compileFiniteQuantifier quantifiedVariables q args binder env body
      ------------
      -- Errors --
      ------------
      _ -> return $ Left $ TemporaryError $ NonBooleanQueryStructure expr

    compileEquality ::
      CumulativeCtx ->
      Bool ->
      StandardNormExpr ->
      EqualityOp ->
      StandardNormExpr ->
      TensorDimensions ->
      (StandardNormExpr -> StandardNormExpr -> StandardNormExpr) ->
      m QueryStructureResult
    compileEquality quantifiedVariables alreadyLiftedIfs lhs eq rhs dims mkRel = do
      let ctx = cumulativeVarsToCtx quantifiedVariables
      let expr = mkRel lhs rhs
      logDebug MaxDetail $ "Identified (in)equality:" <+> prettyFriendly (WithContext expr ctx)

      maybeResult <- elimIfs alreadyLiftedIfs quantifiedVariables (mkRel lhs rhs)
      case maybeResult of
        Just result -> return result
        Nothing -> case eq of
          Eq -> do
            let assertion
                  | null dims = VectorEqualityAssertion $ VectorEquality lhs rhs dims mkRel
                  | otherwise = NonVectorEqualityAssertion expr
            return $ Right ([], NonTrivial $ Query assertion, [])
          Neq ->
            return $ Left $ SeriousError UnsupportedInequalityOp

    compileOrder :: Bool -> CumulativeCtx -> StandardNormExpr -> m QueryStructureResult
    compileOrder alreadyLiftedIfs quantifiedVariables expr = do
      -- Even though we're sticking the result in a `NonVectorEqualityAssertion` we still need
      -- to lift and eliminate `if`s as they may be inside network applications.
      maybeResult <- elimIfs alreadyLiftedIfs quantifiedVariables expr
      case maybeResult of
        Just result -> return result
        Nothing -> do
          let assertion = NonVectorEqualityAssertion expr
          return $ Right ([], NonTrivial $ Query assertion, [])

    elimIfs :: Bool -> CumulativeCtx -> StandardNormExpr -> m (Maybe QueryStructureResult)
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
              (_, queryDeclCtx) <- ask
              let env = variableCtxToNormEnv quantifiedVariables
              let normDeclCtx = queryDeclCtxToNormDeclCtx queryDeclCtx
              normExprWithoutIf <- runNormT defaultEvalOptions normDeclCtx mempty $ reeval env exprWithoutIf
              logDebug MaxDetail $ "Normalised to:" <+> prettyFriendly (WithContext normExprWithoutIf ctx)
              Just <$> go True quantifiedVariables normExprWithoutIf
          decrCallDepth
          return result

    compileOp2 ::
      (forall a. MaybeTrivial (BooleanExpr a) -> MaybeTrivial (BooleanExpr a) -> MaybeTrivial (BooleanExpr a)) ->
      Bool ->
      CumulativeCtx ->
      StandardNormExpr ->
      StandardNormExpr ->
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

eliminateNot :: StandardNormExpr -> Maybe StandardNormExpr
eliminateNot arg = case arg of
  -- Base cases
  VBoolLiteral b -> Just $ VBoolLiteral (not b)
  VBuiltinFunction (Order dom ord) args -> Just $ VBuiltinFunction (Order dom (neg ord)) args
  VBuiltinFunction (Equals dom eq) args -> Just $ VBuiltinFunction (Equals dom (neg eq)) args
  VBuiltinFunction Not [e] -> Just e
  -- Inductive cases
  VBuiltinFunction Or args -> do
    args' <- traverse eliminateNot args
    return $ VBuiltinFunction And args'
  VBuiltinFunction And args -> do
    args' <- traverse eliminateNot args
    return $ VBuiltinFunction Or args'
  VBuiltinFunction If [c, e1, e2] -> do
    e1' <- eliminateNot e1
    e2' <- eliminateNot e2
    return $ VBuiltinFunction If [c, e1', e2']
  -- Quantifier cases
  -- We can't actually lower the `not` throw the body of the quantifier as
  -- the body is not yet unnormalised. However, it's fine to stop here as we'll
  -- simply continue to normalise it once we re-encounter it again after
  -- normalising the quantifier.
  VInfiniteQuantifier q dom args binder env body -> do
    let p = mempty
    let negatedBody = NotExpr p [ExplicitArg p body]
    Just $ VInfiniteQuantifier (neg q) dom args binder env negatedBody
  VFiniteQuantifier q args binder env body -> do
    let p = mempty
    let negatedBody = NotExpr p [ExplicitArg p body]
    Just $ VFiniteQuantifier (neg q) args binder env negatedBody
  -- Errors
  _ -> Nothing

--------------------------------------------------------------------------------
-- Finite quantifier elimination

compileFiniteQuantifier ::
  (MonadQueryStructure m) =>
  CumulativeCtx ->
  Quantifier ->
  StandardSpine ->
  StandardNormBinder ->
  StandardEnv ->
  TypeCheckedExpr ->
  m QueryStructureResult
compileFiniteQuantifier quantifiedVariables q quantSpine binder env body = do
  (_, queryDeclCtx) <- ask
  let normCtx = queryDeclCtxToNormDeclCtx queryDeclCtx
  let funcCtx = queryDeclCtxToInfoDeclCtx queryDeclCtx

  let foldedResult = do
        let foldedExpr = VFiniteQuantifier q quantSpine binder env body
        let unnormalisedAssertion = NonVectorEqualityAssertion foldedExpr
        logDebug MidDetail $ "Keeping folded:" <+> pretty q <+> prettyVerbose binder
        return $ Right (mempty, NonTrivial $ Query unnormalisedAssertion, mempty)

  if canLeaveFiniteQuantifierUnexpanded funcCtx env body
    then foldedResult
    else do
      normResult <- runNormT defaultEvalOptions normCtx mempty $ do
        quantImplementation <- lookupFreeVar (fromFiniteQuantifier q)
        evalApp quantImplementation (VFiniteQuantifierSpine quantSpine binder env body)
      compileBoolExpr quantifiedVariables normResult

-- | This is a sound, inexpensive, but incomplete, check for whether
-- we can leave the finite quantifier folded as it will only contain
-- conjunctions.
canLeaveFiniteQuantifierUnexpanded ::
  DeclCtx UsedFunctionsInfo ->
  StandardEnv ->
  StandardExpr ->
  Bool
canLeaveFiniteQuantifierUnexpanded ctx env expr = do
  let (usedFunctions, usedFreeVars) = getUsedFunctions ctx (getUsedFunctionsCtx ctx env) expr
  let forbiddenFunctions =
        HashSet.fromList
          [ Quantifier Exists QuantRat,
            Quantifier Forall QuantRat,
            Quantifier Exists QuantInt,
            Quantifier Forall QuantInt,
            Quantifier Exists QuantNat,
            Quantifier Forall QuantNat,
            Quantifier Exists QuantVec,
            Quantifier Forall QuantVec
          ]

  let doesNotHaveDisjunction = HashSet.null (HashSet.intersection usedFunctions forbiddenFunctions)
  let doesNotHaveNetwork = Set.null (Set.difference usedFreeVars (Map.keysSet ctx))
  doesNotHaveDisjunction && doesNotHaveNetwork

--------------------------------------------------------------------------------
-- Infinite quantifier elimination

compileInfiniteQuantifier ::
  (MonadQueryStructure m) =>
  CumulativeCtx ->
  QuantifierDomain ->
  StandardNormBinder ->
  StandardEnv ->
  TypeCheckedExpr ->
  m QueryStructureResult
compileInfiniteQuantifier quantifiedVariables _ binder env body = do
  let variableName = getBinderName binder
  let variableDoc = "variable" <+> quotePretty variableName
  logCompilerPass MidDetail ("compilation of quantified" <+> variableDoc) $ do
    let isDuplicateName = any (\v -> userVarName v == variableName) quantifiedVariables
    if isDuplicateName
      then do
        (declProv, _) <- ask
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

calculateVariableDimensions :: StandardNormBinder -> Either PropertyError TensorDimensions
calculateVariableDimensions binder = go (typeOf binder)
  where
    go :: StandardNormType -> Either PropertyError TensorDimensions
    go = \case
      VVectorType tElem (VNatLiteral dim) -> do
        dims <- go tElem
        return $ dim : dims
      t -> goBase t

    goBase :: StandardNormType -> Either PropertyError TensorDimensions
    goBase = \case
      VRatType {} -> return []
      baseType -> Left $ SeriousError $ UnsupportedQuantifierType binder baseType

--------------------------------------------------------------------------------
-- Builtin and free variable tracking

type QueryDeclCtx = DeclCtx (NormDeclCtxEntry StandardBuiltinType, UsedFunctionsInfo)

queryDeclCtxToNormDeclCtx :: QueryDeclCtx -> StandardNormDeclCtx
queryDeclCtxToNormDeclCtx = fmap fst

queryDeclCtxToInfoDeclCtx :: QueryDeclCtx -> DeclCtx UsedFunctionsInfo
queryDeclCtxToInfoDeclCtx = fmap snd

type UsedFunctionsInfo = (HashSet BuiltinFunction, Set Identifier)

getUsedFunctions ::
  DeclCtx UsedFunctionsInfo ->
  BoundCtx UsedFunctionsInfo ->
  StandardExpr ->
  UsedFunctionsInfo
getUsedFunctions declCtx boundCtx expr = case expr of
  Universe {} -> mempty
  Meta {} -> mempty
  Hole {} -> mempty
  Pi {} -> mempty
  BoundVar _ v -> getUsedVarsBoundVar boundCtx v
  Builtin _ b -> getUsedFunctionsBuiltin b
  FreeVar _ ident -> getUsedFunctionsFreeVar declCtx ident
  Ann _ term _typ -> getUsedFunctions declCtx boundCtx term
  App _ fun args -> foldr (<>) (getUsedFunctions declCtx boundCtx fun) (fmap (getUsedFunctions declCtx boundCtx . argExpr) args)
  Let _ e1 _binder e2 -> getUsedFunctions declCtx boundCtx e1 <> getUsedFunctions declCtx boundCtx e2
  Lam _ _binder e -> getUsedFunctions declCtx (mempty : boundCtx) e

getUsedNormFunctions ::
  DeclCtx UsedFunctionsInfo ->
  BoundCtx UsedFunctionsInfo ->
  StandardNormExpr ->
  UsedFunctionsInfo
getUsedNormFunctions declCtx boundCtx expr = case expr of
  VPi {} -> mempty
  VUniverse {} -> mempty
  VMeta {} -> mempty
  VLam _ env body -> do
    let envInfo = getUsedFunctionsEnv declCtx boundCtx env
    let bodyInfo = getUsedFunctions declCtx (mempty : boundCtx) body
    envInfo <> bodyInfo
  VBoundVar v spine -> do
    let varInfo = getUsedVarsBoundVar boundCtx (dbLevelToIndex (Lv (length boundCtx)) v)
    let spineInfo = getUsedFunctionsSpine declCtx boundCtx (fmap argExpr spine)
    varInfo <> spineInfo
  VFreeVar ident spine -> do
    let identInfo = getUsedFunctionsFreeVar declCtx ident
    let spineInfo = getUsedFunctionsSpine declCtx boundCtx (fmap argExpr spine)
    identInfo <> spineInfo
  VBuiltin b spine -> do
    let builtinInfo = getUsedFunctionsBuiltin b
    let spineInfo = getUsedFunctionsSpine declCtx boundCtx spine
    builtinInfo <> spineInfo

getUsedFunctionsBuiltin ::
  NormalisableBuiltin StandardBuiltinType ->
  UsedFunctionsInfo
getUsedFunctionsBuiltin = \case
  CFunction f -> (HashSet.singleton f, mempty)
  _ -> mempty

getUsedFunctionsFreeVar ::
  DeclCtx UsedFunctionsInfo ->
  Identifier ->
  UsedFunctionsInfo
getUsedFunctionsFreeVar declCtx ident =
  fromMaybe (mempty, Set.singleton ident) (Map.lookup ident declCtx)

getUsedVarsBoundVar ::
  BoundCtx UsedFunctionsInfo ->
  Ix ->
  UsedFunctionsInfo
getUsedVarsBoundVar boundCtx ix =
  fromMaybe mempty (lookupIx boundCtx ix)

getUsedFunctionsSpine ::
  DeclCtx UsedFunctionsInfo ->
  BoundCtx UsedFunctionsInfo ->
  StandardExplicitSpine ->
  UsedFunctionsInfo
getUsedFunctionsSpine declCtx boundCtx =
  foldMap (getUsedNormFunctions declCtx boundCtx)

getUsedFunctionsEnv ::
  DeclCtx UsedFunctionsInfo ->
  BoundCtx UsedFunctionsInfo ->
  StandardEnv ->
  UsedFunctionsInfo
getUsedFunctionsEnv declCtx boundCtx =
  foldMap (getUsedNormFunctions declCtx boundCtx . snd)

getUsedFunctionsCtx ::
  DeclCtx UsedFunctionsInfo ->
  StandardEnv ->
  BoundCtx UsedFunctionsInfo
getUsedFunctionsCtx declCtx =
  foldr (\u v -> getUsedNormFunctions declCtx v (snd u) : v) mempty
