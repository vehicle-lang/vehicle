
module Vehicle.Compile.Type
  ( runTypeCheck
  ) where

import Prelude hiding (pi)
import Control.Monad (when)
import Control.Monad.Except (MonadError(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..), asks)
import Control.Monad.State (MonadState, evalStateT)
import Data.Foldable (foldrM)
import Data.Map qualified as Map
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid (Endo(..), appEndo)
import Data.Text (pack)

import Vehicle.Prelude
import Vehicle.Compile.Error
import Vehicle.Language.AST
import Vehicle.Language.DSL
import Vehicle.Language.Print (prettyVerbose)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Unify
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.TypeClass
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.MetaSet qualified as MetaSet (null)

runTypeCheck :: (AsTypeError e, MonadLogger m, MonadError e m, TypeCheck f, MetaSubstitutable (f UncheckedBinding UncheckedVar UncheckedAnn))
             => f UncheckedBinding UncheckedVar UncheckedAnn
             -> m (f CheckedBinding CheckedVar CheckedAnn)
runTypeCheck e = do
  let prog1 = runAll e
  let prog2 = runReaderT prog1 emptyVariableCtx
  prog3 <- evalStateT prog2 emptyMetaCtx
  return prog3

runAll :: (TCM e m, TypeCheck f, MetaSubstitutable (f UncheckedBinding UncheckedVar UncheckedAnn))
       => f UncheckedBinding UncheckedVar UncheckedAnn
       -> m (f CheckedBinding CheckedVar CheckedAnn)
runAll e1 = do
  -- logDebug $ prettyVerbose e1 <> line
  e2 <- typeCheck e1
  metaSubstitution <- solveMetas
  return $ substMetas metaSubstitution e2

solveMetas :: MonadConstraintSolving e m => m MetaSubstitution
solveMetas = do
  logDebug "Starting constraint solving\n"
  constraints <- getConstraints
  solution <- loop $ Progress
    { newConstraints = constraints
    , solvedMetas    = mempty
    }
  logDebug "Finished constraint solving\n"
  return solution
  where
    loop :: MonadConstraintSolving e m
         => ConstraintProgress
         -> m MetaSubstitution
    loop progress = do
      allConstraints <- getConstraints
      currentSubstitution <- getMetaSubstitution
      case allConstraints of
        -- If there are no constraints to be solved then return the solution
        []       -> return currentSubstitution
        -- Otherwise see if we made progress
        (c : cs) -> case progress of
          Stuck -> throwError $ mkUnsolvedConstraints (c :| cs)
          Progress newConstraints solvedMetas -> do
            -- If we have failed to make useful progress last pass then abort
            if MetaSet.null solvedMetas && null newConstraints then
              throwError $ mkUnsolvedConstraints (c :| cs)
            -- Otherwise start a new pass
            else do
              logDebug "Starting new pass"

              let updatedConstraints = substMetas currentSubstitution allConstraints
              logDebug $ "current-constraints:" <+> align (prettyVerbose updatedConstraints)

              -- TODO try only the non-blocked metas
              setConstraints []
              newProgress <- mconcat `fmap` traverse tryToSolveConstraint updatedConstraints

              metaSubst <- getMetaSubstitution
              logDebug $ "current-solution:" <+> prettyVerbose metaSubst <> "\n"
              loop newProgress

    tryToSolveConstraint :: MonadConstraintSolving e m
                         => Constraint
                         -> m ConstraintProgress
    tryToSolveConstraint constraint@(Constraint ctx baseConstraint) = do
      logDebug $ "trying" <+> prettyVerbose constraint
      incrCallDepth

      result <- case baseConstraint of
        Unify eq  -> solveUnificationConstraint ctx eq
        m `Has` e -> solveTypeClassConstraint ctx m e

      case result of
        Progress newConstraints _ -> addConstraints newConstraints
        Stuck                     -> addConstraints [constraint]

      decrCallDepth
      return result

class TypeCheck f where
  typeCheck :: TCM e m
            => f UncheckedBinding UncheckedVar UncheckedAnn
            -> m (f CheckedBinding CheckedVar CheckedAnn)

instance TypeCheck Prog where
  typeCheck = inferProg

instance TypeCheck Expr where
  typeCheck e = fst <$> infer e

--------------------------------------------------------------------------------
-- Contexts

-- | The type-checking monad
type TCM e m =
  ( AsTypeError e
  , MonadError  e m
  , MonadState  MetaCtx     m
  , MonadReader VariableCtx m
  , MonadLogger             m
  )

getDeclCtx :: TCM e m => m DeclCtx
getDeclCtx = asks declCtx

addToDeclCtx :: TCM e m => Identifier -> CheckedExpr -> Maybe CheckedExpr -> m a -> m a
addToDeclCtx n t e = local add
  where
    add :: VariableCtx -> VariableCtx
    add VariableCtx{..} = VariableCtx{declCtx = Map.insert n (t, e) declCtx, ..}

getBoundCtx :: TCM e m => m BoundCtx
getBoundCtx = asks boundCtx

getVariableCtx :: TCM e m => m VariableCtx
getVariableCtx = ask

addToBoundCtx :: TCM e m => Maybe Symbol -> CheckedExpr -> m a -> m a
addToBoundCtx n e = local add
  where
    add :: VariableCtx -> VariableCtx
    add VariableCtx{..} = VariableCtx{boundCtx = (n, e) : boundCtx, ..}

--------------------------------------------------------------------------------
-- Debug functions

showDeclEntry :: TCM e m => Identifier -> m ()
showDeclEntry ident = do
  logDebug ("decl-entry" <+> pretty ident)
  incrCallDepth

showDeclExit :: TCM e m => Identifier -> m ()
showDeclExit ident = do
  decrCallDepth
  logDebug ("decl-exit" <+> pretty ident)

showCheckEntry :: TCM e m => CheckedExpr -> UncheckedExpr -> m ()
showCheckEntry t e = do
  logDebug ("check-entry" <+> prettyVerbose e <+> "<-" <+> prettyVerbose t)
  incrCallDepth

showCheckExit :: TCM e m => CheckedExpr -> m ()
showCheckExit e = do
  decrCallDepth
  logDebug ("check-exit " <+> prettyVerbose e)

showInferEntry :: TCM e m => UncheckedExpr -> m ()
showInferEntry e = do
  logDebug ("infer-entry" <+> prettyVerbose e)
  incrCallDepth

showInferExit :: TCM e m => (CheckedExpr, CheckedExpr) -> m ()
showInferExit (e, t) = do
  decrCallDepth
  logDebug ("infer-exit " <+> prettyVerbose e <+> "->" <+> prettyVerbose t)

-------------------------------------------------------------------------------
-- Utility functions

assertIsType :: TCM e m => Provenance -> CheckedExpr -> m ()
assertIsType _ (Type _) = return ()
-- TODO: add a new TypingError 'ExpectedAType'
assertIsType p e        = do
  ctx <- getBoundCtx
  throwError $ mkMismatch p ctx e (Type 0)

unify :: TCM e m => Provenance -> CheckedExpr -> CheckedExpr -> m CheckedExpr
unify p e1 e2 = do
  ctx <- getVariableCtx
  addUnificationConstraint p ctx e1 e2
  -- TODO calculate the most general unifier
  return e1

freshMeta :: TCM e m
          => Provenance
          -> m (Meta, CheckedExpr)
freshMeta p = do
  ctx <- getBoundCtx
  freshMetaWith ctx p

-- Takes the expected type of a function and the user-provided arguments
-- and traverses through checking each argument type against the type of the
-- matching pi binder and inserting any required implicit/instance arguments.
-- Returns the type of the function when applied to the full list of arguments
-- (including inserted arguments) and that list of arguments.
inferArgs :: TCM e m
          => Provenance     -- Provenance of the function
          -> CheckedExpr    -- Type of the function
          -> [UncheckedArg] -- User-provided arguments of the function
          -> m (CheckedExpr, [CheckedArg])
inferArgs p (Pi _ binder resultType) (arg : args)
  | visibilityOf binder == visibilityOf arg = do
    -- Check the type of the argument.
    checkedArgExpr <- check (typeOf binder) (argExpr arg)

    -- Generate the new checked arg
    let checkedArg = replaceArgExpr checkedArgExpr arg

    -- Substitute argument in `resultType`
    let updatedResultType = checkedArgExpr `substInto` resultType

    -- Recurse into the list of args
    (typeAfterApplication, checkedArgs) <- inferArgs p updatedResultType args

    -- Return the appropriately annotated type with its inferred kind.
    return (typeAfterApplication, checkedArg : checkedArgs)

inferArgs _ (Pi _ binder _) (arg : _)
  | visibilityOf binder /= visibilityOf arg && visibilityOf binder == Explicit = do
    ctx <- getBoundCtx
    throwError $ mkMissingExplicitArg ctx arg (typeOf binder)

-- This case handles either
-- (`visibilityOf binder /= visibilityOf arg` and `visibilityOf binder /= Explicit`)
-- or args == []
inferArgs p (Pi _ binder resultType) args = do
    logDebug ("insert-arg" <+> pretty (visibilityOf binder) <+> prettyVerbose (typeOf binder))
    let binderVis = visibilityOf binder
    let ann = (provenanceOf binder, TheMachine)

    -- Generate a new meta-variable for the argument
    (meta, metaExpr) <- freshMeta p
    let metaArg = Arg ann binderVis metaExpr

    -- Check if the required argument is a type-class
    when (binderVis == Instance) $ do
      ctx <- getVariableCtx
      addTypeClassConstraint ctx meta (typeOf binder)

    -- Substitute meta-variable in tRes
    let updatedResultType = metaExpr `substInto` resultType

    -- Recurse into the list of args
    (typeAfterApplication, checkedArgs) <- inferArgs p updatedResultType args

    -- Return the appropriately annotated type with its inferred kind.
    return (typeAfterApplication, metaArg : checkedArgs)

inferArgs _p functionType [] = return (functionType, [])

inferArgs p functionType args = do
  ctx <- getBoundCtx
  let mkRes = [Endo $ \tRes -> unnamedPi (visibilityOf arg) (tHole ("arg" <> pack (show i))) (const tRes)
              | (i, arg) <- zip [0::Int ..] args]
  let expectedType = fromDSL (appEndo (mconcat mkRes) (tHole "res"))
  throwError $ mkMismatch p ctx functionType expectedType

-- |Takes a function and its arguments, inserts any needed implicits
-- or instance arguments and then returns the function applied to the full
-- list of arguments as well as the result type.
inferApp :: TCM e m
         => CheckedAnn
         -> CheckedExpr
         -> CheckedExpr
         -> [UncheckedArg]
         -> m (CheckedExpr, CheckedExpr)
inferApp ann fun funType args = do
  (appliedFunType, checkedArgs) <- inferArgs (provenanceOf fun) funType args
  return (normAppList ann fun checkedArgs, appliedFunType)

--------------------------------------------------------------------------------
-- Type-checking of expressions

check :: TCM e m
      => CheckedExpr   -- Type we're checking against
      -> UncheckedExpr -- Expression being type-checked
      -> m CheckedExpr -- Updated expression
check expectedType expr = do
  showCheckEntry expectedType expr
  res <- case (expectedType, expr) of
    (Pi _ piBinder resultType, Lam ann lamBinder body)
      | visibilityOf piBinder == visibilityOf lamBinder -> do
        checkedLamBinderType <- check Type0 (typeOf lamBinder)

        -- Unify the result with the type of the pi binder.
        _ <- unify (provenanceOf ann) (typeOf piBinder) checkedLamBinderType

        -- Add bound variable to context
        addToBoundCtx (nameOf lamBinder) checkedLamBinderType $ do
          body' <- check resultType body
          let checkedLamBinder = replaceBinderType checkedLamBinderType lamBinder
          return $ Lam ann checkedLamBinder body'

    (Pi _ binder resultType, e) ->
      -- Add the binder to the context
      addToBoundCtx Nothing (typeOf binder) $ do
        let ann = (provenanceOf binder, TheMachine)

        -- Check if the type of the expression matches the expected result type.
        checkedExpr <- check resultType (liftDBIndices 1 e)

        -- Create a new binder mirroring the implicit Pi binder expected
        let lamBinder = Binder ann Implicit (nameOf binder) (typeOf binder)

        -- Prepend a new lambda to the expression with the implicit binder
        return $ Lam ann lamBinder checkedExpr

    (_, Lam ann binder _) -> do
          ctx <- getBoundCtx
          let expected = fromDSL $ unnamedPi (visibilityOf binder) (tHole "a") (const (tHole "b"))
          throwError $ mkMismatch (provenanceOf ann) ctx expectedType expected

    (_, Hole ann _name) -> do
      -- Replace the hole with meta-variable of the expected type.
      -- NOTE, different uses of the same hole name will be interpreted as different meta-variables.
      (_, meta) <- freshMeta (provenanceOf ann)
      return meta

    (_, e@(Type _))            -> viaInfer emptyMachineAnn  expectedType e
    (_, e@(Meta _ _))          -> viaInfer emptyMachineAnn  expectedType e
    (_, e@(App     ann _ _))   -> viaInfer ann              expectedType e
    (_, e@(Pi      ann _ _))   -> viaInfer ann              expectedType e
    (_, e@(Builtin ann _))     -> viaInfer ann              expectedType e
    (_, e@(Var     ann _))     -> viaInfer ann              expectedType e
    (_, e@(Let     ann _ _ _)) -> viaInfer ann              expectedType e
    (_, e@(Literal ann _))     -> viaInfer ann              expectedType e
    (_, e@(Seq     ann _))     -> viaInfer ann              expectedType e
    (_, e@(Ann     ann _ _))   -> viaInfer ann              expectedType e
    (_, PrimDict{})            -> developerError "PrimDict should never be type-checked"

  showCheckExit res
  return res

-- | Takes in an unchecked expression and attempts to infer it's type.
-- Returns the expression annotated with its type as well as the type itself.
infer :: TCM e m
      => UncheckedExpr
      -> m (CheckedExpr, CheckedExpr)
infer e = do
  showInferEntry e
  res <- case e of
    Type l ->
      return (Type l , Type (l + 1))

    Meta _ m -> developerError $ "Trying to infer the type of a meta-variable" <+> pretty m

    Hole ann s ->
      throwError $ mkUnresolvedHole (provenanceOf ann) s

    Ann ann expr exprType   -> do
      (checkedExprType, _) <- infer exprType
      checkedExpr <- check checkedExprType expr
      return (Ann ann checkedExpr checkedExprType , checkedExprType)

    App p fun args -> do
      (checkedFun, checkedFunType) <- infer fun
      inferApp p checkedFun checkedFunType (NonEmpty.toList args)

    Pi p binder resultType -> do
      (checkedBinderType, typeOfBinderType) <- infer (typeOf binder)

      addToBoundCtx (nameOf binder) checkedBinderType $ do
        (checkedResultType, typeOfResultType) <- infer resultType
        let maxResultType = typeOfBinderType `tMax` typeOfResultType
        let checkedBinder = replaceBinderType checkedBinderType binder
        return (Pi p checkedBinder checkedResultType , maxResultType)

    Var ann (Bound i) -> do
      -- Lookup the type of the variable in the context.
      ctx <- getBoundCtx
      case ctx !!? i of
        Just (_, checkedType) -> do
          let liftedCheckedType = liftDBIndices (i+1) checkedType
          return (Var ann (Bound i), liftedCheckedType)
        Nothing      -> developerError $
          "Index" <+> pretty i <+> "out of bounds when looking" <+>
          "up variable in context" <+> prettyVerbose ctx <+> "at" <+> pretty (provenanceOf ann)

    Var ann (Free ident) -> do
      -- Lookup the type of the declaration variable in the context.
      ctx <- getDeclCtx
      case Map.lookup ident ctx of
        Just (checkedType, _) -> return (Var ann (Free ident), checkedType)
        -- This should have been caught during scope checking
        Nothing -> developerError $
          "Declaration'" <+> pretty ident <+> "'not found when" <+>
          "looking up variable in context" <+> pretty ctx <+> "at" <+> pretty (provenanceOf ann)

    Let p boundExpr binder body -> do

      (checkedBoundExpr, typeOfBoundExpr) <-
        if isHole (typeOf binder) then
          -- No information is provided so infer the type of the bound expression
          infer boundExpr
        else do
          -- Check the type of the bound expression against the provided type
          (typeOfBoundExpr, _) <- infer (typeOf binder)
          checkedBoundExpr  <- check typeOfBoundExpr boundExpr
          return (checkedBoundExpr, typeOfBoundExpr)

      -- Update the context with the bound variable
      addToBoundCtx (nameOf binder) typeOfBoundExpr $ do
        -- Infer the type of the body
        (checkedBody , typeOfBody) <- infer body
        let checkedBinder = replaceBinderType typeOfBoundExpr binder
        return (Let p checkedBoundExpr checkedBinder checkedBody , typeOfBody)

    Lam p binder body -> do
      -- Infer the type of the bound variable from the binder
      (typeOfBinder, _) <- infer (typeOf binder)

      -- Update the context with the bound variable
      addToBoundCtx (nameOf binder) typeOfBinder $ do
        (checkedBody , typeOfBody) <- infer body
        let checkedBinder = replaceBinderType typeOfBody binder
        let t' = Pi p checkedBinder typeOfBody
        return (Lam p checkedBinder checkedBody , t')

    Literal p l ->
      inferApp p (Literal p l) (typeOfLiteral p l) []

    Builtin p op -> do
      return (Builtin p op, typeOfBuiltin p op)

    Seq ann elems -> do
      let p = provenanceOf ann
      (checkedElems, typesOfElems) <- unzip <$> traverse infer elems

      -- Generate a meta-variable for the applied container type, e.g. List Int
      (_, typeOfContainer) <- freshMeta p
      -- Generate a fresh meta variable for the type of elements in the list, e.g. Int
      (_, typeOfElems) <- freshMeta p

      -- Unify the types of all the elements in the sequence
      _ <- foldrM (unify p) typeOfElems typesOfElems

      -- Enforce that the applied container type must be a container
      -- Generate a fresh meta-variable for the container function, e.g. List
      (meta, typeOfMeta) <- freshMeta p
      ctx <- getVariableCtx
      addTypeClassConstraint ctx meta (mkIsContainer ann typeOfElems typeOfContainer)

      -- Add in the type and type-class arguments
      let checkedSeq = normAppList ann (Seq ann checkedElems)
            [ ImplicitArg ann typeOfElems
            , ImplicitArg ann typeOfContainer
            , InstanceArg ann typeOfMeta
            ]

      return (checkedSeq , typeOfContainer)

    PrimDict{} -> developerError "PrimDict should never be type-checked"

  showInferExit res
  return res

-- TODO: unify DeclNetw and DeclData
inferDecls :: TCM e m => [UncheckedDecl] -> m [CheckedDecl]
inferDecls [] = return []
inferDecls (d : ds) = do
  let ident = identifierOf d
  showDeclEntry ident

  (checkedDecl, checkedDeclBody, checkedDeclType) <- case d of
    DeclNetw p _ t -> do
      (checkedType, typeOfType) <- infer t
      assertIsType p typeOfType
      let checkedDecl = DeclNetw p ident checkedType
      return (checkedDecl, Nothing, checkedType)

    DeclData p _ t -> do
      (checkedType, typeOfType) <- infer t
      assertIsType p typeOfType
      let checkedDecl = DeclData p ident checkedType
      return (checkedDecl, Nothing, checkedType)

    DefFun p _ t body -> do
      (checkedType, typeOfType) <- infer t
      assertIsType p typeOfType
      checkedBody <- check checkedType body
      let checkedDecl = DefFun p ident checkedType checkedBody
      return (checkedDecl, Just checkedBody, checkedType)

  showDeclExit ident
  checkedDecls <- addToDeclCtx ident checkedDeclType checkedDeclBody $ inferDecls ds
  return $ checkedDecl : checkedDecls

inferProg :: TCM e m => UncheckedProg -> m CheckedProg
inferProg (Main ds) = do
  logDebug "Beginning initial type-checking pass"
  result <- Main <$> inferDecls ds
  logDebug "Ending initial type-checking pass\n"
  return result

viaInfer :: TCM e m => CheckedAnn -> CheckedExpr -> UncheckedExpr -> m CheckedExpr
viaInfer ann expectedType e = do
  -- Switch to inference mode
  (checkedExpr, actualType) <- infer e
  -- Insert any needed implicit or instance arguments
  (appliedCheckedExpr, resultType) <- inferApp ann checkedExpr actualType []
  -- Assert the expected and the actual types are equal
  _t <- unify (provenanceOf ann) expectedType resultType
  return appliedCheckedExpr

--------------------------------------------------------------------------------
-- Typing of literals and builtins

-- | Return the type of the provided literal,
typeOfLiteral :: CheckedAnn -> Literal -> CheckedExpr
typeOfLiteral ann l = fromDSL $ case l of
  LNat  _ -> forall type0 $ \t -> isNatural  ann t ~~~> t
  LInt  _ -> forall type0 $ \t -> isIntegral ann t ~~~> t
  LRat  _ -> forall type0 $ \t -> isReal     ann t ~~~> t
  LBool _ -> forall type0 $ \t -> isTruth    ann t ~~~> t

-- | Return the type of the provided builtin.
typeOfBuiltin :: CheckedAnn -> Builtin -> CheckedExpr
typeOfBuiltin ann b = fromDSL $ case b of
  BooleanType   _           -> type0
  NumericType   _           -> type0
  ContainerType List        -> type0 ~> type0
  ContainerType Tensor      -> type0 ~> tList tNat ~> type0

  TypeClass HasEq           -> type0 ~> type0 ~> type0
  TypeClass HasOrd          -> type0 ~> type0 ~> type0
  TypeClass IsTruth         -> type0 ~> type0
  TypeClass IsNatural       -> type0 ~> type0
  TypeClass IsIntegral      -> type0 ~> type0
  TypeClass IsRational      -> type0 ~> type0
  TypeClass IsReal          -> type0 ~> type0
  TypeClass IsContainer     -> type0 ~> type0
  TypeClass IsQuantifiable  -> type0 ~> type0 ~> type0

  If   -> typeOfIf
  Not          -> typeOfBoolOp1 ann
  BooleanOp2 _ -> typeOfBoolOp2 ann
  Neg          -> typeOfNumOp1 (isIntegral ann)
  NumericOp2 _ -> typeOfNumOp2 (isNatural  ann)

  Equality _ -> typeOfEqualityOp ann
  Order    _ -> typeOfComparisonOp ann

  Cons -> typeOfCons ann
  At   -> typeOfAtOp ann
  Map  -> typeOfMapOp
  Fold -> typeOfFoldOp ann

  Quant   _ -> typeOfQuantifierOp
  QuantIn _ -> typeOfQuantifierInOp ann

typeOfIf :: DSLExpr
typeOfIf =
  forall type0 $ \t ->
    tProp ~> t ~> t ~> t

typeOfEqualityOp :: CheckedAnn -> DSLExpr
typeOfEqualityOp ann =
  forall type0 $ \t ->
    forall type0 $ \r ->
      hasEq ann t r ~~~> t ~> t ~> r

typeOfComparisonOp :: CheckedAnn -> DSLExpr
typeOfComparisonOp ann =
  forall type0 $ \t ->
    forall type0 $ \r ->
      hasOrd ann t r ~~~> t ~> t ~> r

typeOfBoolOp2 :: CheckedAnn -> DSLExpr
typeOfBoolOp2 ann =
  forall type0 $ \t ->
    isTruth ann t ~~~> t ~> t ~> t

typeOfBoolOp1 :: CheckedAnn -> DSLExpr
typeOfBoolOp1 ann =
  forall type0 $ \t ->
    isTruth ann t ~~~> t ~> t

typeOfNumOp2 :: (DSLExpr -> DSLExpr) -> DSLExpr
typeOfNumOp2 numConstraint =
  forall type0 $ \t ->
    numConstraint t ~~~> t ~> t ~> t

typeOfNumOp1 :: (DSLExpr -> DSLExpr) -> DSLExpr
typeOfNumOp1 numConstraint =
  forall type0 $ \t ->
    numConstraint t ~~~> t ~> t

typeOfQuantifierOp :: DSLExpr
typeOfQuantifierOp =
  forall type0 $ \t ->
    (t ~> tProp) ~> tProp

typeOfQuantifierInOp :: CheckedAnn -> DSLExpr
typeOfQuantifierInOp ann =
  forall type0 $ \tElem ->
    forall type0 $ \tCont ->
      forall type0 $ \tRes ->
        isContainer ann tElem tCont ~~~> (tElem ~> tRes) ~> tCont ~> tRes

typeOfCons :: CheckedAnn -> DSLExpr
typeOfCons _ann =
  forall type0 $ \tElem ->
      tElem ~> tList tElem ~> tList tElem

typeOfAtOp :: CheckedAnn -> DSLExpr
typeOfAtOp _ann =
  forall type0 $ \tElem ->
    forall type0 $ \tDims ->
      tTensor tElem tDims ~> tNat ~> tElem

-- TODO generalise these to tensors etc. (remember to do mkMap' in utils as well)
typeOfMapOp :: DSLExpr
typeOfMapOp =
  forall type0 $ \tFrom ->
    forall type0 $ \tTo ->
      (tFrom ~> tTo) ~> tList tFrom ~> tList tTo

typeOfFoldOp :: CheckedAnn -> DSLExpr
typeOfFoldOp ann =
  forall type0 $ \tElem ->
    forall type0 $ \tCont ->
      forall type0 $ \tRes ->
        isContainer ann tElem tCont ~~~> (tElem ~> tRes ~> tRes) ~> tRes ~> tCont ~> tRes