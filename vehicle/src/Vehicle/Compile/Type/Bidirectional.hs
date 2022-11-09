module Vehicle.Compile.Type.Bidirectional
  ( TCM
  , checkExpr
  , inferExpr
  ) where

import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Monoid (Endo (..), appEndo)
import Data.Text (pack)
import Prelude hiding (pi)

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Builtin
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.VariableContext (TypingBoundCtx)
import Vehicle.Language.DSL
import Vehicle.Language.Print

--------------------------------------------------------------------------------
-- Bidirectional type-checking

-- Recurses through the expression, switching between check and infer modes.
-- Inserts meta-variables for missing implicit and instance arguments and
-- gathers the constraints over those meta-variables.

--------------------------------------------------------------------------------
-- Debug functions

showCheckEntry :: MonadLogger m => CheckedType -> UncheckedExpr -> m ()
showCheckEntry t e = do
  logDebug MaxDetail ("check-entry" <+> prettyVerbose e <+> "<-" <+> prettyVerbose t)
  incrCallDepth

showCheckExit :: MonadLogger m => CheckedExpr -> m ()
showCheckExit e = do
  decrCallDepth
  logDebug MaxDetail ("check-exit " <+> prettyVerbose e)

showInferEntry :: MonadLogger m => UncheckedExpr -> m ()
showInferEntry e = do
  logDebug MaxDetail ("infer-entry" <+> prettyVerbose e)
  incrCallDepth

showInferExit :: MonadLogger m => (CheckedExpr, CheckedType) -> m ()
showInferExit (e, t) = do
  decrCallDepth
  logDebug MaxDetail ("infer-exit " <+> prettyVerbose e <+> "->" <+> prettyVerbose t)

-------------------------------------------------------------------------------
-- Utility functions

-- | Type checking monad with additional bound context for the bidirectional
-- type-checking pass.
type MonadBidirectional m =
  ( TCM m
  , MonadReader TypingBoundCtx m
  )

checkExprTypesEqual :: MonadBidirectional m
                    => Provenance
                    -> CheckedExpr
                    -> CheckedType
                    -> CheckedType
                    -> m ()
checkExprTypesEqual p expr expectedType actualType = do
  ctx <- ask
  let origin = CheckingExprType expr expectedType actualType
  addFreshUnificationConstraint TypeGroup p ctx origin expectedType actualType

checkBinderTypesEqual :: MonadBidirectional m
                      => Provenance
                      -> DBBinding
                      -> CheckedType
                      -> CheckedType
                      -> m ()
checkBinderTypesEqual p binderName expectedType actualType = do
  ctx <- ask
  let origin = CheckingBinderType binderName expectedType actualType
  addFreshUnificationConstraint TypeGroup p ctx origin expectedType actualType

--------------------------------------------------------------------------------
-- Checking

checkExpr :: MonadBidirectional m
          => CheckedType   -- Type we're checking against
          -> UncheckedExpr -- Expression being type-checked
          -> m CheckedExpr -- Updated expression
checkExpr expectedType expr = do
  showCheckEntry expectedType expr
  res <- case (expectedType, expr) of

    -- In the case where we have a matching pi binder and lam binder use the pi-binder to
    -- aid inference of lambda binder.
    (Pi _ piBinder resultType, Lam p lamBinder body)
      | visibilityOf piBinder == visibilityOf lamBinder -> do
        let binderName = nameOf lamBinder
        -- Check that the type of the lambda binder is a type.
        checkedLamBinderType <- checkExpr (TypeUniverse (inserted p) 0) (typeOf lamBinder)

        -- Check that the lambda and pi binders have the same type.
        checkBinderTypesEqual p binderName (typeOf piBinder) checkedLamBinderType

        -- Add bound variable to context
        checkedBody <- addToBoundCtx (binderName, checkedLamBinderType, Nothing) $ do

          -- Check if the type of the expression matches the expected result type.
          checkExpr resultType body

        let checkedLamBinder = replaceBinderType checkedLamBinderType lamBinder
        return $ Lam p checkedLamBinder checkedBody

    -- In the case where we have an implicit or instance pi binder then insert a new
    -- lambda expression.
    (Pi _ piBinder resultType, e)
      | isImplicit piBinder || isInstance piBinder -> do
      -- Then eta-expand
      let ann = inserted $ provenanceOf piBinder
      let binderName = nameOf piBinder
      let binderType = typeOf piBinder

      -- Add the pi-bound variable to the context
      checkedExpr <- addToBoundCtx (binderName, binderType, Nothing) $
        -- Check if the type of the expression matches the expected result type.
        checkExpr resultType (liftFreeDBIndices 1 e)

      -- Create a new binder mirroring the Pi binder expected
      let lamBinder = Binder ann (visibilityOf piBinder) (relevanceOf piBinder) binderName binderType

      -- Prepend a new lambda to the expression with the implicit binder
      return $ Lam ann lamBinder checkedExpr

    (_, Hole p _name) -> do
      -- Replace the hole with meta-variable.
      -- NOTE, different uses of the same hole name will be interpreted as
      -- different meta-variables.
      freshExprMeta p expectedType =<< getBoundCtx

    -- Otherwise switch to inference mode
    (_, _) -> viaInfer expectedType expr

  showCheckExit res
  return res

viaInfer :: MonadBidirectional m => CheckedType -> UncheckedExpr -> m CheckedExpr
viaInfer expectedType expr = do
  let p = provenanceOf expr
  -- Switch to inference mode
  (checkedExpr, actualType) <- inferExpr expr
  -- Insert any needed implicit or instance arguments
  (appliedCheckedExpr, resultType) <- inferApp p checkedExpr actualType []
  -- Check the expected and the actual types are equal
  checkExprTypesEqual p expr expectedType resultType
  return appliedCheckedExpr

--------------------------------------------------------------------------------
-- Inference

-- | Takes in an unchecked expression and attempts to infer it's type.
-- Returns the expression annotated with its type as well as the type itself.
inferExpr :: MonadBidirectional m
          => UncheckedExpr
          -> m (CheckedExpr, CheckedType)
inferExpr e = do
  showInferEntry e
  res <- case e of
    Universe ann u -> case u of
      TypeUniv l   -> return (e , TypeUniverse (inserted ann) (l + 1))
      _            -> compilerDeveloperError $
        "Should not be trying to infer the type of" <+> pretty u

    Meta _ m -> do
      metaType <- getMetaType m
      return (e, metaType)

    Hole p _name -> do
      -- Replace the hole with meta-variable.
      -- NOTE, different uses of the same hole name will be interpreted
      -- as different meta-variables.
      metaType <- freshExprMeta p (TypeUniverse p 0) =<< getBoundCtx
      metaExpr <- freshExprMeta p metaType =<< getBoundCtx
      checkExprTypesEqual p metaExpr metaType (TypeUniverse p 0)
      return (metaExpr, metaType)

    Ann p expr exprType -> do
      -- Check the annotation is a type.
      (checkedExprType, exprTypeType) <- inferExpr exprType
      checkExprTypesEqual p exprType (TypeUniverse (inserted p) 0) exprTypeType

      checkedExpr <- checkExpr checkedExprType expr
      return (Ann p checkedExpr checkedExprType , checkedExprType)

    Pi p binder resultType -> do
      (checkedBinderType, typeOfBinderType) <- inferExpr (typeOf binder)

      (checkedResultType, typeOfResultType) <-
        addToBoundCtx (nameOf binder, checkedBinderType, Nothing) $ inferExpr resultType

      let maxResultType = typeOfBinderType `tMax` typeOfResultType
      let checkedBinder = replaceBinderType checkedBinderType binder
      return (Pi p checkedBinder checkedResultType , maxResultType)

    -- Literals are slightly tricky to type-check, as by default they
    -- probably are standalone, i.e. not wrapped in an `App`, in which
    -- case we need to insert an `App` around them. However, if the
    -- user has provided an implicit argument to them or we are type
    -- checking a second time, then the `App` will already be present.
    -- One approach might be to pass a boolean flag through `infer`
    -- which signals whether the parent node is an `App`, however
    -- for now it's simplier to split into the following two cases:
    App p (Literal p' l) args -> do
      let (checkedLit, checkedLitType) = inferLiteral p' l
      inferApp p checkedLit checkedLitType (NonEmpty.toList args)

    Literal p l -> do
      let (checkedLit, checkedLitType) = inferLiteral p l
      inferApp p checkedLit checkedLitType []

    App p fun args -> do
      (checkedFun, checkedFunType) <- inferExpr fun
      inferApp p checkedFun checkedFunType (NonEmpty.toList args)

    Var p (Bound i) -> do
      -- Lookup the type of the variable in the context.
      ctx <- getBoundCtx
      case ctx !!? i of
        Just (_, checkedType, _) -> do
          let liftedCheckedType = liftFreeDBIndices (i+1) checkedType
          return (Var p (Bound i), liftedCheckedType)
        Nothing      -> compilerDeveloperError $
          "DBIndex" <+> pretty i <+> "out of bounds when looking" <+>
          "up variable in context" <+> prettyVerbose (boundContextOf ctx) <+> "at" <+> pretty p

    Var p (Free ident) -> do
      originalType <- getDeclType p ident
      return (Var p (Free ident), originalType)

    Let p boundExpr binder body -> do
      -- Check the type of the bound expression against the provided type
      (typeOfBoundExpr, typeOfBoundExprType) <- inferExpr (typeOf binder)
      checkExprTypesEqual p typeOfBoundExpr (TypeUniverse (inserted p) 0) typeOfBoundExprType

      checkedBoundExpr <- checkExpr typeOfBoundExpr boundExpr
      let checkedBinder = replaceBinderType typeOfBoundExpr binder

      (checkedBody, typeOfBody) <-
        addToBoundCtx (nameOf binder, typeOfBoundExpr, Just checkedBoundExpr) $ inferExpr body

      -- It's possible for the type of the body to depend on the let bound variable,
      -- e.g. `let y = Nat in (2 : y)` so in order to avoid the DeBruijn index escaping
      -- it's context we need to substitute the bound expression into the type.
      normTypeOfBody <- if isMeta typeOfBody
        then return typeOfBody
        else do
          let normTypeOfBody = checkedBoundExpr `substInto` typeOfBody
          when (normTypeOfBody /= typeOfBody) $
            logDebug MaxDetail $ "normalising" <+> prettyVerbose typeOfBody <+> "to" <+> prettyVerbose normTypeOfBody
          return normTypeOfBody

      return (Let p checkedBoundExpr checkedBinder checkedBody , normTypeOfBody)

    Lam p binder body -> do
      -- Infer the type of the bound variable from the binder
      (typeOfBinder, typeOfBinderType) <- inferExpr (typeOf binder)

      let p' = inserted p
      checkExprTypesEqual p typeOfBinder (TypeUniverse p' 0) typeOfBinderType
      let checkedBinder = replaceBinderType typeOfBinder binder

      -- Update the context with the bound variable
      (checkedBody , typeOfBody) <-
        addToBoundCtx (nameOf binder, typeOfBinder, Nothing) $ inferExpr body

      let t' = Pi p' checkedBinder typeOfBody
      return (Lam p checkedBinder checkedBody , t')

    Builtin p op -> do
      return (Builtin p op, typeOfBuiltin p op)

    LVec ann elems -> do
      let p = provenanceOf ann

      -- Infer the type for each element in the list
      elemTypePairs <- traverse inferExpr elems
      -- Insert any implicit arguments for each element in the list to try and
      -- standardise the types
      elemTypePairs' <- traverse (uncurry $ insertNonExplicitArgs p) elemTypePairs
      let (checkedElems, typesOfElems) = unzip elemTypePairs'

      -- Create the new type.
      -- Roughly [x1, ..., xn] has type
      --  forall {tElem} {{TypesEqual tElem [t1, ..., tn]}} . Vector tElem n
      let liftedTypesOfElems = liftFreeDBIndices 3 <$> typesOfElems
      let typesOfElemsSeq = mkList p (TypeUniverse p 0) liftedTypesOfElems
      let tc = AlmostEqualConstraint
      let elemsTC tElem = BuiltinTypeClass p tc (ExplicitArg p <$> [tElem, typesOfElemsSeq])
      let typeOfContainer =
            Pi p (ImplicitBinder p Nothing (TypeUniverse p 0)) $
              Pi p (IrrelevantInstanceBinder p Nothing (elemsTC (Var p (Bound 0)))) $
                VectorType p (Var p (Bound 1)) (NatLiteral p (length elems))

      -- Return the result
      return (LVec ann checkedElems, typeOfContainer)

      -- TODO re-enable once we have the universe solver up and running.
      {-
      (checkedTypeClass, typeClassType) <- inferExpr typeClass
      unify ann typeClassType (TypeUniverse (inserted ann) 0)
      return (PrimDict ann checkedTypeClass, checkedTypeClass)
      -}

  showInferExit res
  return res

inferLiteral :: Provenance -> Literal -> (CheckedExpr, CheckedType)
inferLiteral p l = (Literal p l, typeOfLiteral p l)

-- | Takes a function and its arguments, inserts any needed implicits
-- or instance arguments and then returns the function applied to the full
-- list of arguments as well as the result type.
inferApp :: MonadBidirectional m
         => Provenance
         -> CheckedExpr
         -> CheckedType
         -> [UncheckedArg]
         -> m (CheckedExpr, CheckedType)
inferApp p fun funType args = do
  (appliedFunType, checkedArgs) <- inferArgs (fun, args) funType args
  return (normAppList p fun checkedArgs, appliedFunType)

-- | Takes the expected type of a function and the user-provided arguments
-- and traverses through checking each argument type against the type of the
-- matching pi binder and inserting any required implicit/instance arguments.
-- Returns the type of the function when applied to the full list of arguments
-- (including inserted arguments) and that list of arguments.
inferArgs :: MonadBidirectional m
          => (CheckedExpr, [UncheckedArg])    -- The original function and its arguments
          -> CheckedType    -- Type of the function
          -> [UncheckedArg] -- User-provided arguments of the function
          -> m (CheckedType, [CheckedArg])
inferArgs original@(fun, args') piT@(Pi _ binder resultType) args
  | isExplicit binder && null args = return (piT, [])
  | otherwise = do
    let p = provenanceOf fun

    -- Determine whether we have an arg that matches the binder
    (matchedUncheckedArg, remainingUncheckedArgs) <- case args of
      [] -> return (Nothing, args)
      (arg : remainingArgs)
        | visibilityMatches binder arg -> return (Just arg, remainingArgs)
        | isExplicit binder            -> missingExplicitArgumentError binder arg
        | otherwise                    -> return (Nothing, args)

    -- Calculate what the new checked arg should be, create a fresh meta if no arg was matched above
    checkedArgExpr <- do
      let binderType = typeOf binder
      case matchedUncheckedArg of
        Just arg -> checkExpr binderType (argExpr arg)
        Nothing
          | isImplicit binder -> freshExprMeta p binderType =<< getBoundCtx
          | otherwise         -> do
              ctx <- getBoundCtx
              meta <- addFreshTypeClassConstraint ctx fun args' binderType
              return $ Meta p meta

    let checkedArg = Arg p (visibilityOf binder) (relevanceOf binder) checkedArgExpr

    -- Substitute the checked arg through the result of the Pi type.
    let substResultType = argExpr checkedArg `substInto` resultType

    -- Recurse if necessary to check the remaining unchecked args
    let needToRecurse = not (null remainingUncheckedArgs) || visibilityOf binder /= Explicit
    (typeAfterApplication, checkedArgs) <- if needToRecurse
      then inferArgs original substResultType remainingUncheckedArgs
      else return (substResultType, [])

    -- Return the result
    return (typeAfterApplication, checkedArg : checkedArgs)

inferArgs fun nonPiType args
  | null args = return (nonPiType, [])
  | otherwise    = do
    ctx <- getBoundCtx
    let p = provenanceOf fun
    let p' = inserted p
    let mkRes = [Endo $ \tRes -> pi (visibilityOf arg) (relevanceOf arg) (tHole ("arg" <> pack (show i))) (const tRes)
                | (i, arg) <- zip [0::Int ..] args]
    let expectedType = fromDSL p' (appEndo (mconcat mkRes) (tHole "res"))
    throwError $ TypeMismatch p (boundContextOf ctx) nonPiType expectedType

insertNonExplicitArgs :: MonadBidirectional m
                      => Provenance
                      -> CheckedExpr
                      -> CheckedType
                      -> m (CheckedExpr, CheckedType)
insertNonExplicitArgs ann checkedExpr actualType = inferApp ann checkedExpr actualType []

missingExplicitArgumentError :: MonadBidirectional m => CheckedBinder -> UncheckedArg -> m a
missingExplicitArgumentError expectedBinder actualArg = do
  -- Then we're expecting an explicit arg but have a non-explicit arg so error
  ctx <- getBoundCtx
  throwError $ MissingExplicitArg (boundContextOf ctx) actualArg (typeOf expectedBinder)

--------------------------------------------------------------------------------
-- Typing of literals and builtins

-- | Return the type of the provided literal,
typeOfLiteral :: Provenance -> Literal -> CheckedType
typeOfLiteral ann l = fromDSL ann $ case l of
  LUnit      -> tUnit
  LBool _    -> tAnnBool constant unquantified
  LIndex n _ -> tIndex (natLit n)
  LNat{}     -> tNat
  LInt{}     -> tInt
  LRat{}     -> tAnnRat constant
