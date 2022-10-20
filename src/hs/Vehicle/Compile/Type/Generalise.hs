module Vehicle.Compile.Type.Generalise
  ( generaliseOverUnsolvedTypeClassConstraints
  , generaliseOverUnsolvedMetaVariables
  ) where

import Control.Monad ( foldM, forM )
import Data.List.NonEmpty ((<|))

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Language.Print
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.MetaSet qualified as MetaSet
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Monad

--------------------------------------------------------------------------------
-- Type-class generalisation

-- Finds any unsolved type class constraints that are blocked on
-- metas that occur in the type of the declaration. It then appends these
-- constraints as instance arguments to the declaration.
generaliseOverUnsolvedTypeClassConstraints :: TCM m
                                           => CheckedDecl
                                           -> m CheckedDecl
generaliseOverUnsolvedTypeClassConstraints decl = do
  -- Tests if a constraint is prependable
  let isPrependable c = do
        if not (isTypeClassConstraint c)
          then return False
        else do
          let metaFilter = if isAuxiliaryTypeClassConstraint c
              then (\e -> isPolarityUniverse e || isLinearityUniverse e)
              else isTypeUniverse
          -- Find any unsolved meta variables that are transitively linked
          -- by constraints of the same type.
          linkedMetas <- getMetasLinkedToMetasIn (typeOf decl) metaFilter
          -- Only prepend the constraint if all variables in the constraint
          -- are so linked.
          return $ metasIn c `MetaSet.isSubsetOf` linkedMetas

  (prependableConstraints, nonPrependableConstraints) <-
    partitionM isPrependable =<< getUnsolvedConstraints
  setConstraints nonPrependableConstraints

  if null prependableConstraints
    then return decl
    else logCompilerPass MinDetail "generalisation over unsolved type-class constraints" $ do
      result <- foldM prependConstraint decl prependableConstraints
      return result

prependConstraint :: TCM m
                  => CheckedDecl
                  -> Constraint
                  -> m CheckedDecl
prependConstraint decl constraint = do
  (typeClass, meta) <- case constraint of
    TC _ (Has meta tc args) -> do
      let p = originalProvenance $ constraintContext constraint
      return (BuiltinTypeClass p tc args, meta)
    UC{}                    -> compilerDeveloperError
      "Unification constraints should have been filtered out earlier"

  relevancy <- case typeClass of
    BuiltinTypeClass _ tc _ -> return $ relevanceOf tc
    _                       -> compilerDeveloperError "Malformed type-class when finding relevancy"

  substTypeClass <- substMetas typeClass
  logCompilerPass MaxDetail ("generalisation over" <+> prettySimple substTypeClass) $
    prependBinderAndSolveMeta meta Instance relevancy Nothing substTypeClass decl

--------------------------------------------------------------------------------
-- Unsolved meta generalisation

-- | Finds any unsolved metas that occur in the type of the declaration. For
-- each such meta, it then prepends a new quantified variable to the declaration
-- type and then solves the meta as that new variable.
generaliseOverUnsolvedMetaVariables :: TCM m
                                    => CheckedDecl
                                    -> m CheckedDecl
generaliseOverUnsolvedMetaVariables decl = do
  let declType = typeOf decl

  unsolvedMetas <- if isTypeUniverse declType
    -- In a type synonym so quantify only over auxiliary metas (unsure about this!)
    then getUnsolvedAuxiliaryMetas
    -- Quantify over any unsolved type-level meta variables
    else return $ metasIn (typeOf decl)

  if MetaSet.null unsolvedMetas
    then return decl
    else logCompilerPass MinDetail "generalisation of unsolved metas in declaration type" $ do
      result <- foldM quantifyOverMeta decl (MetaSet.toList unsolvedMetas)
      substMetas result

quantifyOverMeta :: TCM m
                 => CheckedDecl
                 -> Meta
                 -> m CheckedDecl
quantifyOverMeta decl meta = do
  metaType <- substMetas =<< getMetaType meta
  if isMeta metaType
    then compilerDeveloperError $
      "Haven't thought about what to do when type of unsolved meta is also" <+>
      "an unsolved meta."
  else do
    -- TODO more principled to store relevance with the meta itself.
    let relevance = if isAuxiliaryUniverse metaType then Irrelevant else Relevant
    metaDoc <- prettyMeta meta
    logCompilerPass MinDetail ("generalisation over" <+> metaDoc) $ do
      -- Prepend the implicit binders for the new generalised variable.
      prependBinderAndSolveMeta meta Implicit relevance Nothing metaType decl

--------------------------------------------------------------------------------
-- Utilities

prependBinderAndSolveMeta :: TCM m
                          => Meta
                          -> Visibility
                          -> Relevance
                          -> DBBinding
                          -> CheckedType
                          -> CheckedDecl
                          -> m CheckedDecl
prependBinderAndSolveMeta meta v r binderName binderType decl = do
  -- All the metas contained within the type of the binder about to be
  -- appended cannot have any dependencies on variables later on in the expression.
  -- So the replace them with meta-variables with empty contexts.
  (substBinderType, substDecl) <- removeContextsOfMetasIn binderType decl

  -- Construct the new binder and prepend it to both the type and
  -- (if applicable) the body of the declaration.
  let binder = Binder (provenanceOf decl) v r binderName substBinderType
  prependedDecl <- case substDecl of
    DefResource p rt ident t   ->
      return $ DefResource p rt ident (Pi p binder t)
    DefFunction p ident t e ->
      return $ DefFunction p ident (Pi p binder t) (Lam p binder e)
    DefPostulate{} ->
      compilerDeveloperError "Generalisation over postulates not yet supported"

  -- Then we add i) the new binder to the context of the meta-variable being
  -- solved, and ii) a new argument to all uses of the meta-variable so
  -- that meta-subsitution will work later.
  addNewBinderToMetaContext meta binderName substBinderType
  let consistentDecl = addNewArgumentToMetaUses meta prependedDecl

  logDebug MaxDetail $ "prepended-fresh-binder:" <+> prettyVerbose consistentDecl

  -- We now solve the meta as the newly bound variable
  MetaInfo _ _ metaCtx <- getMetaInfo meta
  let ann = provenanceOf consistentDecl
  let solution = Var ann (Bound (length metaCtx - 1))
  metaSolved meta solution

  -- Substitute the new meta solution through.
  resultDecl <- substMetas consistentDecl

  logCompilerPassOutput $ prettyVerbose resultDecl
  return resultDecl

removeContextsOfMetasIn :: TCM m
                        => CheckedType
                        -> CheckedDecl
                        -> m (CheckedType, CheckedDecl)
removeContextsOfMetasIn binderType decl =
  logCompilerPass MaxDetail "removing dependencies from dependent metas" $ do
    let metasInBinder = metasIn binderType
    newMetas <- or <$> forM (MetaSet.toList metasInBinder) (\m -> do
      MetaInfo p t ctx <- getMetaInfo m
      if null ctx then
        return False
      else do
        newMeta <- freshExprMeta p t []
        metaSolved m newMeta
        return True)

    if not newMetas then
      return (binderType, decl)
    else do
      substDecl <- substMetas decl
      substBinderType <- substMetas binderType
      logCompilerPassOutput (prettyVerbose substDecl)
      return (substBinderType, substDecl)

addNewArgumentToMetaUses :: Meta -> CheckedDecl -> CheckedDecl
addNewArgumentToMetaUses meta = mapDeclExprs (go (-1))
  where
    go :: BindingDepth -> CheckedExpr -> CheckedExpr
    go d expr = case expr of
      Meta p m
        | m == meta -> App p (Meta p m) [newVar p]
      App p (Meta p' m) args
        | m == meta -> App p (Meta p' m) (newVar p <| goArgs args)

      Universe{}                   -> expr
      Hole{}                       -> expr
      Meta{}                       -> expr
      Literal{}                    -> expr
      Builtin{}                    -> expr
      Var{}                        -> expr
      Ann      p e t               -> Ann p (go d e) (go d t)
      App p fun args               -> App p (go d fun) (goArgs args)
      Pi       p binder result     -> Pi p (goBinder binder) (go (d+1) result)
      Let      p bound binder body -> Let p (go d bound) (goBinder binder) (go (d+1) body)
      Lam      p binder body       -> Lam p (goBinder binder) (go (d+1) body)
      LVec     p xs                -> LVec p (map (go d) xs)
      where
        newVar p = ExplicitArg p (Var p (Bound d))
        goBinder = fmap (go d)
        goArgs   = fmap (fmap (go d))

addNewBinderToMetaContext :: TCM m => Meta -> DBBinding -> CheckedType -> m ()
addNewBinderToMetaContext m newVarName newVarType =
  modifyMetasInfo m $ \(MetaInfo p n ctx) ->
    let entry = (newVarName, newVarType, Nothing) in
    MetaInfo p n (ctx <> [entry])