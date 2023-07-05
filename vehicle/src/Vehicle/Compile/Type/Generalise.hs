module Vehicle.Compile.Type.Generalise
  ( generaliseOverUnsolvedConstraints,
    generaliseOverUnsolvedMetaVariables,
  )
where

import Control.Monad (foldM, forM)
import Data.Data (Proxy (..))
import Data.List.NonEmpty ((<|))
import Data.Maybe (fromMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Meta.Substitution (substMetas)
import Vehicle.Compile.Type.Monad
import Vehicle.Expr.DeBruijn

--------------------------------------------------------------------------------
-- Type-class generalisation

-- Finds any unsolved type class constraints that are blocked on
-- metas that occur in the type of the declaration. It then appends these
-- constraints as instance arguments to the declaration.
generaliseOverUnsolvedConstraints ::
  (TCM builtin m) =>
  Decl Ix builtin ->
  m (Decl Ix builtin)
generaliseOverUnsolvedConstraints decl =
  logCompilerPass MidDetail "generalisation over unsolved type-class constraints" $ do
    unsolvedTypeClassConstraints <- traverse substMetas =<< getActiveTypeClassConstraints
    unsolvedConstraints <- traverse substMetas =<< getActiveConstraints

    (generalisedDecl, rejectedTypeClassConstraints) <-
      foldM (generaliseOverConstraint unsolvedConstraints) (decl, []) unsolvedTypeClassConstraints
    setTypeClassConstraints rejectedTypeClassConstraints
    return generalisedDecl

generaliseOverConstraint ::
  (TCM builtin m) =>
  [WithContext (Constraint builtin)] ->
  (Decl Ix builtin, [WithContext (TypeClassConstraint builtin)]) ->
  WithContext (TypeClassConstraint builtin) ->
  m (Decl Ix builtin, [WithContext (TypeClassConstraint builtin)])
generaliseOverConstraint allConstraints (decl, rejected) c@(WithContext tc ctx) = do
  -- Find any unsolved meta variables that are transitively linked
  -- by constraints of the same type.
  linkedMetas <- getMetasLinkedToMetasIn allConstraints (typeOf decl)
  -- Only prepend the constraint if all variables in the constraint
  -- are so linked.
  substTC <- substMetas tc
  constraintMetas <- metasIn substTC
  let prependable = constraintMetas `MetaSet.isSubsetOf` linkedMetas

  if not prependable
    then do
      logDebug MaxDetail $ "Found non-prependable type-class constraint" <+> prettyVerbose c
      return (decl, c : rejected)
    else do
      generalisedDecl <- prependConstraint decl (WithContext substTC ctx)
      return (generalisedDecl, rejected)

prependConstraint ::
  (TCM builtin m) =>
  Decl Ix builtin ->
  WithContext (TypeClassConstraint builtin) ->
  m (Decl Ix builtin)
prependConstraint decl (WithContext (Has meta expr) ctx) = do
  let p = originalProvenance ctx
  typeClass <- quote p 0 expr
  relevancy <- typeClassRelevancy expr

  substTypeClass <- substMetas typeClass
  logCompilerPass MaxDetail ("generalisation over" <+> prettyVerbose substTypeClass) $
    prependBinderAndSolveMeta meta (BinderDisplayForm OnlyType True) (Instance True) relevancy substTypeClass decl

--------------------------------------------------------------------------------
-- Unsolved meta generalisation

-- | Finds any unsolved metas that occur in the type of the declaration. For
-- each such meta, it then prepends a new quantified variable to the declaration
-- type and then solves the meta as that new variable.
generaliseOverUnsolvedMetaVariables ::
  forall builtin m.
  (TCM builtin m) =>
  Decl Ix builtin ->
  m (Decl Ix builtin)
generaliseOverUnsolvedMetaVariables decl = do
  let declType = typeOf decl

  if isTypeUniverse declType
    then -- In a type synonym so don't quantify over anything
      return decl
    else do
      -- Quantify over any unsolved type-level meta variables
      unsolvedMetas <- metasIn (typeOf decl)
      if MetaSet.null unsolvedMetas
        then return decl
        else logCompilerPass MidDetail "generalisation of unsolved metas in declaration type" $ do
          result <- foldM quantifyOverMeta decl (MetaSet.toList unsolvedMetas)
          substMetas result

quantifyOverMeta ::
  forall builtin m.
  (TCM builtin m) =>
  Decl Ix builtin ->
  MetaID ->
  m (Decl Ix builtin)
quantifyOverMeta decl meta = do
  metaType <- substMetas =<< getMetaType meta
  if isMeta metaType
    then
      compilerDeveloperError $
        "Haven't thought about what to do when type of unsolved meta is also"
          <+> "an unsolved meta."
    else do
      metaDoc <- prettyMeta (Proxy @builtin) meta
      logCompilerPass MidDetail ("generalisation over" <+> metaDoc) $ do
        -- Prepend the implicit binders for the new generalised variable.
        binderName <- getBinderNameOrFreshName Nothing metaType
        let binderDisplayForm = BinderDisplayForm (OnlyName binderName) True
        prependBinderAndSolveMeta meta binderDisplayForm (Implicit True) Relevant metaType decl

isMeta :: Expr Ix builtin -> Bool
isMeta Meta {} = True
isMeta (App _ Meta {} _) = True
isMeta _ = False

--------------------------------------------------------------------------------
-- Utilities

prependBinderAndSolveMeta ::
  forall builtin m.
  (TCM builtin m) =>
  MetaID ->
  BinderDisplayForm ->
  Visibility ->
  Relevance ->
  Type Ix builtin ->
  Decl Ix builtin ->
  m (Decl Ix builtin)
prependBinderAndSolveMeta meta f v r binderType decl = do
  -- All the metas contained within the type of the binder about to be
  -- appended cannot have any dependencies on variables later on in the expression.
  -- So the replace them with meta-variables with empty contexts.
  (substBinderType, substDecl) <- removeContextsOfMetasIn binderType decl

  -- Construct the new binder and prepend it to both the type and
  -- (if applicable) the body of the declaration.
  let typeBinder = Binder (provenanceOf decl) f v r substBinderType
  let bodyBinderForm = BinderDisplayForm (OnlyName (fromMaybe "_" (nameOf f))) True
  let bodyBinder = Binder (provenanceOf decl) bodyBinderForm v r substBinderType
  prependedDecl <- case substDecl of
    DefAbstract p rt ident t ->
      return $ DefAbstract p rt ident (Pi p typeBinder t)
    DefFunction p ident anns t e ->
      return $ DefFunction p ident anns (Pi p typeBinder t) (Lam p bodyBinder e)

  -- Then we add i) the new binder to the context of the meta-variable being
  -- solved, and ii) a new argument to all uses of the meta-variable so
  -- that meta-subsitution will work later.
  extendBoundCtxOfMeta meta typeBinder
  let updatedDecl = addNewArgumentToMetaUses meta prependedDecl

  -- We now solve the meta as the newly bound variable
  metaCtx <- getMetaCtx @builtin meta
  let p = provenanceOf prependedDecl
  let solution = BoundVar p (Ix $ length metaCtx - 1)
  solveMeta meta solution metaCtx

  logDebug MaxDetail $ "prepended-fresh-binder:" <+> prettyVerbose updatedDecl

  -- Substitute the new meta solution through.
  resultDecl <- substMetas updatedDecl

  logCompilerPassOutput $ prettyVerbose resultDecl
  return resultDecl

removeContextsOfMetasIn ::
  forall builtin m.
  (TCM builtin m) =>
  Type Ix builtin ->
  Decl Ix builtin ->
  m (Type Ix builtin, Decl Ix builtin)
removeContextsOfMetasIn binderType decl =
  logCompilerPass MaxDetail "removing dependencies from dependent metas" $ do
    metasInBinder <- metasIn binderType
    newMetas <- or <$> forM (MetaSet.toList metasInBinder) (removeMetaDependencies (Proxy @builtin))

    if not newMetas
      then return (binderType, decl)
      else do
        substDecl <- substMetas decl
        substBinderType <- substMetas binderType
        logCompilerPassOutput (prettyVerbose substDecl)
        return (substBinderType, substDecl)

addNewArgumentToMetaUses :: MetaID -> Decl Ix builtin -> Decl Ix builtin
addNewArgumentToMetaUses meta = fmap (go (-1))
  where
    go :: Lv -> Expr Ix builtin -> Expr Ix builtin
    go d expr = case expr of
      Meta p m
        | m == meta -> App p (Meta p m) [newVar p]
      App p (Meta p' m) args
        | m == meta -> App p (Meta p' m) (newVar p <| goArgs args)
      Universe {} -> expr
      Hole {} -> expr
      Meta {} -> expr
      Builtin {} -> expr
      FreeVar {} -> expr
      BoundVar {} -> expr
      Ann p e t -> Ann p (go d e) (go d t)
      App p fun args -> App p (go d fun) (goArgs args)
      Pi p binder result -> Pi p (goBinder binder) (go (d + 1) result)
      Let p bound binder body -> Let p (go d bound) (goBinder binder) (go (d + 1) body)
      Lam p binder body -> Lam p (goBinder binder) (go (d + 1) body)
      where
        newVar p = ExplicitArg p (BoundVar p $ shiftDBIndex 0 d)
        goBinder = fmap (go d)
        goArgs = fmap (fmap (go d))
