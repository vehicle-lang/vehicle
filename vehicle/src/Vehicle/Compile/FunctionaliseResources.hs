module Vehicle.Compile.FunctionaliseResources
  ( functionaliseResources,
  )
where

import Control.Monad (when)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Writer (MonadWriter (..), execWriterT)
import Data.Bifunctor (Bifunctor (..))
import Data.Map (Map)
import Data.Map qualified as Map (fromList, insert, lookup)
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OMap (assocs, empty, filter, member, (>|))
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set (fromList, member, singleton)
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Variable.Bound.Level (dbLevelToIndex)

--------------------------------------------------------------------------------
-- Public interface

-- | This method traverses the program removing the resource declarations and
-- instead changing functions that use them so that they take the resources
-- explicitly as arguments instead. e.g.
--
--    @network
--    f : Rat -> Rat
--
--    @property
--    prop : Bool
--    prop = forall x . f x > 0
--
-- becomes:
--
--    @property
--    prop : (f : Rat -> Rat) -> Bool
--    prop f = forall x . f x > 0
--
-- Note that the semantics of properties therefore change slightly as they
-- are no longer guaranteed to be of type `Bool`.
functionaliseResources ::
  (MonadCompile m, PrintableBuiltin builtin) =>
  Prog builtin ->
  m (Prog builtin)
functionaliseResources prog =
  logCompilerSection2 MinDetail "resource functionalisation" $ do
    runReaderT (functionaliseProg prog) (FuncState OMap.empty mempty)

--------------------------------------------------------------------------------
-- Utilities

data FuncState builtin = FuncState
  { resourceDeclarations :: OMap Name (Type builtin),
    resourceUsageFreeCtx :: GenericFreeCtx [Name]
  }

addResourceDeclaration :: Identifier -> Type builtin -> FuncState builtin -> FuncState builtin
addResourceDeclaration resource typ FuncState {..} =
  FuncState
    { resourceDeclarations = resourceDeclarations OMap.>| (nameOf resource, typ),
      ..
    }

addResourceUsage :: Identifier -> [Name] -> FuncState builtin -> FuncState builtin
addResourceUsage ident newArgNames FuncState {..} =
  FuncState
    { resourceUsageFreeCtx = Map.insert ident newArgNames resourceUsageFreeCtx,
      ..
    }

type MonadResource m builtin =
  ( MonadCompile m,
    MonadReader (FuncState builtin) m,
    PrintableBuiltin builtin
  )

--------------------------------------------------------------------------------
-- Utilities

functionaliseProg ::
  (MonadResource m builtin) =>
  Prog builtin ->
  m (Prog builtin)
functionaliseProg (Main ds) =
  Main . catMaybes <$> traverseListLocal functionaliseDecl ds

functionaliseDecl ::
  (MonadResource m builtin) =>
  Decl builtin ->
  m (FuncState builtin -> FuncState builtin, Maybe (Decl builtin))
functionaliseDecl d =
  logCompilerSection2 MaxDetail ("functionalising" <+> quotePretty (nameOf d)) $ case d of
    DefAbstract p i s initialType -> do
      typeResourceUsage <- findResourceUses initialType
      (mkBinder, binders, binderNames) <- createBinders True p typeResourceUsage
      finalType <- replaceResourceUses (mkBinder, binders, binderNames) initialType

      return $ case s of
        PostulateDef {} -> (addResourceUsage i binderNames, Just (DefAbstract p i s finalType))
        _ -> (addResourceUsage i binderNames . addResourceDeclaration i finalType, Nothing)
    DefFunction p i anns initialType initialBody -> do
      typeResourceUsage <- findResourceUses initialType
      bodyResourceUsage <- findResourceUses initialBody
      let resourceUsage = typeResourceUsage <> bodyResourceUsage

      (mkTypeBinder, typeBinders, _) <- createBinders True p resourceUsage
      (mkBodyBinder, bodyBinders, binderNames) <- createBinders False p resourceUsage

      finalType <- replaceResourceUses (mkTypeBinder, typeBinders, binderNames) initialType
      finalBody <- replaceResourceUses (mkBodyBinder, bodyBinders, binderNames) initialBody

      let fun = DefFunction p i anns finalType finalBody
      logDebug MaxDetail $ "Prepending resources" <+> pretty binderNames
      logDebug MaxDetail $ prettyFriendly fun
      return (addResourceUsage i binderNames, Just fun)
    DefRecord {} ->
      return (id, Just d)

findResourceUses ::
  (MonadResource m builtin) =>
  Expr builtin ->
  m (Set Name)
findResourceUses e = do
  execWriterT $ traverseFreeVarsM (const id) updateFn e
  where
    updateFn recGo p ident args = do
      args' <- traverse (traverse recGo) args
      FuncState {..} <- ask
      let name = nameOf ident
      when (name `OMap.member` resourceDeclarations) $ do
        tell (Set.singleton name)
      let resourceArgs = lookupInFreeCtx ident resourceUsageFreeCtx
      tell (Set.fromList resourceArgs)
      return $ normAppList (FreeVar p ident) args'

replaceResourceUses ::
  forall m builtin.
  (MonadResource m builtin) =>
  (Binder builtin -> Expr builtin -> Expr builtin, [Binder builtin], [Name]) ->
  Expr builtin ->
  m (Expr builtin)
replaceResourceUses (mkBinder, binders, binderNames) initialExpr = do
  funcState <- ask
  let resourceLevels = Map.fromList (zip binderNames [(0 :: Lv) ..])
  let underBinder _b = local (first (+ 1))
  let readerState = (Lv 0, (funcState, resourceLevels))
  processedAppsExpr <- runReaderT (traverseFreeVarsM underBinder updateFn initialExpr) readerState
  return $ foldr mkBinder processedAppsExpr binders
  where
    updateFn :: FreeVarUpdate (ReaderT (Lv, (FuncState builtin, Map Name Lv)) m) builtin
    updateFn recGo p ident args = do
      args' <- traverse (traverse recGo) args
      (currentOldLv, (FuncState {..}, resourceLevels)) <- ask
      let currentNewLv = Lv (length resourceLevels) + currentOldLv
      let name = nameOf ident

      let mkResourceVar resourceName = do
            let maybeResourceLevel = Map.lookup resourceName resourceLevels
            case maybeResourceLevel of
              Nothing -> internalScopingError (pretty ident)
              Just resourceLv -> do
                let resourceIx = dbLevelToIndex currentNewLv resourceLv
                -- logDebug MaxDetail $ pretty name <+> pretty resourceName <+> pretty currentOldLv <+> pretty currentNewLv <+> pretty resourceLv <+> pretty resourceIx
                return $ BoundVar p resourceIx

      newFun <-
        if name `OMap.member` resourceDeclarations
          then mkResourceVar name
          else return $ FreeVar p ident

      let extraResourceNames = lookupInFreeCtx ident resourceUsageFreeCtx
      extraResourceVarArgs <- traverse mkResourceVar extraResourceNames
      let extraResourceArgs = fmap (Arg p Explicit Relevant) extraResourceVarArgs
      return $ normAppList newFun (extraResourceArgs <> args')

createBinders ::
  (MonadResource m builtin) =>
  Bool ->
  Provenance ->
  Set Name ->
  m (Binder builtin -> Expr builtin -> Expr builtin, [Binder builtin], [Name])
createBinders isType p idents = do
  FuncState {..} <- ask
  let identsAndTypes = OMap.filter (\i _ -> Set.member i idents) resourceDeclarations
  let identsAndTypesList = OMap.assocs identsAndTypes
  let mkBindingForm ident
        | isType = BinderDisplayForm OnlyType True
        | otherwise = BinderDisplayForm (OnlyName (nameOf ident)) True
  let mkBinder (ident, typ) = Binder p (mkBindingForm ident) Explicit Relevant typ
  let binders = fmap mkBinder identsAndTypesList
  let binderConstructor
        | isType = Pi p
        | otherwise = Lam p
  let binderNames = fmap (nameOf . fst) identsAndTypesList
  return (binderConstructor, binders, binderNames)
