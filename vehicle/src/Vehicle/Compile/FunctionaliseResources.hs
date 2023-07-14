module Vehicle.Compile.FunctionaliseResources
  ( functionaliseResources,
  )
where

import Control.Monad (when)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Writer (MonadWriter (..), execWriterT)
import Data.Bifunctor (Bifunctor (..))
import Data.LinkedHashMap (LinkedHashMap)
import Data.LinkedHashMap qualified as LinkedHashMap (empty, filterWithKey, insert, member, toList)
import Data.Map (Map)
import Data.Map qualified as Map (fromList, insert, lookup)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set (fromList, member, singleton)
import Vehicle.Compile.Error (MonadCompile, internalScopingError, lookupInDeclCtx)
import Vehicle.Compile.Prelude (FreeVarUpdate, traverseFreeVarsM)
import Vehicle.Compile.Prelude.Contexts (DeclCtx)
import Vehicle.Compile.Print (PrintableBuiltin, prettyFriendly)
import Vehicle.Expr.DeBruijn (Ix, Lv (..), dbLevelToIndex)
import Vehicle.Prelude (traverseListLocal)
import Vehicle.Prelude.Logging
import Vehicle.Prelude.Prettyprinter
import Vehicle.Syntax.AST

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
  Prog Ix builtin ->
  m (Prog Ix builtin)
functionaliseResources prog =
  logCompilerPass MidDetail currentPass $ do
    runReaderT (functionaliseProg prog) (FuncState LinkedHashMap.empty mempty)

--------------------------------------------------------------------------------
-- Utilities

currentPass :: CompilerPass
currentPass = "resource functionalisation"

data FuncState builtin = FuncState
  { resourceDeclarations :: LinkedHashMap Name (Type Ix builtin),
    resourceUsageDeclCtx :: DeclCtx [Name]
  }

addResourceDeclaration :: Identifier -> Type Ix builtin -> FuncState builtin -> FuncState builtin
addResourceDeclaration resource typ FuncState {..} =
  FuncState
    { resourceDeclarations = LinkedHashMap.insert (nameOf resource) typ resourceDeclarations,
      ..
    }

addResourceUsage :: Identifier -> [Name] -> FuncState builtin -> FuncState builtin
addResourceUsage ident newArgNames FuncState {..} =
  FuncState
    { resourceUsageDeclCtx = Map.insert ident newArgNames resourceUsageDeclCtx,
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
  Prog Ix builtin ->
  m (Prog Ix builtin)
functionaliseProg (Main ds) =
  Main . catMaybes <$> traverseListLocal functionaliseDecl ds

functionaliseDecl ::
  (MonadResource m builtin) =>
  Decl Ix builtin ->
  m (FuncState builtin -> FuncState builtin, Maybe (Decl Ix builtin))
functionaliseDecl d =
  logCompilerPass MaxDetail ("functionalising" <+> quotePretty (nameOf d)) $ case d of
    DefAbstract p i s initialType -> do
      typeResourceUsage <- findResourceUses initialType
      (mkBinder, binders, binderNames) <- createBinders True p typeResourceUsage
      finalType <- replaceResourceUses (mkBinder, binders, binderNames) initialType

      return $ case s of
        PostulateDef -> (addResourceUsage i binderNames, Just (DefAbstract p i s finalType))
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

findResourceUses ::
  (MonadResource m builtin) =>
  Expr Ix builtin ->
  m (Set Name)
findResourceUses e = execWriterT $ traverseFreeVarsM (const id) updateFn e
  where
    updateFn recGo p1 p2 ident args = do
      args' <- traverse (traverse recGo) args
      FuncState {..} <- ask
      let name = nameOf ident
      when (name `LinkedHashMap.member` resourceDeclarations) $ do
        tell (Set.singleton name)
      resourceArgs <- lookupInDeclCtx currentPass ident resourceUsageDeclCtx
      tell (Set.fromList resourceArgs)
      return $ normAppList p1 (FreeVar p2 ident) args'

replaceResourceUses ::
  forall m builtin.
  (MonadResource m builtin) =>
  (Binder Ix builtin -> Expr Ix builtin -> Expr Ix builtin, [Binder Ix builtin], [Name]) ->
  Expr Ix builtin ->
  m (Expr Ix builtin)
replaceResourceUses (mkBinder, binders, binderNames) initialExpr = do
  funcState <- ask
  let resourceLevels = Map.fromList (zip binderNames [(0 :: Lv) ..])
  let underBinder _b = local (first (+ 1))
  let readerState = (Lv 0, (funcState, resourceLevels))
  processedAppsExpr <- runReaderT (traverseFreeVarsM underBinder updateFn initialExpr) readerState
  return $ foldr mkBinder processedAppsExpr binders
  where
    updateFn :: FreeVarUpdate (ReaderT (Lv, (FuncState builtin, Map Name Lv)) m) Ix builtin
    updateFn recGo p1 p2 ident args = do
      args' <- traverse (traverse recGo) args
      (currentOldLv, (FuncState {..}, resourceLevels)) <- ask
      let currentNewLv = Lv (length resourceLevels) + currentOldLv
      let name = nameOf ident

      let mkResourceVar resourceName = do
            let maybeResourceLevel = Map.lookup resourceName resourceLevels
            case maybeResourceLevel of
              Nothing -> internalScopingError currentPass ident
              Just resourceLv -> do
                let resourceIx = dbLevelToIndex currentNewLv resourceLv
                -- logDebug MaxDetail $ pretty name <+> pretty resourceName <+> pretty currentOldLv <+> pretty currentNewLv <+> pretty resourceLv <+> pretty resourceIx
                return $ BoundVar p1 resourceIx

      newFun <-
        if name `LinkedHashMap.member` resourceDeclarations
          then mkResourceVar name
          else return $ FreeVar p2 ident

      extraResourceNames <- lookupInDeclCtx currentPass ident resourceUsageDeclCtx
      extraResourceVarArgs <- traverse mkResourceVar extraResourceNames
      let extraResourceArgs = fmap (ExplicitArg p1) extraResourceVarArgs
      return $ normAppList p1 newFun (extraResourceArgs <> args')

createBinders ::
  (MonadResource m builtin) =>
  Bool ->
  Provenance ->
  Set Name ->
  m (Binder Ix builtin -> Expr Ix builtin -> Expr Ix builtin, [Binder Ix builtin], [Name])
createBinders isType p idents = do
  FuncState {..} <- ask
  let identsAndTypes = LinkedHashMap.filterWithKey (\i _ -> Set.member i idents) resourceDeclarations
  let identsAndTypesList = LinkedHashMap.toList identsAndTypes
  let mkBindingForm ident
        | isType = BinderDisplayForm (NameAndType (nameOf ident)) True
        | otherwise = BinderDisplayForm (OnlyName (nameOf ident)) True
  let mkBinder (ident, typ) = Binder p (mkBindingForm ident) Explicit Relevant typ
  let binders = fmap mkBinder identsAndTypesList
  let binderConstructor
        | isType = Pi p
        | otherwise = Lam p
  let binderNames = fmap (nameOf . fst) identsAndTypesList
  return (binderConstructor, binders, binderNames)
