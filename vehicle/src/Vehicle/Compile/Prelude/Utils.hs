module Vehicle.Compile.Prelude.Utils where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Maybe (mapMaybe)
import GHC.Stack (HasCallStack)
import Vehicle.Data.AST.Expr.Scoped
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Utility functions

isTypeUniverse :: Expr builtin -> Bool
isTypeUniverse TypeUniverse {} = True
isTypeUniverse _ = False

--------------------------------------------------------------------------------
-- Enumeration functions

freeNamesIn :: Expr builtin -> [Identifier]
freeNamesIn = \case
  FreeVar _ ident -> [ident]
  BoundVar {} -> []
  Universe {} -> []
  Hole {} -> []
  Meta {} -> []
  Builtin {} -> []
  App fun args -> freeNamesIn fun <> concatMap (freeNamesIn . argExpr) args
  Pi _ binder result -> freeNamesIn (typeOf binder) <> freeNamesIn result
  Let _ bound binder body -> freeNamesIn bound <> freeNamesIn (typeOf binder) <> freeNamesIn body
  Lam _ binder body -> freeNamesIn (typeOf binder) <> freeNamesIn body
  Record _ _ fields -> concatMap (freeNamesIn . snd) fields
  RecordProj _ t r _ -> freeNamesIn t <> freeNamesIn r

--------------------------------------------------------------------------------
-- Destruction functions

toHead :: Expr builtin -> (Expr builtin, [Arg builtin])
toHead (App fun args) = (fun, NonEmpty.toList args)
toHead e = (e, [])

exprHead :: Expr builtin -> Expr builtin
exprHead = fst . toHead

onlyExplicit :: NonEmpty (GenericArg expr) -> [expr]
onlyExplicit args = argExpr <$> filter isExplicit (NonEmpty.toList args)

--------------------------------------------------------------------------------
-- Views

getMetaID :: Expr builtin -> Maybe MetaID
getMetaID e = case exprHead e of
  Meta _ m -> Just m
  _ -> Nothing

-- | Should only be called on binders that are guaranteed to have a name.
getBinderName :: (HasCallStack) => GenericBinder expr -> Name
getBinderName binder = fst $ getNamedBinderInfo binder

getMaybeNamedBinderInfo :: GenericBinder expr -> Maybe (Name, Provenance)
getMaybeNamedBinderInfo binder = case binderNamingForm binder of
  NameAndType name p -> Just (name, p)
  OnlyName name p -> Just (name, p)
  OnlyType -> Nothing

getNamedBinderInfo :: (HasCallStack) => GenericBinder expr -> (Name, Provenance)
getNamedBinderInfo binder = case getMaybeNamedBinderInfo binder of
  Just (name, p) -> (name, p)
  Nothing -> developerError "Binder unexpectedly does not appear to have a name"

getExplicitArg :: GenericArg expr -> Maybe expr
getExplicitArg arg
  | isExplicit arg = Just (argExpr arg)
  | otherwise = Nothing

getImplicitArg :: Arg builtin -> Maybe (Expr builtin)
getImplicitArg arg
  | isImplicit arg = Just (argExpr arg)
  | otherwise = Nothing

getRelevantArg :: GenericArg expr -> Maybe expr
getRelevantArg arg
  | isRelevant arg = Just (argExpr arg)
  | otherwise = Nothing

filterOutNonExplicitArgs :: [GenericArg expr] -> [expr]
filterOutNonExplicitArgs = mapMaybe getExplicitArg
