module Vehicle.Compile.Prelude.Utils where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Maybe (mapMaybe)
import Vehicle.Data.Code.Expr
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

getBinderName :: GenericBinder expr -> Name
getBinderName binder = case binderNamingForm binder of
  NameAndType name -> name
  OnlyName name -> name
  OnlyType -> developerError "Binder unexpectedly does not appear to have a name"

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

filterOutNonExplicitArgs :: NonEmpty (Arg builtin) -> [Expr builtin]
filterOutNonExplicitArgs args = mapMaybe getExplicitArg (NonEmpty.toList args)
