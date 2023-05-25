module Vehicle.Compile.Prelude.Utils where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Maybe (mapMaybe)
import Vehicle.Prelude
import Vehicle.Syntax.AST

--------------------------------------------------------------------------------
-- Utility functions

isTypeUniverse :: Expr binder var builtin -> Bool
isTypeUniverse TypeUniverse {} = True
isTypeUniverse _ = False

--------------------------------------------------------------------------------
-- Enumeration functions

freeNamesIn :: Expr binder var builtin -> [Identifier]
freeNamesIn = \case
  FreeVar _ ident -> [ident]
  BoundVar {} -> []
  Universe {} -> []
  Hole {} -> []
  Meta {} -> []
  Builtin {} -> []
  Ann _ e t -> freeNamesIn e <> freeNamesIn t
  App _ fun args -> freeNamesIn fun <> concatMap (freeNamesIn . argExpr) args
  Pi _ binder result -> freeNamesIn (binderType binder) <> freeNamesIn result
  Let _ bound binder body -> freeNamesIn bound <> freeNamesIn (binderType binder) <> freeNamesIn body
  Lam _ binder body -> freeNamesIn (binderType binder) <> freeNamesIn body

--------------------------------------------------------------------------------
-- Destruction functions

toHead :: Expr binder var builtin -> (Expr binder var builtin, [Arg binder var builtin])
toHead (App _ fun args) = (fun, NonEmpty.toList args)
toHead e = (e, [])

exprHead :: Expr binder var builtin -> Expr binder var builtin
exprHead = fst . toHead

onlyExplicit :: NonEmpty (GenericArg expr) -> [expr]
onlyExplicit args = argExpr <$> filter isExplicit (NonEmpty.toList args)

--------------------------------------------------------------------------------
-- Views

getMetaID :: Expr binder var builtin -> Maybe MetaID
getMetaID e = case exprHead e of
  Meta _ m -> Just m
  _ -> Nothing

getBinderName :: GenericBinder () expr -> Name
getBinderName binder = case binderNamingForm binder of
  NameAndType name -> name
  OnlyName name -> name
  OnlyType -> developerError "Binder unexpectedly does not appear to have a name"

getExplicitArg :: GenericArg expr -> Maybe expr
getExplicitArg (ExplicitArg _ expr) = Just expr
getExplicitArg _ = Nothing

getImplicitArg :: Arg binder var builtin -> Maybe (Expr binder var builtin)
getImplicitArg (ImplicitArg _ arg) = Just arg
getImplicitArg _ = Nothing

filterOutNonExplicitArgs :: NonEmpty (Arg binder var builtin) -> [Expr binder var builtin]
filterOutNonExplicitArgs args = mapMaybe getExplicitArg (NonEmpty.toList args)

findInstanceArg :: [GenericArg a] -> (a, [GenericArg a])
findInstanceArg (InstanceArg _ inst : xs) = (inst, xs)
findInstanceArg (_ : xs) = findInstanceArg xs
findInstanceArg [] = developerError "Malformed type class operation"
