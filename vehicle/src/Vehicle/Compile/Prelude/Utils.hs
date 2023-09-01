module Vehicle.Compile.Prelude.Utils where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Maybe (mapMaybe)
import Vehicle.Data.NormalisedExpr
import Vehicle.Prelude
import Vehicle.Syntax.AST

--------------------------------------------------------------------------------
-- HasType

class HasType expr typ | expr -> typ where
  typeOf :: expr -> typ

instance HasType (Binder var builtin) (Type var builtin) where
  typeOf = binderValue

instance HasType (VBinder builtin) (VType builtin) where
  typeOf = binderValue

instance HasType (GenericDecl expr) expr where
  typeOf = \case
    DefAbstract _ _ _ t -> t
    DefFunction _ _ _ t _ -> t

--------------------------------------------------------------------------------
-- Utility functions

isTypeUniverse :: Expr var builtin -> Bool
isTypeUniverse TypeUniverse {} = True
isTypeUniverse _ = False

--------------------------------------------------------------------------------
-- Enumeration functions

freeNamesIn :: Expr var builtin -> [Identifier]
freeNamesIn = \case
  FreeVar _ ident -> [ident]
  BoundVar {} -> []
  Universe {} -> []
  Hole {} -> []
  Meta {} -> []
  Builtin {} -> []
  App _ fun args -> freeNamesIn fun <> concatMap (freeNamesIn . argExpr) args
  Pi _ binder result -> freeNamesIn (typeOf binder) <> freeNamesIn result
  Let _ bound binder body -> freeNamesIn bound <> freeNamesIn (typeOf binder) <> freeNamesIn body
  Lam _ binder body -> freeNamesIn (typeOf binder) <> freeNamesIn body

--------------------------------------------------------------------------------
-- Destruction functions

toHead :: Expr var builtin -> (Expr var builtin, [Arg var builtin])
toHead (App _ fun args) = (fun, NonEmpty.toList args)
toHead e = (e, [])

exprHead :: Expr var builtin -> Expr var builtin
exprHead = fst . toHead

onlyExplicit :: NonEmpty (GenericArg expr) -> [expr]
onlyExplicit args = argExpr <$> filter isExplicit (NonEmpty.toList args)

--------------------------------------------------------------------------------
-- Views

getMetaID :: Expr var builtin -> Maybe MetaID
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

getImplicitArg :: Arg var builtin -> Maybe (Expr var builtin)
getImplicitArg arg
  | isImplicit arg = Just (argExpr arg)
  | otherwise = Nothing

getRelevantArg :: GenericArg expr -> Maybe expr
getRelevantArg arg
  | isRelevant arg = Just (argExpr arg)
  | otherwise = Nothing

filterOutNonExplicitArgs :: NonEmpty (Arg var builtin) -> [Expr var builtin]
filterOutNonExplicitArgs args = mapMaybe getExplicitArg (NonEmpty.toList args)
