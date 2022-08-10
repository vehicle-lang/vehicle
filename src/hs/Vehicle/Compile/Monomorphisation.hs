

module Vehicle.Compile.Monomorphisation where

import Control.Monad.Reader (MonadReader (local), asks, ReaderT (runReaderT))
import Control.Monad.State (MonadState(..), modify, StateT (runStateT))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (splitAt, length)
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import Data.Map qualified as Map (insert, lookup, insertWith, toList)
import Data.Set qualified as Set ()
import Data.Text qualified as Text

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Language.Print (prettyFriendly)

--------------------------------------------------------------------------------
-- Public interface

-- | Tries to monomorphise any polymorphic functions by creating a copy per
-- concrete type each function is used with.
-- Not very sophisticated at the moment, if this needs to be improved perhaps
-- http://mrg.doc.ic.ac.uk/publications/featherweight-go/main.pdf
-- by Wen et al is a good starting point.
monomorphise :: MonadCompile m
             => CheckedProg
             -> m CheckedProg
monomorphise prog = logCompilerPass MinDetail "monomorphisation" $ do
  (prog2, monomorphisations) <- runStateT (runReaderT (collect prog) mempty) mempty
  return $ cleanUp monomorphisations prog2

--------------------------------------------------------------------------------
-- Initial pass

type Monomorphisations = Map Identifier [[CheckedArg]]

type MonadMono m =
  ( MonadCompile m
  , MonadReader (Map Identifier Int) m
  , MonadState Monomorphisations m
  )

class Monomorphise a where
  collect :: MonadMono m => a -> m a

instance Monomorphise CheckedProg where
  collect (Main ds) = Main <$> collect ds

instance Monomorphise [CheckedDecl] where
  collect = \case
    []     -> return []
    d : ds -> do
      (d', alt) <- monoDecl d
      ds' <- local alt (collect ds)
      return $ d' : ds'

monoDecl :: MonadMono m => CheckedDecl -> m (CheckedDecl, Map Identifier Int -> Map Identifier Int)
monoDecl decl = do
  result <- traverseDeclExprs collect decl
  let alteration = case result of
        DefResource{}  -> id
        DefPostulate{} -> id
        DefFunction _ ident t _ -> do
          case isMonomorphisationCandidate t of
            Nothing -> id
            Just d  -> Map.insert ident d
  return (result, alteration)

instance Monomorphise CheckedExpr where
  collect expr = case expr of
    App p fun args -> do
      fun'  <- collect fun
      args' <- traverse collect args
      collectFun p fun' args'

    Var{}         -> return expr
    Universe{}    -> return expr
    Meta{}        -> return expr
    Hole{}        -> return expr
    Builtin{}     -> return expr
    Literal{}     -> return expr

    LVec p es                -> LVec p <$> traverse collect es
    Ann  p e t               -> Ann  p <$> collect e <*> collect t
    Pi   p binder res        -> Pi   p <$> collect binder <*> collect res
    Lam  p binder body       -> Lam  p <$> collect binder <*> collect body
    Let  p bound binder body -> Let  p <$> collect bound  <*> collect binder <*> collect body

collectFun :: MonadMono m => Provenance -> CheckedExpr -> NonEmpty CheckedArg -> m CheckedExpr
collectFun p fun args =
  case getFreeVar fun of
    Nothing    -> return $ App p fun args
    Just ident -> do
      result <- asks (Map.lookup ident)
      case result of
        Nothing -> return $ App p fun args
        Just d  -> if d > NonEmpty.length args
          then compilerDeveloperError "Monomorphisation does not yet support partially applied polymorphic functions"
          else do
            let (argsToMono, remainingArgs) = NonEmpty.splitAt d args
            let suffix   = getMonomorphisedSuffix argsToMono
            let newIdent = Identifier $ nameOf ident <> suffix
            modify (Map.insertWith (<>) ident [argsToMono])
            return $ normAppList p (FreeVar p newIdent) remainingArgs

instance Monomorphise CheckedBinder where
  collect = traverseBinderType collect

instance Monomorphise CheckedArg where
  collect = traverseArgExpr collect

isMonomorphisationCandidate :: CheckedType -> Maybe Int
isMonomorphisationCandidate = \case
  Pi _ binder result -> case (visibilityOf binder, typeOf binder) of
    (Implicit, TypeUniverse _ 0) -> maybe (Just 1) (\v -> Just (v + 1)) (isMonomorphisationCandidate result)
    (Instance, _)                -> maybe (Just 1) (\v -> Just (v + 1)) (isMonomorphisationCandidate result)
    _                            -> Nothing

  _ -> Nothing

getMonomorphisedSuffix :: [CheckedArg] -> Symbol
getMonomorphisedSuffix args = do
  let implicits = mapMaybe getImplicitArg args
  let typesText = fmap (layoutAsText . prettyFriendly) implicits
  let typeNames = fmap (\v -> "[" <> Text.replace " " "-" v <> "]") typesText
  Text.intercalate "-" typeNames

--------------------------------------------------------------------------------
-- Insertion pass

cleanUp :: Monomorphisations -> CheckedProg -> CheckedProg
cleanUp monos (Main ds) = Main $ mconcat (fmap (cleanUpDecl monos) ds)

cleanUpDecl :: Monomorphisations -> CheckedDecl -> [CheckedDecl]
cleanUpDecl monos decl = case decl of
  DefPostulate{} -> [decl]
  DefResource{}  -> [decl]

  DefFunction p ident t e -> do
    case Map.lookup ident monos of
      Nothing -> [decl]
      Just xs -> do
        let namesAndArgs = foldr (\args -> Map.insert (getMonomorphisedSuffix args) args) mempty xs

        flip fmap (Map.toList namesAndArgs) $ \(suffix, args) -> do
          let newIdent = Identifier $ nameOf ident <> suffix
          let (t', e') = substituteArgsThrough (t, e, args)
          DefFunction p newIdent t' e'

substituteArgsThrough :: (CheckedType, CheckedExpr, [CheckedArg]) -> (CheckedType, CheckedExpr)
substituteArgsThrough = \case
  (t,        e,         [])         -> (t, e)
  (Pi _ _ t, Lam _ _ e, arg : args) -> do
    let expr = argExpr arg
    substituteArgsThrough (expr `substInto` t, expr `substInto` e, args)
  _ -> developerError "Unexpected type/body of function undergoing monomorphisation"