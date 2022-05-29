module Vehicle.Compile.Type.Auxiliary
  ( InsertAuxiliaryAnnotations
  , insertHolesForAuxiliaryAnnotations
  , RemoveAuxiliaryArguments
  , removeAuxiliaryArguments
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty

import Vehicle.Compile.Prelude

-- | Inserts holes for all the non-user facing auxilliary annotations, e.g.
-- polarity annotations on the `Bool` type and linearity annotations on the
-- `Rat` type.
insertHolesForAuxiliaryAnnotations :: InsertAuxiliaryAnnotations a => a -> a
insertHolesForAuxiliaryAnnotations = insert

-- | Removes all the non-user facing auxilliary arguments, i.e. arguments of
-- type Polarity/Linearity as well as type-level binders for auxiliary types.
removeAuxiliaryArguments :: RemoveAuxiliaryArguments a => a -> a
removeAuxiliaryArguments = remove

-------------------------------------------------------------------------------
-- Inserting polarity and linearity annotations

class InsertAuxiliaryAnnotations a where
  insert :: a -> a

instance InsertAuxiliaryAnnotations UncheckedProg where
  insert (Main ds) = Main (insert <$> ds)

instance InsertAuxiliaryAnnotations UncheckedDecl where
  insert = \case
    DefResource ann resourceType ident t ->
      DefResource ann resourceType ident (insert t)

    DefFunction ann usage ident t e ->
      DefFunction ann usage ident (insert t) (insert e)

instance InsertAuxiliaryAnnotations UncheckedExpr where
  insert expr = case expr of
    -- Insert a suitable hole for types which have annotations
    Builtin ann Bool -> App ann expr [ImplicitArg ann (mkHole ann "boolAnn")]

    -- Needed to ensure idempotence of type-checking
    App _ (Builtin _ Bool) _ -> expr

    Ann  ann e t               -> Ann ann (insert e) (insert t)
    App  ann fun args          -> App ann (insert fun) (insert <$> args)
    Pi   ann binder res        -> Pi  ann (insert binder) (insert res)
    Let  ann bound binder body -> Let ann (insert bound) (insert binder) (insert body)
    Lam  ann binder body       -> Lam ann (insert binder) (insert body)
    LSeq ann tc xs             -> LSeq ann (insert tc) (insert <$> xs)
    PrimDict ann t             -> PrimDict ann (insert t)

    Type{}    -> expr
    Var{}     -> expr
    Hole{}    -> expr
    Meta{}    -> expr
    Literal{} -> expr
    Builtin{} -> expr

instance InsertAuxiliaryAnnotations UncheckedArg where
  insert = mapArgExpr insert

instance InsertAuxiliaryAnnotations UncheckedBinder where
  insert = mapBinderType insert

-------------------------------------------------------------------------------
-- Remove polarity and linearity annotations

class RemoveAuxiliaryArguments a where
  remove :: a -> a

instance RemoveAuxiliaryArguments UncheckedProg where
  remove (Main ds) = Main (remove <$> ds)

instance RemoveAuxiliaryArguments UncheckedDecl where
  remove = \case
    DefResource ann resourceType ident t ->
      DefResource ann resourceType ident (remove t)

    DefFunction ann usage ident t e ->
      DefFunction ann usage ident (remove t) (remove e)

instance RemoveAuxiliaryArguments UncheckedExpr where
  remove expr = case expr of
    App ann fun args ->
      normAppList ann (remove fun) (remove <$> removePolarityArgs args)

    Ann  ann e t               -> Ann ann (remove e) (remove t)
    Pi   ann binder res        -> Pi  ann (remove binder) (remove res)
    Let  ann bound binder body -> Let ann (remove bound) (remove binder) (remove body)
    Lam  ann binder body       -> Lam ann (remove binder) (remove body)
    LSeq ann tc xs             -> LSeq ann (remove tc) (remove <$> xs)
    PrimDict ann t             -> PrimDict ann (remove t)

    Type{}    -> expr
    Var{}     -> expr
    Hole{}    -> expr
    Meta{}    -> expr
    Literal{} -> expr
    Builtin{} -> expr

instance RemoveAuxiliaryArguments UncheckedArg where
  remove = mapArgExpr remove

instance RemoveAuxiliaryArguments UncheckedBinder where
  remove = mapBinderType remove

removePolarityArgs :: NonEmpty CheckedArg -> [CheckedArg]
removePolarityArgs = NonEmpty.filter (not . isPolarityArg)
  where
  isPolarityArg :: CheckedArg -> Bool
  isPolarityArg arg = case argExpr arg of
    Builtin _ Polarity{} -> True
    _                    -> False