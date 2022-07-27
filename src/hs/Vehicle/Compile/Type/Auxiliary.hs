module Vehicle.Compile.Type.Auxiliary
  ( InsertAuxiliaryAnnotations
  , insertHolesForAuxiliaryAnnotations
  ) where

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error (compilerDeveloperError)
import Vehicle.Compile.Type.Bidirectional
import Vehicle.Compile.Type.Meta (freshLinearityMeta, freshPolarityMeta, getDeclType)

-- | Inserts holes for all the non-user facing auxilliary annotations, e.g.
-- polarity annotations on the `Bool` type and linearity annotations on the
-- `Rat` type.
insertHolesForAuxiliaryAnnotations :: (TCM m
                                     , InsertAuxiliaryAnnotations a)
                                   => a
                                   -> m a
insertHolesForAuxiliaryAnnotations = insert

-------------------------------------------------------------------------------
-- Inserting polarity and linearity annotations

class InsertAuxiliaryAnnotations a where
  insert :: TCM m => a -> m a

instance InsertAuxiliaryAnnotations UncheckedProg where
  insert (Main ds) = Main <$> traverse insert ds

instance InsertAuxiliaryAnnotations UncheckedDecl where
  insert = traverseDeclExprs insert

instance InsertAuxiliaryAnnotations UncheckedExpr where
  insert expr = case expr of
    -- Insert meta-variables for types which have annotations
    BoolType p -> do
      lin <- freshLinearityMeta p
      pol <- freshPolarityMeta p
      return $ App p expr
        [ IrrelevantImplicitArg p lin
        , IrrelevantImplicitArg p pol
        ]

    RatType p -> do
      lin <- freshLinearityMeta p
      return $ App p expr
        [ IrrelevantImplicitArg p lin
        ]

    Var p (Free ident) -> do
      declType <- getDeclType p ident
      auxArgs <- declAuxArgs p declType
      return $ normAppList p expr auxArgs

    -- Needed to ensure idempotence of type-checking
    App p fun@BoolType{}     args -> App p fun <$> traverse insert args
    App p fun@RatType{}      args -> App p fun <$> traverse insert args
    App p fun@(Var _ Free{}) args -> App p fun <$> traverse insert args

    Ann  p e t               -> Ann p <$> insert e <*> insert t
    App  p fun args          -> App p <$> insert fun <*> traverse insert args
    Pi   p binder res        -> Pi  p <$> insert binder <*> insert res
    Let  p bound binder body -> Let p <$> insert bound <*> insert binder <*> insert body
    Lam  p binder body       -> Lam p <$> insert binder <*> insert body
    LVec p xs                -> LVec p <$> traverse insert xs

    Universe{} -> return expr
    Var{}      -> return expr
    Hole{}     -> return expr
    Meta{}     -> return expr
    Literal{}  -> return expr
    Builtin{}  -> return expr

instance InsertAuxiliaryAnnotations UncheckedArg where
  insert = traverseArgExpr insert

instance InsertAuxiliaryAnnotations UncheckedBinder where
  insert = traverseBinderType insert

declAuxArgs :: TCM m => Provenance -> CheckedExpr -> m [CheckedArg]
declAuxArgs p = \case
  Pi _ binder r
    | visibilityOf binder == Implicit && isAuxiliaryUniverse (typeOf binder) -> do
    xs <- declAuxArgs p r
    meta <- case typeOf binder of
      PolarityUniverse{}  -> freshPolarityMeta p
      LinearityUniverse{} -> freshLinearityMeta p
      _                   -> compilerDeveloperError "Mismatch between cases and 'isAuxiliaryUniverse'"
    return $ IrrelevantImplicitArg p meta : xs
  _ -> return []