module Vehicle.Data.RelevantExpr where

import GHC.Generics (Generic)
import Vehicle.Compile.Arity
import Vehicle.Syntax.AST (HasName (..), Name, Provenance)

--------------------------------------------------------------------------------
-- Relevant expressions

-- This file contains an AST that ideally only contains computationally relevant
-- information suitable for exporting to the Python interpreter. This ambition
-- is not yet quite achieved, as it still contains universes and full pi-types,
-- but ideally this should be fixed once we get irrelevance up and running.

newtype RelProg var builtin
  = Main [RelDecl var builtin]
  deriving (Generic)

data RelDecl var builtin
  = DefPostulate Provenance Name (RelExpr var builtin)
  | DefFunction Provenance Name (RelExpr var builtin) (RelExpr var builtin)
  deriving (Generic)

data RelExpr var builtin
  = Universe Provenance Int
  | App Provenance (RelExpr var builtin) [RelExpr var builtin]
  | -- | Because we're probably not going to a functional language we
    -- need to mark partial applications as such to avoid excessive currying.
    PartialApp Provenance Arity (RelExpr var builtin) [RelExpr var builtin]
  | Pi Provenance (RelBinder var builtin) (RelExpr var builtin)
  | Builtin Provenance builtin
  | BoundVar Provenance var
  | FreeVar Provenance Name
  | Let Provenance (RelExpr var builtin) (RelBinder var builtin) (RelExpr var builtin)
  | Lam Provenance [RelBinder var builtin] (RelExpr var builtin)
  deriving (Generic)

data RelBinder var builtin = Binder Provenance (Maybe Name) (RelExpr var builtin)
  deriving (Generic)

instance HasName (RelBinder var builtin) (Maybe Name) where
  nameOf (Binder _ name _) = name

--------------------------------------------------------------------------------
-- Utils

fmapRelDecl ::
  (RelExpr var1 builtin1 -> RelExpr var2 builtin2) ->
  RelDecl var1 builtin1 ->
  RelDecl var2 builtin2
fmapRelDecl f = \case
  DefPostulate p n t -> DefPostulate p n (f t)
  DefFunction p n t e -> DefFunction p n (f t) (f e)

fmapRelProg ::
  (RelExpr var1 builtin1 -> RelExpr var2 builtin2) ->
  RelProg var1 builtin1 ->
  RelProg var2 builtin2
fmapRelProg f (Main ds) = Main (fmap (fmapRelDecl f) ds)
