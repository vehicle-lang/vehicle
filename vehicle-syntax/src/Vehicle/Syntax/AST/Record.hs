module Vehicle.Syntax.AST.Record where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OMap
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), squotes, (<+>))
import Vehicle.Syntax.AST.Name (HasName (..), Name)
import Vehicle.Syntax.AST.Provenance (HasProvenance (..), Provenance)
import Vehicle.Syntax.Prelude (developerError)

--------------------------------------------------------------------------------
-- Record field names

data FieldName = FieldName Provenance Name
  deriving (Show, Generic)

instance Eq FieldName where
  FieldName _ n1 == FieldName _ n2 = n1 == n2

instance Ord FieldName where
  FieldName _ n1 <= FieldName _ n2 = n1 <= n2

instance NFData FieldName

instance Serialize FieldName

instance Hashable FieldName

instance Pretty FieldName where
  pretty (FieldName _ name) = pretty name

instance HasProvenance FieldName where
  provenanceOf (FieldName p _) = p

instance HasName FieldName Name where
  nameOf (FieldName _ name) = name

--------------------------------------------------------------------------------
-- Record fields

type RecordField expr = (FieldName, expr)

mapRecordField ::
  (expr1 -> expr2) ->
  RecordField expr1 ->
  RecordField expr2
mapRecordField f (name, typ) = (name, f typ)

traverseRecordField ::
  (Monad m) =>
  (expr1 -> m expr2) ->
  RecordField expr1 ->
  m (RecordField expr2)
traverseRecordField f (name, typ) = (name,) <$> f typ

type RecordFields expr = [RecordField expr]

mapRecordFields ::
  (expr1 -> expr2) ->
  RecordFields expr1 ->
  RecordFields expr2
mapRecordFields f = fmap (mapRecordField f)

traverseRecordFields ::
  (Monad m) =>
  (expr1 -> m expr2) ->
  RecordFields expr1 ->
  m (RecordFields expr2)
traverseRecordFields f = traverse (traverseRecordField f)

lookupRecordField ::
  RecordFields expr ->
  FieldName ->
  expr
lookupRecordField fields field = case lookup field fields of
  Just value -> value
  Nothing -> developerError $ "Ill-scoped record, could not find field" <+> squotes (pretty field)

type SearchableRecordFields expr = OMap FieldName expr

lookupRecordFieldS ::
  SearchableRecordFields expr ->
  FieldName ->
  expr
lookupRecordFieldS fields field = case OMap.lookup field fields of
  Just value -> value
  Nothing -> developerError $ "Ill-scoped record, could not find field" <+> squotes (pretty field)
