module Vehicle.Syntax.AST.Record where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))
import Vehicle.Syntax.AST.Name (Name)
import Vehicle.Syntax.AST.Provenance (HasProvenance (..), Provenance)

--------------------------------------------------------------------------------
-- Record field names

data FieldName = FieldName Provenance Name
  deriving (Eq, Show, Generic)

instance NFData FieldName

instance Serialize FieldName

instance Hashable FieldName

instance Pretty FieldName where
  pretty (FieldName _ name) = pretty name

instance HasProvenance FieldName where
  provenanceOf (FieldName p _) = p

--------------------------------------------------------------------------------

type RecordDefField expr = (FieldName, expr)

type RecordDefFields expr = [RecordDefField expr]

traverseRecordDefField :: (Monad m) => (expr1 -> m expr2) -> RecordDefField expr1 -> m (RecordDefField expr2)
traverseRecordDefField f (name, typ) = (name,) <$> f typ

traverseRecordDefFields :: (Monad m) => (expr1 -> m expr2) -> RecordDefFields expr1 -> m (RecordDefFields expr2)
traverseRecordDefFields f = traverse (traverseRecordDefField f)

pattern SelfRecordRef :: Name
pattern SelfRecordRef = "_self"
