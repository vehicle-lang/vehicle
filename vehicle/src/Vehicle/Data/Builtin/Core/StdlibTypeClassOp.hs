-- | The type-class operations defined in the stdlib, which certain overloaded
-- surface-syntax operators are desugared into rather than into builtins
-- (e.g. `x > y` is desugared into `gtTC x y`, where `gtTC` is an operation of
-- the `HasComparison` type class). These are the stdlib counterparts of the
-- builtin `TypeClassOp`s.
--
-- This module is the single source of truth for the names of those operations.
-- It is used both when desugaring the surface-syntax operators, and when
-- resugaring the type-class operations back into them for display to the user.
module Vehicle.Data.Builtin.Core.StdlibTypeClassOp
  ( StdlibTypeClassOp (..),
    stdlibTypeClassOps,
    stdlibTypeClassOpName,
    lookupStdlibTypeClassOp,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Vehicle.Data.Builtin.Core.BasicOperations (ComparisonOp (..), Quantifier (..), comparisonOps)

data StdlibTypeClassOp
  = QuantifierTC Quantifier
  | AddTC
  | SubTC
  | MulTC
  | DivTC
  | ComparisonTC ComparisonOp
  deriving stock (Eq, Show)

-- | All the stdlib type-class operations.
stdlibTypeClassOps :: [StdlibTypeClassOp]
stdlibTypeClassOps =
  fmap QuantifierTC [Forall, Exists]
    <> [AddTC, SubTC, MulTC, DivTC]
    <> fmap ComparisonTC comparisonOps

-- | The name of the type-class operation in the stdlib.
stdlibTypeClassOpName :: StdlibTypeClassOp -> Text
stdlibTypeClassOpName = \case
  QuantifierTC Forall -> "forallTC"
  QuantifierTC Exists -> "existsTC"
  AddTC -> "addTC"
  SubTC -> "subTC"
  MulTC -> "mulTC"
  DivTC -> "divTC"
  ComparisonTC Le -> "leTC"
  ComparisonTC Lt -> "ltTC"
  ComparisonTC Ge -> "geTC"
  ComparisonTC Gt -> "gtTC"
  ComparisonTC Eq -> "eqTC"
  ComparisonTC Ne -> "neTC"

-- | Finds the stdlib type-class operation with the provided name, if any.
lookupStdlibTypeClassOp :: Text -> Maybe StdlibTypeClassOp
lookupStdlibTypeClassOp name = Map.lookup name opsByName

opsByName :: Map Text StdlibTypeClassOp
opsByName =
  Map.fromList [(stdlibTypeClassOpName m, m) | m <- stdlibTypeClassOps]
