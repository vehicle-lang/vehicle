{-# LANGUAGE TemplateHaskell #-}

module Vehicle.Libraries.Stdlib
  ( getStdlib,
    stdlibName,
    stdlibContent,
    stdlibBuiltinModulePath,
    stdlibInstanceOps,
    isBuiltinModule,
    validNetworkTypeIdent,
    validNetworkIOTypeIdent,
    validNetworkFieldTypeIdent,
    hasQuantifierIdent,
    hasAddIdent,
    hasSubIdent,
    hasMulIdent,
    hasDivIdent,
    addTCProj,
    subTCProj,
    mulTCProj,
    divTCProj,
    hasComparisonIdent,
    stdlibCompareRatTensorReduced,
    validDatasetTypeIdent,
    validDatasetListElementTypeIdent,
    differentiableTensorLogicIdent,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor (Bifunctor (..))
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Map (fromList, keys)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text.Encoding (decodeUtf8)
import Vehicle.Backend.Prelude
import Vehicle.Data.Builtin.Core (ComparisonOp)
import Vehicle.Data.Builtin.Core.BasicOperations (ComparisonOp (..))
import Vehicle.Data.Builtin.Core.StdlibTypeClassOp
import Vehicle.Libraries
import Vehicle.Libraries.Core (LibraryContent)
import Vehicle.Prelude

stdlibInstanceOps :: Set Identifier
stdlibInstanceOps =
  Set.fromList $ fmap (stdlibIdentifier . stdlibTypeClassOpName) stdlibTypeClassOps

stdlibBuiltinModulePath :: Maybe SecondaryTypeSystem -> ModulePath
stdlibBuiltinModulePath typeSystem = ModulePath $ case typeSystem of
  Nothing -> ["Builtins", "Standard"]
  Just PolarityTypes -> ["Builtins", "Polarity"]
  Just LinearityTypes -> ["Builtins", "Linearity"]
  Just DecidabilityTypes -> ["Builtins", "Decidability"]
  Just GradientCarryingTypes -> ["Builtins", "GradientCarrying"]

validNetworkTypeIdent :: Identifier
validNetworkTypeIdent = stdlibIdentifier "HasValidNetworkType"

validNetworkIOTypeIdent :: Identifier
validNetworkIOTypeIdent = stdlibIdentifier "HasValidNetworkIOType"

validNetworkFieldTypeIdent :: Identifier
validNetworkFieldTypeIdent = stdlibIdentifier "HasValidNetworkFieldType"

validDatasetTypeIdent :: Identifier
validDatasetTypeIdent = stdlibIdentifier "HasValidDatasetType"

validDatasetListElementTypeIdent :: Identifier
validDatasetListElementTypeIdent = stdlibIdentifier "HasValidDatasetListElementType"

hasQuantifierIdent :: Identifier
hasQuantifierIdent = stdlibIdentifier "HasQuantifier"

hasAddIdent :: Identifier
hasAddIdent = stdlibIdentifier "HasAdd"

addTCProj :: Identifier
addTCProj = stdlibIdentifier (stdlibTypeClassOpName AddTC)

hasSubIdent :: Identifier
hasSubIdent = stdlibIdentifier "HasSub"

subTCProj :: Identifier
subTCProj = stdlibIdentifier (stdlibTypeClassOpName SubTC)

hasMulIdent :: Identifier
hasMulIdent = stdlibIdentifier "HasMul"

mulTCProj :: Identifier
mulTCProj = stdlibIdentifier (stdlibTypeClassOpName MulTC)

hasDivIdent :: Identifier
hasDivIdent = stdlibIdentifier "HasDiv"

divTCProj :: Identifier
divTCProj = stdlibIdentifier (stdlibTypeClassOpName DivTC)

differentiableTensorLogicIdent :: Identifier
differentiableTensorLogicIdent = stdlibIdentifier "DifferentiableTensorLogic"

hasComparisonIdent :: Identifier
hasComparisonIdent = stdlibIdentifier "HasComparison"

isBuiltinModule :: ModulePath -> Bool
isBuiltinModule = \case
  ModulePath ("Builtins" : _) -> True
  _ -> False

stdlibName :: LibraryName
stdlibName = "std"

stdlibCompareRatTensorReduced :: ComparisonOp -> Identifier
stdlibCompareRatTensorReduced op = do
  let prefix = case op of
        Le -> "le"
        Lt -> "lt"
        Ge -> "ge"
        Gt -> "gt"
        Eq -> "eq"
        Ne -> "ne"

  stdlibIdentifier $ prefix <> "RatTensorReduced"

stdlibContent :: LibraryContent
stdlibContent =
  fromList $
    fmap
      (second decodeUtf8)
      [ (stdlibDefinitionsModulePath, $(makeRelativeToProject "lib/Definitions.vcl" >>= embedFile))
      -- (stdlibBuiltinModulePath Nothing, $(makeRelativeToProject "lib/Builtins/Standard.vcl" >>= embedFile)),
      -- (stdlibBuiltinModulePath $ Just PolarityTypes, $(makeRelativeToProject "lib/Builtins/Polarity.vcl" >>= embedFile)),
      -- (stdlibBuiltinModulePath $ Just LinearityTypes, $(makeRelativeToProject "lib/Builtins/Linearity.vcl" >>= embedFile)),
      -- (stdlibBuiltinModulePath $ Just DecidabilityTypes, $(makeRelativeToProject "lib/Builtins/Decidability.vcl" >>= embedFile))
      ]

getStdlib :: (MonadIO m) => m Library
getStdlib = do
  key <- getPreciseVehicleVersion
  return $
    Library
      { libraryName = stdlibName,
        libraryVersion = key,
        libraryModules = keys stdlibContent
      }
