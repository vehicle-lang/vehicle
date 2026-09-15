{-# LANGUAGE TemplateHaskell #-}

module Vehicle.Libraries.StandardLibrary
  ( standardLibrary,
    standardLibraryName,
    standardLibraryContent,
    standardLibraryBuiltinModulePath,
    standardLibraryDefinitionsModulePath,
    standardLibraryInstanceOps,
    isBuiltinModule,
    standardLibIdent,
    isStandardLibIdent,
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
    standardLibraryCompareRatTensorReduced,
    validDatasetTypeIdent,
    validDatasetListElementTypeIdent,
    differentiableTensorLogicIdent,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Map (fromList, keys)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text.Encoding (decodeUtf8)
import Vehicle.Backend.Prelude
import Vehicle.Data.Builtin.Core (ComparisonOp)
import Vehicle.Data.Builtin.Core.BasicOperations (ComparisonOp (..))
import Vehicle.Libraries
import Vehicle.Libraries.Core (LibraryContent)
import Vehicle.Prelude

standardLibIdent :: Name -> Identifier
standardLibIdent = Identifier standardLibraryDefinitionsModulePath

isStandardLibIdent :: Identifier -> Bool
isStandardLibIdent ident = modulePath ident == standardLibraryDefinitionsModulePath

standardLibraryDefinitionsModulePath :: ModulePath
standardLibraryDefinitionsModulePath = ModulePath ["Definitions"]

standardLibraryInstanceOps :: Set Identifier
standardLibraryInstanceOps =
  Set.fromList
    [ addTCProj,
      subTCProj,
      mulTCProj,
      divTCProj,
      standardLibIdent "forallTC",
      standardLibIdent "existsTC",
      standardLibIdent "leTC",
      standardLibIdent "ltTC",
      standardLibIdent "geTC",
      standardLibIdent "gtTC",
      standardLibIdent "eqTC",
      standardLibIdent "neTC"
    ]

standardLibraryBuiltinModulePath :: Maybe SecondaryTypeSystem -> ModulePath
standardLibraryBuiltinModulePath typeSystem = ModulePath $ case typeSystem of
  Nothing -> ["Builtins", "Standard"]
  Just PolarityTypes -> ["Builtins", "Polarity"]
  Just LinearityTypes -> ["Builtins", "Linearity"]
  Just DecidabilityTypes -> ["Builtins", "Decidability"]
  Just GradientCarryingTypes -> ["Builtins", "GradientCarrying"]

validNetworkTypeIdent :: Identifier
validNetworkTypeIdent = standardLibIdent "HasValidNetworkType"

validNetworkIOTypeIdent :: Identifier
validNetworkIOTypeIdent = standardLibIdent "HasValidNetworkIOType"

validNetworkFieldTypeIdent :: Identifier
validNetworkFieldTypeIdent = standardLibIdent "HasValidNetworkFieldType"

validDatasetTypeIdent :: Identifier
validDatasetTypeIdent = standardLibIdent "HasValidDatasetType"

validDatasetListElementTypeIdent :: Identifier
validDatasetListElementTypeIdent = standardLibIdent "HasValidDatasetListElementType"

hasQuantifierIdent :: Identifier
hasQuantifierIdent = standardLibIdent "HasQuantifier"

hasAddIdent :: Identifier
hasAddIdent = standardLibIdent "HasAdd"

addTCProj :: Identifier
addTCProj = standardLibIdent "addTC"

hasSubIdent :: Identifier
hasSubIdent = standardLibIdent "HasSub"

subTCProj :: Identifier
subTCProj = standardLibIdent "subTC"

hasMulIdent :: Identifier
hasMulIdent = standardLibIdent "HasMul"

mulTCProj :: Identifier
mulTCProj = standardLibIdent "mulTC"

hasDivIdent :: Identifier
hasDivIdent = standardLibIdent "HasDiv"

divTCProj :: Identifier
divTCProj = standardLibIdent "divTC"

differentiableTensorLogicIdent :: Identifier
differentiableTensorLogicIdent = standardLibIdent "DifferentiableTensorLogic"

hasComparisonIdent :: Identifier
hasComparisonIdent = standardLibIdent "HasComparison"

isBuiltinModule :: ModulePath -> Bool
isBuiltinModule = \case
  ModulePath ("Builtins" : _) -> True
  _ -> False

standardLibraryName :: LibraryName
standardLibraryName = "std"

standardLibraryCompareRatTensorReduced :: ComparisonOp -> Identifier
standardLibraryCompareRatTensorReduced op = do
  let prefix = case op of
        Le -> "le"
        Lt -> "lt"
        Ge -> "ge"
        Gt -> "gt"
        Eq -> "eq"
        Ne -> "ne"

  standardLibIdent $ prefix <> "RatTensorReduced"

standardLibraryContent :: LibraryContent
standardLibraryContent =
  fromList $
    fmap
      (second decodeUtf8)
      [ (standardLibraryDefinitionsModulePath, $(makeRelativeToProject "lib/Definitions.vcl" >>= embedFile))
      -- (standardLibraryBuiltinModulePath Nothing, $(makeRelativeToProject "lib/Builtins/Standard.vcl" >>= embedFile)),
      -- (standardLibraryBuiltinModulePath $ Just PolarityTypes, $(makeRelativeToProject "lib/Builtins/Polarity.vcl" >>= embedFile)),
      -- (standardLibraryBuiltinModulePath $ Just LinearityTypes, $(makeRelativeToProject "lib/Builtins/Linearity.vcl" >>= embedFile)),
      -- (standardLibraryBuiltinModulePath $ Just DecidabilityTypes, $(makeRelativeToProject "lib/Builtins/Decidability.vcl" >>= embedFile))
      ]

standardLibrary :: Library
standardLibrary =
  Library
    { libraryName = standardLibraryName,
      libraryVersion = preciseVehicleVersion,
      libraryModules = keys standardLibraryContent
    }
