{-# LANGUAGE TemplateHaskell #-}

module Vehicle.Libraries.StandardLibrary
  ( standardLibrary,
    standardLibraryName,
    standardLibraryContent,
    standardLibraryBuiltinModulePath,
    standardLibraryDefinitionsModulePath,
    standardLibrarySTLModulePath,
    standardLibraryInstanceOps,
    isBuiltinModule,
    standardLibIdent,
    stlLibIdent,
    validNetworkTypeIdent,
    validNetworkIOTypeIdent,
    validDynamicsTypeIdent,
    hasQuantifierIdent,
    hasAddIdent,
    hasSubIdent,
    hasMulIdent,
    hasDivIdent,
    hasComparisonIdent,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Map (fromList, keys)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text.Encoding (decodeUtf8)
import Vehicle.Backend.Prelude
import Vehicle.Libraries
import Vehicle.Libraries.Core (LibraryContent)
import Vehicle.Prelude

standardLibIdent :: Name -> Identifier
standardLibIdent = Identifier standardLibraryDefinitionsModulePath

standardLibraryDefinitionsModulePath :: ModulePath
standardLibraryDefinitionsModulePath = ModulePath ["Definitions"]

-- | The optional STL extension module (not auto-imported).
standardLibrarySTLModulePath :: ModulePath
standardLibrarySTLModulePath = ModulePath ["STL"]

stlLibIdent :: Name -> Identifier
stlLibIdent = Identifier standardLibrarySTLModulePath

standardLibraryInstanceOps :: Set Identifier
standardLibraryInstanceOps =
  Set.fromList
    [ standardLibIdent "addTC",
      standardLibIdent "subTC",
      standardLibIdent "mulTC",
      standardLibIdent "divTC",
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

validNetworkTypeIdent :: Identifier
validNetworkTypeIdent = standardLibIdent "HasValidNetworkType"

validNetworkIOTypeIdent :: Identifier
validNetworkIOTypeIdent = standardLibIdent "HasValidNetworkIOType"

validDynamicsTypeIdent :: Identifier
validDynamicsTypeIdent = standardLibIdent "HasValidDynamicsType"

hasQuantifierIdent :: Identifier
hasQuantifierIdent = standardLibIdent "HasQuantifier"

hasAddIdent :: Identifier
hasAddIdent = standardLibIdent "HasAdd"

hasSubIdent :: Identifier
hasSubIdent = standardLibIdent "HasSub"

hasMulIdent :: Identifier
hasMulIdent = standardLibIdent "HasMul"

hasDivIdent :: Identifier
hasDivIdent = standardLibIdent "HasDiv"

hasComparisonIdent :: Identifier
hasComparisonIdent = standardLibIdent "HasComparison"

isBuiltinModule :: ModulePath -> Bool
isBuiltinModule = \case
  ModulePath ("Builtins" : _) -> True
  _ -> False

standardLibraryName :: LibraryName
standardLibraryName = "std"

standardLibraryContent :: LibraryContent
standardLibraryContent =
  fromList $
    fmap
      (second decodeUtf8)
      [ (standardLibraryDefinitionsModulePath, $(makeRelativeToProject "lib/Definitions.vcl" >>= embedFile)),
        (standardLibrarySTLModulePath, $(makeRelativeToProject "lib/STL.vcl" >>= embedFile))
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
