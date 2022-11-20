module Vehicle.Backend.Prelude where

import Data.Text.IO qualified as TIO
import Data.Version (Version, makeVersion)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Vehicle.Prelude
import Vehicle.Verify.Core

import Paths_vehicle qualified as VehiclePath


data Backend
  = ITP ITP
  | VerifierBackend VerifierIdentifier
  | LossFunction DifferentiableLogic
  | TypeCheck
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- different available  differentiable logics (types of translation from the constraint 
-- to loss function) are as listed below:

data DifferentiableLogic
  = DL2
  | Godel
  | Lukasiewicz
  | Product
  | Yager
  deriving (Eq, Show, Read, Bounded, Enum)

data ITP
  = Agda
  deriving (Eq, Show, Read, Bounded, Enum)

pattern AgdaBackend :: Backend
pattern AgdaBackend = ITP Agda

pattern MarabouBackend :: Backend
pattern MarabouBackend = VerifierBackend Marabou

pattern LossFunctionDL2 :: Backend
pattern LossFunctionDL2 = LossFunction DL2

pattern LossFunctionGodel :: Backend
pattern LossFunctionGodel = LossFunction Godel

pattern LossFunctionLukasiewicz :: Backend
pattern LossFunctionLukasiewicz = LossFunction Lukasiewicz

pattern LossFunctionProduct :: Backend
pattern LossFunctionProduct = LossFunction Product

pattern LossFunctionYager :: Backend
pattern LossFunctionYager = LossFunction Yager

instance Pretty Backend where
  pretty = \case
    ITP x             -> pretty $ show x
    VerifierBackend x -> pretty $ show x
    LossFunction _      -> "LossFunction"
    TypeCheck         -> "TypeCheck"

instance Read Backend where
  readsPrec _d x = case x of
    "Marabou"                       -> [(MarabouBackend, [])]
    "LossFunction"                  -> [(LossFunctionDL2, [])] -- |this is a default loss translation
    "LossFunction-DL2"              -> [(LossFunctionDL2, [])]
    "LossFunction-Godel"            -> [(LossFunctionGodel, [])]
    "LossFunction-Lukasiewicz"      -> [(LossFunctionLukasiewicz, [])]
    "LossFunction-Product"          -> [(LossFunctionProduct, [])]
    "LossFunction-Yager"            -> [(LossFunctionYager, [])]
    "Agda"                          -> [(AgdaBackend, [])]
    "TypeCheck"                     -> [(TypeCheck, [])]
    _                               -> []

commentTokenOf :: Backend -> Maybe (Doc a)
commentTokenOf = \case
  VerifierBackend Marabou -> Nothing
  ITP Agda                -> Just "--"
  LossFunction{}           -> Nothing
  TypeCheck               -> Nothing

versionOf :: Backend -> Maybe Version
versionOf target = case target of
  VerifierBackend Marabou -> Nothing
  ITP Agda                -> Just $ makeVersion [2,6,2]
  LossFunction{}            -> Nothing
  TypeCheck               -> Nothing

extensionOf :: Backend -> String
extensionOf = \case
  VerifierBackend Marabou -> "-marabou"
  ITP Agda                -> ".agda"
  LossFunction{}            -> ".json"
  TypeCheck               -> ""

-- |Generate the file header given the token used to start comments in the
-- target language
prependfileHeader :: Doc a -> Backend -> Doc a
prependfileHeader doc target = case commentTokenOf target of
  Nothing           -> doc
  Just commentToken -> vsep (map (commentToken <+>)
    [ "WARNING: This file was generated automatically by Vehicle"
    , "and should not be modified manually!"
    , "Metadata"
    , " -" <+> pretty target <> " version:" <+> targetVersion
    , " - AISEC version:" <+> pretty VehiclePath.version
    , " - Time generated: ???"
    ]) <> line <> line <> doc
  where targetVersion = maybe "N/A" pretty (versionOf target)

writeResultToFile :: Backend -> Maybe FilePath -> Doc a -> IO ()
writeResultToFile target filepath doc = do
  let text = layoutAsText $ prependfileHeader doc target
  case filepath of
    Nothing             -> TIO.putStrLn text
    Just outputFilePath -> do
      createDirectoryIfMissing True (takeDirectory outputFilePath)
      TIO.writeFile outputFilePath text
