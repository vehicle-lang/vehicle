module Vehicle.List where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (ToJSON (..))
import Data.Aeson.Encode.Pretty (encodePretty')
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Text (Text, pack)
import GHC.Generics
import Vehicle.Backend.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Prelude.Logging.Instance
import Vehicle.TypeCheck (TypeCheckOptions (..), runCompileMonad, typeCheckUserProg)

data ListOptions = ListOptions
  { specification :: FilePath,
    listEntities :: ListableEntities,
    outputAsJSON :: Bool
  }
  deriving (Eq, Show)

list :: (MonadStdIO IO) => LoggingSettings -> ListOptions -> IO ()
list loggingSettings ListOptions {..} = runCompileMonad loggingSettings $ do
  -- always typecheck first
  (imports, typedProg) <-
    typeCheckUserProg $
      TypeCheckOptions
        { specification = specification,
          typingSystem = Standard
        }
  let mergedProg = mergeImports imports typedProg
  printResources mergedProg listEntities outputAsJSON

printResources :: (MonadIO m, MonadCompile m, PrintableBuiltin builtin) => Prog builtin -> ListableEntities -> Bool -> m ()
printResources (Main decls) listEntities outputAsJSON = do
  let filteredDecls =
        if listEntities == ExternalResources
          then filter isAbstractDecl decls
          else filter isPropertyDecl decls
  let listDecls = fmap (\decl -> convertDeclToListEntity decl (if listEntities == ExternalResources then pack $ show $ pretty (abstractSortOf decl) else "@property")) filteredDecls
  let outputDocs =
        if outputAsJSON
          then pretty $ unpack $ encodePretty' prettyJSONConfig $ toJSON listDecls
          else pretty listDecls
  programOutput outputDocs

-- | Data Structure for listable entities
data ListEntity = ListEntity
  { entitySort :: Text,
    entityName :: Text,
    entityType :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON ListEntity

instance Pretty ListEntity where
  pretty listEntity = pretty (entitySort listEntity) <+> pretty (entityName listEntity) <+> pretty (entityType listEntity)

convertDeclToListEntity :: (PrintableBuiltin builtin) => Decl builtin -> Name -> ListEntity
convertDeclToListEntity decl entitySort =
  ListEntity
    { entitySort = entitySort,
      entityName = identifierName $ identifierOf decl,
      entityType = pack $ show $ prettyFriendlyEmptyCtx (typeOf decl)
    }
