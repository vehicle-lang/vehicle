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
  { listEntities :: ListableEntities,
    specification :: FilePath,
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

printResources ::
  (MonadIO m, MonadCompile m, PrintableBuiltin builtin) =>
  Prog builtin ->
  ListableEntities ->
  Bool ->
  m ()
printResources (Main decls) listEntities outputAsJSON = do
  let filterFn = case listEntities of
        ExternalResources -> isAbstractDecl
        Properties -> isPropertyDecl
  let filteredDecls = filter filterFn decls
  let listDecls =
        fmap
          ( \decl ->
              convertDeclToListEntity
                decl
                (if listEntities == ExternalResources then pretty (abstractSortOf decl) else "@property")
          )
          filteredDecls
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
  pretty listEntity =
    pretty (entitySort listEntity)
      <+> pretty (entityName listEntity)
      <+> pretty (entityType listEntity)

convertDeclToListEntity :: (PrintableBuiltin builtin) => Decl builtin -> Doc a -> ListEntity
convertDeclToListEntity decl entitySort =
  ListEntity
    { entitySort = pack $ show entitySort,
      entityName = identifierName $ identifierOf decl,
      entityType = pack $ show $ prettyFriendlyEmptyCtx (typeOf decl)
    }
