module SemanticTokens.Common
  ( expectTokens,
    tokenAt,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Bits (testBit)
import Language.LSP.Protocol.Capabilities (fullLatestClientCaps)
import Language.LSP.Protocol.Types
  ( LanguageKind (LanguageKind_Custom),
    SemanticTokenAbsolute (..),
    SemanticTokenModifiers,
    SemanticTokenRelative (..),
    SemanticTokenTypes (..),
    SemanticTokens (..),
    SemanticTokensLegend (..),
    UInt,
    absolutizeTokens,
    defaultSemanticTokensLegend,
    fromOpenEnumBaseType,
    pattern InL,
  )
import Language.LSP.Test (getSemanticTokens, openDoc, runSession)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty.HUnit (assertFailure, (@?=))

expectTokens :: String -> [SemanticTokenAbsolute] -> IO ()
expectTokens specification expectedTokens =
  withSystemTempDirectory "vehicle-lsp-test" $ \root -> do
    writeFile (root </> "test.vcl") specification
    runSession "vehicle lsp" fullLatestClientCaps root $
      openDoc "test.vcl" (LanguageKind_Custom "vehicle") >>= \doc -> do
        result <- getSemanticTokens doc
        case result of
          InL semanticTokens ->
            case decodeSemanticTokens semanticTokens of
              Left err -> liftIO $ assertFailure err
              Right tokens ->
                liftIO $
                  tokens @?= expectedTokens
          _ -> liftIO $ assertFailure "semantic tokens request returned no tokens"

tokenAt :: UInt -> UInt -> UInt -> SemanticTokenTypes -> SemanticTokenAbsolute
tokenAt line startColumn endColumn tokenType =
  SemanticTokenAbsolute
    { _line = line - 1,
      _startChar = startColumn - 1,
      _length = endColumn - startColumn + 1,
      _tokenType = tokenType,
      _tokenModifiers = []
    }

decodeSemanticTokens :: SemanticTokens -> Either String [SemanticTokenAbsolute]
decodeSemanticTokens (SemanticTokens _ data_) = do
  relative <- decodeRelativeTokens data_
  pure $ absolutizeTokens relative
  where
    SemanticTokensLegend tokenTypesText tokenModifiersText = defaultSemanticTokensLegend
    tokenTypes = fmap fromOpenEnumBaseType tokenTypesText
    tokenModifiers = fmap fromOpenEnumBaseType tokenModifiersText

    decodeRelativeTokens :: [UInt] -> Either String [SemanticTokenRelative]
    decodeRelativeTokens [] = Right []
    decodeRelativeTokens (deltaLine : deltaStartChar : length' : tokenTypeIndex : tokenMods : rest) = do
      tokenType <- indexInto "token type index" tokenTypes tokenTypeIndex
      let modifiers = decodeModifiers tokenMods
      remaining <- decodeRelativeTokens rest
      pure $ SemanticTokenRelative deltaLine deltaStartChar length' tokenType modifiers : remaining
    decodeRelativeTokens _ =
      Left "invalid semantic token payload: expected a multiple of 5 integers"

    decodeModifiers :: UInt -> [SemanticTokenModifiers]
    decodeModifiers bits =
      [ modifier
        | (index, modifier) <- zip [0 ..] tokenModifiers,
          testBit (fromIntegral bits :: Int) index
      ]

indexInto :: String -> [a] -> UInt -> Either String a
indexInto label values index =
  case drop (fromIntegral index) values of
    value : _ -> Right value
    [] -> Left $ "invalid semantic token payload: unknown " <> label <> " " <> show index
