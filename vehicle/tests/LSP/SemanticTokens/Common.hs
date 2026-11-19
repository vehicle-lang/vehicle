{-# LANGUAGE OverloadedStrings #-}

module SemanticTokens.Common
  ( ExpectedTokens,
    expectTokens,
    as,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Bits (testBit)
import Data.String (IsString (..))
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

data ExpectedToken
  = SomeToken String SemanticTokenTypes [SemanticTokenModifiers]
  | NoToken String
  deriving (Eq)

valueOf :: ExpectedToken -> String
valueOf = \case
  SomeToken v _ _ -> v
  NoToken v -> v

instance IsString ExpectedToken where
  fromString = NoToken

as :: String -> (SemanticTokenTypes, [SemanticTokenModifiers]) -> ExpectedToken
as v (t, m) = SomeToken v t m

type ExpectedTokens = [ExpectedToken]

toTokens :: ExpectedTokens -> [SemanticTokenAbsolute]
toTokens ts = absolutizeTokens $ go 0 0 ts
  where
    go :: UInt -> UInt -> ExpectedTokens -> [SemanticTokenRelative]
    go _ _ [] = []
    go dl dc (NoToken value : rest) = do
      let dl' = dl + getDeltaLine value
      let dc' = (if dl' > 0 then 0 else dc) + getDeltaStartChar value
      go dl' dc' rest
    go dl dc (SomeToken value typ mods : rest) =
      SemanticTokenRelative
        { _deltaLine = dl,
          _deltaStartChar = dc,
          _length = fromIntegral (length value),
          _tokenType = typ,
          _tokenModifiers = mods
        }
        : go (getDeltaLine value) (getDeltaStartChar value) rest

    getDeltaLine = fromIntegral . length . filter (== '\n')
    getDeltaStartChar = fromIntegral . length . fst . break (== '\n') . reverse -- TODO: incorrect

toString :: ExpectedTokens -> String
toString [] = ""
toString (t : ts) = valueOf t ++ toString ts

expectTokens :: ExpectedTokens -> IO ()
expectTokens expectedTokens =
  withSystemTempDirectory "vehicle-lsp-test" $ \root -> do
    writeFile (root </> "test.vcl") (toString expectedTokens)
    runSession "vehicle lsp" fullLatestClientCaps root $
      openDoc "test.vcl" (LanguageKind_Custom "vehicle") >>= \doc -> do
        result <- getSemanticTokens doc
        case result of
          InL semanticTokens ->
            case decodeSemanticTokens semanticTokens of
              Left err -> liftIO $ assertFailure err
              Right tokens ->
                liftIO $
                  tokens @?= toTokens expectedTokens
          _ -> liftIO $ assertFailure "semantic tokens request returned no tokens"

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
