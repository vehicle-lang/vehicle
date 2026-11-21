{-# LANGUAGE OverloadedStrings #-}

-- | A declarative syntax for writing semantic highlighting tests, where the expected class of each
-- token is written directly in the test case.
--
-- Usage:
--
-- @
-- testCase "simple example" $
--   expectTokens
--     [ "foo" `as` (SemanticTokenTypes_Function, []),
--       " = ",
--       "42" `as` (SemanticTokenTypes_Number, []),
--       "\n"
--     ]
--     NoOverlaps
-- @
--
-- You may want to specify a token assertion which will `eventually` be true:
--
-- @
-- expectTokens
--   [ "@parameter" `eventually` (SemanticTokenTypes_Decorator, []),
--     "\n",
--     "p" `as` (SemanticTokenTypes_Variable, []),
--     " : ",
--     "Nat" `as` (SemanticTokenTypes_Type, []),
--     "\n"
--   ]
--   NoOverlaps
-- @
--
-- Failing tests are shown line-by-line, with the expected and actual tokens rendered in diff-style
-- colours:
--
-- @
-- bar = True
-- ^^^
-- (SemanticTokenTypes_Variable,[])
--       ^^^^
--       (SemanticTokenTypes_Keyword,[])    (green - an extra token that was not expected)
--       ^^^^
--       (SemanticTokenTypes_Comment,[])    (red - a token that was expected but not found)
-- @
module SemanticTokens.Common
  ( OverlapMode (..),
    ExpectedTokens,
    expectTokens,
    as,
    eventually,
  )
where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Bits (testBit)
import Data.Char (ord)
import Data.List (sortOn)
import Data.String (IsString (..))
import Language.LSP.Protocol.Lens qualified as Lsp
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
import Language.LSP.Test (getSemanticTokens, openDoc)
import Session.Common (runSessionWithDiagnostics)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty.HUnit (assertFailure)

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

-- | Expect a token with a specific type and modifiers.
--
-- The following example will cause the test to fail if the token at `foo`'s position is not a
-- `SemanticTokenTypes_Variable` with zero modifiers:
--
-- > "foo" `as` (SemanticTokenTypes_Variable, [])
as :: String -> (SemanticTokenTypes, [SemanticTokenModifiers]) -> ExpectedToken
as v (t, m) = SomeToken v t m

-- | Expect no token, but document which token we would like to expect in the future. Useful for
-- "expected-failure" situations, where we would eventually like to assert a certain token, but we
-- can't right now.
--
-- The following example will cause the test to fail there is any token at `foo`'s position.
--
-- > "foo" `eventually` (SemanticTokenTypes_Variable, [])
eventually :: String -> (SemanticTokenTypes, [SemanticTokenModifiers]) -> ExpectedToken
eventually v _ = NoToken v

type ExpectedTokens = [ExpectedToken]

toTokens :: ExpectedTokens -> [SemanticTokenAbsolute]
toTokens ts = absolutizeTokens $ go (0, 0) (0, 0) ts
  where
    go :: (UInt, UInt) -> (UInt, UInt) -> ExpectedTokens -> [SemanticTokenRelative]
    go _ _ [] = []
    go previous cursor (NoToken value : rest) =
      go previous (advancePosition cursor value) rest
    go (previousLine, previousChar) cursor@(cursorLine, cursorChar) (SomeToken value typ mods : rest) = do
      let relativeLine = cursorLine - previousLine
      let relativeChar =
            if relativeLine == 0
              then cursorChar - previousChar
              else cursorChar
      let relativeToken =
            SemanticTokenRelative
              { _deltaLine = relativeLine,
                _deltaStartChar = relativeChar,
                _length = fromIntegral (utf16Length value),
                _tokenType = typ,
                _tokenModifiers = mods
              }
      relativeToken : go cursor (advancePosition cursor value) rest

utf16Length :: String -> Int
-- LSP character offsets are based on the negotiated PositionEncodingKind; the
-- mandatory/default encoding is UTF-16, where non-BMP code points take two code
-- units. Source: LSP 3.17 spec ("Position"/"PositionEncodingKind"):
-- https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#position
utf16Length = sum . fmap (\char -> if ord char > 0xFFFF then 2 else 1)

advancePosition :: (UInt, UInt) -> String -> (UInt, UInt)
advancePosition (line, char) value =
  case break (== '\n') value of
    (before, []) -> (line, char + fromIntegral (utf16Length before))
    (_before, _ : after) -> advancePosition (line + 1, 0) after

data TokenDiffStatus
  = TokenMatched
  | TokenAdded
  | TokenRemoved

data TokenDiff
  = TokenDiff
      UInt -- line (0-indexed)
      UInt -- start column (0-indexed)
      UInt -- token length
      SemanticTokenTypes
      [SemanticTokenModifiers]
      TokenDiffStatus

renderSemanticTokenDiff :: String -> [SemanticTokenAbsolute] -> [SemanticTokenAbsolute] -> String
renderSemanticTokenDiff source expected actual =
  unlines $
    concatMap renderLine ([0 .. sourceLineCount - 1] :: [Int])
  where
    diffs = diffTokens expected actual
    sourceLines = lines source
    sourceLineCount = max 1 (length sourceLines)

    renderLine :: Int -> [String]
    renderLine line = do
      let sourceLine = sourceLineAt line sourceLines
      let lineDiffs =
            sortOn
              (\(TokenDiff _ start _ _ _ _) -> start)
              [diff | diff <- diffs, fromIntegral (tokenDiffLine diff) == line]

      map ("\ESC[0m" ++) (sourceLine : concatMap renderDiff lineDiffs)

    renderDiff :: TokenDiff -> [String]
    renderDiff (TokenDiff _ start length' typ mods status) =
      [ indent start ++ colour status (replicate (fromIntegral length') '^'),
        indent start ++ colour status (show (typ, mods))
      ]

    indent :: UInt -> String
    indent = flip replicate ' ' . fromIntegral

    colour :: TokenDiffStatus -> String -> String
    colour TokenMatched text = text
    colour TokenAdded text = "\ESC[32m" ++ text ++ "\ESC[0m"
    colour TokenRemoved text = "\ESC[31m" ++ text ++ "\ESC[0m"

sourceLineAt :: Int -> [String] -> String
sourceLineAt line = \case
  [] -> ""
  (first : rest)
    | line == 0 -> first
    | otherwise -> sourceLineAt (line - 1) rest

tokenDiffLine :: TokenDiff -> UInt
tokenDiffLine (TokenDiff line _ _ _ _ _) = line

diffTokens :: [SemanticTokenAbsolute] -> [SemanticTokenAbsolute] -> [TokenDiff]
diffTokens expected actual =
  snd (foldl matchActual (expected, []) actual) ++ removedDiffs
  where
    matchActual (remaining, diffs) token =
      case findMatching token remaining of
        Just (remaining', matched) ->
          (remaining', diffs ++ [toDiff TokenMatched matched])
        Nothing ->
          (remaining, diffs ++ [toDiff TokenAdded token])

    removedDiffs =
      map (toDiff TokenRemoved) (fst (foldlOnlyRemaining expected actual))

    foldlOnlyRemaining remaining [] = (remaining, ())
    foldlOnlyRemaining remaining (token : rest) =
      case findMatching token remaining of
        Just (remaining', _) -> foldlOnlyRemaining remaining' rest
        Nothing -> foldlOnlyRemaining remaining rest

findMatching ::
  SemanticTokenAbsolute ->
  [SemanticTokenAbsolute] ->
  Maybe ([SemanticTokenAbsolute], SemanticTokenAbsolute)
findMatching _ [] = Nothing
findMatching token (candidate : rest)
  | token == candidate = Just (rest, candidate)
  | otherwise =
      case findMatching token rest of
        Nothing -> Nothing
        Just (remaining, matched) -> Just (candidate : remaining, matched)

toDiff :: TokenDiffStatus -> SemanticTokenAbsolute -> TokenDiff
toDiff status SemanticTokenAbsolute {..} =
  TokenDiff _line _startChar _length _tokenType _tokenModifiers status

toString :: ExpectedTokens -> String
toString [] = ""
toString (t : ts) = valueOf t ++ toString ts

-- | This is a stop-gap to allow us to test semantic tokens despite the fact that they currently
-- generate lots of overlapping tokens, which are hidden by VSCode. Obviously this token-hiding
-- behaviour is client-specific and non-portable, so eventually we will fix it and stop generating
-- overlapping tokens, at which point this can be removed.
data OverlapMode
  = -- | No post-processing is done to remove overlapping tokens.
    NoOverlaps
  | -- | Overlaps whose line, startChar, and length all match are removed.
    AllowStrictOverlaps
  | -- | Overlaps whose line and startChar match are removed, but their lengths
    -- do not need to match. This matches VSCode's observed behaviour.
    AllowLaxOverlaps

removeOverlaps :: OverlapMode -> [SemanticTokenAbsolute] -> [SemanticTokenAbsolute]
removeOverlaps _ [] = []
removeOverlaps _ (t : []) = [t]
removeOverlaps mode (t1 : t2 : ts)
  | overlapping mode = removeOverlaps mode (t1 : ts)
  | otherwise = t1 : removeOverlaps mode (t2 : ts)
  where
    overlapping NoOverlaps = False
    overlapping AllowStrictOverlaps =
      overlapping AllowLaxOverlaps
        && t1 ^. Lsp.length == t2 ^. Lsp.length
    overlapping AllowLaxOverlaps =
      t1 ^. Lsp.line == t2 ^. Lsp.line
        && t1 ^. Lsp.startChar == t2 ^. Lsp.startChar

expectTokens :: ExpectedTokens -> OverlapMode -> IO ()
expectTokens expectedTokens overlapMode =
  withSystemTempDirectory "vehicle-lsp-test" $ \root -> do
    writeFile (root </> "test.vcl") (toString expectedTokens)
    runSessionWithDiagnostics "vehicle lsp" root $
      openDoc "test.vcl" (LanguageKind_Custom "vehicle") >>= \doc -> do
        result <- getSemanticTokens doc
        case result of
          InL semanticTokens ->
            case decodeSemanticTokens semanticTokens of
              Left err -> liftIO $ assertFailure err
              Right rawTokens -> do
                let expected = removeOverlaps overlapMode $ toTokens expectedTokens
                let tokens = removeOverlaps overlapMode rawTokens
                if tokens == expected
                  then pure ()
                  else
                    liftIO $
                      assertFailure $
                        "Semantic tokens differ:\n"
                          <> renderSemanticTokenDiff (toString expectedTokens) expected tokens
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
