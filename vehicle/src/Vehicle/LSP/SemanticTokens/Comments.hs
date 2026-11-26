module Vehicle.LSP.SemanticTokens.Comments
  ( collectCommentsTokens,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Types as Lsp
  ( SemanticTokenAbsolute (..),
    SemanticTokenTypes (..),
  )
import Vehicle.LSP.SemanticTokens.Common (IntermediateSemTok)
import Vehicle.Prelude
  ( Position (..),
    Provenance (..),
    Range (..),
  )
import Vehicle.Verify.Specification.IO.Read (MonadReadSpecification, readSpecification)

lineCommentStart :: Text
lineCommentStart = "--"

blockCommentStart :: Text
blockCommentStart = "{-"

blockCommentEnd :: Text
blockCommentEnd = "-}"

--------------------------------------------------------------------------------
-- API

-- | Collect tokens from comments using a separate pass over the file. This is necessary because
-- comments are removed during lexing and are not given to the parser. Note that this also means
-- the comment tokens (`--`, `{-`, `-}`) are hardcoded in this file, since they are not accessible
-- from the generated parser code.
collectCommentsTokens :: (MonadReadSpecification m) => FilePath -> m [IntermediateSemTok]
collectCommentsTokens filePath = do
  moduleText <- readSpecification filePath
  return $ collectCommentsTokensFromModuleText moduleText

--------------------------------------------------------------------------------
-- Token creation

collectCommentsTokensFromModuleText :: Text -> [IntermediateSemTok]
collectCommentsTokensFromModuleText moduleText = map makeToken commentProvs
  where
    moduleLines = T.lines moduleText
    commentProvs = findCommentProvs moduleLines 0 NotInComment

-- | Note: The `Provenance` used here uses 0-based lines and columns, different from the 1-based
-- `Provenance` values used elsewhere. This is mainly done for ease of implementation, and because
-- LSP uses 0-based lines and columns too.
type CommentProvenance = FilePath -> Provenance

-- | See `CommentProvenance` -- we don't use Vehicle.LSP.SemanticTokens.Internal.mkTok
-- because it assumes 1-based lines and columns.
makeToken :: CommentProvenance -> IntermediateSemTok
makeToken commentProv =
  Just
    Lsp.SemanticTokenAbsolute
      { _line = fromIntegral sl,
        _startChar = fromIntegral sc,
        _length = fromIntegral (ec - sc),
        _tokenType = tType,
        _tokenModifiers = modifiers
      }
  where
    (Provenance (Range (Position sl sc) (Position _el ec)) _) = commentProv mempty
    tType = SemanticTokenTypes_Comment
    modifiers = []

--------------------------------------------------------------------------------
-- Parser state machine

data ParserState
  = NotInComment
  | InBlockComment
      Position -- start position

data NotInCommentLineParseResult
  = NoCommentStart
  | LineComment
      Int -- start column
  | BlockCommentStart
      Int -- start column
      Text -- input line with the processed text blanked out

data InBlockCommentLineParseResult
  = NoCommentEnd
  | BlockCommentEnd
      Int -- end column
      Text -- input line with the processed text blanked out

findCommentProvs :: [Text] -> Int -> ParserState -> [CommentProvenance]
findCommentProvs [] _ = const []
findCommentProvs (lineContent : ls) thisLine = \case
  NotInComment ->
    case parseLineNotInComment lineContent of
      NoCommentStart ->
        findCommentProvs ls nextLine NotInComment
      LineComment startCol ->
        Provenance
          ( Range
              (Position thisLine startCol)
              endOfThisLine
          )
          : findCommentProvs ls nextLine NotInComment
      BlockCommentStart startCol newLineContent ->
        findCommentProvs (newLineContent : ls) thisLine (InBlockComment (Position thisLine startCol))
  InBlockComment startPos ->
    case parseLineInBlockComment lineContent of
      NoCommentEnd ->
        -- Split multi-line block comments into one token per line
        Provenance
          ( Range
              startPos
              endOfThisLine
          )
          : findCommentProvs ls nextLine (InBlockComment (Position nextLine 0))
      BlockCommentEnd endCol newLineContent ->
        Provenance
          ( Range
              startPos
              (Position thisLine endCol)
          )
          : findCommentProvs (newLineContent : ls) thisLine NotInComment
  where
    nextLine = thisLine + 1
    endOfThisLine = Position thisLine (T.length lineContent)

parseLineNotInComment :: Text -> NotInCommentLineParseResult
parseLineNotInComment line = do
  let (linePrefix, lineSuffix) = T.breakOn lineCommentStart line
  let (blockPrefix, blockSuffix) = T.breakOn blockCommentStart linePrefix
  -- The line is now split like:
  --
  --  | blockPrefix | blockSuffix | lineSuffix |
  --  | ******* linePrefix ****** |            |
  --
  -- If either suffix has > 0 length, it will contain their comment token.
  --
  -- We want to prioritise the earlier start token (i.e. `-- {-` does not open a block comment,
  -- but `{- --` does). Due to the above splitting layout, the block comment start token will
  -- always be earlier if it is present.
  if T.length blockSuffix > 0
    then do
      let startCommentCol = T.length blockPrefix
      let startRestCol = startCommentCol + T.length blockCommentStart
      BlockCommentStart startCommentCol (hideFirstNChars startRestCol line)
    else
      if T.length lineSuffix > 0
        then LineComment (T.length linePrefix)
        else NoCommentStart

parseLineInBlockComment :: Text -> InBlockCommentLineParseResult
parseLineInBlockComment line = do
  -- If we're in a block comment, we don't care if there happens to be a line comment token.
  -- For example, `-- -}` will still close the block comment.
  let (pre, suf) = T.breakOn blockCommentEnd line
  if T.length suf > 0
    then do
      let endCol = T.length pre + T.length blockCommentEnd
      BlockCommentEnd endCol (hideFirstNChars endCol line)
    else NoCommentEnd

-- | If we find a start/end block comment marker, we still need to process the rest of the
-- line. We want to preserve the column indices during this process, so rather than just dropping
-- the first already-processed parts of the line, we "hide" them by overwriting them with spaces.
-- This is quite hacky, but means we don't have to maintain a "how many columns have we already
-- dropped" integer in the parser state.
hideFirstNChars :: Int -> Text -> Text
hideFirstNChars n s = T.replicate n blank <> T.drop n s
  where
    blank = " "
