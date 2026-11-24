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
import Vehicle.LSP.SemanticTokens.Internal (IntermediateSemTok)
import Vehicle.Prelude
  ( Position (..),
    Provenance (..),
    Range (..),
  )
import Vehicle.Verify.Specification.IO.Read (MonadReadSpecification, readSpecification)

--------------------------------------------------------------------------------
-- API

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
  | InMultiLineComment
      Position -- start position

data NotInCommentLineParseResult
  = NoCommentStart
  | SingleLineComment
      Int -- start column
      Int -- end column
  | MultiLineCommentStart
      Int -- start column
      Text -- input line with the processed text blanked out

data InMultiLineCommentLineParseResult
  = NoCommentEnd
  | MultiLineCommentEnd
      Int -- end column
      Text -- input line with the processed text blanked out

findCommentProvs :: [Text] -> Int -> ParserState -> [CommentProvenance]
findCommentProvs [] _ = const []
findCommentProvs (lineContent : ls) thisLine = \case
  NotInComment ->
    case parseLineNotInComment lineContent of
      NoCommentStart ->
        findCommentProvs ls nextLine NotInComment
      SingleLineComment startCol endCol ->
        Provenance
          ( Range
              (Position thisLine startCol)
              (Position thisLine endCol)
          )
          : findCommentProvs ls nextLine NotInComment
      MultiLineCommentStart startCol newLineContent ->
        findCommentProvs (newLineContent : ls) thisLine (InMultiLineComment (Position thisLine startCol))
  InMultiLineComment startPos ->
    case parseLineInMultiLineComment lineContent of
      NoCommentEnd ->
        -- Split multi-line comments into one token per line
        Provenance
          ( Range
              startPos
              (Position thisLine (T.length lineContent))
          )
          : findCommentProvs ls nextLine (InMultiLineComment (Position nextLine 0))
      MultiLineCommentEnd endCol newLineContent ->
        Provenance
          ( Range
              startPos
              (Position thisLine endCol)
          )
          : findCommentProvs (newLineContent : ls) thisLine NotInComment
  where
    nextLine = thisLine + 1

parseLineNotInComment :: Text -> NotInCommentLineParseResult
parseLineNotInComment line = do
  let (sPre, sSuf) = T.breakOn "--" line
  let (mPre, mSuf) = T.breakOn "{-" sPre
  if T.length mSuf > 0
    then do
      let startCommentCol = T.length mPre
      let startRestCol = startCommentCol + T.length "{-"
      MultiLineCommentStart startCommentCol (hideFirstNChars startRestCol line)
    else
      if T.length sSuf > 0
        then SingleLineComment (T.length sPre) (T.length line)
        else NoCommentStart

parseLineInMultiLineComment :: Text -> InMultiLineCommentLineParseResult
parseLineInMultiLineComment line = do
  let (pre, suf) = T.breakOn "-}" line
  if T.length suf > 0
    then do
      let endCol = T.length pre + T.length "-}"
      MultiLineCommentEnd endCol (hideFirstNChars endCol line)
    else NoCommentEnd

-- | If we find a start/end multiline comment marker, we still need to process the rest of the
-- line. We want to preserve the column indices during this process, so rather than just dropping
-- the first already-processed parts of the line, we "hide" them by overwriting them with spaces.
-- This is quite hacky, but means we don't have to maintain a "how many columns have we already
-- dropped" integer in the parser state.
hideFirstNChars :: Int -> Text -> Text
hideFirstNChars n s = T.replicate n blank <> T.drop n s
  where
    blank = " "
