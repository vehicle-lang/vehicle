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

collectCommentsTokens :: (MonadReadSpecification m) => FilePath -> m [IntermediateSemTok]
collectCommentsTokens filePath = do
  moduleText <- readSpecification filePath
  return $ collectCommentsTokensFromModuleText moduleText

collectCommentsTokensFromModuleText :: Text -> [IntermediateSemTok]
collectCommentsTokensFromModuleText moduleText = map makeToken commentProvs
  where
    moduleLines = T.lines moduleText
    commentProvs = findCommentProvs moduleLines (Position (-1) (-1)) NotInComment

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

findCommentProvs :: [Text] -> Position -> ParserState -> [CommentProvenance]
findCommentProvs [] endOfLastLine = \case
  NotInComment -> []
  InMultiLineComment startPos -> do
    [Provenance (Range startPos endOfLastLine)]
findCommentProvs (line : ls) endOfLastLine = \case
  NotInComment ->
    case parseLineNotInComment line of
      NoCommentStart -> findCommentProvs ls endOfThisLine NotInComment
      SingleLineComment startCol endCol ->
        Provenance
          ( Range
              (Position thisLine startCol)
              (Position thisLine endCol)
          )
          : findCommentProvs ls endOfThisLine NotInComment
      MultiLineCommentStart startCol newLine ->
        findCommentProvs (newLine : ls) endOfLastLine (InMultiLineComment (Position thisLine startCol))
  InMultiLineComment startPos ->
    case parseLineInMultiLineComment line of
      NoCommentEnd ->
        -- Split multi-line comments into one token per line
        Provenance (Range startPos endOfThisLine)
          : findCommentProvs ls endOfThisLine (InMultiLineComment startOfNextLine)
      MultiLineCommentEnd endCol newLine ->
        Provenance
          ( Range
              startPos
              (Position thisLine endCol)
          )
          : findCommentProvs (newLine : ls) endOfLastLine NotInComment
  where
    thisLine = (posLine endOfLastLine + 1)
    endOfThisLine = Position thisLine (T.length line)
    startOfNextLine = Position (thisLine + 1) 0

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

hideFirstNChars :: Int -> Text -> Text
hideFirstNChars n s = T.replicate n blank <> T.drop n s
  where
    blank = " "
