module Vehicle.LSP.SemanticTokens.Common
  ( IntermediateSemTok,
    LspErrorMessage,
    TokenClass,
    mkTokPure,
  )
where

import Data.Text (Text)
import Language.LSP.Protocol.Types as Lsp
  ( SemanticTokenAbsolute (..),
    SemanticTokenModifiers,
    SemanticTokenTypes (..),
  )
import Vehicle.Prelude
  ( Position (..),
    Provenance (Provenance),
    Range (..),
  )

-- Inherited from `Language.LSP.Protocol.Types.SemanticTokens.makeSemanticTokens`,
-- type-aliased for readability.
type LspErrorMessage = Text

type IntermediateSemTok = Maybe Lsp.SemanticTokenAbsolute

type TokenClass = (Lsp.SemanticTokenTypes, [Lsp.SemanticTokenModifiers])

mkTokPure :: FilePath -> Provenance -> TokenClass -> IntermediateSemTok
mkTokPure filePath (Provenance (Range (Position sl sc) (Position _el ec)) provFilePath) (tType, modifiers)
  | filePath == provFilePath && sl > 0 && sc > 0 =
      Just $
        -- LSP expects 0-indexed lines and columns but we use 1-indexed, so subtract 1 from 'sl' and 'sc'
        -- This also means that sl <= 0 || sc <= 0 indicates an invalid location.
        Lsp.SemanticTokenAbsolute
          { _line = fromIntegral sl - 1,
            _startChar = fromIntegral sc - 1,
            _length = fromIntegral (ec - sc),
            _tokenType = tType,
            _tokenModifiers = modifiers
          }
  | otherwise = Nothing
