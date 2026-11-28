module Vehicle.LSP.SemanticTokens.Syntax
  ( collectSyntaxTokens,
  )
where

import Control.Lens (uncons, unsnoc, (^.))
import Control.Monad.Writer
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Lens qualified as Lsp
import Language.LSP.Protocol.Types
  ( SemanticTokenAbsolute,
    SemanticTokenTypes (..),
  )
import Vehicle.Data.AST.Provenance
  ( Position (..),
    Provenance (..),
    Range (..),
    tkProvenance,
  )
import Vehicle.LSP.SemanticTokens.Common (IntermediateSemTok, LspErrorMessage, TokenClass, mkTokPure)
import Vehicle.Syntax.External.Abs
import Vehicle.Syntax.Parse (parseExternalModule)
import Vehicle.Syntax.Token
import Vehicle.Verify.Specification.IO.Read (MonadReadSpecification, readSpecification)

--------------------------------------------------------------------------------
-- API

-- | Collect tokens from static syntax constructs (i.e. those that don't require semantic info to identify).
collectSyntaxTokens :: (MonadReadSpecification m) => FilePath -> m (Either LspErrorMessage [SemanticTokenAbsolute])
collectSyntaxTokens filePath = do
  moduleText <- readSpecification filePath
  case parseExternalModule moduleText of
    Left err -> return $ Left (T.pack err)
    Right modul -> do
      (maybeToks, searchTokenRanges) <- runWriterT $ collect modul
      let searchToks = map (findSearchToken moduleText) searchTokenRanges
      return $ Right $ removeFakeToks $ catMaybes $ (maybeToks ++ searchToks)

--------------------------------------------------------------------------------
-- Token creation

type TokenProvenance = FilePath -> Provenance

mkTok :: TokenProvenance -> TokenClass -> IntermediateSemTok
mkTok p = mkTokPure mempty (p mempty)

mkTokFrom :: (IsToken a) => a -> TokenClass -> IntermediateSemTok
mkTokFrom tk = mkTok (tkProvenance tk)

mkElemOfTok :: TokElemOf -> IntermediateSemTok
mkElemOfTok = flip mkTokFrom (SemanticTokenTypes_Operator, [])

mkKeywordTok :: (IsToken a) => a -> IntermediateSemTok
mkKeywordTok = flip mkTokFrom (SemanticTokenTypes_Keyword, [])

mkOperatorTok :: (IsToken a) => a -> IntermediateSemTok
mkOperatorTok = flip mkTokFrom (SemanticTokenTypes_Operator, [])

mkTypeTok :: (IsToken a) => a -> IntermediateSemTok
mkTypeTok = flip mkTokFrom (SemanticTokenTypes_Type, [])

mkClassTok :: (IsToken a) => a -> IntermediateSemTok
mkClassTok = flip mkTokFrom (SemanticTokenTypes_Class, [])

mkNumberTok :: (IsToken a) => a -> IntermediateSemTok
mkNumberTok = flip mkTokFrom (SemanticTokenTypes_Number, [])

--------------------------------------------------------------------------------
-- Search tokens and fake tokens
--
-- Certain tokens -- in particular, the "in" keyword in "let .. in", "forall .. in",
-- and "exists .. in" -- cannot be position tokens due to an upstream limitation
-- in BNFC (see https://github.com/BNFC/bnfc/issues/372), and therefore we cannot
-- get their provenance directly.
--
-- To work around this, we introduce a "search token": a range in the document
-- where we expect to find this elusive token, based on the surrounding tokens
-- whose provenance we can get directly. Then, at the end, we search for all such
-- tokens by substring match in their respective ranges.
--
-- We can be confident that this will work as long as there is only one search
-- token between otherwise-adjacent position tokens. It falls apart if, between
-- the search token and its nearest position tokens, there is a non-position
-- token which contains the search token as a substring.
--
-- To facilitate this, we also use "fake tokens": tokens which will not be
-- emitted to the LSP client, but which are created so that their provenances
-- are recorded. For example:
--
--   let width = thin in
--
-- Here, `thin` is a Var, which does not emit a token on the syntax pass.
-- However, if we discard its token, our search range will be too wide; when
-- we search for "in", the first instance will be the latter half of "thin".
-- So we keep around "fake tokens" which are discarded at the end.

data SearchTokenRange
  = SearchToken
      Text -- the token text to search for
      TokenClass -- class of the token, once it's found
      Range -- where in the document to search for the token
      SearchDirection -- use the first or last instance in the range?
  deriving stock (Show)

data SearchDirection
  = Forwards
  | Backwards
  deriving stock (Show)

type MonadTokenCollection m = MonadWriter [SearchTokenRange] m

fakeTokenIdentifier :: Text
fakeTokenIdentifier = "__FAKE_TOK__"

mkFakeTok :: (IsToken a) => a -> IntermediateSemTok
mkFakeTok = flip mkTokFrom (SemanticTokenTypes_Custom fakeTokenIdentifier, [])

removeFakeToks :: [SemanticTokenAbsolute] -> [SemanticTokenAbsolute]
removeFakeToks = filter $ \tok ->
  case tok ^. Lsp.tokenType of
    SemanticTokenTypes_Custom ident -> ident /= fakeTokenIdentifier
    _ -> True

--------------------------------------------------------------------------------
-- Token collection

class TokenCollectable a where
  collect :: (MonadTokenCollection m) => a -> m [IntermediateSemTok]

  collectMany :: (MonadTokenCollection m) => [a] -> m [IntermediateSemTok]
  collectMany tks = concat <$> traverse collect tks

instance TokenCollectable Module where
  collect = collectSyntaxTokensFromModule

instance TokenCollectable ImportStatement where
  collect = collectSyntaxTokensFromImport

instance TokenCollectable Decl where
  collect = collectSyntaxTokensFromDecl

instance TokenCollectable Expr where
  collect = collectSyntaxTokensFromExpr

instance TokenCollectable BasicBinder where
  collect = collectSyntaxTokensFromBasicBinder

instance TokenCollectable NameBinder where
  collect = collectSyntaxTokensFromNameBinder

instance TokenCollectable TypeBinder where
  collect = collectSyntaxTokensFromTypeBinder

instance TokenCollectable LetDecl where
  collect = collectSyntaxTokensFromLetDecl

instance TokenCollectable Arg where
  collect = collectSyntaxTokensFromArg

instance TokenCollectable Lit where
  collect = collectSyntaxTokensFromLiteral

instance TokenCollectable RecordFieldAssign where
  collect = collectSyntaxTokensFromRecordFieldAssign

instance TokenCollectable RecordFieldDef where
  collect = collectSyntaxTokensFromRecordFieldDef

instance TokenCollectable DeclAnnOption where
  collect = collectSyntaxTokensFromDeclAnnOpt

(<:>) :: (Monad m) => a -> m [a] -> m [a]
x <:> mxs = do
  xs <- mxs
  return $ x : xs

infixr 5 <:>

(<++>) :: (Monad m) => m [a] -> m [a] -> m [a]
(<++>) = liftA2 (<>)

infixr 5 <++>

collectSyntaxTokensFromModule :: (MonadTokenCollection m) => Module -> m [IntermediateSemTok]
collectSyntaxTokensFromModule (DefModule imports decls) =
  collectMany imports
    <++> collectMany decls

-- TODO: Process (resolved) import statements in semantic pass
collectSyntaxTokensFromImport :: (MonadTokenCollection m) => ImportStatement -> m [IntermediateSemTok]
collectSyntaxTokensFromImport (Import tk _) = return [mkKeywordTok tk]

collectSyntaxTokensFromDecl :: (MonadTokenCollection m) => Decl -> m [IntermediateSemTok]
collectSyntaxTokensFromDecl = \case
  DefAnn tk declAnnOpts -> do
    let annTok = mkTokFrom tk (SemanticTokenTypes_Decorator, [])
    let optToks = case declAnnOpts of
          DeclAnnWithOpts opts -> collectMany opts
          DeclAnnWithoutOpts -> return []
    annTok <:> optToks
  DefType tk _name binders expr ->
    mkKeywordTok tk
      <:> collectMany binders
      <++> collect expr
  DefFunType _name tkElemOf expr ->
    mkElemOfTok tkElemOf
      <:> collect expr
  DefFunExpr _name binders expr ->
    collectMany binders
      <++> collect expr
  DefRecord tkRecord _name binders tkWhere fields supports -> do
    let supportsToks = return $ case supports of
          NoSupports -> []
          -- TODO: This maybe should be the responsibility of the semantic pass?
          Supports tk s -> mkKeywordTok tk : map (\(DerivedClass t) -> mkTypeTok t) s
    mkKeywordTok tkRecord
      <:> mkKeywordTok tkWhere
      <:> collectMany binders
      <++> collectMany fields
      <++> supportsToks

collectSyntaxTokensFromRecordFieldDef :: (MonadTokenCollection m) => RecordFieldDef -> m [IntermediateSemTok]
collectSyntaxTokensFromRecordFieldDef (FieldDef _ tk expr) =
  mkElemOfTok tk
    <:> collect expr

collectSyntaxTokensFromExpr :: (MonadTokenCollection m) => Expr -> m [IntermediateSemTok]
collectSyntaxTokensFromExpr = \case
  Type tk -> return [mkTypeTok tk]
  Ann e1 tk e2 ->
    mkElemOfTok tk
      <:> collect e1
      <++> collect e2
  Let tk letDecls e -> do
    let letTok = mkKeywordTok tk
    letDeclToks <- collectMany letDecls
    exprToks <- collect e
    mkSearchToken "in" (SemanticTokenTypes_Keyword, []) (letTok : letDeclToks) exprToks
    return $ letTok : letDeclToks ++ exprToks
  Lam tkLambda binders tkArrow e ->
    mkKeywordTok tkLambda
      <:> mkOperatorTok tkArrow
      <:> collectMany binders
      <++> collect e
  Fun binder tk e ->
    mkOperatorTok tk
      <:> collect binder
      <++> collect e
  App e arg ->
    collect e
      <++> collect arg
  Record recordFieldAssigns ->
    collectMany recordFieldAssigns
  RecordAcc e _fieldAccess ->
    collect e
  Var name -> return [mkFakeTok name]
  Literal lit -> collect lit
  Hole holeToken -> return [mkTokFrom holeToken (SemanticTokenTypes_Macro, [])]
  Foreach tk binders e ->
    mkKeywordTok tk
      <:> collectMany binders
      <++> collect e
  Forall tk binders e ->
    mkKeywordTok tk
      <:> collectMany binders
      <++> collect e
  Exists tk binders e ->
    mkKeywordTok tk
      <:> collectMany binders
      <++> collect e
  ForallIn tk binder e1 e2 -> do
    binderToks <- collect binder
    e1Toks <- collect e1
    e2Toks <- collect e2
    mkSearchToken "in" (SemanticTokenTypes_Keyword, []) binderToks e1Toks
    return $
      mkKeywordTok tk
        : binderToks
        ++ e1Toks
        ++ e2Toks
  ExistsIn tk binder e1 e2 -> do
    binderToks <- collect binder
    e1Toks <- collect e1
    e2Toks <- collect e2
    mkSearchToken "in" (SemanticTokenTypes_Keyword, []) binderToks e1Toks
    return $
      mkKeywordTok tk
        : binderToks
        ++ e1Toks
        ++ e2Toks
  Add e1 tk e2 ->
    mkOperatorTok tk
      <:> collectMany [e1, e2]
  Sub e1 tk e2 ->
    mkOperatorTok tk
      <:> collectMany [e1, e2]
  Mul e1 tk e2 ->
    mkOperatorTok tk
      <:> collectMany [e1, e2]
  Div e1 tk e2 ->
    mkOperatorTok tk
      <:> collectMany [e1, e2]
  Pow e1 tk e2 ->
    mkOperatorTok tk
      <:> collectMany [e1, e2]
  Neg tk e ->
    mkOperatorTok tk
      <:> collect e
  If tkIf e1 tkThen e2 tkElse e3 ->
    mkKeywordTok tkIf
      <:> mkKeywordTok tkThen
      <:> mkKeywordTok tkElse
      <:> collectMany [e1, e2, e3]
  Impl e1 tk e2 ->
    mkOperatorTok tk
      <:> collectMany [e1, e2]
  And e1 tk e2 ->
    mkKeywordTok tk
      <:> collectMany [e1, e2]
  Or e1 tk e2 ->
    mkKeywordTok tk
      <:> collectMany [e1, e2]
  Not tk e ->
    mkKeywordTok tk
      <:> collect e
  Eq e1 tk e2 ->
    mkOperatorTok tk
      <:> collectMany [e1, e2]
  Ne e1 tk e2 ->
    mkOperatorTok tk
      <:> collectMany [e1, e2]
  Le e1 tk e2 ->
    mkOperatorTok tk
      <:> collectMany [e1, e2]
  Lt e1 tk e2 ->
    mkOperatorTok tk
      <:> collectMany [e1, e2]
  Ge e1 tk e2 ->
    mkOperatorTok tk
      <:> collectMany [e1, e2]
  Gt e1 tk e2 ->
    mkOperatorTok tk
      <:> collectMany [e1, e2]
  EqPoint e1 tk e2 ->
    mkOperatorTok tk
      <:> collectMany [e1, e2]
  NePoint e1 tk e2 ->
    mkOperatorTok tk
      <:> collectMany [e1, e2]
  LePoint e1 tk e2 ->
    mkOperatorTok tk
      <:> collectMany [e1, e2]
  LtPoint e1 tk e2 ->
    mkOperatorTok tk
      <:> collectMany [e1, e2]
  GePoint e1 tk e2 ->
    mkOperatorTok tk
      <:> collectMany [e1, e2]
  GtPoint e1 tk e2 ->
    mkOperatorTok tk
      <:> collectMany [e1, e2]
  VecLiteral tkOpen exprs tkClose ->
    mkOperatorTok tkOpen
      <:> mkOperatorTok tkClose
      <:> collectMany exprs
  Cons e1 tk e2 ->
    mkOperatorTok tk
      <:> collectMany [e1, e2]
  At e1 tk e2 ->
    mkOperatorTok tk
      <:> collectMany [e1, e2]
  Min tk -> return [mkKeywordTok tk]
  Max tk -> return [mkKeywordTok tk]
  Log tk -> return [mkKeywordTok tk]
  Exp tk -> return [mkKeywordTok tk]
  AddNat tk -> return [mkKeywordTok tk]
  MulNat tk -> return [mkKeywordTok tk]
  AddRealTensor tk -> return [mkKeywordTok tk]
  SubRealTensor tk -> return [mkKeywordTok tk]
  MulRealTensor tk -> return [mkKeywordTok tk]
  DivRealTensor tk -> return [mkKeywordTok tk]
  NegRealTensor tk -> return [mkKeywordTok tk]
  AtVector tk -> return [mkKeywordTok tk]
  AtTensor tk -> return [mkKeywordTok tk]
  ForeachVector tk -> return [mkKeywordTok tk]
  ForeachTensor tk -> return [mkKeywordTok tk]
  MapList tk -> return [mkKeywordTok tk]
  FoldList tk -> return [mkKeywordTok tk]
  QuantifyForAllIndex tk -> return [mkKeywordTok tk]
  QuantifyExistsIndex tk -> return [mkKeywordTok tk]
  QuantifyForallRealTensor tk -> return [mkKeywordTok tk]
  QuantifyExistsRealTensor tk -> return [mkKeywordTok tk]
  QuantifyForallTensorLike tk -> return [mkKeywordTok tk]
  QuantifyExistsTensorLike tk -> return [mkKeywordTok tk]
  CompareIndexEq tk -> return [mkKeywordTok tk]
  CompareIndexNe tk -> return [mkKeywordTok tk]
  CompareIndexLe tk -> return [mkKeywordTok tk]
  CompareIndexLt tk -> return [mkKeywordTok tk]
  CompareIndexGe tk -> return [mkKeywordTok tk]
  CompareIndexGt tk -> return [mkKeywordTok tk]
  CompareNatEq tk -> return [mkKeywordTok tk]
  CompareNatNe tk -> return [mkKeywordTok tk]
  CompareNatLe tk -> return [mkKeywordTok tk]
  CompareNatLt tk -> return [mkKeywordTok tk]
  CompareNatGe tk -> return [mkKeywordTok tk]
  CompareNatGt tk -> return [mkKeywordTok tk]
  CompareRatTensorPointwiseEq tk -> return [mkKeywordTok tk]
  CompareRatTensorPointwiseNe tk -> return [mkKeywordTok tk]
  CompareRatTensorPointwiseLe tk -> return [mkKeywordTok tk]
  CompareRatTensorPointwiseLt tk -> return [mkKeywordTok tk]
  CompareRatTensorPointwiseGe tk -> return [mkKeywordTok tk]
  CompareRatTensorPointwiseGt tk -> return [mkKeywordTok tk]
  CompareRatTensorReducedEq tk -> return [mkKeywordTok tk]
  CompareRatTensorReducedNe tk -> return [mkKeywordTok tk]
  CompareRatTensorReducedLe tk -> return [mkKeywordTok tk]
  CompareRatTensorReducedLt tk -> return [mkKeywordTok tk]
  CompareRatTensorReducedGe tk -> return [mkKeywordTok tk]
  CompareRatTensorReducedGt tk -> return [mkKeywordTok tk]
  Nil tk -> return [mkKeywordTok tk]
  Append tk -> return [mkKeywordTok tk]
  Const tk -> return [mkKeywordTok tk]
  Transpose tk -> return [mkKeywordTok tk]
  Map tk -> return [mkKeywordTok tk]
  Fold tk -> return [mkKeywordTok tk]
  ReduceOr tk -> return [mkKeywordTok tk]
  ReduceAnd tk -> return [mkKeywordTok tk]
  ReduceAdd tk -> return [mkKeywordTok tk]
  ReduceMul tk -> return [mkKeywordTok tk]
  ReduceMin tk -> return [mkKeywordTok tk]
  ReduceMax tk -> return [mkKeywordTok tk]
  Unit tk -> return [mkTypeTok tk]
  Bool tk -> return [mkTypeTok tk]
  Index tk -> return [mkTypeTok tk]
  Nat tk -> return [mkTypeTok tk]
  Real tk -> return [mkTypeTok tk]
  List tk -> return [mkTypeTok tk]
  Vector tk -> return [mkTypeTok tk]
  Tensor tk -> return [mkTypeTok tk]
  IsTensorType tk -> return [mkClassTok tk]
  NonCastingTensor tk -> return [mkClassTok tk]
  NonCastingReal tk -> return [mkClassTok tk]

collectSyntaxTokensFromBasicBinder :: (MonadTokenCollection m) => BasicBinder -> m [IntermediateSemTok]
collectSyntaxTokensFromBasicBinder binder =
  mkElemOfTok tk
    <:> collect expr
  where
    (tk, expr) = case binder of
      ExplicitBinder _ _ t e -> (t, e)
      ImplicitBinder _ _ t e -> (t, e)
      InstanceBinder _ _ t e -> (t, e)

collectSyntaxTokensFromNameBinder :: (MonadTokenCollection m) => NameBinder -> m [IntermediateSemTok]
collectSyntaxTokensFromNameBinder = \case
  ExplicitNameBinder n -> return [mkFakeTok n]
  ImplicitNameBinder _ n -> return [mkFakeTok n]
  InstanceNameBinder _ n -> return [mkFakeTok n]
  BasicNameBinder b -> collect b

collectSyntaxTokensFromTypeBinder :: (MonadTokenCollection m) => TypeBinder -> m [IntermediateSemTok]
collectSyntaxTokensFromTypeBinder = \case
  ExplicitTypeBinder e -> collect e
  ExplicitTypeBinderMods _ _ e -> collect e
  ImplicitTypeBinder e -> collect e
  ImplicitTypeBinderMods _ _ e -> collect e
  InstanceTypeBinder _ e -> collect e
  BasicTypeBinder b -> collect b

collectSyntaxTokensFromLetDecl :: (MonadTokenCollection m) => LetDecl -> m [IntermediateSemTok]
collectSyntaxTokensFromLetDecl (LDecl binder expr) =
  collect binder
    <++> collect expr

collectSyntaxTokensFromArg :: (MonadTokenCollection m) => Arg -> m [IntermediateSemTok]
collectSyntaxTokensFromArg arg = collect expr
  where
    expr = case arg of
      ExplicitArg e -> e
      ExplicitArgMods _ _ e -> e
      ImplicitArg _ e -> e
      InstanceArg _ e -> e

collectSyntaxTokensFromLiteral :: (MonadTokenCollection m) => Lit -> m [IntermediateSemTok]
collectSyntaxTokensFromLiteral lit = case lit of
  UnitLiteral u -> return [mkKeywordTok u]
  BoolLiteral b -> return [mkKeywordTok b]
  NatLiteral n -> return [mkNumberTok n]
  RatLiteral r -> return [mkNumberTok r]
  InfLiteral inf -> return [mkKeywordTok inf]

collectSyntaxTokensFromRecordFieldAssign :: (MonadTokenCollection m) => RecordFieldAssign -> m [IntermediateSemTok]
collectSyntaxTokensFromRecordFieldAssign (FieldAssign _name expr) = collect expr

collectSyntaxTokensFromDeclAnnOpt :: (MonadTokenCollection m) => DeclAnnOption -> m [IntermediateSemTok]
collectSyntaxTokensFromDeclAnnOpt = \case
  -- TODO: Need to deal with annotations containing names/exprs during semantic pass,
  -- but NameAnnOption and TypeAnnOption currently never make it to the desugared AST.
  -- (See: `Vehicle.Compile.Sugar.Desugar.parseAnnotation`)
  NameAnnOption tk _name ->
    return
      [ mkTokFrom tk (SemanticTokenTypes_Parameter, [])
      ]
  InferAnnOption tk b ->
    return
      [ mkTokFrom tk (SemanticTokenTypes_Parameter, []),
        mkKeywordTok b
      ]
  DefaultAnnOption tk n ->
    return
      [ mkTokFrom tk (SemanticTokenTypes_Parameter, []),
        mkNumberTok n
      ]
  TypeAnnOption name expr ->
    mkTokFrom name (SemanticTokenTypes_Parameter, [])
      <:> collect expr

--------------------------------------------------------------------------------
-- Search tokens

endOfFile :: Position
endOfFile = Position (-1) (-1)

findSearchToken :: Text -> SearchTokenRange -> IntermediateSemTok
findSearchToken moduleText (SearchToken str cls (Range start endOrEof) direction) = do
  let xs = T.lines moduleText
  case xs of
    [] -> Nothing
    moduleLines -> do
      let end =
            if endOrEof == endOfFile
              then Position (length moduleLines) (T.length $ last moduleLines)
              else endOrEof

      let zstart = oneToZero start
      let zend = oneToZero end
      let searchLines = slice moduleLines (posLine zstart) (posLine zend)
      case searchLines of
        [] -> Nothing
        l : ls -> do
          let searchLines' = sliceStartAndEndLines (l :| ls) (posColumn zstart) (posColumn zend)
          let maybeFound = find zstart zend (NonEmpty.toList searchLines')
          maybe Nothing (flip mkTok cls . Provenance . zeroToOneR) maybeFound
  where
    oneToZero (Position l c) = Position (l - 1) (c - 1)
    zeroToOne (Position l c) = Position (l + 1) (c + 1)
    zeroToOneR (Range s e) = Range (zeroToOne s) (zeroToOne e)

    slice xs from to = take (to - from + 1) (drop from xs)

    find startPos endPos = case direction of
      Forwards -> findForwards (posLine startPos)
      Backwards -> findBackwards (posLine endPos)

    findForwards :: Int -> [Text] -> Maybe Range
    findForwards startLine allxs = do
      (x, xs) <- uncons allxs
      let (prefix, suffix) = T.breakOn str x
      if T.length suffix > 0
        then
          Just $
            Range
              (Position startLine (T.length prefix))
              (Position startLine (T.length prefix + T.length str))
        else findForwards (startLine + 1) xs

    findBackwards :: Int -> [Text] -> Maybe Range
    findBackwards endLine allxs = do
      (xs, x) <- unsnoc allxs
      let (prefix, _suffix) = T.breakOnEnd str x
      if T.length prefix > 0
        then
          Just $
            Range
              (Position endLine (T.length prefix - T.length str))
              (Position endLine (T.length prefix))
        else findBackwards (endLine - 1) xs

    sliceStartAndEndLines :: (NonEmpty Text) -> Int -> Int -> (NonEmpty Text)
    sliceStartAndEndLines xs startColumn endColumn = do
      let xs' = editFirst (hideFirstNChars startColumn) xs
      editLast (\x -> hideLastNChars (T.length x - endColumn) x) xs'

    hideFirstNChars n s = T.replicate n " " <> T.drop n s
    hideLastNChars n s = T.dropEnd n s <> T.replicate n " "

    editFirst :: (a -> a) -> (NonEmpty a) -> (NonEmpty a)
    editFirst f (x :| xs) = f x :| xs

    editLast :: (a -> a) -> (NonEmpty a) -> (NonEmpty a)
    editLast f (x :| []) = f x :| []
    editLast f (x :| (y : ys)) = x :| (NonEmpty.toList $ editLast f (y :| ys))

mkSearchToken :: (MonadTokenCollection m) => Text -> TokenClass -> [IntermediateSemTok] -> [IntermediateSemTok] -> m ()
mkSearchToken s cls beforeToks afterToks =
  case (beforePositions, afterPositions) of
    ([], []) -> return ()
    ([], afters) -> go Backwards startOfFile (minimum afters)
    (befores, []) -> go Forwards (maximum befores) endOfFile
    (befores, afters) -> go Forwards (maximum befores) (minimum afters)
  where
    go d b a = tell [SearchToken s cls (Range b a) d]

    beforePositions = map endPosFromSemTok $ catMaybes beforeToks
    afterPositions = map startPosFromSemTok $ catMaybes afterToks

    startOfFile = Position 1 1

    startPosFromSemTok :: SemanticTokenAbsolute -> Position
    startPosFromSemTok semTk =
      Position
        (fromIntegral $ semTk ^. Lsp.line + 1)
        (fromIntegral $ semTk ^. Lsp.startChar + 1)

    endPosFromSemTok :: SemanticTokenAbsolute -> Position
    endPosFromSemTok semTk =
      Position
        (fromIntegral $ semTk ^. Lsp.line + 1)
        (fromIntegral $ semTk ^. Lsp.startChar + semTk ^. Lsp.length + 1)
