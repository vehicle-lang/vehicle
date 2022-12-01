{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Syntax.BNFC.Elaborate.External
  ( UnparsedExpr(..)
  , elaborateProg
  , elaborateExpr
  ) where

import Control.Monad.Reader (MonadReader(..), runReaderT)
import Control.Monad.Except (MonadError (..), foldM, throwError)
import Control.Monad.Writer (MonadWriter (..), runWriterT)
import Data.Bitraversable (bitraverse)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set (singleton)
import Data.Text (Text, unpack)
import Text.Read (readMaybe)

import Data.Set (Set)
import Vehicle.Syntax.AST qualified as V
import Vehicle.Syntax.BNFC.Utils (tokArrow, tokDot, tokForallT, tokLambda,
                                  tokType)
import Vehicle.Syntax.External.Abs qualified as B
import Vehicle.Syntax.Parse.Error (ParseError (..))
import Vehicle.Syntax.Parse.Token
import Vehicle.Syntax.Prelude (developerError, readNat, readRat)
import Vehicle.Syntax.Sugar

--------------------------------------------------------------------------------
-- Public interface

newtype UnparsedExpr = UnparsedExpr B.Expr

-- | We elaborate from the simple AST generated automatically by BNFC to our
-- more complicated internal version of the AST.
elaborateProg :: MonadError ParseError m
              => V.Module
              -> B.Prog
              -> m (V.GenericProg UnparsedExpr, Set V.Identifier)
elaborateProg mod prog = runReaderT (runWriterT $ elabProg prog) mod

elaborateExpr :: MonadError ParseError m
              => V.Module
              -> UnparsedExpr
              -> m V.InputExpr
elaborateExpr mod (UnparsedExpr expr) = runReaderT (elabExpr expr) mod

--------------------------------------------------------------------------------
-- Algorithm

type MonadProgElab m =
  ( MonadError ParseError m
  , MonadWriter (Set V.Identifier) m
  , MonadReader V.Module m
  )

elabProg :: MonadProgElab m => B.Prog -> m (V.GenericProg UnparsedExpr)
elabProg (B.Main decls) = V.Main <$> elabDecls decls

elabDecls :: MonadProgElab m => [B.Decl] -> m [V.GenericDecl UnparsedExpr]
elabDecls = \case
  []           -> return []
  decl : decls -> do
    (d', ds) <- elabDeclGroup (decl :| decls)
    ds' <- elabDecls ds
    return $ d' : ds'

elabDeclGroup :: MonadProgElab m => NonEmpty B.Decl -> m (V.GenericDecl UnparsedExpr, [B.Decl])
elabDeclGroup dx = case dx of
  -- Type definition.
  B.DefType n bs t :| ds -> do
    d' <- elabTypeDef n bs t
    return (d', ds)

  -- Function declaration and body.
  B.DefFunType typeName _ t :| B.DefFunExpr exprName bs e : ds -> do
    d' <- elabDefFun typeName exprName t bs e
    return (d', ds)

  -- Function body without a declaration.
  B.DefFunExpr n bs e :| ds -> do
    let unknownType = constructUnknownDefType n bs
    d' <- elabDefFun n n unknownType bs e
    return (d', ds)

  -- ERROR: Function declaration with no body
  B.DefFunType n _tk _t :| _ ->
    throwError $ FunctionNotGivenBody (V.tkProvenance n) (tkSymbol n)

  -- Property declaration.
  B.DefAnn a@B.Property{} opts :| B.DefFunType typeName _tk t : B.DefFunExpr exprName bs e : ds -> do
    checkNoAnnotationOptions a opts
    d' <- elabDefFun typeName exprName t bs e
    tell (Set.singleton (V.identifierOf d'))
    return (d', ds)

  -- ERROR: Property with no body
  B.DefAnn B.Property{} _ :| B.DefFunType n _ _ : _ ->
    throwError $ PropertyNotGivenBody (V.tkProvenance n) (tkSymbol n)

  -- ERROR: Other annotation with body
  B.DefAnn a _ :| B.DefFunType typeName _ _ : B.DefFunExpr bodyName _ _ : _
    | tkSymbol typeName == tkSymbol bodyName ->
      throwError $ ResourceGivenBody (V.tkProvenance typeName) (annotationSymbol a) (tkSymbol typeName)

  --Network declaration.
  B.DefAnn a@B.Network{} opts :| B.DefFunType n _ t : ds -> do
    checkNoAnnotationOptions a opts
    d' <- elabResource n t V.Network
    return (d', ds)

  -- Dataset declaration.
  B.DefAnn a@B.Dataset{} opts :| B.DefFunType n _ t : ds -> do
    checkNoAnnotationOptions a opts
    d' <- elabResource n t V.Dataset
    return (d', ds)

  -- Parameter declaration.
  (B.DefAnn B.Parameter{} opts :| B.DefFunType n _ t : ds) -> do
    r <- elabParameterOptions opts
    d' <- elabResource n t r
    return (d', ds)

  (B.DefAnn a _ :| _) ->
    throwError $ AnnotationWithNoDeclaration (annotationProvenance a) (annotationSymbol a)

annotationSymbol :: B.DeclAnnName -> Text
annotationSymbol = \case
  B.Network   tk -> tkSymbol tk
  B.Dataset   tk -> tkSymbol tk
  B.Parameter tk -> tkSymbol tk
  B.Property  tk -> tkSymbol tk

annotationProvenance :: B.DeclAnnName -> V.Provenance
annotationProvenance = \case
  B.Network   tk -> mkAnn tk
  B.Dataset   tk -> mkAnn tk
  B.Parameter tk -> mkAnn tk
  B.Property  tk -> mkAnn tk

checkNoAnnotationOptions :: MonadProgElab m => B.DeclAnnName -> B.DeclAnnOpts -> m ()
checkNoAnnotationOptions annName = \case
  B.DeclAnnWithoutOpts      -> return ()
  B.DeclAnnWithOpts []      -> return ()
  B.DeclAnnWithOpts (o : _) -> do
    let B.BooleanOption paramName _ = o
    throwError $ InvalidAnnotationOption (V.tkProvenance paramName) (annotationSymbol annName) (tkSymbol paramName) []

elabResource :: MonadElab m => B.Name -> B.Expr -> V.Resource -> m (V.GenericDecl UnparsedExpr)
elabResource n t r = V.DefResource (mkAnn n) r <$> elabName n <*> pure (UnparsedExpr t)

elabTypeDef :: MonadElab m => B.Name -> [B.Binder] -> B.Expr -> m (V.GenericDecl UnparsedExpr)
elabTypeDef n binders e = do
  name <- elabName n
  let typeTyp
        | null binders = tokType 0
        | otherwise    = B.ForallT tokForallT binders tokDot (tokType 0)
  return $ V.DefFunction (mkAnn n) name (UnparsedExpr typeTyp) (UnparsedExpr e)

elabDefFun :: MonadElab m =>  B.Name -> B.Name -> B.Expr -> [B.Binder] -> B.Expr -> m (V.GenericDecl UnparsedExpr)
elabDefFun n1 n2 t binders e
  | tkSymbol n1 /= tkSymbol n2 =
    throwError $ FunctionWithMismatchedNames (mkAnn n1) (tkSymbol n1) (tkSymbol n2)
  | otherwise = do
    name <- elabName n1
    let body
          | null binders   = e
          | otherwise = B.Lam tokLambda binders tokArrow e
    return $ V.DefFunction (mkAnn n1) name (UnparsedExpr t) (UnparsedExpr body)

--------------------------------------------------------------------------------
-- Expr elaboration

type MonadElab m =
  ( MonadError ParseError m
  , MonadReader V.Module m
  )

elabExpr :: MonadElab m => B.Expr -> m V.InputExpr
elabExpr = \case
  B.Type t                  -> return $ V.Universe (mkAnn t) $ V.TypeUniv (parseTypeLevel t)
  B.Var  n                  -> return $ V.Var  (mkAnn n) (tkSymbol n)
  B.Hole n                  -> return $ V.mkHole (V.tkProvenance n) (tkSymbol n)
  B.Literal l               -> elabLiteral l

  B.Ann e tk t              -> op2 V.Ann  tk  (elabExpr e) (elabExpr t)
  B.Fun t1 tk t2            -> op2 V.Pi   tk  (elabFunInputType t1) (elabExpr t2)
  B.VecLiteral tk1 es _tk2  -> elabVecLiteral tk1 es

  B.App e1 e2               -> elabApp e1 e2
  B.Let tk1 ds e            -> unfoldLet (mkAnn tk1) <$> bitraverse (traverse elabLetDecl) elabExpr (ds, e)
  B.ForallT tk1 ns _tk2 t   -> do checkNonEmpty tk1 ns; unfoldForall (mkAnn tk1) <$> elabBindersAndBody ns t
  B.Lam tk1 ns _tk2 e       -> do checkNonEmpty tk1 ns; unfoldLam    (mkAnn tk1) <$> elabBindersAndBody ns e

  B.Forall    tk1 ns    _tk2 e  -> elabQuantifier   tk1 V.Forall ns e
  B.Exists    tk1 ns    _tk2 e  -> elabQuantifier   tk1 V.Exists ns e
  B.ForallIn  tk1 ns e1 _tk2 e2 -> elabQuantifierIn tk1 V.Forall ns e1 e2
  B.ExistsIn  tk1 ns e1 _tk2 e2 -> elabQuantifierIn tk1 V.Exists ns e1 e2

  B.Foreach   tk1 ns    _tk2 e  -> elabForeach   tk1 ns e

  B.Unit   tk -> constructor V.Unit   tk []
  B.Bool   tk -> constructor V.Bool   tk []
  B.Index  tk -> constructor V.Index  tk []
  B.Nat    tk -> constructor V.Nat    tk []
  B.Int    tk -> constructor V.Int    tk []
  B.Rat    tk -> constructor V.Rat    tk []
  B.List   tk -> constructor V.List   tk []
  B.Vector tk -> constructor V.Vector tk []

  B.Tensor tk -> builtin V.Tensor tk []

  B.Nil tk                  -> constructor V.Cons tk []
  B.Cons e1 tk e2           -> constructor V.Cons tk [e1, e2]

  B.Not tk e                -> builtin (V.TypeClassOp V.NotTC)     tk  [e]
  B.Impl e1 tk e2           -> builtin (V.TypeClassOp V.ImpliesTC) tk  [e1, e2]
  B.And e1 tk e2            -> builtin (V.TypeClassOp V.AndTC)     tk  [e1, e2]
  B.Or e1 tk e2             -> builtin (V.TypeClassOp V.OrTC)      tk  [e1, e2]
  B.If tk1 e1 _ e2 _ e3     -> builtin V.If tk1 [e1, e2, e3]

  B.Eq  e1 tk e2            -> builtin (V.TypeClassOp $ V.EqualsTC V.Eq)  tk [e1, e2]
  B.Neq e1 tk e2            -> builtin (V.TypeClassOp $ V.EqualsTC V.Neq) tk [e1, e2]

  B.Le e1 tk e2             -> elabOrder V.Le tk e1 e2
  B.Lt e1 tk e2             -> elabOrder V.Lt tk e1 e2
  B.Ge e1 tk e2             -> elabOrder V.Ge tk e1 e2
  B.Gt e1 tk e2             -> elabOrder V.Gt tk e1 e2

  B.Add e1 tk e2            -> builtin (V.TypeClassOp V.AddTC) tk [e1, e2]
  B.Sub e1 tk e2            -> builtin (V.TypeClassOp V.SubTC) tk [e1, e2]
  B.Mul e1 tk e2            -> builtin (V.TypeClassOp V.MulTC) tk [e1, e2]
  B.Div e1 tk e2            -> builtin (V.TypeClassOp V.DivTC) tk [e1, e2]
  B.Neg tk e                -> builtin (V.TypeClassOp V.NegTC) tk [e]

  B.At e1 tk e2             -> builtin V.At   tk [e1, e2]
  B.Map tk e1 e2            -> builtin (V.TypeClassOp V.MapTC)  tk [e1, e2]
  B.Fold tk e1 e2 e3        -> builtin (V.TypeClassOp V.FoldTC) tk [e1, e2, e3]

elabArg :: MonadElab m => B.Arg -> m V.InputArg
elabArg = \case
  B.ExplicitArg e -> mkArg V.Explicit <$> elabExpr e
  B.ImplicitArg e -> mkArg V.Implicit <$> elabExpr e
  B.InstanceArg e -> mkArg V.Instance <$> elabExpr e

mkArg :: V.Visibility -> V.InputExpr -> V.InputArg
mkArg v e = V.Arg (V.expandByArgVisibility v (V.provenanceOf e)) v V.Relevant e

elabName :: MonadElab m => B.Name -> m V.Identifier
elabName n = do
  mod <- ask
  return $ V.Identifier mod $ tkSymbol n

elabBinder :: MonadElab m => B.Binder -> m V.InputBinder
elabBinder = \case
  B.ExplicitBinder    n         -> return $ mkBinder n V.Explicit Nothing
  B.ImplicitBinder    n         -> return $ mkBinder n V.Implicit Nothing
  B.InstanceBinder    n         -> return $ mkBinder n V.Instance Nothing
  B.ExplicitBinderAnn n _tk typ -> mkBinder n V.Explicit . Just <$> elabExpr typ
  B.ImplicitBinderAnn n _tk typ -> mkBinder n V.Implicit . Just <$> elabExpr typ
  B.InstanceBinderAnn n _tk typ -> mkBinder n V.Instance . Just <$> elabExpr typ

mkBinder :: B.Name -> V.Visibility -> Maybe V.InputExpr -> V.InputBinder
mkBinder n v e = V.Binder (V.expandByArgVisibility v p) v V.Relevant (Just (tkSymbol n)) t
  where
  (p, t) = case e of
    Nothing  -> (V.tkProvenance n, V.mkHole (V.tkProvenance n) ("typeOf[" <> tkSymbol n <> "]"))
    Just t1  -> (V.fillInProvenance (V.tkProvenance n :| [V.provenanceOf t1]), t1)

elabLetDecl :: MonadElab m => B.LetDecl -> m (V.InputBinder, V.InputExpr)
elabLetDecl (B.LDecl b e) = bitraverse elabBinder elabExpr (b,e)

elabLiteral :: MonadElab m => B.Lit -> m V.InputExpr
elabLiteral = \case
  B.UnitLiteral   -> return $ V.Literal mempty V.LUnit
  B.BoolLiteral t -> do
    let p = mkAnn t
    let b = read (unpack $ tkSymbol t)
    return $ V.Literal p $ V.LBool b
  B.NatLiteral   t -> do
    let p = mkAnn t
    let n = readNat (tkSymbol t)
    let fromNat = V.Builtin p (V.TypeClassOp $ V.FromNatTC n)
    return $ app fromNat [V.Literal p $ V.LNat n]
  B.RatLiteral   t -> do
    let p = mkAnn t
    let r = readRat (tkSymbol t)
    let fromRat = V.Builtin p (V.TypeClassOp V.FromRatTC)
    return $ app fromRat [V.Literal p $ V.LRat r]

elabParameterOptions :: MonadElab m =>B.DeclAnnOpts -> m V.Resource
elabParameterOptions = \case
  B.DeclAnnWithoutOpts   -> return V.Parameter
  B.DeclAnnWithOpts opts -> foldM parseOpt V.Parameter opts
  where
  parseOpt :: MonadElab m => V.Resource -> B.DeclAnnOption -> m V.Resource
  parseOpt _r (B.BooleanOption nameToken valueToken) = do
    let name = tkSymbol nameToken
    let value = tkSymbol valueToken
    if name /= V.InferableOption
      then throwError $
        InvalidAnnotationOption (V.tkProvenance nameToken) "@parameter" name [V.InferableOption]
      else case readMaybe (unpack value) of
          Just True  -> return V.InferableParameter
          Just False -> return V.Parameter
          Nothing    -> throwError $ InvalidAnnotationOptionValue (V.tkProvenance valueToken) name value

parseTypeLevel :: B.TypeToken -> Int
parseTypeLevel s = read (drop 4 (unpack (tkSymbol s)))

op1 :: (MonadElab m, V.HasProvenance a, IsToken token)
    => (V.Provenance -> a -> b)
    -> token -> m a -> m b
op1 mk t e = do
  ce <- e
  let p = V.fillInProvenance (V.tkProvenance t :| [V.provenanceOf ce])
  return $ mk p ce

op2 :: (MonadElab m, V.HasProvenance a, V.HasProvenance b, IsToken token)
    => (V.Provenance -> a -> b -> c)
    -> token -> m a -> m b -> m c

op2 mk t e1 e2 = do
  ce1 <- e1
  ce2 <- e2
  let p = V.fillInProvenance (V.tkProvenance t :| [V.provenanceOf ce1, V.provenanceOf ce2])
  return $ mk p ce1 ce2

builtin :: (MonadElab m, IsToken token) => V.Builtin -> token -> [B.Expr] -> m V.InputExpr
builtin b t args = app (V.Builtin (V.tkProvenance t) b) <$> traverse elabExpr args

constructor :: (MonadElab m, IsToken token) => V.BuiltinConstructor -> token -> [B.Expr] -> m V.InputExpr
constructor b = builtin (V.Constructor b)

app :: V.InputExpr -> [V.InputExpr] -> V.InputExpr
app fun argExprs = V.normAppList p' fun args
  where
    p'   = V.fillInProvenance (V.provenanceOf fun :| map V.provenanceOf args)
    args = fmap (mkArg V.Explicit) argExprs

elabVecLiteral :: (MonadElab m, IsToken token) => token -> [B.Expr] -> m V.InputExpr
elabVecLiteral tk xs = do
  let n = length xs
  let p = V.tkProvenance tk
  xs' <- op1 V.LVec tk (traverse elabExpr xs)
  return $ app (V.Builtin p (V.TypeClassOp $ V.FromVecTC n)) [xs']

elabFunInputType :: MonadElab m => B.Expr -> m V.InputBinder
elabFunInputType t = do
  t' <- elabExpr t
  return $ V.ExplicitBinder (V.provenanceOf t') Nothing t'

elabApp :: MonadElab m => B.Expr -> B.Arg -> m V.InputExpr
elabApp fun arg = do
  fun' <- elabExpr fun
  arg' <- elabArg arg
  let p = V.fillInProvenance (V.provenanceOf fun' :| [V.provenanceOf arg'])
  return $ V.normAppList p fun' [arg']

elabBindersAndBody :: MonadElab m => [B.Binder] -> B.Expr -> m ([V.InputBinder], V.InputExpr)
elabBindersAndBody bs body = bitraverse (traverse elabBinder) elabExpr (bs, body)

elabQuantifier :: (MonadElab m, IsToken token) => token -> V.Quantifier -> [B.Binder] -> B.Expr -> m V.InputExpr
elabQuantifier t q bs body = do
  checkNonEmpty t bs
  let qExpr = V.TypeClassOp $ V.QuantifierTC q
  unfoldQuantifier (mkAnn t) qExpr <$> elabBindersAndBody bs body

elabQuantifierIn :: (MonadElab m, IsToken token) => token -> V.Quantifier -> [B.Binder] -> B.Expr -> B.Expr -> m V.InputExpr
elabQuantifierIn t q bs container body = do
  checkNonEmpty t bs
  unfoldQuantifierIn (mkAnn t) q <$> elabExpr container <*> elabBindersAndBody bs body

elabTensor :: (MonadElab m, IsToken token) => token -> B.Expr -> B.Expr -> m V.InputExpr
elabTensor tk tElem tDims = builtin V.Tensor tk [tElem, tDims]

elabForeach :: (MonadElab m, IsToken token) => token -> [B.Binder] -> B.Expr -> m V.InputExpr
elabForeach t bs body = do
  checkNonEmpty t bs
  unfoldQuantifier (mkAnn t) V.Foreach <$> elabBindersAndBody bs body

elabOrder :: (MonadElab m, IsToken token) => V.OrderOp -> token -> B.Expr -> B.Expr -> m V.InputExpr
elabOrder order tk e1 e2 = do
  let Tk tkDetails@(tkPos, _) = toToken tk
  let chainedOrder = case e1 of
        B.Le _ _ e -> Just (V.Le, e)
        B.Lt _ _ e -> Just (V.Lt, e)
        B.Ge _ _ e -> Just (V.Ge, e)
        B.Gt _ _ e -> Just (V.Gt, e)
        _          -> Nothing

  case chainedOrder of
    Nothing -> builtin (V.TypeClassOp $ V.OrderTC order) tk [e1, e2]
    Just (prevOrder, e)
      | not (V.chainable prevOrder order) ->
        throwError $ UnchainableOrders (V.tkProvenance tk) prevOrder order
      | otherwise -> elabExpr $ B.And e1 (B.TokAnd (tkPos, "and")) $ case order of
        V.Le -> B.Le e (B.TokLe tkDetails) e2
        V.Lt -> B.Lt e (B.TokLt tkDetails) e2
        V.Ge -> B.Ge e (B.TokGe tkDetails) e2
        V.Gt -> B.Gt e (B.TokGt tkDetails) e2

mkAnn :: IsToken a => a -> V.Provenance
mkAnn = V.tkProvenance

checkNonEmpty :: (MonadElab m, IsToken token) => token -> [a] -> m ()
checkNonEmpty tk = checkNonEmpty' (V.tkProvenance tk) (tkSymbol tk)

checkNonEmpty' :: (MonadElab m) => V.Provenance -> Text -> [a] -> m ()
checkNonEmpty' p s []      = throwError $ MissingVariables p s
checkNonEmpty' _ _ (_ : _) = return ()

-- |Constructs a pi type filled with an appropriate number of holes for
-- a definition which has no accompanying type.
constructUnknownDefType :: B.Name -> [B.Binder] -> B.Expr
constructUnknownDefType n binders
  | null binders = returnType
  | otherwise    = B.ForallT tokForallT binders tokDot returnType
  where
  returnType :: B.Expr
  returnType = B.Hole $ mkToken B.HoleToken (typifyName (tkSymbol n))

  typifyName :: Text -> Text
  typifyName x = "typeOf_" <> x
