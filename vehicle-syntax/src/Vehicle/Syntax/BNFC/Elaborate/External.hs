{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Syntax.BNFC.Elaborate.External
  ( UnparsedExpr (..),
    UnparsedProg,
    UnparsedDecl,
    elaborateProg,
    elaborateDecl,
    elaborateExpr,
  )
where

import Control.Monad (foldM)
import Control.Monad.Except (MonadError (..), throwError)
import Control.Monad.Reader (MonadReader (..), runReaderT)
import Data.Bitraversable (bitraverse)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set (singleton)
import Data.Text (Text, unpack)
import Data.These (These (..))
import Text.Read (readMaybe)
import Vehicle.Syntax.AST qualified as V
import Vehicle.Syntax.BNFC.Utils
  ( MonadElab,
    mkProvenance,
    tokArrow,
    tokDot,
    tokForallT,
    tokLambda,
    tokType,
  )
import Vehicle.Syntax.External.Abs qualified as B
import Vehicle.Syntax.Parse.Error (ParseError (..))
import Vehicle.Syntax.Parse.Token
import Vehicle.Syntax.Prelude (developerError, readNat, readRat)
import Vehicle.Syntax.Sugar

--------------------------------------------------------------------------------
-- Public interface

newtype UnparsedExpr = UnparsedExpr B.Expr

type UnparsedDecl = V.GenericDecl UnparsedExpr

type UnparsedProg = V.GenericProg UnparsedExpr

-- | We elaborate from the simple AST generated automatically by BNFC to our
-- more complicated internal version of the AST.
elaborateProg ::
  (MonadError ParseError m) =>
  V.Module ->
  B.Prog ->
  m UnparsedProg
elaborateProg mod prog = runReaderT (elabProg prog) mod

elaborateDecl ::
  (MonadError ParseError m) =>
  V.Module ->
  UnparsedDecl ->
  m V.InputDecl
elaborateDecl mod decl = flip runReaderT mod $ case decl of
  V.DefResource p n r t -> V.DefResource p n r <$> elabDeclType t
  V.DefPostulate p n t -> V.DefPostulate p n <$> elabDeclType t
  V.DefFunction p n b t e -> V.DefFunction p n b <$> elabDeclType t <*> elabDeclBody e

elaborateExpr ::
  (MonadError ParseError m) =>
  V.Module ->
  UnparsedExpr ->
  m V.InputExpr
elaborateExpr mod (UnparsedExpr expr) = runReaderT (elabExpr expr) mod

--------------------------------------------------------------------------------
-- Algorithm

elabProg :: (MonadElab m) => B.Prog -> m UnparsedProg
elabProg (B.Main decls) = V.Main <$> elabDecls decls

elabDecls :: (MonadElab m) => [B.Decl] -> m [UnparsedDecl]
elabDecls = \case
  [] -> return []
  decl : decls -> do
    (d', ds) <- elabDeclGroup (decl :| decls)
    ds' <- elabDecls ds
    return $ d' : ds'

elabDeclGroup :: (MonadElab m) => NonEmpty B.Decl -> m (UnparsedDecl, [B.Decl])
elabDeclGroup dx = case dx of
  -- Type definition.
  B.DefType n bs t :| ds -> do
    d' <- elabTypeDef n bs t
    return (d', ds)

  -- Function declaration and body.
  B.DefFunType typeName _ t :| B.DefFunExpr exprName bs e : ds -> do
    d' <- elabDefFun False typeName exprName t bs e
    return (d', ds)

  -- Function body without a declaration.
  B.DefFunExpr n bs e :| ds -> do
    let unknownType = constructUnknownDefType n bs
    d' <- elabDefFun False n n unknownType bs e
    return (d', ds)

  -- ERROR: Function declaration with no body
  B.DefFunType n _tk _t :| _ -> do
    p <- mkProvenance n
    throwError $ FunctionNotGivenBody p (tkSymbol n)
  -- Property declaration.
  B.DefAnn a@B.Property {} opts :| B.DefFunType typeName _tk t : B.DefFunExpr exprName bs e : ds -> do
    checkNoAnnotationOptions a opts
    d' <- elabDefFun True typeName exprName t bs e
    return (d', ds)

  -- ERROR: Property with no body
  B.DefAnn B.Property {} _ :| B.DefFunType n _ _ : _ -> do
    p <- mkProvenance n
    throwError $ PropertyNotGivenBody p (tkSymbol n)
  -- ERROR: Other annotation with body
  B.DefAnn a _ :| B.DefFunType typeName _ _ : B.DefFunExpr bodyName _ _ : _
    | tkSymbol typeName == tkSymbol bodyName -> do
        p <- mkProvenance typeName
        throwError $ ResourceGivenBody p (annotationSymbol a) (tkSymbol typeName)
  -- Network declaration.
  B.DefAnn a@B.Network {} opts :| B.DefFunType n _ t : ds -> do
    checkNoAnnotationOptions a opts
    d' <- elabResource n t V.Network
    return (d', ds)

  -- Dataset declaration.
  B.DefAnn a@B.Dataset {} opts :| B.DefFunType n _ t : ds -> do
    checkNoAnnotationOptions a opts
    d' <- elabResource n t V.Dataset
    return (d', ds)

  -- Parameter declaration.
  (B.DefAnn B.Parameter {} opts :| B.DefFunType n _ t : ds) -> do
    r <- elabParameterOptions opts
    d' <- elabResource n t r
    return (d', ds)
  (B.DefAnn B.Postulate {} opts :| B.DefFunType n _ t : ds) -> do
    d' <- elabPostulate n t
    return (d', ds)
  (B.DefAnn a _ :| _) -> do
    p <- annotationProvenance a
    throwError $ AnnotationWithNoDeclaration p (annotationSymbol a)

elabDeclType ::
  (MonadElab m) =>
  UnparsedExpr ->
  m V.InputExpr
elabDeclType (UnparsedExpr expr) = elabExpr expr

elabDeclBody ::
  (MonadElab m) =>
  UnparsedExpr ->
  m V.InputExpr
elabDeclBody (UnparsedExpr expr) = case expr of
  B.Lam tk binders _ body -> do
    binders' <- traverse (elabNameBinder True) binders
    body' <- elabExpr body
    p <- mkProvenance tk
    return $ foldr (V.Lam p) body' binders'
  _ -> developerError "Invalid declaration body - no lambdas found"

annotationSymbol :: B.DeclAnnName -> Text
annotationSymbol = \case
  B.Network tk -> tkSymbol tk
  B.Dataset tk -> tkSymbol tk
  B.Parameter tk -> tkSymbol tk
  B.Property tk -> tkSymbol tk
  B.Postulate tk -> tkSymbol tk

annotationProvenance :: (MonadElab m) => B.DeclAnnName -> m V.Provenance
annotationProvenance = \case
  B.Network tk -> mkProvenance tk
  B.Dataset tk -> mkProvenance tk
  B.Parameter tk -> mkProvenance tk
  B.Property tk -> mkProvenance tk
  B.Postulate tk -> mkProvenance tk

checkNoAnnotationOptions :: (MonadElab m) => B.DeclAnnName -> B.DeclAnnOpts -> m ()
checkNoAnnotationOptions annName = \case
  B.DeclAnnWithoutOpts -> return ()
  B.DeclAnnWithOpts [] -> return ()
  B.DeclAnnWithOpts (o : _) -> do
    let B.BooleanOption paramName _ = o
    p <- mkProvenance paramName
    throwError $ InvalidAnnotationOption p (annotationSymbol annName) (tkSymbol paramName) []

elabResource :: (MonadElab m) => B.Name -> B.Expr -> V.Resource -> m (V.GenericDecl UnparsedExpr)
elabResource n t r = V.DefResource <$> mkProvenance n <*> elabName n <*> pure r <*> pure (UnparsedExpr t)

elabPostulate :: (MonadElab m) => B.Name -> B.Expr -> m (V.GenericDecl UnparsedExpr)
elabPostulate n t = V.DefPostulate <$> mkProvenance n <*> elabName n <*> pure (UnparsedExpr t)

elabTypeDef :: (MonadElab m) => B.Name -> [B.NameBinder] -> B.Expr -> m (V.GenericDecl UnparsedExpr)
elabTypeDef n binders e = do
  p <- mkProvenance n
  name <- elabName n
  let typeTyp
        | null binders = tokType 0
        | otherwise = B.ForallT tokForallT binders tokDot (tokType 0)
  let typeBody = B.Lam tokLambda binders tokArrow e
  return $ V.DefFunction p name False (UnparsedExpr typeTyp) (UnparsedExpr typeBody)

elabDefFun :: (MonadElab m) => Bool -> B.Name -> B.Name -> B.Expr -> [B.NameBinder] -> B.Expr -> m (V.GenericDecl UnparsedExpr)
elabDefFun isProperty n1 n2 t binders e
  | tkSymbol n1 /= tkSymbol n2 = do
      p <- mkProvenance n1
      throwError $ FunctionWithMismatchedNames p (tkSymbol n1) (tkSymbol n2)
  | otherwise = do
      p <- mkProvenance n1
      name <- elabName n1
      -- This is a bit evil, we don't normally store possibly empty set of
      -- binders, but we will use this to indicate the set of LHS variables.
      let body = B.Lam tokLambda binders tokArrow e
      return $ V.DefFunction p name isProperty (UnparsedExpr t) (UnparsedExpr body)

--------------------------------------------------------------------------------
-- Expr elaboration

elabExpr :: (MonadElab m) => B.Expr -> m V.InputExpr
elabExpr = \case
  B.Type t -> V.Universe <$> mkProvenance t <*> pure (V.UniverseLevel 0)
  B.Var n -> V.BoundVar <$> mkProvenance n <*> pure (tkSymbol n)
  B.Hole n -> V.mkHole <$> mkProvenance n <*> pure (tkSymbol n)
  B.Literal l -> elabLiteral l
  B.Ann e tk t -> op2 V.Ann tk (elabExpr e) (elabExpr t)
  B.Fun t1 tk t2 -> op2 V.Pi tk (elabTypeBinder False t1) (elabExpr t2)
  B.VecLiteral tk1 es _tk2 -> elabVecLiteral tk1 es
  B.App e1 e2 -> elabApp e1 e2
  B.Let tk1 ds e -> elabLet tk1 ds e
  B.ForallT tk1 ns _tk2 t -> elabForallT tk1 ns t
  B.Lam tk1 ns _tk2 e -> elabLam tk1 ns e
  B.Forall tk1 ns _tk2 e -> elabQuantifier tk1 V.Forall ns e
  B.Exists tk1 ns _tk2 e -> elabQuantifier tk1 V.Exists ns e
  B.ForallIn tk1 ns e1 _tk2 e2 -> elabQuantifierIn tk1 V.Forall ns e1 e2
  B.ExistsIn tk1 ns e1 _tk2 e2 -> elabQuantifierIn tk1 V.Exists ns e1 e2
  B.Foreach tk1 ns _tk2 e -> elabForeach tk1 ns e
  B.Unit tk -> builtinType V.Unit tk []
  B.Bool tk -> builtinType V.Bool tk []
  B.Index tk -> builtinType V.Index tk []
  B.Nat tk -> builtinType V.Nat tk []
  B.Int tk -> builtinType V.Int tk []
  B.Rat tk -> builtinType V.Rat tk []
  B.List tk -> builtinType V.List tk []
  B.Vector tk -> builtinType V.Vector tk []
  B.Nil tk -> constructor V.Nil tk []
  B.Cons e1 tk e2 -> constructor V.Cons tk [e1, e2]
  B.ConsVector e1 tk e2 -> builtinFunction V.ConsVector tk [e1, e2]
  B.Indices tk -> builtinFunction V.Indices tk []
  B.Not tk e -> builtinFunction V.Not tk [e]
  B.Impl e1 tk e2 -> builtinFunction V.Implies tk [e1, e2]
  B.And e1 tk e2 -> builtinFunction V.And tk [e1, e2]
  B.Or e1 tk e2 -> builtinFunction V.Or tk [e1, e2]
  B.If tk1 e1 _ e2 _ e3 -> builtinFunction V.If tk1 [e1, e2, e3]
  B.Eq e1 tk e2 -> builtin (V.TypeClassOp $ V.EqualsTC V.Eq) tk [e1, e2]
  B.Neq e1 tk e2 -> builtin (V.TypeClassOp $ V.EqualsTC V.Neq) tk [e1, e2]
  B.Le e1 tk e2 -> elabOrder V.Le tk e1 e2
  B.Lt e1 tk e2 -> elabOrder V.Lt tk e1 e2
  B.Ge e1 tk e2 -> elabOrder V.Ge tk e1 e2
  B.Gt e1 tk e2 -> elabOrder V.Gt tk e1 e2
  B.Add e1 tk e2 -> builtin (V.TypeClassOp V.AddTC) tk [e1, e2]
  B.Sub e1 tk e2 -> builtin (V.TypeClassOp V.SubTC) tk [e1, e2]
  B.Mul e1 tk e2 -> builtin (V.TypeClassOp V.MulTC) tk [e1, e2]
  B.Div e1 tk e2 -> builtin (V.TypeClassOp V.DivTC) tk [e1, e2]
  B.Neg tk e -> builtin (V.TypeClassOp V.NegTC) tk [e]
  B.At e1 tk e2 -> builtinFunction V.At tk [e1, e2]
  B.Map tk -> builtin (V.TypeClassOp V.MapTC) tk []
  B.Fold tk -> builtin (V.TypeClassOp V.FoldTC) tk []
  B.DepFold tk -> builtinFunction (V.Fold V.FoldVector) tk []
  B.HasEq tk -> builtinTypeClass (V.HasEq V.Eq) tk []
  B.HasNotEq tk -> builtinTypeClass (V.HasEq V.Neq) tk []
  B.HasAdd tk -> builtinTypeClass V.HasAdd tk []
  B.HasSub tk -> builtinTypeClass V.HasSub tk []
  B.HasMul tk -> builtinTypeClass V.HasMul tk []
  B.HasMap tk -> builtinTypeClass V.HasMap tk []
  B.HasFold tk -> builtinTypeClass V.HasFold tk []

elabArg :: (MonadElab m) => B.Arg -> m V.InputArg
elabArg = \case
  B.ExplicitArg e -> mkArg V.Explicit <$> elabExpr e
  B.ImplicitArg e -> mkArg (V.Implicit False) <$> elabExpr e
  B.InstanceArg e -> mkArg (V.Instance False) <$> elabExpr e

elabName :: (MonadElab m) => B.Name -> m V.Identifier
elabName n = do
  mod <- ask
  return $ V.Identifier mod $ tkSymbol n

elabBasicBinder :: (MonadElab m) => Bool -> B.BasicBinder -> m V.InputBinder
elabBasicBinder folded = \case
  B.ExplicitBinder n _tk typ -> mkBinder folded V.Explicit . These n =<< elabExpr typ
  B.ImplicitBinder n _tk typ -> mkBinder folded (V.Implicit False) . These n =<< elabExpr typ
  B.InstanceBinder n _tk typ -> mkBinder folded (V.Instance False) . These n =<< elabExpr typ

elabNameBinder :: (MonadElab m) => Bool -> B.NameBinder -> m V.InputBinder
elabNameBinder folded = \case
  B.ExplicitNameBinder n -> mkBinder folded V.Explicit (This n)
  B.ImplicitNameBinder n -> mkBinder folded (V.Implicit False) (This n)
  B.InstanceNameBinder n -> mkBinder folded (V.Instance False) (This n)
  B.BasicNameBinder b -> elabBasicBinder folded b

elabTypeBinder :: (MonadElab m) => Bool -> B.TypeBinder -> m V.InputBinder
elabTypeBinder folded = \case
  B.ExplicitTypeBinder t -> mkBinder folded V.Explicit . That =<< elabExpr t
  B.ImplicitTypeBinder t -> mkBinder folded (V.Implicit False) . That =<< elabExpr t
  B.InstanceTypeBinder t -> mkBinder folded (V.Instance False) . That =<< elabExpr t
  B.BasicTypeBinder b -> elabBasicBinder folded b

mkArg :: V.Visibility -> V.InputExpr -> V.InputArg
mkArg v e = V.Arg (V.expandByArgVisibility v (V.provenanceOf e)) v V.Relevant e

mkBinder :: (MonadElab m) => V.BinderFoldingForm -> V.Visibility -> These B.Name V.InputExpr -> m V.InputBinder
mkBinder folded v nameTyp = do
  (exprProv, form, typ) <- case nameTyp of
    This nameTk -> do
      p <- mkProvenance nameTk
      let name = tkSymbol nameTk
      let typ = V.mkHole p $ "typeOf[" <> name <> "]"
      let naming = V.OnlyName name
      return (p, naming, typ)
    That typ -> do
      let p = V.provenanceOf typ
      let naming = V.OnlyType
      return (V.provenanceOf typ, naming, typ)
    These nameTk typ -> do
      nameProv <- mkProvenance nameTk
      let p = V.fillInProvenance ((nameProv :: V.Provenance) :| [V.provenanceOf typ])
      let name = tkSymbol nameTk
      let naming = V.NameAndType name
      return (p, naming, typ)

  let prov = V.expandByArgVisibility v exprProv
  let displayForm = V.BinderDisplayForm form folded
  return $ V.Binder prov displayForm v V.Relevant () typ

elabLetDecl :: (MonadElab m) => B.LetDecl -> m (V.InputBinder, V.InputExpr)
elabLetDecl (B.LDecl b e) = bitraverse (elabNameBinder False) elabExpr (b, e)

elabLiteral :: (MonadElab m) => B.Lit -> m V.InputExpr
elabLiteral = \case
  B.UnitLiteral -> return $ V.Builtin mempty $ V.Constructor V.LUnit
  B.BoolLiteral t -> do
    p <- mkProvenance t
    let b = read (unpack $ tkSymbol t)
    return $ V.Builtin p $ V.Constructor $ V.LBool b
  B.NatLiteral t -> do
    p <- mkProvenance t
    let n = readNat (tkSymbol t)
    let fromNat = V.Builtin p (V.TypeClassOp $ V.FromNatTC n)
    return $ app fromNat [V.Builtin p $ V.Constructor $ V.LNat n]
  B.RatLiteral t -> do
    p <- mkProvenance t
    let r = readRat (tkSymbol t)
    let fromRat = V.Builtin p (V.TypeClassOp V.FromRatTC)
    return $ app fromRat [V.Builtin p $ V.Constructor $ V.LRat r]

elabParameterOptions :: (MonadElab m) => B.DeclAnnOpts -> m V.Resource
elabParameterOptions = \case
  B.DeclAnnWithoutOpts -> return V.Parameter
  B.DeclAnnWithOpts opts -> foldM parseOpt V.Parameter opts
  where
    parseOpt :: (MonadElab m) => V.Resource -> B.DeclAnnOption -> m V.Resource
    parseOpt _r (B.BooleanOption nameToken valueToken) = do
      let name = tkSymbol nameToken
      let value = tkSymbol valueToken
      if name /= V.InferableOption
        then do
          p <- mkProvenance nameToken
          throwError $ InvalidAnnotationOption p "@parameter" name [V.InferableOption]
        else case readMaybe (unpack value) of
          Just True -> return V.InferableParameter
          Just False -> return V.Parameter
          Nothing -> do
            p <- mkProvenance nameToken
            throwError $ InvalidAnnotationOptionValue p name value

parseTypeLevel :: B.TokType -> Int
parseTypeLevel s = read (drop 4 (unpack (tkSymbol s)))

op1 ::
  (MonadElab m, V.HasProvenance a, IsToken token) =>
  (V.Provenance -> a -> b) ->
  token ->
  m a ->
  m b
op1 mk t e = do
  ce <- e
  tProv <- mkProvenance t
  let p = V.fillInProvenance (tProv :| [V.provenanceOf ce])
  return $ mk p ce

op2 ::
  (MonadElab m, V.HasProvenance a, V.HasProvenance b, IsToken token) =>
  (V.Provenance -> a -> b -> c) ->
  token ->
  m a ->
  m b ->
  m c
op2 mk t e1 e2 = do
  ce1 <- e1
  ce2 <- e2
  tProv <- mkProvenance t
  let p = V.fillInProvenance (tProv :| [V.provenanceOf ce1, V.provenanceOf ce2])
  return $ mk p ce1 ce2

builtin :: (MonadElab m, IsToken token) => V.Builtin -> token -> [B.Expr] -> m V.InputExpr
builtin b t args = do
  tProv <- mkProvenance t
  app (V.Builtin tProv b) <$> traverse elabExpr args

constructor :: (MonadElab m, IsToken token) => V.BuiltinConstructor -> token -> [B.Expr] -> m V.InputExpr
constructor b = builtin (V.Constructor b)

builtinType :: (MonadElab m, IsToken token) => V.BuiltinType -> token -> [B.Expr] -> m V.InputExpr
builtinType b = builtin (V.BuiltinType b)

builtinTypeClass :: (MonadElab m, IsToken token) => V.TypeClass -> token -> [B.Expr] -> m V.InputExpr
builtinTypeClass b = builtin (V.TypeClass b)

builtinFunction :: (MonadElab m, IsToken token) => V.BuiltinFunction -> token -> [B.Expr] -> m V.InputExpr
builtinFunction b = builtin (V.BuiltinFunction b)

app :: V.InputExpr -> [V.InputExpr] -> V.InputExpr
app fun argExprs = V.normAppList p' fun args
  where
    p' = V.fillInProvenance (V.provenanceOf fun :| map V.provenanceOf args)
    args = fmap (mkArg V.Explicit) argExprs

elabVecLiteral :: (MonadElab m, IsToken token) => token -> [B.Expr] -> m V.InputExpr
elabVecLiteral tk xs = do
  let n = length xs
  p <- mkProvenance tk
  xs' <- traverse elabExpr xs
  let lit = app (V.Builtin p $ V.Constructor $ V.LVec n) xs'
  return $ app (V.Builtin p (V.TypeClassOp V.FromVecTC)) [lit]

elabApp :: (MonadElab m) => B.Expr -> B.Arg -> m V.InputExpr
elabApp fun arg = do
  fun' <- elabExpr fun
  arg' <- elabArg arg
  let p = V.fillInProvenance (V.provenanceOf fun' :| [V.provenanceOf arg'])
  return $ V.normAppList p fun' [arg']

elabOrder :: (MonadElab m, IsToken token) => V.OrderOp -> token -> B.Expr -> B.Expr -> m V.InputExpr
elabOrder order tk e1 e2 = do
  let Tk tkDetails@(tkPos, _) = toToken tk
  let chainedOrder = case e1 of
        B.Le _ _ e -> Just (V.Le, e)
        B.Lt _ _ e -> Just (V.Lt, e)
        B.Ge _ _ e -> Just (V.Ge, e)
        B.Gt _ _ e -> Just (V.Gt, e)
        _ -> Nothing

  case chainedOrder of
    Nothing -> builtin (V.TypeClassOp $ V.OrderTC order) tk [e1, e2]
    Just (prevOrder, e)
      | not (V.chainable prevOrder order) -> do
          p <- mkProvenance tk
          throwError $ UnchainableOrders p prevOrder order
      | otherwise -> elabExpr $ B.And e1 (B.TokAnd (tkPos, "and")) $ case order of
          V.Le -> B.Le e (B.TokLe tkDetails) e2
          V.Lt -> B.Lt e (B.TokLt tkDetails) e2
          V.Ge -> B.Ge e (B.TokGe tkDetails) e2
          V.Gt -> B.Gt e (B.TokGt tkDetails) e2

-- | Unfolds a list of binders into a consecutative forall expressions
elabForallT :: (MonadElab m) => B.TokForallT -> [B.NameBinder] -> B.Expr -> m V.InputExpr
elabForallT tk binders body = do
  p <- mkProvenance tk
  binders' <- elabNamedBinders tk binders
  body' <- elabExpr body
  return $ foldr (V.Pi p) body' binders'

elabLam :: (MonadElab m) => B.TokLambda -> [B.NameBinder] -> B.Expr -> m V.InputExpr
elabLam tk binders body = do
  p <- mkProvenance tk
  binders' <- elabNamedBinders tk binders
  body' <- elabExpr body
  return $ foldr (V.Lam p) body' binders'

elabQuantifier ::
  (MonadElab m, IsToken token) =>
  token ->
  V.Quantifier ->
  [B.NameBinder] ->
  B.Expr ->
  m V.InputExpr
elabQuantifier tk q binders body = do
  p <- mkProvenance tk
  let builtin = V.Builtin p $ V.TypeClassOp $ V.QuantifierTC q

  binders' <- elabNamedBinders tk binders
  body' <- elabExpr body

  let mkQuantifier binder body =
        let p' = V.provenanceOf binder
         in V.normAppList
              p'
              builtin
              [ mkArg V.Explicit (V.Lam p' binder body)
              ]

  return $ foldr mkQuantifier body' binders'

elabQuantifierIn ::
  (MonadElab m, IsToken token) =>
  token ->
  V.Quantifier ->
  B.NameBinder ->
  B.Expr ->
  B.Expr ->
  m V.InputExpr
elabQuantifierIn tk q binder container body = do
  p <- mkProvenance tk
  let builtin = V.FreeVar p $ case q of
        V.Forall -> V.Identifier V.StdLib "forallIn"
        V.Exists -> V.Identifier V.StdLib "existsIn"

  binder' <- elabNameBinder False binder
  container' <- elabExpr container
  body' <- elabExpr body

  let p' = V.provenanceOf binder'
  return $
    V.normAppList
      p'
      builtin
      [ mkArg V.Explicit (V.Lam p' binder' body'),
        mkArg V.Explicit container'
      ]

elabForeach ::
  (MonadElab m, IsToken token) =>
  token ->
  B.NameBinder ->
  B.Expr ->
  m V.InputExpr
elabForeach tk binder body = do
  p <- mkProvenance tk
  let builtin = V.FreeVar p (V.Identifier V.StdLib "foreachVector")

  binder' <- elabNameBinder False binder
  body' <- elabExpr body

  let p' = V.provenanceOf binder'
  return $
    V.normAppList
      p'
      builtin
      [ mkArg V.Explicit (V.Lam p' binder' body')
      ]

elabLet :: (MonadElab m) => B.TokLet -> [B.LetDecl] -> B.Expr -> m V.InputExpr
elabLet tk decls body = do
  p <- mkProvenance tk
  decls' <- traverse elabLetDecl decls
  body' <- elabExpr body
  return $ foldr (insertLet p) body' decls'
  where
    insertLet :: V.Provenance -> (V.InputBinder, V.InputExpr) -> V.InputExpr -> V.InputExpr
    insertLet p (binder, bound) = V.Let p bound binder

elabNamedBinders :: (MonadElab m, IsToken token) => token -> [B.NameBinder] -> m (NonEmpty V.InputBinder)
elabNamedBinders tk binders = case binders of
  [] -> do
    p <- mkProvenance tk
    throwError $ MissingVariables p (tkSymbol tk)
  (d : ds) -> do
    d' <- elabNameBinder False d
    ds' <- traverse (elabNameBinder True) ds
    return (d' :| ds')

-- | Constructs a pi type filled with an appropriate number of holes for
--  a definition which has no accompanying type.
constructUnknownDefType :: B.Name -> [B.NameBinder] -> B.Expr
constructUnknownDefType n binders
  | null binders = returnType
  | otherwise = B.ForallT tokForallT binders tokDot returnType
  where
    returnType :: B.Expr
    returnType = B.Hole $ mkToken B.HoleToken (typifyName (tkSymbol n))

    typifyName :: Text -> Text
    typifyName x = "typeOf_" <> x
