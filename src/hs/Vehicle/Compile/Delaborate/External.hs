{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Vehicle.Compile.Delaborate.External
  ( Delaborate (delab, delabWithLogging)
  ) where

import Data.Text ( pack )
import Data.List.NonEmpty qualified as NonEmpty (toList)

import Vehicle.External.Abs qualified as B

import Vehicle.Prelude
import Vehicle.Language.AST qualified as V
import Vehicle.Language.Sugar
import Vehicle.Language.AST.Visibility (HasVisibility(visibilityOf))

--------------------------------------------------------------------------------
-- Conversion to BNFC AST

-- | Constraint for the monad stack used by the elaborator.
type MonadDelab m = MonadLogger m

tokArrow = mkToken B.TokArrow "->"
tokForallT = mkToken B.TokForallT "forall"
tokIf = mkToken B.TokIf "if"
tokThen = mkToken B.TokThen "then"
tokElse = mkToken B.TokElse "else"
tokDot = mkToken B.TokDot "."
tokElemOf = mkToken B.TokElemOf ":"
tokLambda = mkToken B.TokLambda "\\"
tokTensor = mkToken B.TokTensor "Tensor"
tokList = mkToken B.TokList "List"
tokReal = mkToken B.TokReal "Real"
tokRat = mkToken B.TokRat "Rat"
tokInt = mkToken B.TokInt "Int"
tokNat = mkToken B.TokNat "Nat"
tokBool = mkToken B.TokBool "Bool"
tokProp = mkToken B.TokProp "Prop"
tokFin = mkToken B.TokFin "Fin"
tokForall = mkToken B.TokForall "forall"
tokExists = mkToken B.TokExists "exists"
tokImpl = mkToken B.TokImpl "=>"
tokAnd = mkToken B.TokAnd "and"
tokOr = mkToken B.TokOr "or"
tokNot = mkToken B.TokNot "not"
tokEq = mkToken B.TokEq "=="
tokNeq = mkToken B.TokNeq "!="
tokLe = mkToken B.TokLe "<="
tokLt = mkToken B.TokLt "<"
tokGe = mkToken B.TokGe ">="
tokGt = mkToken B.TokGt ">"
tokMul = mkToken B.TokMul "*"
tokDiv = mkToken B.TokDiv "/"
tokAdd = mkToken B.TokAdd "+"
tokSub = mkToken B.TokSub "-"
tokSeqOpen = mkToken B.TokSeqOpen "["
tokSeqClose = mkToken B.TokSeqClose "]"
tokCons = mkToken B.TokCons "::"
tokAt = mkToken B.TokAt "!"
tokMap = mkToken B.TokMap "map"
tokFold = mkToken B.TokFold "fold"
tokTrue = mkToken B.TokTrue "True"
tokFalse = mkToken B.TokFalse "False"
tokTCEq = mkToken B.TokTCEq "HasEq"
tokTCOrd = mkToken B.TokTCOrd "HasOrd"
tokTCContainer = mkToken B.TokTCContainer "IsContainer"
tokTCTruth = mkToken B.TokTCTruth "IsTruth"
tokTCQuantify = mkToken B.TokTCQuantify "IsQuantify"
tokTCNatOps  = mkToken B.TokTCNatOps "HasNatOperations"
tokTCIntOps  = mkToken B.TokTCIntOps "HasIntOperations"
tokTCRatOps  = mkToken B.TokTCRatOps "HasRatOperations"
tokTCNatLits = mkToken B.TokTCNatLits "HasNatLiteralsUpTo"
tokTCIntLits = mkToken B.TokTCIntLits "HasIntLiterals"
tokTCRatLits = mkToken B.TokTCRatLits "HasRatLiterals"

-- * Conversion

class Delaborate t bnfc | t -> bnfc, bnfc -> t where
  delabM :: MonadDelab m => t ann -> m bnfc

  -- | Delaborates the program and throws away the logs, should only be used in
  -- user-facing error messages
  delab :: t ann -> bnfc
  delab = discardWarningsAndLogs . delabM

  delabWithLogging :: MonadDelab m => t ann -> m bnfc
  delabWithLogging x = do
    logDebug "Beginning delaboration"
    result <- delabM x
    logDebug "Ending delaboration\n"
    return result

--  delab :: t ann -> bnfc
--  delab = _
-- |Elaborate programs.
instance Delaborate (V.Prog Symbol Symbol) B.Prog where
  delabM (V.Main decls) = B.Main . concat <$> traverse delabM decls

-- |Elaborate declarations.
instance Delaborate (V.Decl Symbol Symbol) [B.Decl] where
  delabM = \case
    -- Elaborate a network declaration.
    (V.DeclNetw _ n t) -> do
      let n' = delabIdentifier n
      t' <- delabM t
      return [B.DeclNetw n' tokElemOf t']

    -- Elaborate a dataset declaration.
    (V.DeclData _ n t) -> do
      let n' = delabIdentifier n
      t' <- delabM t
      return [B.DeclData n' tokElemOf t']

    -- Elaborate a type definition.
    (V.DefFun _ n t e) -> delabFun n t e

instance Delaborate (V.Expr Symbol Symbol) B.Expr where
  delabM expr = case expr of
    V.Type l        -> return $ B.Type (fromIntegral l)
    V.Var _ n       -> return $ B.Var  (delabSymbol n)
    V.Hole _ n      -> return $ B.Hole (mkToken B.HoleToken n)
    V.Literal _ l   -> return $ B.Literal (delabLiteral l)

    V.Ann _ e t     -> B.Ann <$> delabM e <*> pure tokElemOf <*> delabM t
    V.LSeq _ _ es   -> B.LSeq tokSeqOpen <$> traverse delabM es <*> pure tokSeqClose

    V.Pi  ann t1 t2 -> delabPi ann t1 t2
    V.Let{}         -> delabLet expr
    V.Lam{}         -> delabLam expr
    V.Meta _ m      -> return $ B.Var (mkToken B.Name (layoutAsText (pretty m)))

    V.App _ (V.Builtin _ b)  args -> delabBuiltin b <$> traverse (delabM . V.argExpr) (removeNonExplicitArgs $ NonEmpty.toList args)
    V.App _ (V.Literal _ l) _args -> return $ B.Literal (delabLiteral l)
    V.App _ fun args             -> delabApp <$> delabM fun <*> traverse delabM (reverse (NonEmpty.toList args))
    V.Builtin _ op               -> return $ delabBuiltin op []

    -- This is a hack to get printing of PrimDicts to work without explicitly including
    -- them in the grammar
    V.PrimDict _ t  -> B.App (B.Var (mkToken B.Name "PrimDict")) . B.ExplicitArg <$> delabM t

instance Delaborate (V.Arg Symbol Symbol) B.Arg where
  delabM (V.Arg _i v e) = case v of
    V.Explicit -> B.ExplicitArg <$> delabM e
    V.Implicit -> B.ImplicitArg <$> delabM e
    V.Instance -> B.InstanceArg <$> delabM e

instance Delaborate (V.Binder Symbol Symbol) B.Binder where
  delabM (V.Binder _ann v n _t) = case v of
    -- TODO track whether type was provided manually and so use ExplicitBinderAnn
    V.Explicit -> return $ B.ExplicitBinder $ delabSymbol n
    V.Implicit -> return $ B.ImplicitBinder $ delabSymbol n
    V.Instance -> return $ B.InstanceBinder $ delabSymbol n

delabLetBinding :: MonadDelab m => (V.NamedBinder ann, V.NamedExpr ann) -> m B.LetDecl
delabLetBinding (binder, bound) = B.LDecl <$> delabM binder <*> delabM bound

delabLiteral :: V.Literal -> B.Lit
delabLiteral l = case l of
  V.LBool True  -> B.LitTrue  tokTrue
  V.LBool False -> B.LitFalse tokFalse
  V.LNat n      -> B.LitInt   (fromIntegral n)
  V.LInt i      -> B.LitInt   (fromIntegral i)
  V.LRat r      -> B.LitRat   (mkToken B.Rational (pack $ show (fromRational r :: Double)))

delabSymbol :: Symbol -> B.Name
delabSymbol = mkToken B.Name

delabIdentifier :: V.Identifier -> B.Name
delabIdentifier (V.Identifier n) = mkToken B.Name n

delabApp :: B.Expr -> [B.Arg] -> B.Expr
delabApp fun []           = fun
delabApp fun (arg : args) = B.App (delabApp fun args) arg

delabBuiltin :: V.Builtin -> [B.Expr] -> B.Expr
delabBuiltin fun args = case fun of
  V.BooleanType   V.Bool   -> B.Bool tokBool
  V.BooleanType   V.Prop   -> B.Prop tokProp
  V.NumericType   V.Nat    -> B.Nat  tokNat
  V.NumericType   V.Int    -> B.Int  tokInt
  V.NumericType   V.Rat    -> B.Rat  tokRat
  V.NumericType   V.Real   -> B.Real tokReal
  V.ContainerType V.List   -> delabOp1 B.List   tokList   args
  V.ContainerType V.Tensor -> delabOp2 B.Tensor tokTensor args
  V.Fin                    -> delabOp1 B.Fin    tokFin    args

  V.TypeClass V.IsTruth            -> delabOp1 B.TCTruth tokTCTruth     args
  V.TypeClass V.HasNatOps          -> delabOp1 B.TCNatOps  tokTCNatOps  args
  V.TypeClass V.HasIntOps          -> delabOp1 B.TCIntOps  tokTCIntOps  args
  V.TypeClass V.HasRatOps          -> delabOp1 B.TCRatOps  tokTCRatOps  args
  V.TypeClass (V.HasNatLitsUpTo n) -> delabOp1 (\tk -> B.TCNatLits tk (toInteger n)) tokTCNatLits args
  V.TypeClass V.HasIntLits         -> delabOp1 B.TCIntLits tokTCIntLits args
  V.TypeClass V.HasRatLits         -> delabOp1 B.TCRatLits tokTCRatLits args
  V.TypeClass V.IsQuantifiable     -> delabOp1 B.TCQuant tokTCQuantify  args
  V.TypeClass V.HasEq              -> delabOp2 B.TCEq    tokTCEq        args
  V.TypeClass V.HasOrd             -> delabOp2 B.TCOrd   tokTCOrd       args
  V.TypeClass V.IsContainer        -> delabOp2 B.TCCont  tokTCContainer args

  V.Equality V.Eq  -> delabInfixOp2 B.Eq  tokEq args
  V.Equality V.Neq -> delabInfixOp2 B.Neq tokNeq args

  V.If                -> delabIf args
  V.BooleanOp2 V.And  -> delabInfixOp2 B.And  tokAnd  args
  V.BooleanOp2 V.Or   -> delabInfixOp2 B.Or   tokOr   args
  V.BooleanOp2 V.Impl -> delabInfixOp2 B.Impl tokImpl args
  V.Not               -> delabOp1 B.Not tokNot args

  V.Order V.Le -> delabInfixOp2 B.Le tokLe args
  V.Order V.Lt -> delabInfixOp2 B.Lt tokLt args
  V.Order V.Ge -> delabInfixOp2 B.Ge tokGe args
  V.Order V.Gt -> delabInfixOp2 B.Gt tokGt args

  V.NumericOp2 V.Add -> delabInfixOp2 B.Add tokAdd args
  V.NumericOp2 V.Sub -> delabInfixOp2 B.Sub tokSub args
  V.NumericOp2 V.Mul -> delabInfixOp2 B.Mul tokMul args
  V.NumericOp2 V.Div -> delabInfixOp2 B.Div tokDiv args
  V.Neg              -> delabOp1      B.Neg tokSub args

  V.Cons -> delabInfixOp2 B.Cons tokCons args
  V.At   -> delabInfixOp2 B.At   tokAt   args
  V.Map  -> delabOp2      B.Map  tokMap  args
  V.Fold -> delabOp3      B.Fold tokFold args

  V.Quant   q -> delabQuant   q args
  V.QuantIn q -> delabQuantIn q args

delabOp1 :: IsToken token => (token -> B.Expr -> B.Expr) -> token -> [B.Expr] -> B.Expr
delabOp1 op tk [arg] = op tk arg
delabOp1 _  tk args  = argsError (tkSymbol tk) 1 args

delabOp2 :: IsToken token => (token -> B.Expr -> B.Expr -> B.Expr) -> token -> [B.Expr] -> B.Expr
delabOp2 op tk [arg1, arg2] = op tk arg1 arg2
delabOp2 _  tk args         = argsError (tkSymbol tk) 2 args

delabOp3 :: IsToken token => (token -> B.Expr -> B.Expr -> B.Expr -> B.Expr) -> token -> [B.Expr] -> B.Expr
delabOp3 op tk [arg1, arg2, arg3] = op tk arg1 arg2 arg3
delabOp3 _  tk args               = argsError (tkSymbol tk) 3 args

delabInfixOp2 :: IsToken token => (B.Expr -> token -> B.Expr -> B.Expr) -> token -> [B.Expr] -> B.Expr
delabInfixOp2 op tk [arg1, arg2] = op arg1 tk arg2
delabInfixOp2 _  tk args         = argsError (tkSymbol tk) 2 args

delabIf :: [B.Expr] -> B.Expr
delabIf [arg1, arg2, arg3] = B.If tokIf arg1 tokThen arg2 tokElse arg3
delabIf args               = argsError "if" 3 args

delabQuant :: V.Quantifier -> [B.Expr] -> B.Expr
delabQuant V.All [B.Lam _ binders _ body] = B.Forall tokForall binders tokDot body
delabQuant V.Any [B.Lam _ binders _ body] = B.Exists  tokExists  binders tokDot body
delabQuant V.All args                     = argsError (tkSymbol tokForall) 1 args
delabQuant V.Any args                     = argsError (tkSymbol tokExists)  1 args

delabQuantIn :: V.Quantifier -> [B.Expr] -> B.Expr
delabQuantIn V.All [B.Lam _ binders _ body, xs] = B.ForallIn tokForall binders xs tokDot body
delabQuantIn V.Any [B.Lam _ binders _ body, xs] = B.ExistsIn  tokExists  binders xs tokDot body
delabQuantIn V.All args                         = argsError (tkSymbol tokForall) 2 args
delabQuantIn V.Any args                         = argsError (tkSymbol tokExists)  2 args

argsError :: Symbol -> Int -> [B.Expr] -> a
argsError s n args = developerError $
  "Expecting" <+> pretty n <+> "arguments for" <+> squotes (pretty s) <+>
  "but found" <+> pretty (length args) <+> squotes (pretty (show args))

-- | Collapses pi expressions into either a function or a sequence of forall bindings
delabPi :: MonadDelab m => ann -> V.NamedBinder ann -> V.NamedExpr ann -> m B.Expr
delabPi ann input result = case foldPi ann input result of
  Left  (binders, body)    -> B.ForallT tokForallT <$> traverse delabM binders <*> pure tokDot <*> delabM body
  Right (domain, codomain) -> B.Fun <$> delabM domain <*> pure tokArrow <*> delabM codomain

-- | Collapses let expressions into a sequence of let declarations
delabLet :: MonadDelab m => V.NamedExpr ann -> m B.Expr
delabLet expr = let (boundExprs, body) = foldLet expr in
  B.Let <$> traverse delabLetBinding boundExprs <*> delabM body

-- | Collapses consecutative lambda expressions into a sequence of binders
delabLam :: MonadDelab m => V.NamedExpr ann -> m B.Expr
delabLam expr = let (binders, body) = foldLam expr in
  B.Lam tokLambda <$> traverse delabM binders <*> pure tokArrow <*> delabM body

delabFun :: MonadDelab m => V.Identifier -> V.NamedExpr ann -> V.NamedExpr ann -> m [B.Decl]
delabFun n typ expr = do
  let n' = delabIdentifier n
  case foldDefFun typ expr of
    Left  (t, (binders, body)) -> do
      defType <- B.DefFunType n' tokElemOf <$> delabM t
      defExpr <- B.DefFunExpr n' <$> traverse delabM binders <*> delabM body
      return [defType, defExpr]
    Right (binders, body)      -> do
      defType <- B.DefType n' <$> traverse delabM binders <*> delabM body
      return [defType]

removeNonExplicitArgs :: [V.NamedArg ann] -> [V.NamedArg ann]
removeNonExplicitArgs = filter (\arg -> visibilityOf arg == V.Explicit)