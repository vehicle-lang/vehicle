{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Vehicle.Language.Delaborate.Frontend
  ( Delaborate
  , runDelab
  , runDelabWithoutLogging
  ) where

import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Text (pack)

import Vehicle.Frontend.Abs qualified as B

import Vehicle.Prelude
import Vehicle.Language.AST qualified as V
import Vehicle.Language.Sugar

runDelab :: Delaborate a b => a -> Logger b
runDelab x = do
  -- TODO filter out free variables from the expression in the supply monad
  logDebug "Beginning delaboration"
  let freshNames = [ "_x" <> pack (show i) | i <- [0::Int ..]]
  result <- runSupplyT (delab x) freshNames
  logDebug "Ending delaboration\n"
  return result

-- | Delaborates the program and throws away the logs, should only be used in
-- user-facing error messages
runDelabWithoutLogging :: Delaborate a b => a -> b
runDelabWithoutLogging x = discardLogger $ runDelab x

--------------------------------------------------------------------------------
-- Conversion to BNFC AST

-- | Constraint for the monad stack used by the elaborator.
type MonadDelab m = (MonadLogger m, MonadSupply Symbol m)

tokArrow = mkToken B.TokArrow "->"
tokForall = mkToken B.TokForall "forall"
tokIf = mkToken B.TokIf "if"
tokThen = mkToken B.TokThen "then"
tokElse = mkToken B.TokElse "else"
tokDot = mkToken B.TokDot "."
tokElemOf = mkToken B.TokElemOf ":"
tokLambda = mkToken B.TokLambda "\\"
tokTensor = mkToken B.TokTensor "Tensor"
tokList = mkToken B.TokList "List"
tokReal = mkToken B.TokReal "Real"
tokInt = mkToken B.TokInt "Int"
tokNat = mkToken B.TokNat "Nat"
tokBool = mkToken B.TokBool "Bool"
tokProp = mkToken B.TokProp "Prop"
tokEvery = mkToken B.TokEvery "every"
tokSome = mkToken B.TokSome "some"
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
tokTCNatural = mkToken B.TokTCNatural "IsNatural"
tokTCIntegral = mkToken B.TokTCIntegral "IsIntegral"
tokTCRational = mkToken B.TokTCRational "IsRational"
tokTCReal = mkToken B.TokTCReal "IsReal"

-- * Conversion

class Delaborate vf vc where
  delab :: MonadDelab m => vf -> m vc

-- |Elaborate programs.
instance Delaborate (V.Prog V.Name ann) B.Prog where
  delab (V.Main decls) = B.Main . concat <$> traverse delab decls

-- |Elaborate declarations.
instance Delaborate (V.Decl V.Name ann) [B.Decl] where
  delab = \case
    -- Elaborate a network declaration.
    (V.DeclNetw _ n t) -> do
      n' <- delab n
      t' <- delab t
      return [B.DeclNetw n' tokElemOf t']

    -- Elaborate a dataset declaration.
    (V.DeclData _ n t) -> do
      n' <- delab n
      t' <- delab t
      return [B.DeclData n' tokElemOf t']

    -- Elaborate a type definition.
    (V.DefFun _ n t e) -> delabFun n t e

instance Delaborate (V.Expr V.Name ann) B.Expr where
  delab expr = case expr of
    V.Type l        -> return $ B.Type (fromIntegral l)
    V.Var _ n       -> B.Var  <$> delab n
    V.Hole _ n      -> return $ B.Hole (mkToken B.HoleToken n)
    V.Literal _ l   -> B.Literal <$> delab l

    V.Ann _ e t     -> B.Ann <$> delab e <*> pure tokElemOf <*> delab t
    V.Pi  ann t1 t2 -> delabPi ann t1 t2
    V.Seq _ es      -> B.Seq tokSeqOpen <$> traverse delab es <*> pure tokSeqClose

    V.Let{}         -> delabLet expr
    V.Lam{}         -> delabLam expr
    V.Meta _ m      -> return $ B.Var (mkToken B.Name (layoutAsText (pretty m)))
    V.PrimDict _    -> developerError "Instance arguments not currently in grammar"

    V.App _ (V.Builtin _ b) args -> delabBuiltin b <$> traverse (delab . V.argExpr) (NonEmpty.toList args)
    V.App _ fun args             -> delabApp <$> delab fun <*> traverse delab (reverse (NonEmpty.toList args))
    V.Builtin _ op               -> return $ delabBuiltin op []

instance Delaborate (V.Arg V.Name ann) B.Arg where
  delab (V.Arg _i v e) = case v of
    V.Explicit -> B.ExplicitArg <$> delab e
    V.Implicit -> B.ImplicitArg <$> delab e
    V.Instance -> B.InstanceArg <$> delab e

instance Delaborate V.Name B.Name where
  delab (V.User s) = return $ mkToken B.Name s
  delab V.Machine  = mkToken B.Name <$> demand

instance Delaborate V.Identifier B.Name where
  delab (V.Identifier n) = return $ mkToken B.Name n

instance Delaborate (V.Binder V.Name ann, V.Expr V.Name ann) B.LetDecl where
  delab (binder, bound) = B.LDecl <$> delab binder <*> delab bound

instance Delaborate (V.Binder V.Name name) B.Binder where
  delab (V.Binder _i _p v n _t) = case v of
    -- TODO track whether type was provided manually and so use ExplicitBinderAnn
    V.Explicit -> B.ExplicitBinder <$> delab n
    V.Implicit -> B.ImplicitBinder <$> delab n
    V.Instance -> developerError "User specified instance arguments not yet supported"

instance Delaborate V.Literal B.Lit where
  delab l = return $ case l of
    V.LBool True  -> B.LitTrue  tokTrue
    V.LBool False -> B.LitFalse tokFalse
    V.LNat n      -> B.LitInt   (fromIntegral n)
    V.LInt i      -> B.LitInt   (fromIntegral i)
    V.LRat r      -> B.LitReal  r

delabApp :: B.Expr -> [B.Arg] -> B.Expr
delabApp fun []           = fun
delabApp fun (arg : args) = B.App (delabApp fun args) arg

delabBuiltin :: V.Builtin -> [B.Expr] -> B.Expr
delabBuiltin fun args = case fun of
  V.Bool   -> B.Bool tokBool
  V.Prop   -> B.Prop tokProp
  V.Nat    -> B.Nat  tokNat
  V.Int    -> B.Int  tokInt
  V.Real   -> B.Real tokReal
  V.List   -> delabOp1 B.List   tokList   args
  V.Tensor -> delabOp2 B.Tensor tokTensor args

  V.TypeClass V.IsTruth        -> delabOp1 (\tk e -> B.TypeC (B.TCTruth tk e)) tokTCTruth    args
  V.TypeClass V.IsNatural      -> delabOp1 (\tk e -> B.TypeC (B.TCNat   tk e)) tokTCNatural  args
  V.TypeClass V.IsIntegral     -> delabOp1 (\tk e -> B.TypeC (B.TCInt   tk e)) tokTCIntegral args
  V.TypeClass V.IsRational     -> delabOp1 (\tk e -> B.TypeC (B.TCRat   tk e)) tokTCRational args
  V.TypeClass V.IsReal         -> delabOp1 (\tk e -> B.TypeC (B.TCReal  tk e)) tokTCReal     args
  V.TypeClass V.IsQuantifiable -> delabOp1 (\tk e -> B.TypeC (B.TCQuant tk e)) tokTCQuantify args
  V.TypeClass V.HasEq          -> delabOp2 (\tk e1 e2 -> B.TypeC (B.TCEq   tk e1 e2)) tokTCEq        args
  V.TypeClass V.HasOrd         -> delabOp2 (\tk e1 e2 -> B.TypeC (B.TCOrd  tk e1 e2)) tokTCOrd       args
  V.TypeClass V.IsContainer    -> delabOp2 (\tk e1 e2 -> B.TypeC (B.TCCont tk e1 e2)) tokTCContainer args

  V.Eq  -> delabInfixOp2 B.Eq  tokEq args
  V.Neq -> delabInfixOp2 B.Neq tokNeq args

  V.If   -> delabIf args
  V.Not  -> delabOp1 B.Not tokNot args
  V.And  -> delabInfixOp2 B.And  tokAnd  args
  V.Or   -> delabInfixOp2 B.Or   tokOr   args
  V.Impl -> delabInfixOp2 B.Impl tokImpl args

  V.Order V.Le -> delabInfixOp2 B.Le tokLe args
  V.Order V.Lt -> delabInfixOp2 B.Lt tokLt args
  V.Order V.Ge -> delabInfixOp2 B.Ge tokGe args
  V.Order V.Gt -> delabInfixOp2 B.Gt tokGt args

  V.Neg -> delabOp1      B.Neg tokSub args
  V.Add -> delabInfixOp2 B.Add tokAdd args
  V.Sub -> delabInfixOp2 B.Sub tokSub args
  V.Mul -> delabInfixOp2 B.Mul tokMul args
  V.Div -> delabInfixOp2 B.Div tokDiv args

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
delabQuant V.All [B.Lam _ binders _ body] = B.Every tokEvery binders tokDot body
delabQuant V.Any [B.Lam _ binders _ body] = B.Some  tokSome  binders tokDot body
delabQuant V.All args                     = argsError (tkSymbol tokEvery) 1 args
delabQuant V.Any args                     = argsError (tkSymbol tokSome)  1 args

delabQuantIn :: V.Quantifier -> [B.Expr] -> B.Expr
delabQuantIn V.All [B.Lam _ binders _ body, xs] = B.EveryIn tokEvery binders xs tokDot body
delabQuantIn V.Any [B.Lam _ binders _ body, xs] = B.SomeIn  tokSome  binders xs tokDot body
delabQuantIn V.All args                         = argsError (tkSymbol tokEvery) 2 args
delabQuantIn V.Any args                         = argsError (tkSymbol tokSome)  2 args

argsError :: Symbol -> Int -> [B.Expr] -> a
argsError s n args = developerError $
  "Expecting" <+> pretty n <+> "arguments for" <+> squotes (pretty s) <+>
  "but found" <+> pretty (length args) <+> squotes (pretty (show args))

-- | Collapses pi expressions into either a function or a sequence of forall bindings
delabPi :: MonadDelab m => ann -> V.Binder V.Name ann -> V.Expr V.Name ann -> m B.Expr
delabPi ann input result = case foldPi ann input result of
  Left  (binders, body)    -> B.Forall tokForall <$> traverse delab binders <*> pure tokDot <*> delab body
  Right (domain, codomain) -> B.Fun <$> delab domain <*> pure tokArrow <*> delab codomain

-- | Collapses let expressions into a sequence of let declarations
delabLet :: MonadDelab m => V.Expr V.Name ann -> m B.Expr
delabLet expr = let (boundExprs, body) = foldLet expr in
  B.Let <$> traverse delab boundExprs <*> delab body

-- | Collapses consecutative lambda expressions into a sequence of binders
delabLam :: MonadDelab m => V.Expr V.Name ann -> m B.Expr
delabLam expr = let (binders, body) = foldLam expr in
  B.Lam tokLambda <$> traverse delab binders <*> pure tokArrow <*> delab body

delabFun :: MonadDelab m => V.Identifier -> V.Expr V.Name ann -> V.Expr V.Name ann -> m [B.Decl]
delabFun n typ expr = do
  n' <- delab n
  case foldDefFun typ expr of
    Left  (t, (binders, body)) -> do
      defType <- B.DefFunType n' tokElemOf <$> delab t
      defExpr <- B.DefFunExpr n' <$> traverse delab binders <*> delab body
      return [defType, defExpr]
    Right (binders, body)      -> do
      defType <- B.DefType n' <$> traverse delab binders <*> delab body
      return [defType]