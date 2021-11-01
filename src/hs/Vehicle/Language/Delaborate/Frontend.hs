{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Vehicle.Language.Delaborate.Frontend
  ( Delaborate
  , runDelab
  , runDelabWithoutLogging
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Text (pack)

import Vehicle.Frontend.Abs qualified as B

import Vehicle.Prelude
import Vehicle.Language.AST qualified as V

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

-- * Provenance

type TokenConstructor a = ((Int, Int), Symbol) -> a

-- | A slightly shorter name for `tkProvenance`
mkToken :: TokenConstructor a -> Symbol -> a
mkToken mk s = mk ((0,0), s)

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
    (V.DefFun _ n t e) -> foldFun n t e

instance Delaborate (V.Expr V.Name ann) B.Expr where
  delab expr = case expr of
    V.Type l       -> return $ B.Type (fromIntegral l)
    V.Var _ n      -> B.Var  <$> delab n
    V.Hole _ n     -> return $ B.Hole (mkToken B.HoleToken n)
    V.Literal _ l  -> B.Literal <$> delab l

    V.Ann _ e t     -> B.Ann <$> delab e <*> pure tokElemOf <*> delab t
    V.Pi  ann t1 t2 -> foldPi ann t1 t2
    V.Seq _ es      -> B.Seq tokSeqOpen <$> traverse delab es <*> pure tokSeqClose

    V.Let{}        -> foldLet expr
    V.Lam{}        -> foldLam expr
    V.Meta _ m     -> return $ B.Var (mkToken B.Name (layoutAsText (pretty m)))
    V.PrimDict _   -> developerError "Instance arguments not currently in grammar"

    V.App _ (V.Builtin _ b) args -> convBuiltin b <$> traverse (delab . V.argExpr) (filterUserArgs args)
    V.App _ fun args             -> convApp <$> delab fun <*> traverse delab (reverse (filterUserArgs args))
    V.Builtin _ op               -> return $ convBuiltin op []

instance Delaborate (V.Arg V.Name ann) B.Arg where
  delab (V.Arg Explicit e) = B.ExplicitArg <$> delab e
  delab (V.Arg Implicit e) = B.ImplicitArg <$> delab e
  delab (V.Arg Instance _) = developerError "User specified type classes not yet supported"

instance Delaborate V.Name B.Name where
  delab (V.User s) = return $ mkToken B.Name s
  delab V.Machine  = mkToken B.Name <$> demand

instance Delaborate V.Identifier B.Name where
  delab (V.Identifier n) = return $ mkToken B.Name n

instance Delaborate (V.Binder V.Name name) B.Binder where
  delab (V.Binder _p v n _t) = case v of
    -- TODO track whether type was provided manually and so use ExplicitBinderAnn
    Explicit -> B.ExplicitBinder <$> delab n
    Implicit -> B.ImplicitBinder <$> delab n
    Instance -> developerError "User specified instance arguments not yet supported"

instance Delaborate V.Literal B.Lit where
  delab l = return $ case l of
    V.LBool True  -> B.LitTrue  tokTrue
    V.LBool False -> B.LitFalse tokFalse
    V.LNat n      -> B.LitInt   (fromIntegral n)
    V.LInt i      -> B.LitInt   (fromIntegral i)
    V.LRat r      -> B.LitReal  r

filterUserArgs :: NonEmpty (V.Arg var ann) -> [V.Arg var ann]
filterUserArgs args = filter isUserProvidedArg (NonEmpty.toList args)
  where
    isUserProvidedArg :: V.Arg var ann -> Bool
    isUserProvidedArg arg = vis arg == Explicit

convApp :: B.Expr -> [B.Arg] -> B.Expr
convApp fun []           = fun
convApp fun (arg : args) = B.App (convApp fun args) arg

convBuiltin :: V.Builtin -> [B.Expr] -> B.Expr
convBuiltin fun args = case fun of
  V.Bool   -> B.Bool tokBool
  V.Prop   -> B.Prop tokProp
  V.Nat    -> B.Nat  tokNat
  V.Int    -> B.Int  tokInt
  V.Real   -> B.Real tokReal
  V.List   -> convOp1 B.List   tokList   args
  V.Tensor -> convOp2 B.Tensor tokTensor args

  V.IsTruth        -> convOp1 (\tk e -> B.TypeC (B.TCTruth tk e)) tokTCTruth    args
  V.IsNatural      -> convOp1 (\tk e -> B.TypeC (B.TCNat   tk e)) tokTCNatural  args
  V.IsIntegral     -> convOp1 (\tk e -> B.TypeC (B.TCInt   tk e)) tokTCIntegral args
  V.IsRational     -> convOp1 (\tk e -> B.TypeC (B.TCRat   tk e)) tokTCRational args
  V.IsReal         -> convOp1 (\tk e -> B.TypeC (B.TCReal  tk e)) tokTCReal     args
  V.IsQuantifiable -> convOp1 (\tk e -> B.TypeC (B.TCQuant tk e)) tokTCQuantify args
  V.HasEq          -> convOp2 (\tk e1 e2 -> B.TypeC (B.TCEq   tk e1 e2)) tokTCEq        args
  V.HasOrd         -> convOp2 (\tk e1 e2 -> B.TypeC (B.TCOrd  tk e1 e2)) tokTCOrd       args
  V.IsContainer    -> convOp2 (\tk e1 e2 -> B.TypeC (B.TCCont tk e1 e2)) tokTCContainer args

  V.Eq  -> convInfixOp2 B.Eq  tokEq args
  V.Neq -> convInfixOp2 B.Neq tokNeq args

  V.If   -> convIf args
  V.Not  -> convOp1 B.Not tokNot args
  V.And  -> convInfixOp2 B.And  tokAnd  args
  V.Or   -> convInfixOp2 B.Or   tokOr   args
  V.Impl -> convInfixOp2 B.Impl tokImpl args

  (V.Order V.Le) -> convInfixOp2 B.Le tokLe args
  (V.Order V.Lt) -> convInfixOp2 B.Lt tokLt args
  (V.Order V.Ge) -> convInfixOp2 B.Ge tokGe args
  (V.Order V.Gt) -> convInfixOp2 B.Gt tokGt args

  V.Neg -> convOp1      B.Neg tokSub args
  V.Add -> convInfixOp2 B.Add tokAdd args
  V.Sub -> convInfixOp2 B.Sub tokSub args
  V.Mul -> convInfixOp2 B.Mul tokMul args
  V.Div -> convInfixOp2 B.Div tokDiv args

  V.Cons -> convInfixOp2 B.Cons tokCons args
  V.At   -> convInfixOp2 B.At   tokAt   args
  V.Map  -> convOp2      B.Map  tokMap  args
  V.Fold -> convOp3      B.Fold tokFold args

  V.Quant   q -> convQuant   q args
  V.QuantIn q -> convQuantIn q args

convOp1 :: IsToken token => (token -> B.Expr -> B.Expr) -> token -> [B.Expr] -> B.Expr
convOp1 op tk [arg] = op tk arg
convOp1 _  tk args  = argsError (tkSymbol tk) 1 args

convOp2 :: IsToken token => (token -> B.Expr -> B.Expr -> B.Expr) -> token -> [B.Expr] -> B.Expr
convOp2 op tk [arg1, arg2] = op tk arg1 arg2
convOp2 _  tk args         = argsError (tkSymbol tk) 2 args

convOp3 :: IsToken token => (token -> B.Expr -> B.Expr -> B.Expr -> B.Expr) -> token -> [B.Expr] -> B.Expr
convOp3 op tk [arg1, arg2, arg3] = op tk arg1 arg2 arg3
convOp3 _  tk args               = argsError (tkSymbol tk) 3 args

convInfixOp2 :: IsToken token => (B.Expr -> token -> B.Expr -> B.Expr) -> token -> [B.Expr] -> B.Expr
convInfixOp2 op tk [arg1, arg2] = op arg1 tk arg2
convInfixOp2 _  tk args         = argsError (tkSymbol tk) 2 args

convIf :: [B.Expr] -> B.Expr
convIf [arg1, arg2, arg3] = B.If tokIf arg1 tokThen arg2 tokElse arg3
convIf args               = argsError "if" 3 args

convQuant :: V.Quantifier -> [B.Expr] -> B.Expr
convQuant V.All [B.Lam _ binders _ body] = B.Every tokEvery binders tokDot body
convQuant V.Any [B.Lam _ binders _ body] = B.Some  tokSome  binders tokDot body
convQuant V.All args                     = argsError (tkSymbol tokEvery) 1 args
convQuant V.Any args                     = argsError (tkSymbol tokSome)  1 args

convQuantIn :: V.Quantifier -> [B.Expr] -> B.Expr
convQuantIn V.All [B.Lam _ binders _ body, xs] = B.EveryIn tokEvery binders xs tokDot body
convQuantIn V.Any [B.Lam _ binders _ body, xs] = B.SomeIn  tokSome  binders xs tokDot body
convQuantIn V.All args                         = argsError (tkSymbol tokEvery) 2 args
convQuantIn V.Any args                         = argsError (tkSymbol tokSome)  2 args

argsError :: Symbol -> Int -> [B.Expr] -> a
argsError s n args = developerError $
  "Expecting" <+> pretty n <+> "arguments for" <+> squotes (pretty s) <+>
  "but found" <+> pretty (length args) <+> squotes (pretty (show args))

-- | Collapses pi expressions into either a function or a sequence of forall bindings
foldPi :: MonadDelab m => ann -> V.Binder V.Name ann -> V.Expr V.Name ann -> m B.Expr
foldPi ann input result = if isFunBinder input
  then B.Fun <$> delab (V.binderType input) <*> pure tokArrow <*> delab result
  else do
    (ns' , body') <- decomposeForall ([], V.Pi ann input result)
    return $ B.Forall tokForall (reverse ns') tokDot body'
    where
      decomposeForall :: MonadDelab m => ([B.Binder], V.Expr V.Name ann) -> m ([B.Binder], B.Expr)
      decomposeForall (args, V.Pi _ binder body)
        | not (isFunBinder binder) = do binder' <- delab binder; decomposeForall (binder' : args, body)
      decomposeForall (args, body) = do body'   <- delab body; return (args , body')

-- | Collapses let expressions into a sequence of let declarations
foldLet :: MonadDelab m => V.Expr V.Name ann -> m B.Expr
foldLet expr = do
  (ds' , body') <- decomposeLet ([], expr)
  return $ B.Let (reverse ds') body'
  where
    decomposeLet :: MonadDelab m => ([B.LetDecl], V.Expr V.Name ann) -> m ([B.LetDecl], B.Expr)
    decomposeLet (args, V.Let _ bound binder body) = do decl  <- delabLetDecl binder bound; decomposeLet (decl : args, body)
    decomposeLet (args, body)                      = do body' <- delab body; return (args , body')

    delabLetDecl :: MonadDelab m => V.Binder V.Name ann -> V.Expr V.Name ann -> m B.LetDecl
    delabLetDecl binder bound = B.LDecl <$> delab binder <*> delab bound

-- | Collapses consecutative lambda expressions into a sequence of binders
foldLam :: MonadDelab m => V.Expr V.Name ann -> m B.Expr
foldLam expr = do
  (ns' , body') <- decomposeLam ([], expr)
  return $ B.Lam tokLambda (reverse ns') tokArrow body'
  where
    decomposeLam :: MonadDelab m => ([B.Binder], V.Expr V.Name ann) -> m ([B.Binder], B.Expr)
    decomposeLam (binders, V.Lam _ binder body) = do binder' <- delab binder; decomposeLam (binder' : binders, body)
    decomposeLam (binders, body)                = do body' <- delab body; return (binders , body')

-- TODO this should reconstruct whether it's a type synonym or not
foldFun :: MonadDelab m => V.Identifier -> V.Expr V.Name ann -> V.Expr V.Name ann -> m [B.Decl]
foldFun n t e = do
  n' <- delab n
  t' <- delab t
  (binders, body) <- decomposeFun ([], e)
  return [B.DefFunType n' tokElemOf t', B.DefFunExpr n' binders body]
  where
    decomposeFun :: MonadDelab m => ([B.Binder], V.Expr V.Name ann) -> m ([B.Binder], B.Expr)
    decomposeFun (binders, V.Lam _ binder body) = do binder' <- delab binder; decomposeFun (binder' : binders, body)
    decomposeFun (binders, body)                = do body'   <- delab body; return (binders , body')


-- | Tests if a binder is from a forall or a function.
isFunBinder :: V.Binder var ann -> Bool
isFunBinder binder = vis binder == Explicit