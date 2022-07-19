{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Vehicle.Compile.Delaborate.External
  ( Delaborate (delab, delabWithLogging),
  )
where

import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Text (pack)
import Vehicle.Compile.Prelude
import Vehicle.External.Abs qualified as B
import Vehicle.Language.AST qualified as V
import Vehicle.Language.Sugar

--------------------------------------------------------------------------------
-- Conversion to BNFC AST

-- | Constraint for the monad stack used by the elaborator.
type MonadDelab m = MonadLogger m

tokArrow = mkToken B.TokArrow "->"

tokForallT = mkToken B.TokForallT "forallT"

tokIf = mkToken B.TokIf "if"

tokThen = mkToken B.TokThen "then"

tokElse = mkToken B.TokElse "else"

tokLet = mkToken B.TokLet "let"

tokDot = mkToken B.TokDot "."

tokElemOf = mkToken B.TokElemOf ":"

tokLambda = mkToken B.TokLambda "\\"

tokTensor = mkToken B.TokTensor "Tensor"

tokList = mkToken B.TokList "List"

tokRat = mkToken B.TokRat "Rat"

tokInt = mkToken B.TokInt "Int"

tokNat = mkToken B.TokNat "Nat"

tokBool = mkToken B.TokBool "Bool"

tokIndex = mkToken B.TokIndex "Index"

tokForall = mkToken B.TokForall "forall"

tokExists = mkToken B.TokExists "exists"

tokForeach = mkToken B.TokForeach "foreach"

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

-- * Conversion

class Delaborate t bnfc | t -> bnfc, bnfc -> t where
  delabM :: MonadDelab m => t -> m bnfc

  -- | Delaborates the program and throws away the logs, should only be used in
  -- user-facing error messages
  delab :: t -> bnfc
  delab = discardLogger . delabM

  delabWithLogging :: MonadDelab m => t -> m bnfc
  delabWithLogging x = logCompilerPass MinDetail "delaboration" $ delabM x

instance Delaborate V.NamedProg B.Prog where
  delabM (V.Main decls) = B.Main . concat <$> traverse delabM decls

instance Delaborate V.NamedDecl [B.Decl] where
  delabM = \case
    -- Elaborate a network declaration.
    V.DefResource _ r n t -> do
      let n' = delabIdentifier n
      t' <- delabM t
      return $ case r of
        Network           -> [B.DeclNetw      n' tokElemOf t']
        Dataset           -> [B.DeclData      n' tokElemOf t']
        Parameter         -> [B.DeclParam     n' tokElemOf t']
        ImplicitParameter -> [B.DeclImplParam n' tokElemOf t']

    -- Elaborate a type definition.
    V.DefFunction _ _ n t e -> delabFun n t e
    V.DefPostulate {} ->
      developerError "Should not be delaborating postulates"

instance Delaborate V.NamedExpr B.Expr where
  delabM expr = case expr of
    V.Universe _ u -> return $ delabUniverse u
    V.Var _ n -> return $ B.Var (delabSymbol n)
    V.Hole _ n -> return $ B.Hole (mkToken B.HoleToken n)
    V.Literal _ l -> return $ delabLiteral l
    V.Ann _ e t -> B.Ann <$> delabM e <*> pure tokElemOf <*> delabM t
    V.LSeq _ es -> B.LSeq tokSeqOpen <$> traverse delabM es <*> pure tokSeqClose
    V.Pi ann t1 t2 -> delabPi ann t1 t2
    V.Let {} -> delabLet expr
    V.Lam {} -> delabLam expr
    V.Meta _ m -> return $ B.Var (mkToken B.Name (layoutAsText (pretty m)))
    V.App _ (V.Builtin _ b) args -> delabBuiltin b <$> traverse delabM (onlyExplicit args)
    V.App _ (V.Literal _ l) _args -> return $ delabLiteral l
    V.App _ fun args -> delabApp <$> delabM fun <*> traverse delabM (reverse (NonEmpty.toList args))
    V.Builtin _ op -> return $ delabBuiltin op []
    -- This is a hack to get printing of PrimDicts to work without explicitly including
    -- them in the grammar
    V.PrimDict _ t -> B.App (B.Var (mkToken B.Name "PrimDict")) . B.ExplicitArg <$> delabM t

instance Delaborate V.NamedArg B.Arg where
  delabM (V.Arg _i v e) = case v of
    V.Explicit -> B.ExplicitArg <$> delabM e
    V.Implicit -> B.ImplicitArg <$> delabM e
    V.Instance -> B.InstanceArg <$> delabM e

instance Delaborate V.NamedBinder B.Binder where
  delabM (V.Binder _ann v n _t) = case v of
    -- TODO track whether type was provided manually and so use ExplicitBinderAnn
    V.Explicit -> return $ B.ExplicitBinder $ delabSymbol n
    V.Implicit -> return $ B.ImplicitBinder $ delabSymbol n
    V.Instance -> return $ B.InstanceBinder $ delabSymbol n

delabLetBinding :: MonadDelab m => (V.NamedBinder, V.NamedExpr) -> m B.LetDecl
delabLetBinding (binder, bound) = B.LDecl <$> delabM binder <*> delabM bound

delabLiteral :: V.Literal -> B.Expr
delabLiteral l = case l of
  V.LBool b -> delabBoolLit b
  V.LNat n -> delabNatLit n
  V.LInt i ->
    if i >= 0
      then delabNatLit i
      else B.Neg tokSub (delabNatLit (-i))
  V.LRat r -> delabRatLit r

delabBoolLit :: Bool -> B.Expr
delabBoolLit True = B.Literal $ B.LitTrue tokTrue
delabBoolLit False = B.Literal $ B.LitFalse tokFalse

delabNatLit :: Int -> B.Expr
delabNatLit n = B.Literal $ B.LitNat (mkToken B.Natural (pack $ show n))

delabRatLit :: Rational -> B.Expr
delabRatLit r = B.Literal $ B.LitRat (mkToken B.Rational (pack $ show (fromRational r :: Double)))

delabSymbol :: Symbol -> B.Name
delabSymbol = mkToken B.Name

delabIdentifier :: V.Identifier -> B.Name
delabIdentifier (V.Identifier n) = mkToken B.Name n

delabApp :: B.Expr -> [B.Arg] -> B.Expr
delabApp fun [] = fun
delabApp fun (arg : args) = B.App (delabApp fun args) arg

delabUniverse :: V.Universe -> B.Expr
delabUniverse = \case
  TypeUniv l -> B.Type (mkToken B.TypeToken ("Type" <> pack (show l)))
  PolarityUniv -> auxiliaryTypeError (pretty PolarityUniv)
  LinearityUniv -> auxiliaryTypeError (pretty LinearityUniv)

delabBuiltin :: V.Builtin -> [B.Expr] -> B.Expr
delabBuiltin fun args = case fun of
  V.Bool   -> B.Bool tokBool
  V.Nat    -> B.Nat  tokNat
  V.Int    -> B.Int  tokInt
  V.Rat    -> B.Rat  tokRat
  V.List   -> delabOp1 B.List tokList args
  V.Tensor -> delabOp2 B.Tensor tokTensor args
  V.Index  -> delabOp1 B.Index tokIndex args
  V.Equality V.Eq -> delabInfixOp2 B.Eq tokEq args
  V.Equality V.Neq -> delabInfixOp2 B.Neq tokNeq args
  V.If -> delabIf args
  V.And -> delabInfixOp2 B.And tokAnd args
  V.Or -> delabInfixOp2 B.Or tokOr args
  V.Implies -> delabInfixOp2 B.Impl tokImpl args
  V.Not -> delabOp1 B.Not tokNot args
  V.Order V.Le -> delabInfixOp2 B.Le tokLe args
  V.Order V.Lt -> delabInfixOp2 B.Lt tokLt args
  V.Order V.Ge -> delabInfixOp2 B.Ge tokGe args
  V.Order V.Gt -> delabInfixOp2 B.Gt tokGt args
  V.Add -> delabInfixOp2 B.Add tokAdd args
  V.Sub -> delabInfixOp2 B.Sub tokSub args
  V.Mul -> delabInfixOp2 B.Mul tokMul args
  V.Div -> delabInfixOp2 B.Div tokDiv args
  V.Neg -> delabOp1 B.Neg tokSub args
  V.Cons -> delabInfixOp2 B.Cons tokCons args
  V.At -> delabInfixOp2 B.At tokAt args
  V.Map -> delabOp2 B.Map tokMap args
  V.Fold -> delabOp3 B.Fold tokFold args
  V.Quant q -> delabQuant q args
  V.QuantIn q -> delabQuantIn q args
  V.Foreach -> delabForeach args
  V.ForeachIn -> delabForeachIn args
  V.Polarity {} -> auxiliaryTypeError (pretty fun)
  V.Linearity {} -> auxiliaryTypeError (pretty fun)
  V.TypeClass tc -> B.Var (delabSymbol (layoutAsText $ pretty tc))

delabOp1 :: IsToken token => (token -> B.Expr -> B.Expr) -> token -> [B.Expr] -> B.Expr
delabOp1 op tk [arg] = op tk arg
delabOp1 _ tk args = argsError (tkSymbol tk) 1 args

delabOp2 :: IsToken token => (token -> B.Expr -> B.Expr -> B.Expr) -> token -> [B.Expr] -> B.Expr
delabOp2 op tk [arg1, arg2] = op tk arg1 arg2
delabOp2 _ tk args = argsError (tkSymbol tk) 2 args

delabOp3 :: IsToken token => (token -> B.Expr -> B.Expr -> B.Expr -> B.Expr) -> token -> [B.Expr] -> B.Expr
delabOp3 op tk [arg1, arg2, arg3] = op tk arg1 arg2 arg3
delabOp3 _ tk args = argsError (tkSymbol tk) 3 args

delabInfixOp2 :: IsToken token => (B.Expr -> token -> B.Expr -> B.Expr) -> token -> [B.Expr] -> B.Expr
delabInfixOp2 op tk [arg1, arg2] = op arg1 tk arg2
delabInfixOp2 _ tk args = argsError (tkSymbol tk) 2 args

delabIf :: [B.Expr] -> B.Expr
delabIf [arg1, arg2, arg3] = B.If tokIf arg1 tokThen arg2 tokElse arg3
delabIf args = argsError "if" 3 args

delabQuant :: V.Quantifier -> [B.Expr] -> B.Expr
delabQuant V.Forall [B.Lam _ binders _ body] = B.Forall tokForall binders tokDot body
delabQuant V.Exists [B.Lam _ binders _ body] = B.Exists tokExists binders tokDot body
delabQuant V.Forall args = argsError (tkSymbol tokForall) 1 args
delabQuant V.Exists args = argsError (tkSymbol tokExists) 1 args

delabQuantIn :: V.Quantifier -> [B.Expr] -> B.Expr
delabQuantIn V.Forall [B.Lam _ binders _ body, xs] = B.ForallIn tokForall binders xs tokDot body
delabQuantIn V.Exists [B.Lam _ binders _ body, xs] = B.ExistsIn tokExists binders xs tokDot body
delabQuantIn V.Forall args = argsError (tkSymbol tokForall) 2 args
delabQuantIn V.Exists args = argsError (tkSymbol tokExists) 2 args

delabForeach :: [B.Expr] -> B.Expr
delabForeach [B.Lam _ binders _ body] = B.Foreach tokForeach binders tokDot body
delabForeach args = argsError (tkSymbol tokForall) 1 args

delabForeachIn :: [B.Expr] -> B.Expr
delabForeachIn [B.Lam _ binders _ body, xs] = B.ForeachIn tokForeach binders xs tokDot body
delabForeachIn args = argsError (tkSymbol tokForall) 2 args

argsError :: Symbol -> Int -> [B.Expr] -> a
argsError s n args =
  developerError $
    "Expecting" <+> pretty n <+> "arguments for" <+> squotes (pretty s)
      <+> "but found"
      <+> pretty (length args)
      <+> squotes (pretty (show args))

-- | Collapses pi expressions into either a function or a sequence of forall bindings
delabPi :: MonadDelab m => Provenance -> V.NamedBinder -> V.NamedExpr -> m B.Expr
delabPi ann input result = case foldPi ann input result of
  Left (binders, body) -> B.ForallT tokForallT <$> traverse delabM binders <*> pure tokDot <*> delabM body
  Right (domain, codomain) -> B.Fun <$> delabM domain <*> pure tokArrow <*> delabM codomain

-- | Collapses let expressions into a sequence of let declarations
delabLet :: MonadDelab m => V.NamedExpr -> m B.Expr
delabLet expr =
  let (boundExprs, body) = foldLet expr
   in B.Let tokLet <$> traverse delabLetBinding boundExprs <*> delabM body

-- | Collapses consecutative lambda expressions into a sequence of binders
delabLam :: MonadDelab m => V.NamedExpr -> m B.Expr
delabLam expr =
  let (binders, body) = foldLam expr
   in B.Lam tokLambda <$> traverse delabM binders <*> pure tokArrow <*> delabM body

delabFun :: MonadDelab m => V.Identifier -> V.NamedExpr -> V.NamedExpr -> m [B.Decl]
delabFun n typ expr = do
  let n' = delabIdentifier n
  case foldDefFun typ expr of
    Left (t, (binders, body)) -> do
      defType <- B.DefFunType n' tokElemOf <$> delabM t
      defExpr <- B.DefFunExpr n' <$> traverse delabM binders <*> delabM body
      return [defType, defExpr]
    Right (binders, body) -> do
      defType <- B.DefType n' <$> traverse delabM binders <*> delabM body
      return [defType]

auxiliaryTypeError :: Doc a -> a
auxiliaryTypeError e =
  developerError $
    "Encountered" <+> squotes e <> ". Should not be delaborating auxiliary-type system code."