{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Vehicle.Syntax.BNFC.Delaborate.External
  ( Delaborate
  , delab
  ) where

import Control.Monad.Identity (Identity (runIdentity), IdentityT)
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Text (Text, pack)
import Prettyprinter (Doc, Pretty (..), squote, squotes, (<+>))

import Data.List.NonEmpty (NonEmpty)
import Vehicle.Syntax.AST qualified as V
import Vehicle.Syntax.AST.Arg
import Vehicle.Syntax.BNFC.Utils
import Vehicle.Syntax.External.Abs qualified as B
import Vehicle.Syntax.Parse.Error
import Vehicle.Syntax.Parse.Token
import Vehicle.Syntax.Prelude
import Vehicle.Syntax.Sugar

--------------------------------------------------------------------------------
-- Conversion to BNFC AST

delab :: (Show t, Delaborate t bnfc) => t -> bnfc
delab e = runIdentity (delabM e)

--------------------------------------------------------------------------------
-- Implementation

-- | Constraint for the monad stack used by the elaborator.
type MonadDelab m = Monad m

-- * Conversion

class Delaborate t bnfc | t -> bnfc, bnfc -> t where
  delabM :: MonadDelab m => t -> m bnfc

instance Delaborate V.NamedProg B.Prog where
  delabM (V.Main decls) = B.Main . concat <$> traverse delabM decls

instance Delaborate V.NamedDecl [B.Decl] where
  delabM = \case
    V.DefResource _ r n t -> do
      defFun <- B.DefFunType (delabIdentifier n) tokElemOf <$> delabM t

      let defAnn = case r of
            V.Network            -> delabAnn networkAnn []
            V.Dataset            -> delabAnn datasetAnn []
            V.Parameter          -> delabAnn parameterAnn []
            V.InferableParameter -> delabAnn parameterAnn [mkDeclAnnOption V.InferableOption True]

      return [defAnn, defFun]

    V.DefFunction _ n t e ->
      delabFun False n t e

    V.DefPostulate {} ->
      error "Should not be delaborating postulates"

instance Delaborate V.NamedExpr B.Expr where
  delabM expr = case expr of
    V.Universe _ u       -> return $ delabUniverse u
    V.Var      _ n       -> return $ B.Var (delabSymbol n)
    V.Hole     _ n       -> return $ B.Hole (mkToken B.HoleToken n)
    V.Literal  _ l       -> return $ delabLiteral l
    V.Ann      _ e t     -> B.Ann <$> delabM e <*> pure tokElemOf <*> delabM t
    V.LVec     _ es      -> B.VecLiteral tokSeqOpen <$> traverse delabM es <*> pure tokSeqClose
    V.Pi       ann t1 t2 -> delabPi ann t1 t2
    V.Let{}              -> delabLet expr
    V.Lam{}              -> delabLam expr
    V.Meta _ m           -> return $ B.Var (mkToken B.Name (layoutAsText (pretty m)))

    V.App _ (V.Builtin _ b) args  -> delabBuiltin b <$> traverse delabM (onlyExplicit args)
    V.App _ (V.Literal _ l) _args -> return $ delabLiteral l
    V.App _ fun args              -> delabApp <$> delabM fun <*> traverse delabM (NonEmpty.toList args)
    V.Builtin _ op                -> return $ delabBuiltin op []

instance Delaborate V.NamedArg B.Arg where
  delabM (V.Arg _ v _ e) = case v of
    V.Explicit{} -> B.ExplicitArg <$> delabM e
    V.Implicit{} -> B.ImplicitArg <$> delabM e
    V.Instance{} -> B.InstanceArg <$> delabM e

instance Delaborate V.NamedBinder B.Binder where
  delabM (V.Binder _ v _ n _t) = case v of
    V.Explicit -> return $ B.ExplicitBinder $ delabSymbol n
    V.Implicit -> return $ B.ImplicitBinder $ delabSymbol n
    V.Instance -> return $ B.InstanceBinder $ delabSymbol n

delabLetBinding :: MonadDelab m => (V.NamedBinder, V.NamedExpr) -> m B.LetDecl
delabLetBinding (binder, bound) = B.LDecl <$> delabM binder <*> delabM bound

delabLiteral :: V.Literal -> B.Expr
delabLiteral l = case l of
  V.LUnit      -> B.Literal B.UnitLiteral
  V.LBool b    -> B.Literal $ B.BoolLiteral $ delabBoolLit b
  V.LIndex _ x -> B.Literal $ B.NatLiteral  $ delabNatLit x
  V.LNat n     -> B.Literal $ B.NatLiteral  $ delabNatLit n
  V.LInt i     -> if i >= 0
    then B.Literal $ B.NatLiteral $ delabNatLit i
    else B.Neg tokSub (B.Literal $ B.NatLiteral $ delabNatLit (-i))
  V.LRat r  -> B.Literal $ B.RatLiteral $ delabRatLit r

delabBoolLit :: Bool -> B.Boolean
delabBoolLit b = mkToken B.Boolean (pack $ show b)

delabNatLit :: Int -> B.Natural
delabNatLit n = mkToken B.Natural (pack $ show n)

delabRatLit :: Rational -> B.Rational
delabRatLit r = mkToken B.Rational (pack $ show (fromRational r :: Double))

delabSymbol :: Text -> B.Name
delabSymbol = mkToken B.Name

delabIdentifier :: V.Identifier -> B.Name
delabIdentifier (V.Identifier _ n) = mkToken B.Name n

delabApp :: B.Expr -> [B.Arg] -> B.Expr
delabApp fun allArgs = go fun (reverse allArgs)
  where
    go fn []           = fn
    go fn (arg : args) = B.App (go fn args) arg

delabUniverse :: V.Universe -> B.Expr
delabUniverse = \case
  V.TypeUniv l    -> tokType l
  V.PolarityUniv  -> auxiliaryTypeError (pretty V.PolarityUniv)
  V.LinearityUniv -> auxiliaryTypeError (pretty V.LinearityUniv)

delabBuiltin :: V.Builtin -> [B.Expr] -> B.Expr
delabBuiltin fun args = case fun of
  V.Constructor c  -> delabConstructor c args

  V.Tensor         -> delabApp (B.Tensor tokTensor) (B.ExplicitArg <$> args)

  V.And            -> delabTypeClassOp V.AndTC args
  V.Or             -> delabTypeClassOp V.OrTC args
  V.Implies        -> delabTypeClassOp V.ImpliesTC args
  V.Not            -> delabTypeClassOp V.NotTC args
  V.If             -> delabIf args

  V.FromNat{}      -> head args
  V.FromRat{}      -> head args
  V.FromVec{}      -> head args

  V.Neg _          -> delabTypeClassOp V.NegTC args
  V.Add _          -> delabTypeClassOp V.AddTC args
  V.Sub _          -> delabTypeClassOp V.SubTC args
  V.Mul _          -> delabTypeClassOp V.MulTC args
  V.Div _          -> delabTypeClassOp V.DivTC args

  V.Equals _ op    -> delabTypeClassOp (V.EqualsTC op) args
  V.Order _ op     -> delabTypeClassOp (V.OrderTC op) args

  V.Fold _         -> primOpError fun
  V.Map  _         -> primOpError fun
  V.At             -> delabInfixOp2 B.At tokAt args
  V.Foreach        -> delabForeach args

  V.TypeClassOp tc -> delabTypeClassOp tc args

delabConstructor :: V.BuiltinConstructor -> [B.Expr] -> B.Expr
delabConstructor fun args = case fun of
  V.Unit           -> B.Unit tokUnit
  V.Bool           -> B.Bool tokBool
  V.Nat            -> B.Nat  tokNat
  V.Int            -> B.Int  tokInt
  V.Rat            -> B.Rat  tokRat
  V.List           -> delabApp (B.List   tokList)   (B.ExplicitArg <$> args)
  V.Vector         -> delabApp (B.Vector tokVector) (B.ExplicitArg <$> args)
  V.Index          -> delabApp (B.Index  tokIndex)  (B.ExplicitArg <$> args)

  V.Polarity{}     -> auxiliaryTypeError (pretty fun)
  V.Linearity{}    -> auxiliaryTypeError (pretty fun)

  V.TypeClass tc   -> delabApp (B.Var (delabSymbol (layoutAsText $ pretty tc))) (B.ExplicitArg <$> args)

  V.Nil            -> B.Nil tokNil
  V.Cons           -> delabInfixOp2 B.Cons tokCons args

delabTypeClassOp :: V.TypeClassOp -> [B.Expr] -> B.Expr
delabTypeClassOp op args = case op of
  V.AndTC       -> delabInfixOp2 B.And tokAnd args
  V.OrTC        -> delabInfixOp2 B.Or tokOr args
  V.ImpliesTC   -> delabInfixOp2 B.Impl tokImpl args
  V.NotTC       -> delabOp1 B.Not tokNot args

  V.FromNatTC{} -> head args
  V.FromRatTC{} -> head args
  V.FromVecTC{} -> head args

  V.NegTC -> delabOp1 B.Neg tokSub args
  V.AddTC -> delabInfixOp2 B.Add tokAdd args
  V.SubTC -> delabInfixOp2 B.Sub tokSub args
  V.MulTC -> delabInfixOp2 B.Mul tokMul args
  V.DivTC -> delabInfixOp2 B.Div tokDiv args

  V.EqualsTC eq -> case eq of
    V.Eq  -> delabInfixOp2 B.Eq tokEq args
    V.Neq -> delabInfixOp2 B.Neq tokNeq args
  V.OrderTC ord -> case ord of
    V.Le -> delabInfixOp2 B.Le tokLe args
    V.Lt -> delabInfixOp2 B.Lt tokLt args
    V.Ge -> delabInfixOp2 B.Ge tokGe args
    V.Gt -> delabInfixOp2 B.Gt tokGt args

  V.MapTC  -> delabOp2 B.Map tokMap args
  V.FoldTC -> delabOp3 B.Fold tokFold args

  V.QuantifierTC q   -> delabQuantifier q args
  V.QuantifierInTC q -> delabQuantifierIn q args

delabOp1 :: IsToken token => (token -> B.Expr -> B.Expr) -> token -> [B.Expr] -> B.Expr
delabOp1 op tk [arg] = op tk arg
delabOp1 _ tk args   = argsError (tkSymbol tk) 1 args

delabOp2 :: IsToken token => (token -> B.Expr -> B.Expr -> B.Expr) -> token -> [B.Expr] -> B.Expr
delabOp2 op tk [arg1, arg2] = op tk arg1 arg2
delabOp2 _ tk args          = argsError (tkSymbol tk) 2 args

delabOp3 :: IsToken token => (token -> B.Expr -> B.Expr -> B.Expr -> B.Expr) -> token -> [B.Expr] -> B.Expr
delabOp3 op tk [arg1, arg2, arg3] = op tk arg1 arg2 arg3
delabOp3 _ tk args                = argsError (tkSymbol tk) 3 args

delabInfixOp2 :: IsToken token => (B.Expr -> token -> B.Expr -> B.Expr) -> token -> [B.Expr] -> B.Expr
delabInfixOp2 op tk [arg1, arg2] = op arg1 tk arg2
delabInfixOp2 _ tk args          = argsError (tkSymbol tk) 2 args

delabIf :: [B.Expr] -> B.Expr
delabIf [arg1, arg2, arg3] = B.If tokIf arg1 tokThen arg2 tokElse arg3
delabIf args               = argsError "if" 3 args

delabQuantifier :: V.Quantifier -> [B.Expr] -> B.Expr
delabQuantifier V.Forall [B.Lam _ binders _ body] = B.Forall tokForall binders tokDot body
delabQuantifier V.Exists [B.Lam _ binders _ body] = B.Exists tokExists binders tokDot body
delabQuantifier V.Forall args = argsError (tkSymbol tokForall) 1 args
delabQuantifier V.Exists args = argsError (tkSymbol tokExists) 1 args

delabQuantifierIn :: V.Quantifier -> [B.Expr] -> B.Expr
delabQuantifierIn V.Forall [B.Lam _ binders _ body, xs] = B.ForallIn tokForall binders xs tokDot body
delabQuantifierIn V.Exists [B.Lam _ binders _ body, xs] = B.ExistsIn tokExists binders xs tokDot body
delabQuantifierIn V.Forall args = argsError (tkSymbol tokForall) 2 args
delabQuantifierIn V.Exists args = argsError (tkSymbol tokExists) 2 args

delabForeach :: [B.Expr] -> B.Expr
delabForeach [B.Lam _ binders _ body] = B.Foreach tokForeach binders tokDot body
delabForeach args                     = argsError (tkSymbol tokForall) 1 args

argsError :: Text -> Int -> [B.Expr] -> a
argsError s n args =
  developerError $
    "Expecting" <+> pretty n <+> "arguments for" <+> squotes (pretty s)
      <+> "but found"
      <+> pretty (length args)
      <+> squotes (pretty (show args))

-- | Collapses pi expressions into either a function or a sequence of forall bindings
delabPi :: MonadDelab m => V.Provenance -> V.NamedBinder -> V.NamedExpr -> m B.Expr
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

delabFun :: MonadDelab m => Bool -> V.Identifier -> V.NamedExpr -> V.NamedExpr -> m [B.Decl]
delabFun isProperty name typ expr = do
  let n' = delabIdentifier name
  case foldDefFun typ expr of
    Left (t, (binders, body)) -> do
      defExpr <- B.DefFunExpr n' <$> traverse delabM binders <*> delabM body
      defType <- B.DefFunType n' tokElemOf <$> delabM t
      let decl = [defType, defExpr]

      return $ if isProperty
        then delabAnn propertyAnn [] : decl
        else decl

    Right (binders, body) -> do
      defType <- B.DefType n' <$> traverse delabM binders <*> delabM body
      return [defType]

delabAnn :: B.DeclAnnName -> [B.DeclAnnOption] -> B.Decl
delabAnn name []  = B.DefAnn name B.DeclAnnWithoutOpts
delabAnn name ops = B.DefAnn name $ B.DeclAnnWithOpts ops

mkDeclAnnOption :: Text -> Bool -> B.DeclAnnOption
mkDeclAnnOption name value = B.BooleanOption (mkToken B.Name name) (delabBoolLit value)

auxiliaryTypeError :: Doc a -> a
auxiliaryTypeError e = developerError $
  "Encountered" <+> squotes e <> ". Should not be delaborating auxiliary-type system code."

primOpError :: V.Builtin -> a
primOpError e = developerError $
  "Encountered" <+> squotes (pretty e) <> ". Delaborating primitive builtins not yet supported."

onlyExplicit :: NonEmpty (GenericArg expr) -> [expr]
onlyExplicit args = argExpr <$> filter V.isExplicit (NonEmpty.toList args)
