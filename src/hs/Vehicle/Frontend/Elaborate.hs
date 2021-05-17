{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Vehicle.Frontend.Elaborate
  ( Elab(..)
  , ElabError(..)
  , MonadElab
  , runElab
  ) where


import           Control.Exception (Exception)
import           Control.Monad.Except (MonadError(..), Except, runExcept)
import           Data.Coerce (coerce)
import           Data.Foldable (foldrM)
import           Data.Functor.Foldable (fold)
import           Data.List (groupBy)
import qualified Vehicle.Core.Abs as VC -- NOTE: In general, avoid importing Abs!
import qualified Vehicle.Frontend.AST as VF
import           Vehicle.Prelude


-- * Elaboration monad

-- | Errors that may arise during elaboration.
data ElabError
  = MissingDefFunType Token
  | MissingDefFunExpr Token
  | DuplicateName [Token]
  | LocalDeclNetw Token
  | LocalDeclData Token
  | LocalDefType Token
  deriving (Show)

instance Exception ElabError

-- | Constraint for the monad stack used by the elaborator.
type MonadElab m = MonadError ElabError m

-- | Run a function in 'MonadElab'.
runElab :: Except ElabError a -> Either ElabError a
runElab = runExcept


-- * Elaboration class

--------------------------------------------------------------------------------
-- $sugar
-- The following definitions are tactics for unfolding various bits of syntactic
-- sugar. These are unfolded /before/ type-checking occurs, as Frontend is never
-- type-checked. Therefore, more clever bits have to wait until type-checking.
--
-- The following pieces of syntactic sugar are unfolded here:
--
--   * @forall a b. TYPE@
--     is unfolded to
--     @(forall a ?_ (forall b ?_ TYPE))@
--     (see 'elabTForalls')
--
--   * @\x y -> EXPR@
--     is unfolded to
--     @(lambda ( x ?_ ) (lambda ( y ?_ ) EXPR))@
--     (see 'elabELams')
--
--   * @\{x y} -> EXPR@
--     is unfolded to
--     @(lambda { x ?_ } (lambda { y ?_ } EXPR))@
--     (see 'elabELams')
--
--   * @let { x : Nat ; x = 1 ; y : Nat ; y = 2 } in EXPR@
--     is unfolded to
--     @(let ( x Nat ) 1 (let ( y Nat ) 2 EXPR@
--
--   * infix operators are rewritten to Polish notation, e.g.,
--     @1 + 5@ is rewritten to @(+ 1 5)@
--
--------------------------------------------------------------------------------

-- |Class for the various elaboration functions.
class Elab vf vc where
  elab :: MonadElab m => vf -> m vc

-- |Elaborate kinds.
instance Elab VF.Kind VC.Kind where
  elab = fold $ \case

    -- Core structure.
    VF.KAppF k1 k2    -> VC.KApp <$> k1 <*> k2

    -- Primitive kinds.
    VF.KFunF k1 tk k2 -> kOp2 tk k1 k2
    VF.KTypeF tk      -> kCon tk
    VF.KDimF tk       -> kCon tk
    VF.KListF tk      -> kCon tk

-- |Elaborate types.
instance Elab VF.Type VC.Type where
  elab = fold $ \case

    -- Core structure.
    VF.TForallF _tk1 ns _tk2 t -> foldr VC.TForall <$> t <*> traverse elab ns
    VF.TAppF t1 t2             -> VC.TApp <$> t1 <*> t2
    VF.TVarF n                 -> elab n

    -- Primitive types.
    VF.TFunF t1 tk t2 -> tOp2 tk t1 t2
    VF.TBoolF tk      -> tCon tk
    VF.TPropF tk      -> tCon tk
    VF.TRealF tk      -> tCon tk
    VF.TIntF tk       -> tCon tk
    VF.TListF tk      -> tCon tk
    VF.TTensorF tk    -> tCon tk

    -- Type-level dimensions.
    VF.TAddF t1 tk t2 -> tOp2 tk t1 t2
    VF.TLitDimF nat   -> return $ VC.TLitDim nat

    -- Type-level lists.
    VF.TConsF t1 tk t2            -> tOp2 tk t1 t2
    VF.TLitDimListF _tk1 ts _tk2  -> VC.TLitDimList <$> sequence ts

-- |Elaborate expressions.
instance Elab VF.Expr VC.Expr where
  elab = fold $ \case

    -- Core structure.
    VF.EAnnF e _tk t               -> VC.EAnn <$> e <*> elab t
    VF.ELetF ds e                  -> eLet (eDecls ds) e
    VF.ELamF _tk1 ns _tk2 e        -> foldr VC.ELam <$> e <*> traverse elab ns
    VF.EAppF e1 e2                 -> VC.EApp <$> e1 <*> e2
    VF.EVarF n                     -> elab n
    VF.ETyAppF e t                 -> VC.ETyApp <$> e <*> elab t
    VF.ETyLamF _tk1 ns _tk2 e      -> foldr VC.ETyLam <$> e <*> traverse elab ns

    -- Conditional expressions.
    VF.EIfF tk1 e1 _tk2 e2 _tk3 e3 -> eOp3 tk1 e1 e2 e3
    VF.EImplF e1 tk e2             -> eOp2 tk e1 e2
    VF.EAndF e1 tk e2              -> eOp2 tk e1 e2
    VF.EOrF e1 tk e2               -> eOp2 tk e1 e2
    VF.ENotF tk e                  -> eOp1 tk e
    VF.ETrueF tk                   -> eCon tk
    VF.EFalseF tk                  -> eCon tk

    -- Integers and reals.
    VF.EEqF e1 tk e2               -> eOp2 tk e1 e2
    VF.ENeqF e1 tk e2              -> eOp2 tk e1 e2
    VF.ELeF e1 tk e2               -> eOp2 tk e1 e2
    VF.ELtF e1 tk e2               -> eOp2 tk e1 e2
    VF.EGeF e1 tk e2               -> eOp2 tk e1 e2
    VF.EGtF e1 tk e2               -> eOp2 tk e1 e2
    VF.EMulF e1 tk e2              -> eOp2 tk e1 e2
    VF.EDivF e1 tk e2              -> eOp2 tk e1 e2
    VF.EAddF e1 tk e2              -> eOp2 tk e1 e2
    VF.ESubF e1 tk e2              -> eOp2 tk e1 e2
    VF.ENegF tk e                  -> eOp1 (tkUpdateText "~" tk) e
    VF.ELitIntF z                  -> return $ VC.ELitInt z
    VF.ELitRealF r                 -> return $ VC.ELitReal r

    -- Lists and tensors.
    VF.EConsF e1 tk e2             -> eOp2 tk e1 e2
    VF.EAtF e1 tk e2               -> eOp2 tk e1 e2
    VF.EAllF tk                    -> eCon tk
    VF.EAnyF tk                    -> eCon tk
    VF.ELitSeqF _tk1 es _tk2       -> VC.ELitSeq <$> sequence es

-- |Elaborate declarations.
instance Elab [VF.Decl] VC.Decl where

  -- Elaborate a network declaration.
  elab [VF.DeclNetw n _tk t] =
    VC.DeclNetw <$> elab n <*> elab t

  -- Elaborate a dataset declaration.
  elab [VF.DeclData n _tk t] =
    VC.DeclData <$> elab n <*> elab t

  -- Elaborate a type definition.
  elab [VF.DefType n ns t] =
    VC.DefType <$> elab n <*> traverse elab ns <*> elab t

  -- Elaborate a function definition.
  elab [VF.DefFunType _n _tk t, VF.DefFunExpr n ns e] =
    VC.DefFun <$> elab n <*> elab t <*> (foldr VC.ETyLam <$> elab e <*> traverse elab ns)

  -- Why did you write the signature AFTER the function?
  elab [VF.DefFunExpr n1 ns e, VF.DefFunType n2 tk t] =
    elab [VF.DefFunType n2 tk t, VF.DefFunExpr n1 ns e]

  -- Missing type or expression declaration.
  elab [VF.DefFunType n _tk _t] =
    throwError (MissingDefFunExpr (toToken n))

  elab [VF.DefFunExpr n _ns _e] =
    throwError (MissingDefFunType (toToken n))

  -- Multiple type of expression declarations with the same n.
  elab ds =
    throwError (DuplicateName (map (toToken . VF.declName) ds))

-- |Elaborate programs.
instance Elab VF.Prog VC.Prog where
  elab (VF.Main decls) = VC.Main <$> eDecls decls


-- |Elaborate a let binding with /multiple/ bindings to a series of let
--  bindings with a single binding each.
eLet :: MonadElab m => m [VC.Decl] -> m VC.Expr -> m VC.Expr
eLet ds e = bindM2 (foldrM declToLet) e ds
  where
    declToLet :: MonadElab m => VC.Decl -> VC.Expr -> m VC.Expr
    declToLet (VC.DefFun n t e1) e2 = return (VC.ELet n (VC.EAnn e1 t) e2)
    declToLet (VC.DeclNetw (VC.MkExprBinder n) _t) _e2 = throwError (LocalDeclNetw (toToken n))
    declToLet (VC.DeclData (VC.MkExprBinder n) _t) _e2 = throwError (LocalDeclData (toToken n))
    declToLet (VC.DefType (VC.MkTypeBinder n) _ns _t) _e2 = throwError (LocalDefType (toToken n))

-- |Takes a list of declarations, and groups type and expression
--  declarations for the same name. If any name does not have exactly one
--  type and one expression declaration, an error is returned.
eDecls :: MonadElab m => [VF.Decl] -> m [VC.Decl]
eDecls decls = traverse elab (groupBy cond decls)
  where
    cond :: VF.Decl -> VF.Decl -> Bool
    cond d1 d2 = VF.isDefFun d1 && VF.isDefFun d2 && VF.declName d1 `tkEq` VF.declName d2

-- |Elaborate any builtin token to a kind.
kCon :: (MonadElab m, IsToken a) => a -> m VC.Kind
kCon tk = pure $ VC.KCon (coerce tk)

-- |Elaborate any builtin token to a type.
tCon :: (MonadElab m, IsToken a) => a -> m VC.Type
tCon tk = pure $ VC.TCon (coerce tk)

-- |Elaborate any builtin token to an expression.
eCon :: (MonadElab m, IsToken a) => a -> m VC.Expr
eCon tk = pure $ VC.ECon (coerce tk)

-- |Elaborate a unary function symbol with its argument to a type.
kOp1 :: (MonadElab m, IsToken a) => a -> m VC.Kind -> m VC.Kind
kOp1 tkOp k1 = VC.KApp <$> kCon tkOp <*> k1

-- |Elaborate a binary function symbol with its arguments to a kind.
kOp2 :: (MonadElab m, IsToken a) => a -> m VC.Kind -> m VC.Kind -> m VC.Kind
kOp2 tkOp k1 k2 = VC.KApp <$> kOp1 tkOp k1 <*> k2

-- |Elaborate a unary function symbol with its argument to a type.
tOp1 :: (MonadElab m, IsToken a) => a -> m VC.Type -> m VC.Type
tOp1 tkOp t1 = VC.TApp <$> tCon tkOp <*> t1

-- |Elaborate a binary function symbol with its arguments to a type.
tOp2 :: (MonadElab m, IsToken a) => a -> m VC.Type -> m VC.Type -> m VC.Type
tOp2 tkOp t1 t2 = VC.TApp <$> tOp1 tkOp t1 <*> t2

-- |Elaborate a unary function symbol with its argument to an expression.
eOp1 :: (MonadElab m, IsToken a) => a -> m VC.Expr -> m VC.Expr
eOp1 tkOp e1 = VC.EApp <$> eCon tkOp <*> e1

-- |Elaborate a binary function symbol with its arguments to an expression.
eOp2 :: (MonadElab m, IsToken a) => a -> m VC.Expr -> m VC.Expr -> m VC.Expr
eOp2 tkOp e1 e2 = VC.EApp <$> eOp1 tkOp e1 <*> e2

-- |Elaborate a ternary function symbol with its arguments to an expression.
eOp3 :: (MonadElab m, IsToken a) => a -> m VC.Expr -> m VC.Expr -> m VC.Expr -> m VC.Expr
eOp3 tkOp e1 e2 e3 = VC.EApp <$> eOp2 tkOp e1 e2 <*> e3

-- |Lift a binary /monadic/ function.
bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f ma mb = do a <- ma; b <- mb; f a b


-- * Convert various token types to constructors or variables

instance Elab VF.Name VC.TypeBinder where
  elab tk = pure $ VC.MkTypeBinder (coerce tk)

instance Elab VF.Name VC.ExprBinder where
  elab tk = pure $ VC.MkExprBinder (coerce tk)

instance Elab VF.Name VC.Type where
  elab tk = pure $ VC.TVar (coerce tk)

instance Elab VF.Name VC.Expr where
  elab tk = pure $ VC.EVar (coerce tk)
