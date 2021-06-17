{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedLists #-}

{-# LANGUAGE TypeFamilies #-}
module Vehicle.Frontend.Elaborate
  ( Elab(..)
  , ElabError(..)
  , MonadElab
  , runElab
  ) where


import Control.Monad.Except (MonadError(..), Except, runExcept)
import Data.Foldable (foldrM)
import Data.List.NonEmpty qualified as NEList (NonEmpty(..), groupBy, map, head)
import Data.Maybe (fromMaybe)

import Vehicle.Core.AST qualified as VC hiding (Name(..))
import Vehicle.Frontend.AST qualified as VF
import Vehicle.Core.Abs (Name(..))
import Vehicle.Prelude

-- * Source core types

type FKind = VF.Kind (K Provenance)
type FType = VF.Type (K Provenance)
type FTArg = VF.TArg (K Provenance)
type FExpr = VF.Expr (K Provenance)
type FEArg = VF.EArg (K Provenance)
type FDecl = VF.Decl (K Provenance)
type FProg = VF.Prog (K Provenance)

-- * Target core types

type CKind = VC.Kind (K Name) (K Provenance)
type CType = VC.Type (K Name) (K Provenance)
type CTArg = VC.TArg (K Name) (K Provenance)
type CExpr = VC.Expr (K Name) (K Provenance)
type CEArg = VC.EArg (K Name) (K Provenance)
type CDecl = VC.Decl (K Name) (K Provenance)
type CProg = VC.Prog (K Name) (K Provenance)


-- * Elaboration monad

-- | Errors that may arise during elaboration.
data ElabError
  = MissingDefFunType Symbol Provenance
  | MissingDefFunExpr Symbol Provenance
  | DuplicateName Symbol (NEList.NonEmpty Provenance)
  | LocalDeclNetw Provenance
  | LocalDeclData Provenance
  | LocalDefType Provenance
  deriving (Show)

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

-- It would be nice to be able to do this via the recursive fold defined for the
-- core tree but unfortunately this doesn't work due to the need to combine the
-- `DefFunType` and `DefFunExpr` into one `DefFun`.

-- |Class for the various elaboration functions.
class Elab vf vc where
  elab :: MonadElab m => vf -> m vc

-- |Elaborate kinds.
instance Elab FKind CKind where
    -- Core structure.
  elab (VF.KApp ann k1 k2)  = VC.KApp ann <$> elab k1 <*> elab k2

    -- Primitive kinds.
  elab (VF.KFun  ann k1 k2) = kOp2 VC.KFun ann k1 k2
  elab (VF.KType ann)       = kCon VC.KType ann
  elab (VF.KDim  ann)       = kCon VC.KDim ann
  elab (VF.KList ann)       = kCon VC.KDimList ann

-- |Elaborate types.
instance Elab FType CType where
  -- Core structure.
  elab (VF.TForall ann ns t)   = foldr (VC.TForall ann) <$> elab t <*> traverse elab ns
  elab (VF.TApp    ann t1 t2)  = VC.TApp ann <$> elab t1 <*> elab t2
  elab (VF.TVar    ann n)      = return $ VC.TVar ann $ mkName ann n

  -- Primitive types.
  elab (VF.TFun    ann t1 t2)  = tOp2 VC.TFun ann t1 t2
  elab (VF.TBool   ann)        = tCon VC.TBool ann
  elab (VF.TProp   ann)        = tCon VC.TProp ann
  elab (VF.TReal   ann)        = tCon VC.TReal ann
  elab (VF.TInt    ann)        = tCon VC.TInt ann
  elab (VF.TList   ann)        = tCon VC.TList ann
  elab (VF.TTensor ann)        = tCon VC.TTensor ann

  -- Type-level dimensions.
  elab (VF.TAdd    ann t1 t2)  = tOp2 VC.TAdd ann t1 t2
  elab (VF.TLitDim ann n)      = return $ VC.TLitDim ann n

  -- Type-level lists.
  elab (VF.TCons ann t1 t2)    = tOp2 VC.TCons ann t1 t2
  elab (VF.TLitDimList ann ts) = VC.TLitDimList ann <$> traverse elab ts

-- |Elaborate type arguments.
instance Elab FTArg CTArg where
  elab (VF.TArg ann n) = return $ VC.TArg ann $ mkName ann n

-- |Elaborate expressions.
instance Elab FExpr CExpr where
  -- Core structure.
  elab (VF.EAnn   ann e t)   = VC.EAnn ann <$> elab e <*> elab t
  elab (VF.ELet   ann ds e)  = elabLet ann (groupDecls ds) (elab e)
  elab (VF.ELam   ann ns e)  = foldr (VC.ELam ann) <$> elab e <*> traverse elab ns
  elab (VF.EApp   ann e1 e2) = VC.EApp ann <$> elab e1 <*> elab e2
  elab (VF.EVar   ann n)     = return $ VC.EVar ann $ mkName ann n
  elab (VF.ETyApp ann e t)   = VC.ETyApp ann <$> elab e <*> elab t
  elab (VF.ETyLam ann ns e)  = foldr (VC.ETyLam ann) <$> elab e <*> traverse elab ns

  -- Conditional expressions.
  elab (VF.EIf    ann e1 e2 e3) = eOp3 VC.EIf    ann e1 e2 e3
  elab (VF.EImpl  ann e1 e2)    = eOp2 VC.EImpl  ann e1 e2
  elab (VF.EAnd   ann e1 e2)    = eOp2 VC.EAnd   ann e1 e2
  elab (VF.EOr    ann e1 e2)    = eOp2 VC.EOr    ann e1 e2
  elab (VF.ENot   ann e)        = eOp1 VC.ENot   ann e
  elab (VF.ETrue  ann)          = eCon VC.ETrue  ann
  elab (VF.EFalse ann)          = eCon VC.EFalse ann

  -- Integers and reals.
  elab (VF.EEq      ann e1 e2) = eOp2 VC.EEq  ann e1 e2
  elab (VF.ENeq     ann e1 e2) = eOp2 VC.ENeq ann e1 e2
  elab (VF.ELe      ann e1 e2) = eOp2 VC.ELe  ann e1 e2
  elab (VF.ELt      ann e1 e2) = eOp2 VC.ELt  ann e1 e2
  elab (VF.EGe      ann e1 e2) = eOp2 VC.EGe  ann e1 e2
  elab (VF.EGt      ann e1 e2) = eOp2 VC.EGt  ann e1 e2
  elab (VF.EMul     ann e1 e2) = eOp2 VC.EMul ann e1 e2
  elab (VF.EDiv     ann e1 e2) = eOp2 VC.EDiv ann e1 e2
  elab (VF.EAdd     ann e1 e2) = eOp2 VC.EAdd ann e1 e2
  elab (VF.ESub     ann e1 e2) = eOp2 VC.ESub ann e1 e2
  elab (VF.ENeg     ann e)     = eOp1 VC.ENeg ann e
  elab (VF.ELitInt  ann z)     = return $ VC.ELitInt ann z
  elab (VF.ELitReal ann r)     = return $ VC.ELitReal ann r

  -- Lists and tensors.
  elab (VF.ECons ann e1 e2) = eOp2 VC.ECons ann e1 e2
  elab (VF.EAt ann e1 e2)   = eOp2 VC.EAt ann e1 e2
  elab (VF.EAll ann)        = eCon VC.EAll ann
  elab (VF.EAny ann)        = eCon VC.EAny ann
  elab (VF.ELitSeq ann es)  = VC.ELitSeq ann <$> traverse elab es

-- |Elaborate expression arguments.
instance Elab FEArg CEArg where
  elab (VF.EArg ann n) = return $ VC.EArg ann $ mkName ann n

-- |Elaborate declarations.
instance Elab (NEList.NonEmpty FDecl) CDecl where

  -- Elaborate a network declaration.
  elab [VF.DeclNetw ann n t] =
    VC.DeclNetw ann <$> elab n <*> elab t

  -- Elaborate a dataset declaration.
  elab [VF.DeclData ann n t] =
    VC.DeclData ann <$> elab n <*> elab t

  -- Elaborate a type definition.
  elab [VF.DefType ann n ns t] =
    VC.DefType ann <$> elab n <*> traverse elab ns <*> elab t

  -- Elaborate a function definition.
  elab [VF.DefFunType ann1 _n t, VF.DefFunExpr ann2 n ns e] =
    VC.DefFun ann <$> elab n <*> elab t <*> expr
      where
        ann  = K (unK ann1 <> unK ann2)
        expr = foldr (VC.ELam (K (unK ann1))) <$> elab e <*> traverse elab ns

  -- Why did you write the signature AFTER the function?
  elab [e1@VF.DefFunExpr {}, e2@VF.DefFunType {}] =
    elab (e2 NEList.:| [e1])

  -- Missing type or expression declaration.
  elab [VF.DefFunType ann n _t] =
    throwError $ MissingDefFunExpr (VF.eArgName n) (unK ann)

  elab [VF.DefFunExpr ann n _ns _e] =
    throwError $ MissingDefFunType (VF.eArgName n) (unK ann)

  -- Multiple type of expression declarations with the same n.
  elab ds =
    throwError $ DuplicateName name provs
      where
        name  = NEList.head (NEList.map VF.declName ds)
        provs = NEList.map (unK . VF.annotation) ds

-- |Elaborate programs.
instance Elab FProg CProg where
  elab (VF.Main ann decls) = VC.Main ann <$> groupDecls decls


-- |Elaborate a let binding with /multiple/ bindings to a series of let
--  bindings with a single binding each.
elabLet :: MonadElab m => K Provenance 'EXPR -> m [CDecl] -> m CExpr -> m CExpr
elabLet ann1 ds e = bindM2 (foldrM declToLet) e ds
  where
    declToLet :: MonadElab m => CDecl -> CExpr -> m CExpr
    declToLet (VC.DefFun   ann2  n t e1)    e2 = return $ VC.ELet ann1 n (VC.EAnn (K (unK ann2)) e1 t) e2
    declToLet (VC.DeclNetw ann2 _n _t)     _e2 = throwError $ LocalDeclNetw (unK ann2)
    declToLet (VC.DeclData ann2 _n _t)     _e2 = throwError $ LocalDeclData (unK ann2)
    declToLet (VC.DefType  ann2 _n _ns _t) _e2 = throwError $ LocalDefType  (unK ann2)

-- |Takes a list of declarations, and groups type and expression
--  declarations for the same name. If any name does not have exactly one
--  type and one expression declaration, an error is returned.
groupDecls :: MonadElab m => [FDecl] -> m [CDecl]
groupDecls decls = traverse elab (NEList.groupBy cond decls)
  where
    cond :: VF.Decl ann -> VF.Decl ann -> Bool
    cond d1 d2 = VF.isDefFun d1 && VF.isDefFun d2 && VF.declName d1 == VF.declName d2

-- |Lift a binary /monadic/ function.
bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f ma mb = do a <- ma; b <- mb; f a b

-- |Elaborate any builtin token to a kind.
kCon :: (MonadElab m) => VC.Builtin 'KIND -> K Provenance 'KIND -> m CKind
kCon b ann = return $ VC.KCon ann b

-- |Elaborate a unary function symbol with its argument to a kind.
kOp1 :: (MonadElab m) => VC.Builtin 'KIND -> K Provenance 'KIND -> FKind -> m CKind
kOp1 b ann k1 = VC.KApp ann <$> kCon b ann <*> elab k1

-- |Elaborate a binary function symbol with its arguments to a kind.
kOp2 :: (MonadElab m) => VC.Builtin 'KIND -> K Provenance 'KIND -> FKind -> FKind -> m CKind
kOp2 b ann k1 k2 = VC.KApp ann <$> kOp1 b ann k1 <*> elab k2

-- |Elaborate any builtin token to a type.
tCon :: (MonadElab m) => VC.Builtin 'TYPE -> K Provenance 'TYPE -> m CType
tCon b ann = return $ VC.TCon ann b

-- |Elaborate a unary function symbol with its argument to a type.
tOp1 :: (MonadElab m) => VC.Builtin 'TYPE -> K Provenance 'TYPE -> FType -> m CType
tOp1 b ann t1 = VC.TApp ann <$> tCon b ann <*> elab t1

-- |Elaborate a binary function symbol with its arguments to a type.
tOp2 :: (MonadElab m) => VC.Builtin 'TYPE -> K Provenance 'TYPE -> FType -> FType -> m CType
tOp2 b ann t1 t2 = VC.TApp ann <$> tOp1 b ann t1 <*> elab t2

-- |Elaborate any builtin token to an expression.
eCon :: (MonadElab m) => VC.Builtin 'EXPR -> K Provenance 'EXPR -> m CExpr
eCon b ann = return $ VC.ECon ann b

-- |Elaborate a unary function symbol with its argument to an expression.
eOp1 :: (MonadElab m) => VC.Builtin 'EXPR -> K Provenance 'EXPR -> FExpr -> m CExpr
eOp1 b ann e1 = VC.EApp ann <$> eCon b ann <*> elab e1

-- |Elaborate a binary function symbol with its arguments to an expression.
eOp2 :: (MonadElab m) => VC.Builtin 'EXPR -> K Provenance 'EXPR -> FExpr -> FExpr -> m CExpr
eOp2 b ann e1 e2 = VC.EApp ann <$> eOp1 b ann e1 <*> elab e2

-- |Elaborate a binary function symbol with its arguments to an expression.
eOp3 :: (MonadElab m) => VC.Builtin 'EXPR -> K Provenance 'EXPR -> FExpr -> FExpr -> FExpr -> m CExpr
eOp3 b ann e1 e2 e3 = VC.EApp ann <$> eOp2 b ann e1 e2 <*> elab e3

-- |Temporary method to construct a name out of the
mkName :: KnownSort sort => K Provenance sort -> Symbol -> K Name sort
mkName (K (Provenance []))     s = K $ Name ((0, 0) , s)
mkName (K (Provenance (p :_))) s = K $ Name (fromMaybe (0, 0) (rangeStart p), s)