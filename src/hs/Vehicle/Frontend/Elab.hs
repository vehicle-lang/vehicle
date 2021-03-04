{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Vehicle.Frontend.Elab where

import           Control.Applicative
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.Supply (MonadSupply(..))
import           Data.Coerce (Coercible,coerce)
import           Data.Foldable (foldrM)
import           Data.List (groupBy)
import           Data.Text (Text)
import qualified Vehicle.Frontend.Abs as VF
import qualified Vehicle.Core.Abs as VC

-- Check a program:
--
-- Group type and expression declarations.
-- If any are missing, throw an error.
-- Perform syntactic elaboration to Core.
-- Type-check Core and perform type-driven elaborations.

data ElabError
        = MissingDeclType VF.Name
        | MissingDeclExpr VF.Name
        | DuplicateName [VF.Name]

type Meta = Integer

class Elab vf vc where
        elab :: (MonadError ElabError m, MonadSupply Meta m) => vf -> m vc

instance Elab VF.Prog VC.Prog where
        -- Takes a list of declarations, and groups type and expression
        -- declarations for the same name. If any name does not have exactly one
        -- type and one expression declaration, an error is returned.
        elab (VF.Main decls) = VC.Main <$> traverse elab (groupBy sameName decls) where
                sameName :: VF.Decl -> VF.Decl -> Bool
                sameName decl1 decl2 = not (isDeclNetw decl1) &&
                                       not (isDeclNetw decl2) &&
                                       declName decl1 `tokEq` declName decl2

instance Elab VF.Kind VC.Kind where

        elab (VF.KApp kind1 kind2) = VC.KApp <$> elab kind1 <*> elab kind2
        elab (VF.KType tok1) = elabKCon tok1
        elab (VF.KNat  tok1) = elabKCon tok1
        elab (VF.KList tok1) = elabKCon tok1

instance Elab VF.Type VC.Type where

        elab (VF.TFun type1 tokArrow type2) = elabTOp tokArrow type1 type2
        elab (VF.TForall _tokForall args1 _tokDot type1) = elabTForalls args1 type1
        elab (VF.TAdd type1 tokAdd type2) = elabTOp tokAdd type1 type2
        elab (VF.TApp type1 type2) = VC.TApp <$> elab type1 <*> elab type2
        elab (VF.TLitNat nat) = elab nat
        elab (VF.TNil tokNil) = elabTCon tokNil
        elab (VF.TCons tokCons) = elabTCon tokCons
        elab (VF.TTensor tokTensor) = elabTCon tokTensor
        elab (VF.TBool tokBool) = elabTCon tokBool
        elab (VF.TReal tokReal) = elabTCon tokReal
        elab (VF.TNat tokNat) = elabTCon tokNat

        -- Elaborate a list literal to a series of Cons and Nil.
        elab (VF.TListOf _tokSeqOpen types _tokSeqClose) =
                undefined

instance Elab VF.Expr VC.Expr where

        elab (EAnn expr1 _tokElemOf type1) =
                undefined
        elab (EForall tokForall name1 tokElemOf (BExpr expr1) _tokDot expr2) =
                undefined
        elab (EForall tokForall name1 tokElemOf (BType type1) _tokDot expr2) =
                undefined
        elab (EExists tokExists name1 tokElemOf (BExpr expr1) _tokDot expr2) =
                undefined
        elab (EExists tokExists name1 tokElemOf (BType type1) _tokDot expr2) =
                undefined
        elab (ELet decls expr1) =
                undefined
        elab (EIf expr1 expr2 expr3) =
                undefined
        elab (ELam _tokLambda args1 _tokArrow expr1) =
                undefined
        elab (EImpl expr1 tokImpl expr2) =
                undefined
        elab (EAnd expr1 tokAnd expr2) =
                undefined
        elab (EOr expr1 tokOr expr2) =
                undefined
        elab (EEq expr1 tokEq expr2) =
                undefined
        elab (ENeq expr1 tokNeq expr2) =
                undefined
        elab (ELe expr1 tokLe expr2) =
                undefined
        elab (ELt expr1 tokLt expr2) =
                undefined
        elab (EGe expr1 tokGe expr2) =
                undefined
        elab (EGt expr1 tokGt expr2) =
                undefined
        elab (EMul expr1 tokMul expr2) =
                undefined
        elab (EDiv expr1 tokDiv expr2) =
                undefined
        elab (EAdd expr1 tokAdd expr2) =
                undefined
        elab (ESub expr1 tokSub expr2) =
                undefined
        elab (ENeg tokNeg expr1) =
                undefined
        elab (EAt expr1 tokAt expr2) =
                undefined
        elab (EApp expr1 expr2) =
                undefined
        elab (EVar name1) =
                undefined
        elab (ELitNat nat) =
                undefined
        elab (ELitReal real) =
                undefined
        elab (ETensor _tokSeqOpen exprs _tokSeqClose) =
                undefined
        elab (ETrue tokTrue) =
                undefined
        elab (EFalse tokFalse) =
                undefined
        elab (ETyApp expr1 type1) =
                undefined
        elab (ETyLam _tokLambda args1 _tokArrow expr1) =
                undefined


instance Elab [VF.Decl] VC.Decl where

        -- Elaborate a network declaration.
        elab [VF.DeclNetw name1 _elemOf type1] =
                VC.DeclNetw <$> elab name1 <*> elab type1

        -- Elaborate a function definition.
        elab [VF.DeclType _name1 _elemOf type1, VF.DeclExpr name1 args1 expr1] =
                VC.DeclExpr <$> elab name1 <*> elab type1 <*> elabELambdas args1 expr1

        -- Why did you write the signature AFTER the function?
        elab [VF.DeclExpr name1 args1 expr1, VF.DeclType _name1 _elemOf type1] =
                elab [VF.DeclType _name1 _elemOf type1, VF.DeclExpr name1 args1 expr1]

        -- Missing type or expression declaration.
        elab [VF.DeclType name1 _elemOf _type1] =
                throwError (MissingDeclExpr name1)

        elab [VF.DeclExpr name1 _args1 _expr1] =
                throwError (MissingDeclType name1)

        -- Multiple type of expression declarations with the same name.
        elab decls =
                throwError (DuplicateName (map declName decls))

-- * Elaborator tactics

kMeta :: MonadSupply Meta m => m VC.Kind
kMeta = VC.KMeta <$> supply

tMeta :: MonadSupply Meta m => m VC.Type
tMeta = VC.TMeta <$> supply

elabKCon :: (MonadError ElabError m, MonadSupply Meta m, Elab a VC.Builtin) => a -> m VC.Kind
elabKCon = fmap VC.KCon . elab

elabTCon :: (MonadError ElabError m, MonadSupply Meta m, Elab a VC.Builtin) => a -> m VC.Type
elabTCon = fmap VC.TCon . elab

elabECon :: (MonadError ElabError m, MonadSupply Meta m, Elab a VC.Builtin) => a -> m VC.Expr
elabECon = fmap VC.ECon . elab

elabTOp :: (MonadError ElabError m, MonadSupply Meta m, Elab a VC.Builtin) => a -> VF.Type -> VF.Type -> m VC.Type
elabTOp tokOp type1 type2 =
        VC.TApp <$> (VC.TApp <$> elabTCon tokOp <*> elab type1) <*> elab type2

elabEOp :: (MonadError ElabError m, MonadSupply Meta m, Elab a VC.Builtin) => a -> VF.Expr -> VF.Expr -> m VC.Expr
elabEOp tokOp expr1 expr2 =
        VC.EApp <$> (VC.EApp <$> elabECon tokOp <*> elab expr1) <*> elab expr2

elabELambdas :: (MonadError ElabError m, MonadSupply Meta m) => [VF.Name] -> VF.Expr -> m VC.Expr
elabELambdas args1 expr1 = bindM2 (foldrM f) (elab expr1) (traverse elab args1) where
        f :: (MonadError ElabError m, MonadSupply Meta m) => VC.Name -> VC.Expr -> m VC.Expr
        f name1 expr2 = do meta1 <- tMeta; return $ VC.ELam name1 meta1 expr2

elabTForalls :: (MonadError ElabError m, MonadSupply Meta m) => [VF.Name] -> VF.Type -> m VC.Type
elabTForalls args1 type1 = bindM2 (foldrM f) (elab type1) (traverse elab args1) where
        f :: (MonadError ElabError m, MonadSupply Meta m) => VC.Name -> VC.Type -> m VC.Type
        f name1 type2 = do meta1 <- kMeta; return $ VC.TForall name1 meta1 type2

-- * Helper functions

bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f ma mb = do a <- ma; b <- mb; f a b

isDeclNetw :: VF.Decl -> Bool
isDeclNetw (VF.DeclNetw _name1 _elemOf _type1) = True
isDeclNetw _ = False

declName :: VF.Decl -> VF.Name
declName (VF.DeclNetw name1 _elemOf _type1) = name1
declName (VF.DeclType name1 _elemOf _type1) = name1
declName (VF.DeclExpr name1 _args1 _expr1) = name1

type Position = (Int, Int)

tokEq :: Coercible a (Position, Text) => a -> a -> Bool
tokEq x y = snd (coerce x :: (Position, Text)) == snd (coerce y :: (Position, Text))

-- * Convert various token types to constructors or variables

instance Elab VF.Nat     VC.Type where elab = fmap VC.TLitNat . elab
instance Elab VF.Nat     VC.Expr where elab = fmap VC.ELitNat . elab
instance Elab VF.Real    VC.Expr where elab = fmap VC.ELitReal . elab
instance Elab VF.Name    VC.Type where elab = fmap VC.TVar . elab
instance Elab VF.Name    VC.Expr where elab = fmap VC.EVar . elab

-- * Convert various token types from Frontend to builtin type in Core

instance Elab VF.TokArrow  VC.Builtin where elab = return . coerce
instance Elab VF.TokForall VC.Builtin where elab = return . coerce
instance Elab VF.TokExists VC.Builtin where elab = return . coerce
instance Elab VF.TokElemOf VC.Builtin where elab = return . coerce
instance Elab VF.TokImpl   VC.Builtin where elab = return . coerce
instance Elab VF.TokAnd    VC.Builtin where elab = return . coerce
instance Elab VF.TokOr     VC.Builtin where elab = return . coerce
instance Elab VF.TokEq     VC.Builtin where elab = return . coerce
instance Elab VF.TokNeq    VC.Builtin where elab = return . coerce
instance Elab VF.TokLe     VC.Builtin where elab = return . coerce
instance Elab VF.TokLt     VC.Builtin where elab = return . coerce
instance Elab VF.TokGe     VC.Builtin where elab = return . coerce
instance Elab VF.TokGt     VC.Builtin where elab = return . coerce
instance Elab VF.TokMul    VC.Builtin where elab = return . coerce
instance Elab VF.TokDiv    VC.Builtin where elab = return . coerce
instance Elab VF.TokAdd    VC.Builtin where elab = return . coerce
instance Elab VF.TokSub    VC.Builtin where elab = return . coerce
instance Elab VF.TokNeg    VC.Builtin where elab = return . coerce
instance Elab VF.TokAt     VC.Builtin where elab = return . coerce
instance Elab VF.TokType   VC.Builtin where elab = return . coerce
instance Elab VF.TokTensor VC.Builtin where elab = return . coerce
instance Elab VF.TokReal   VC.Builtin where elab = return . coerce
instance Elab VF.TokNat    VC.Builtin where elab = return . coerce
instance Elab VF.TokBool   VC.Builtin where elab = return . coerce
instance Elab VF.TokTrue   VC.Builtin where elab = return . coerce
instance Elab VF.TokFalse  VC.Builtin where elab = return . coerce
instance Elab VF.TokList   VC.Builtin where elab = return . coerce
instance Elab VF.TokNil    VC.Builtin where elab = return . coerce
instance Elab VF.TokCons   VC.Builtin where elab = return . coerce
instance Elab VF.Nat       VC.Nat     where elab = return . coerce
instance Elab VF.Real      VC.Real    where elab = return . coerce
instance Elab VF.Name      VC.Name    where elab = return . coerce
-- -}
-- -}
-- -}
-- -}
-- -}
