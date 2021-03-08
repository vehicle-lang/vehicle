{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Vehicle.Frontend.Elaborate where


import           Prelude hiding (exp)
import           Control.Applicative (liftA2)
import           Control.Exception
import           Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import           Control.Monad.Supply (MonadSupply(..), Supply, evalSupply)
import           Data.Coerce (Coercible,coerce)
import           Data.Foldable (foldrM)
import           Data.List (groupBy)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Vehicle.Frontend.Abs as VF
import qualified Vehicle.Core.Abs as VC


-- |Errors that may arise during elaboration.
data ElabError
  = MissingDefFunType VF.Name
  | MissingDefFunExpr VF.Name
  | DuplicateName [VF.Name]
  | LocalDeclNetw VF.Name
  | LocalDeclData VF.Name
  | LocalDefType VF.Name
  | Unknown
  deriving (Show)

instance Exception ElabError

-- |Constraint for the monad stack used by the elaborator.
type MonadElab m = (MonadError ElabError m, MonadSupply Integer m)

runElab :: ExceptT ElabError (Supply Integer) a -> Either ElabError a
runElab x = fromMaybe (Left Unknown) (evalSupply (runExceptT x) [0..])

-- |Class for the various elaboration functions.
class Elab vf vc where
  elab :: MonadElab m => vf -> m vc


instance Elab VF.Kind VC.Kind where

  -- Core structure.
  elab (VF.KApp kind kind2) = VC.KApp <$> elab kind <*> elab kind2

  -- Primitive kinds.
  elab (VF.KType tokType) = elabKCon tokType
  elab (VF.KDim  tokDim) = elabKCon tokDim
  elab (VF.KList tokList) = elabKCon tokList


instance Elab VF.Type VC.Type where

  -- Core structure.
  elab (VF.TForall _tokForall args _tokDot typ) = elabTForalls args typ
  elab (VF.TApp typ1 typ2) = VC.TApp <$> elab typ1 <*> elab typ2
  elab (VF.TVar name) = elab name

  -- Primitive types.
  elab (VF.TFun typ1 tokArrow typ2) = elabTOp2 tokArrow typ1 typ2
  elab (VF.TBool tokBool) = elabTCon tokBool
  elab (VF.TReal tokReal) = elabTCon tokReal
  elab (VF.TInt tokInt) = elabTCon tokInt
  elab (VF.TTensor tokTensor) = elabTCon tokTensor

  -- Type-level naturals.
  elab (VF.TAdd typ1 tokAdd typ2) = elabTOp2 tokAdd typ1 typ2
  elab (VF.TLitDim nat) = return $ VC.TLitDim nat

  -- Type-level lists.
  elab (VF.TNil tokNil) = elabTCon tokNil
  elab (VF.TCons typ1 tokCons typ2) = elabTOp2 tokCons typ1 typ2
  elab (VF.TLitList _tokSeqOpen typs _tokSeqClose) = VC.TLitList <$> traverse elab typs


instance Elab VF.Expr VC.Expr where

  -- Core structure.
  elab (VF.EAnn exp _tokElemOf typ) = VC.EAnn <$> elab exp <*> elab typ
  elab (VF.ELet decls exp) = elabELets decls exp
  elab (VF.ELam _tokLambda args _tokArrow exp) = elabELams args exp
  elab (VF.EApp exp exp2) = VC.EApp <$> elab exp <*> elab exp2
  elab (VF.EVar name) = elab name
  elab (VF.ETyApp exp typ) = VC.ETyApp <$> elab exp <*> elab typ
  elab (VF.ETyLam _tokLambda args _tokArrow exp) = elabETyLams args exp

  -- Conditional expressions.
  elab (VF.EIf tokIf exp1 _tokThen exp2 _tokElse exp3) = elabEOp3 tokIf exp1 exp2 exp3
  elab (VF.EImpl exp1 tokImpl exp2) = elabEOp2 tokImpl exp1 exp2
  elab (VF.EAnd exp1 tokAnd exp2) = elabEOp2 tokAnd exp1 exp2
  elab (VF.EOr exp1 tokOr exp2) = elabEOp2 tokOr exp1 exp2
  elab (VF.ENot tokNot exp) = elabEOp1 tokNot exp
  elab (VF.ETrue tokTrue) = elabECon tokTrue
  elab (VF.EFalse tokFalse) = elabECon tokFalse

  -- Integers and reals.
  elab (VF.EEq exp1 tokEq exp2) = elabEOp2 tokEq exp1 exp2
  elab (VF.ENeq exp1 tokNeq exp2) = elabEOp2 tokNeq exp1 exp2
  elab (VF.ELe exp1 tokLe exp2) = elabEOp2 tokLe exp1 exp2
  elab (VF.ELt exp1 tokLt exp2) = elabEOp2 tokLt exp1 exp2
  elab (VF.EGe exp1 tokGe exp2) = elabEOp2 tokGe exp1 exp2
  elab (VF.EGt exp1 tokGt exp2) = elabEOp2 tokGt exp1 exp2
  elab (VF.EMul exp1 tokMul exp2) = elabEOp2 tokMul exp1 exp2
  elab (VF.EDiv exp1 tokDiv exp2) = elabEOp2 tokDiv exp1 exp2
  elab (VF.EAdd exp1 tokAdd exp2) = elabEOp2 tokAdd exp1 exp2
  elab (VF.ESub exp1 tokSub exp2) = elabEOp2 tokSub exp1 exp2
  elab (VF.ENeg tokNeg exp) = elabEOp1 tokNeg exp
  elab (VF.ELitInt nat) = return $ VC.ELitInt nat
  elab (VF.ELitReal real) = return $ VC.ELitReal real

  -- Tensors.
  elab (VF.EAt exp1 tokAt exp2) = elabEOp2 tokAt exp1 exp2
  elab (VF.EAll tokAll) = elabECon tokAll
  elab (VF.EAny tokAny) = elabECon tokAny
  elab (VF.ELitTensor _tokSeqOpen exprs _tokSeqClose) = VC.ELitTensor <$> traverse elab exprs


instance Elab [VF.Decl] VC.Decl where

  -- Elaborate a network declaration.
  elab [VF.DeclNetw name _tokElemOf typ] =
    VC.DeclNetw <$> elab name <*> elab typ

  -- Elaborate a dataset declaration.
  elab [VF.DeclData name _tokElemOf typ] =
    VC.DeclData <$> elab name <*> elab typ

  -- Elaborate a dataset declaration.
  elab [VF.DefType name args typ] =
    VC.DefType <$> elab name <*> traverse elab args <*> elab typ

  -- Elaborate a function definition.
  elab [VF.DefFunType _name _tokElemOf typ, VF.DefFunExpr name args exp] =
    VC.DefFun <$> elab name <*> elab typ <*> elabELams args exp

  -- Why did you write the signature AFTER the function?
  elab [VF.DefFunExpr name1 args exp, VF.DefFunType name2 tokElemOf typ] =
    elab [VF.DefFunType name2 tokElemOf typ, VF.DefFunExpr name1 args exp]

  -- Missing type or expression declaration.
  elab [VF.DefFunType name _tokElemOf _typ] =
    throwError (MissingDefFunExpr name)

  elab [VF.DefFunExpr name _args _exp] =
    throwError (MissingDefFunType name)

  -- Multiple type of expression declarations with the same name.
  elab decls =
    throwError (DuplicateName (map declName decls))


instance Elab VF.Prog VC.Prog where
  elab (VF.Main decls) = VC.Main <$> elabDecls decls


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

{-# DEPRECATED elabTList "Use TLitList." #-}
elabTList :: MonadElab m => Position -> [VF.Type] -> m VC.Type
elabTList pos typs = foldr tCons tNil <$> traverse elab typs
  where
    tNil = VC.TCon (VC.Builtin (pos, "Nil"))
    tCons typ1 = VC.TApp (VC.TApp (VC.TCon (VC.Builtin (pos, "::"))) typ1)

-- |Elaborate a let binding with /multiple/ bindings to a series of let
--  bindings with a single binding each.
elabELets :: MonadElab m => [VF.Decl] -> VF.Expr -> m VC.Expr
elabELets decls exp = bindM2 (foldrM f) (elab exp) (elabDecls decls)
  where
    f :: MonadElab m => VC.Decl -> VC.Expr -> m VC.Expr
    f (VC.DefFun name typ exp1) exp2 = return $ VC.ELet (VC.MkEArg name typ) exp1 exp2
    f (VC.DeclNetw name _typ) _exp2 = throwError (LocalDeclNetw (coerce name))
    f (VC.DeclData name _typ) _exp2 = throwError (LocalDeclData (coerce name))
    f (VC.DefType name _args _typ) _exp2 = throwError (LocalDefType (coerce name))

-- |Elaborate a lambda abstraction with /multiple/ bindings to a series of
--  lambda abstractions with a single binding each.
elabELams :: MonadElab m => [VF.Name] -> VF.Expr -> m VC.Expr
elabELams args exp = liftA2 (foldr VC.ELam) (elab exp) (traverse elab args)

-- |Elaborate a type abstraction with /multiple/ bindings to a series of type
--  abstractions with a single binding each.
elabETyLams :: MonadElab m => [VF.Name] -> VF.Expr -> m VC.Expr
elabETyLams args exp = liftA2 (foldr VC.ETyLam) (elab exp) (traverse elab args)

-- |Elaborate a universal quantifier with /multiple/ bindings to a series of
--  universal quantifiers with a single binding each.
elabTForalls :: MonadElab m => [VF.Name] -> VF.Type -> m VC.Type
elabTForalls args typ1 = liftA2 (foldr VC.TForall) (elab typ1) (traverse elab args)

-- |Takes a list of declarations, and groups type and expression
--  declarations for the same name. If any name does not have exactly one
--  type and one expression declaration, an error is returned.
elabDecls :: MonadElab m => [VF.Decl] -> m [VC.Decl]
elabDecls decls = traverse elab (groupBy cond decls)
  where
    cond :: VF.Decl -> VF.Decl -> Bool
    cond decl1 decl2 = isDefFun decl1 &&
                       isDefFun decl2 &&
                       declName decl1 `sameName` declName decl2

-- |Generate a kind meta-variable.
kMeta :: MonadElab m => m VC.Kind
kMeta = VC.KMeta <$> supply

-- |Generate a type meta-variable.
tMeta :: MonadElab m => m VC.Type
tMeta = VC.TMeta <$> supply

-- |Elaborate any builtin token to a kind.
elabKCon :: (MonadElab m, Elab a VC.Builtin) => a -> m VC.Kind
elabKCon = fmap VC.KCon . elab

-- |Elaborate any builtin token to a type.
elabTCon :: (MonadElab m, Elab a VC.Builtin) => a -> m VC.Type
elabTCon = fmap VC.TCon . elab

-- |Elaborate any builtin token to an expression.
elabECon :: (MonadElab m, Elab a VC.Builtin) => a -> m VC.Expr
elabECon = fmap VC.ECon . elab

-- |Elaborate a unary function symbol with its argument to a type.
elabTOp1 :: (MonadElab m, Elab a VC.Builtin) => a -> VF.Type -> m VC.Type
elabTOp1 tokOp typ1 = VC.TApp <$> elabTCon tokOp <*> elab typ1

-- |Elaborate a binary function symbol with its arguments to a type.
elabTOp2 :: (MonadElab m, Elab a VC.Builtin) => a -> VF.Type -> VF.Type -> m VC.Type
elabTOp2 tokOp typ1 typ2 = VC.TApp <$> elabTOp1 tokOp typ1 <*> elab typ2

-- |Elaborate a unary function symbol with its argument to an expression.
elabEOp1 :: (MonadElab m, Elab a VC.Builtin) => a -> VF.Expr -> m VC.Expr
elabEOp1 tokOp exp1 = VC.EApp <$> elabECon tokOp <*> elab exp1

-- |Elaborate a binary function symbol with its arguments to an expression.
elabEOp2 :: (MonadElab m, Elab a VC.Builtin) => a -> VF.Expr -> VF.Expr -> m VC.Expr
elabEOp2 tokOp exp1 exp2 = VC.EApp <$> elabEOp1 tokOp exp1 <*> elab exp2

-- |Elaborate a ternary function symbol with its arguments to an expression.
elabEOp3 :: (MonadElab m, Elab a VC.Builtin) => a -> VF.Expr -> VF.Expr -> VF.Expr -> m VC.Expr
elabEOp3 tokOp exp1 exp2 exp3 = VC.EApp <$> elabEOp2 tokOp exp1 exp2 <*> elab exp3

-- |Check if a declaration is a network declaration.
isDefFun :: VF.Decl -> Bool
isDefFun (VF.DefFunType _name _elemOf _typ) = True
isDefFun (VF.DefFunExpr _name _args _exp) = True
isDefFun _ = False

-- |Get the name for any declaration.
declName :: VF.Decl -> VF.Name
declName (VF.DeclNetw name _elemOf _typ) = name
declName (VF.DeclData name _elemOf _typ) = name
declName (VF.DefType name _args _type) = name
declName (VF.DefFunType name _elemOf _typ) = name
declName (VF.DefFunExpr name _args _exp) = name

-- |Lift a binary /monadic/ function.
bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f ma mb = do a <- ma; b <- mb; f a b

-- |Positions in BNFC generated grammars are represented by a pair of a line
--  number and a column number.
type Position = (Int, Int)

-- |Constraint for newtypes which are /position tokens/. Depends on the fact
--  that any /position token/ generated by BNFC with @--text@ will be a newtype
--  wrapping '(Position, Text)', and hence all are coercible to it. This breaks
--  if the @--text@ option is not passed, or if the token is not marked with the
--  @position@ keyword.
type IsToken a = Coercible a (Position, Text)

posOf :: IsToken a => a -> Position
posOf x =
  let (pos, _tok) = coerce x :: (Position, Text) in pos

-- |Compare the text portion of any two position tokens.
sameName :: IsToken a => a -> a -> Bool
sameName x y =
  snd (coerce x :: (Position, Text)) == snd (coerce y :: (Position, Text))


-- * Convert various token types to constructors or variables

instance Elab VF.Name VC.TArg where elab name = VC.MkTArg <$> elab name <*> kMeta
instance Elab VF.Name VC.EArg where elab name = VC.MkEArg <$> elab name <*> tMeta
instance Elab VF.Name VC.Type where elab = fmap VC.TVar . elab
instance Elab VF.Name VC.Expr where elab = fmap VC.EVar . elab


-- * Convert various token types from Frontend to builtin type in Core

instance Elab VF.TokArrow  VC.Builtin where elab = return . coerce
instance Elab VF.TokForall VC.Builtin where elab = return . coerce
instance Elab VF.TokExists VC.Builtin where elab = return . coerce
instance Elab VF.TokIf     VC.Builtin where elab = return . coerce
instance Elab VF.TokThen   VC.Builtin where elab = return . coerce
instance Elab VF.TokElse   VC.Builtin where elab = return . coerce
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
instance Elab VF.TokNot    VC.Builtin where elab = return . coerce
instance Elab VF.TokAt     VC.Builtin where elab = return . coerce
instance Elab VF.TokType   VC.Builtin where elab = return . coerce
instance Elab VF.TokTensor VC.Builtin where elab = return . coerce
instance Elab VF.TokAll    VC.Builtin where elab = return . coerce
instance Elab VF.TokAny    VC.Builtin where elab = return . coerce
instance Elab VF.TokReal   VC.Builtin where elab = return . coerce
instance Elab VF.TokDim    VC.Builtin where elab = return . coerce
instance Elab VF.TokInt    VC.Builtin where elab = return . coerce
instance Elab VF.TokBool   VC.Builtin where elab = return . coerce
instance Elab VF.TokTrue   VC.Builtin where elab = return . coerce
instance Elab VF.TokFalse  VC.Builtin where elab = return . coerce
instance Elab VF.TokList   VC.Builtin where elab = return . coerce
instance Elab VF.TokNil    VC.Builtin where elab = return . coerce
instance Elab VF.TokCons   VC.Builtin where elab = return . coerce
instance Elab VF.Name      VC.Name    where elab = return . coerce

-- -}
-- -}
-- -}
-- -}
-- -}
