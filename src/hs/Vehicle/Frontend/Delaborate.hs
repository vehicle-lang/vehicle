{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Vehicle.Frontend.Delaborate
  ( DelabError(..)
  , runDelab
  ) where

import Control.Monad.Trans (lift)
import Control.Monad.State (MonadState(..), StateT, evalStateT, modify)
import Control.Monad.Except (MonadError(..), Except, runExcept)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Prettyprinter (pretty, (<+>))

import Vehicle.Core.AST qualified as VC hiding (Name(..))
import Vehicle.Frontend.AST qualified as VF
import Vehicle.Error
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Delaboration converts a program in the Core language to the Frontend language

runDelab :: VC.OutputProg -> Either DelabError VF.OutputProg
runDelab prog = runExcept $ evalStateT (unSOT $ delab prog) []

data DelabError
  = PartiallyAppliedOperator Provenance
  | UnsolvedMeta Provenance VC.Meta

instance MeaningfulError DelabError where
  details (PartiallyAppliedOperator p) = DError $ DeveloperError
    { provenance = p
    , problem = "Partially applied operator found at"
    }

  details (UnsolvedMeta p meta) = DError $ DeveloperError
    { provenance = p
    , problem    = "unsolved meta variable" <+> pretty meta
    }

-- * Delaboration class

--------------------------------------------------------------------------------
-- $sugar
-- The following definitions are tactics for refolding various bits of syntactic
-- sugar
--
-- The following pieces of syntactic sugar are unfolded here:
--
--   * @(forall a ?_ (forall b ?_ TYPE))@
--     is refolded to
--     @forall a b. TYPE@
--     (see 'delabTForalls')
--
--   * @(lambda ( x ?_ ) (lambda ( y ?_ ) EXPR))@
--     is unfolded to
--     @\x y -> EXPR@
--     (see 'delabELams')
--
--   * @(lambda { x ?_ } (lambda { y ?_ } EXPR))@
--     is unfolded to
--     @\{x y} -> EXPR@
--     (see 'delabELams')
--
--   * @(let ( x Nat ) 1 (let ( y Nat ) 2 EXPR@
--     is unfolded to
--     @let { x : Nat ; x = 1 ; y : Nat ; y = 2 } in EXPR@
--
--   * infix operators are rewritten from Polish notation, e.g.,
--     @(+ 1 5)@ is rewritten to @1 + 5@
--
--------------------------------------------------------------------------------


-- | Parameter "a" represents the type of the holes left in the expression.
--   Parameter b represents the type of the whole expression.
type Hole a b = StateT [a] (Except DelabError) b

hole :: Provenance -> Hole a a
hole p = get >>= \case
  []       -> throwError $ PartiallyAppliedOperator p
  (x : xs) -> do put xs ; return x

plug :: Hole a b -> Except DelabError b
plug h = evalStateT h []

plugFlow :: SortedOutputTree sort1 -> Hole VF.OutputExpr VF.OutputExpr
plugFlow h = lift (plug (unSOT h))

addArg :: a -> Hole a ()
addArg x = modify (x :)

-- TODO make HasProvenance work
outputProv :: VF.OutputAnn sort -> Provenance
outputProv = unK . isnd

delabBuiltin :: VF.OutputAnn
             -> VC.Builtin
             -> Hole VF.OutputExpr VF.OutputExpr
delabBuiltin ann = let p = outputProv ann in \case
  VC.Type    -> return $ VF.Type ann
  VC.PrimitiveTruth  TBool   -> return $ VF.Bool ann
  VC.PrimitiveTruth  TProp   -> return $ VF.Prop ann
  VC.PrimitiveNumber TInt    -> return $ VF.Int ann
  VC.PrimitiveNumber TReal   -> return $ VF.Real ann
  VC.List    -> VF.List   ann <$> hole p
  VC.Tensor  -> VF.Tensor ann <$> hole p <*> hole p
  VC.Add     -> VF.Add    ann <$> hole p <*> hole p
  VC.Cons    -> VF.Cons   ann <$> hole p <*> hole p
  VC.If      -> VF.If     ann <$> hole p <*> hole p <*> hole p
  VC.Impl    -> VF.Impl   ann <$> hole p <*> hole p
  VC.And     -> VF.And    ann <$> hole p <*> hole p
  VC.Or      -> VF.Or     ann <$> hole p <*> hole p
  VC.Not     -> VF.Not    ann <$> hole p
  VC.Eq      -> VF.Eq     ann <$> hole p <*> hole p
  VC.Neq     -> VF.Neq    ann <$> hole p <*> hole p
  VC.Le      -> VF.Le     ann <$> hole p <*> hole p
  VC.Lt      -> VF.Lt     ann <$> hole p <*> hole p
  VC.Ge      -> VF.Ge     ann <$> hole p <*> hole p
  VC.Gt      -> VF.Gt     ann <$> hole p <*> hole p
  VC.Mul     -> VF.Mul    ann <$> hole p <*> hole p
  VC.Div     -> VF.Div    ann <$> hole p <*> hole p
  VC.Add     -> VF.Add    ann <$> hole p <*> hole p
  VC.Sub     -> VF.Sub    ann <$> hole p <*> hole p
  VC.Neg     -> VF.Neg    ann <$> hole p
  VC.Cons    -> VF.Cons   ann <$> hole p <*> hole p
  VC.At      -> VF.At     ann <$> hole p <*> hole p
  VC.All     -> return $ VF.All ann
  VC.Any     -> return $ VF.Any ann

delabLiteral :: VF.OutputAnn
             -> VC.Literal
             -> Hole VF.OutputExpr VF.OutputExpr
delabLiteral = _

newtype SortedOutputTree =
  SOT { unSOT :: Hole VF.OutputExpr VF.OutputExpr }

delab :: forall sort. KnownSort sort => VC.OutputExpr -> SortedOutputTree
delab t = SOT $ do t <- VC.traverseTreeAnn delabAnn t; delabLayer t
  where
    delabLayer ::
      forall sort1.
      KnownSort sort1 =>
      VC.Tree (K Symbol) (VF.Info :*: K Provenance) sort1 ->
      Hole (VF.OutputTree sort1) (VF.OutputTree sort1)
    delabLayer = unSOT . VC.foldTree (SOT . delabF)

    delabAnn ::
      forall sort1 sort2.
      KnownSort sort1 =>
      (VC.Info (K Symbol) :*: K Provenance) sort1 ->
      Hole (VF.OutputTree sort2) ((VF.Info :*: K Provenance) sort1)
    delabAnn (VC.Info v :*: p) = case sortSing :: SSort sort1 of
      SKIND -> return $ VF.Info v :*: p
      -- TODO extract liftPlug
      STYPE -> do v <- plugFlow (delab v); return $ VF.Info v :*: p
      STARG -> do v <- plugFlow (delab v); return $ VF.Info v :*: p
      SEXPR -> do v <- plugFlow (delab v); return $ VF.Info v :*: p
      SEARG -> do v <- plugFlow (delab v); return $ VF.Info v :*: p
      SDECL -> return $ VF.Info v :*: p
      SPROG -> return $ VF.Info v :*: p

delabF :: KnownSort sort
       => VC.TreeF (K Symbol) VF.OutputAnn sort SortedOutputTree
       -> Hole (VF.OutputTree sort) (VF.OutputTree sort)
delabF (tree :: VC.TreeF (K Symbol) VF.OutputAnn sort SortedOutputTree) =
  case sortSing :: SSort sort of

 -- Kinds
  SKIND -> case tree of
    -- Annotation is not used here, as it was duplicated during elaboration
    VC.KAppF _ann k1 k2 -> do k2 <- unSOT k2; addArg k2; unSOT k1
    VC.KConF  ann op    -> delabBuiltin ann op
    VC.KMetaF ann i     -> throwError $ UnsolvedMeta (outputProv ann) i

  -- Types
  STYPE -> case tree of
    VC.TForallF     ann _k n t -> do n <- plugFlow n; t <- unSOT t; return $ VF.TForall ann (n :| []) t
    -- Annotation is not used here, as it was duplicated during elaboration
    VC.TAppF       _ann t1 t2 -> do t2 <- unSOT t2; addArg t2; unSOT t1
    VC.TVarF        ann n     -> return $ VF.TVar ann (unK n)
    VC.TConF        ann op    -> delabBuiltin ann op
    VC.TLitDimF     ann d     -> return $ VF.TLitDim ann d
    VC.TLitDimListF ann ts    -> do ts <- traverse unSOT ts; return $ VF.TLitDimList ann ts
    VC.TMetaF       ann i     -> throwError $ UnsolvedMeta (outputProv ann) i

  -- Type arguments
  STARG -> case tree of
    VC.TArgF ann n -> return $ VF.TArg ann (unK n)

  -- Expressions
  SEXPR -> case tree of
    VC.EAnnF     ann e t     -> do e <- unSOT e; t <- plugFlow t; return $ VF.EAnn ann e t
    VC.ELetF     ann n e1 e2 -> delabLet ann <$> plugFlow n <*> unSOT e1 <*> unSOT e2
    VC.ELamF     ann n e     -> do n <- plugFlow n; e <- unSOT e; return $ VF.ELam ann (n :| []) e
-- Annotation is not used here, as it was duplicated during elaboration
    VC.EAppF    _ann e1 e2   -> do e2 <- unSOT e2; addArg e2; unSOT e1
    VC.EVarF     ann n       -> return $ VF.EVar ann (unK n)
    VC.ETyAppF   ann e t     -> do e <- unSOT e; t <- plugFlow t; return $ VF.ETyApp ann e t
    VC.ETyLamF   ann n e     -> do n <- plugFlow n; e <- unSOT e; return $ VF.ETyLam ann (n :| []) e
    VC.EConF     ann op      -> delabBuiltin ann op
    VC.ELitIntF  ann z       -> return $ VF.ELitInt ann z
    VC.ELitRealF ann r       -> return $ VF.ELitReal ann r
    VC.ELitSeqF  ann es      -> do es <- traverse unSOT es; return $ VF.ELitSeq ann es

  -- Expression arguments
  SEARG -> case tree of
    VC.EArgF ann n -> return $ VF.EArg ann (unK n)

  -- Declarations
  SDECL -> case tree of
    VC.DeclNetwF ann n t    -> do n <- plugFlow n; t <- plugFlow t; return $ VF.DeclNetw ann n t
    VC.DeclDataF ann n t    -> do n <- plugFlow n; t <- plugFlow t; return $ VF.DeclData ann n t
    VC.DefTypeF  ann n ns t -> do n <- plugFlow n; t <- plugFlow t; ns <- traverse plugFlow ns; return $ VF.DefType ann n ns t
    VC.DefFunF   ann n t e  -> delabFun ann <$> plugFlow n <*> plugFlow t <*> plugFlow e

  -- Programs
  SPROG -> case tree of
    VC.MainF ann ds -> do ds <- traverse plugFlow ds; return $ VF.Main ann ds



delabLet :: VF.OutputAnn 'EXPR -> VF.OutputEArg -> VF.OutputExpr -> VF.OutputExpr -> VF.OutputExpr
delabLet ann n = VF.ELet1 ann (mempty :*: K (outputProv ann)) n (VF.unInfo $ ifst ann)

type FunDecomp = (VF.OutputExpr, [Either VF.OutputTArg VF.OutputEArg])

decomposeFun :: FunDecomp -> FunDecomp
decomposeFun (VF.ELam   _ ns body, args) = decomposeFun (body, args <> map Right (NonEmpty.toList ns))
decomposeFun (VF.ETyLam _ ns body, args) = decomposeFun (body, args <> map Left  (NonEmpty.toList ns))
decomposeFun result                      = result

delabFun :: VF.OutputAnn 'DECL -> VF.OutputEArg -> VF.OutputType -> VF.OutputExpr -> VF.OutputDecl
delabFun ann n t e = let (body , args) = decomposeFun (e , []) in VF.DefFun ann n t args body