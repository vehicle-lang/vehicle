{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Subsystem.Standard.Core
  ( module Vehicle.Compile.Type.Subsystem.Standard.Core,
    module Syntax,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Expr.DSL
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised
import Vehicle.Syntax.Builtin as Syntax

-----------------------------------------------------------------------------
-- Definition

type StandardBuiltin = Builtin

-----------------------------------------------------------------------------
-- Expressions

type StandardExpr = Expr Ix StandardBuiltin

type StandardBinder = Binder Ix StandardBuiltin

type StandardArg = Arg Ix StandardBuiltin

type StandardDecl = Decl Ix StandardBuiltin

type StandardProg = Prog Ix StandardBuiltin

type StandardType = StandardExpr

type StandardTelescope = Telescope Ix StandardBuiltin

type StandardTypingBoundCtx = TypingBoundCtx StandardBuiltin

type StandardInstanceGoal = InstanceGoal StandardBuiltin

type StandardInstanceCandidate = InstanceCandidate StandardBuiltin

-----------------------------------------------------------------------------
-- Norm expressions

type StandardNormDecl = VDecl StandardBuiltin

type StandardNormExpr = Value StandardBuiltin

type StandardNormBinder = VBinder StandardBuiltin

type StandardNormArg = VArg StandardBuiltin

type StandardNormType = VType StandardBuiltin

type StandardSpine = Spine StandardBuiltin

type StandardEnv = Env StandardBuiltin

type StandardNormDeclCtx = NormDeclCtx StandardBuiltin

-----------------------------------------------------------------------------
-- Glued expressions

type StandardGluedExpr = GluedExpr StandardBuiltin

type StandardGluedType = GluedType StandardBuiltin

type StandardGluedProg = GenericProg StandardGluedExpr

type StandardGluedDecl = GenericDecl StandardGluedExpr

type ImportedModules = [StandardGluedProg]

mergeImports :: ImportedModules -> StandardGluedProg -> StandardGluedProg
mergeImports imports userProg = Main $ concatMap (\(Main ds) -> ds) (imports <> [userProg])

-----------------------------------------------------------------------------
-- Constraints

type StandardConstraintProgress = ConstraintProgress StandardBuiltin

type StandardInstanceConstraint = InstanceConstraint StandardBuiltin

type StandardUnificationConstraint = UnificationConstraint StandardBuiltin

type StandardConstraintContext = ConstraintContext StandardBuiltin

type StandardConstraint = Constraint StandardBuiltin

-----------------------------------------------------------------------------

-- * Types post type-checking

type TypeCheckedBinder = Binder Ix StandardBuiltin

type TypeCheckedArg = Arg Ix StandardBuiltin

type TypeCheckedExpr = Expr Ix StandardBuiltin

type TypeCheckedType = Expr Ix StandardBuiltin

type TypeCheckedDecl = Decl Ix StandardBuiltin

type TypeCheckedProg = Prog Ix StandardBuiltin

--------------------------------------------------------------------------------
-- DSL

type StandardDSLExpr = DSLExpr StandardBuiltin

builtinTypeClass :: TypeClass -> StandardDSLExpr
builtinTypeClass = builtin . TypeClass

typeClass :: TypeClass -> NonEmpty StandardDSLExpr -> StandardDSLExpr
typeClass tc args = builtinTypeClass tc @@ args

hasEq :: EqualityOp -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
hasEq eq t1 t2 = typeClass (HasEq eq) [t1, t2]

hasOrd :: OrderOp -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
hasOrd ord t1 t2 = typeClass (HasOrd ord) [t1, t2]

hasQuantifier :: Quantifier -> StandardDSLExpr -> StandardDSLExpr
hasQuantifier q t = typeClass (HasQuantifier q) [t]

numOp2TypeClass :: TypeClass -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
numOp2TypeClass tc t1 t2 t3 = typeClass tc [t1, t2, t3]

hasAdd :: StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
hasAdd = numOp2TypeClass HasAdd

hasSub :: StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
hasSub = numOp2TypeClass HasSub

hasMul :: StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
hasMul = numOp2TypeClass HasMul

hasDiv :: StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
hasDiv = numOp2TypeClass HasDiv

hasNeg :: StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
hasNeg t1 t2 = typeClass HasNeg [t1, t2]

hasMap :: StandardDSLExpr -> StandardDSLExpr
hasMap tCont = typeClass HasMap [tCont]

hasFold :: StandardDSLExpr -> StandardDSLExpr
hasFold tCont = typeClass HasFold [tCont]

hasQuantifierIn :: Quantifier -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
hasQuantifierIn q tCont tElem tRes = typeClass (HasQuantifierIn q) [tCont, tElem, tRes]

hasNatLits :: StandardDSLExpr -> StandardDSLExpr
hasNatLits t = typeClass HasNatLits [t]

hasRatLits :: StandardDSLExpr -> StandardDSLExpr
hasRatLits t = typeClass HasRatLits [t]

hasVecLits :: StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
hasVecLits n d = typeClass HasVecLits [n, d]
