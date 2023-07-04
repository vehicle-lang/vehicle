module Vehicle.Backend.LossFunction.Syntax where

import Vehicle.Backend.JSON as J
import Vehicle.Expr.DeBruijn (Ix)
import Vehicle.Syntax.AST

data LBuiltin

type LProg = Prog Ix JBuiltin

type LDecl = Decl Ix JBuiltin

type LExpr = Expr Ix JBuiltin

type LBinder = Binder Ix JBuiltin

type LArg = Arg Ix JBuiltin
