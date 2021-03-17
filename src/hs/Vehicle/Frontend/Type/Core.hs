module Vehicle.Frontend.Type.Core
  ( module X
  , isDefFun
  , declName
  ) where

import Vehicle.Frontend.Abs as X

-- |Check if a declaration is a network declaration.
isDefFun :: Decl -> Bool
isDefFun (DefFunType _name _elemOf _typ) = True
isDefFun (DefFunExpr _name _args _exp) = True
isDefFun _ = False

-- |Get the name for any declaration.
declName :: Decl -> Name
declName (DeclNetw name _elemOf _typ) = name
declName (DeclData name _elemOf _typ) = name
declName (DefType name _args _type) = name
declName (DefFunType name _elemOf _typ) = name
declName (DefFunExpr name _args _exp) = name
