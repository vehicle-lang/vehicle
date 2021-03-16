module Vehicle.Frontend.Type
  ( module Vehicle.Frontend.Abs
  , isDefFun
  , declName
  ) where

import Vehicle.Frontend.Abs

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
