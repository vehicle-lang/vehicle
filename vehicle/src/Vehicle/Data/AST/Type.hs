module Vehicle.Data.AST.Type where

--------------------------------------------------------------------------------
-- HasType

class HasType expr typ | expr -> typ where
  typeOf :: expr -> typ
