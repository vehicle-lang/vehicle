
module Vehicle.Language.Postulates 
  ( standardPostulates
  ) where

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Language.DSL

standardPostulates :: [DBDecl]
standardPostulates = quantifiers

quantifiers :: [DBDecl]
quantifiers =
  [ postulate "forallNats" $ (tNat ~> tBool) ~> tBool
  , postulate "forallInts" $ (tInt ~> tBool) ~> tBool
  , postulate "forallRats" $ (tRat ~> tBool) ~> tBool
  , postulate "existsNats" $ (tNat ~> tBool) ~> tBool
  , postulate "existsInts" $ (tInt ~> tBool) ~> tBool
  , postulate "existsRats" $ (tRat ~> tBool) ~> tBool
  ]

postulate :: Symbol -> DSLExpr -> DBDecl
postulate name t = DefPostulate mempty (Identifier name) (fromDSL mempty t)
