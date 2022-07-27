
module Vehicle.Language.StandardLibrary.Names where

import Vehicle.Language.AST.Core

isStdLibFunction :: Identifier -> Bool
isStdLibFunction ident = ident `elem` stdLibFunctions

stdLibFunctions :: [Identifier]
stdLibFunctions =
  [ StdExistsBool
  , StdForallBool
  , StdExistsIndex
  , StdForallIndex
  , StdExistsVector
  , StdForallVector
  , StdExistsInList
  , StdForallInList
  , StdExistsInVector
  , StdForallInVector
  , StdEqualsBool
  , StdNotEqualsBool
  , StdEqualsVector
  , StdNotEqualsVector
  , StdAddVector
  , StdSubVector
  ]

pattern StdExistsBool, StdForallBool :: Identifier
pattern StdExistsBool = Identifier "existsBool"
pattern StdForallBool = Identifier "forallBool"

pattern StdExistsIndex, StdForallIndex :: Identifier
pattern StdExistsIndex = Identifier "existsIndex"
pattern StdForallIndex = Identifier "forallIndex"

pattern StdExistsVector, StdForallVector :: Identifier
pattern StdExistsVector = Identifier "existsVector"
pattern StdForallVector = Identifier "forallVector"

pattern StdExistsInList, StdForallInList, StdExistsInVector, StdForallInVector :: Identifier
pattern StdExistsInList   = Identifier "existsInList"
pattern StdForallInList   = Identifier "forallInList"
pattern StdExistsInVector = Identifier "existsInVector"
pattern StdForallInVector = Identifier "forallInVector"

pattern StdEqualsBool, StdNotEqualsBool, StdEqualsVector, StdNotEqualsVector :: Identifier
pattern StdEqualsBool      = Identifier "equalsBool"
pattern StdNotEqualsBool   = Identifier "notEqualsBool"
pattern StdEqualsVector    = Identifier "equalsVector"
pattern StdNotEqualsVector = Identifier "notEqualsVector"

pattern StdAddVector, StdSubVector :: Identifier
pattern StdAddVector = Identifier "addVector"
pattern StdSubVector = Identifier "subVector"

pattern PostulateExistsNat, PostulateForallNat :: Identifier
pattern PostulateExistsNat = Identifier "existsNat"
pattern PostulateForallNat = Identifier "forallNat"

pattern PostulateExistsInt, PostulateForallInt :: Identifier
pattern PostulateExistsInt = Identifier "existsInt"
pattern PostulateForallInt = Identifier "forallInt"

pattern PostulateExistsRat, PostulateForallRat :: Identifier
pattern PostulateExistsRat = Identifier "existsRat"
pattern PostulateForallRat = Identifier "forallRat"
