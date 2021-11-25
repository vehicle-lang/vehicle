{-# OPTIONS --allow-exec #-}

open import Data.Bool.Base using (T; Bool; if_then_else_)
open import Data.String using (String; _++_; lines)
open import Data.Nat.Base using (ℕ)
open import Data.Vec.Base using (Vec)
open import Data.Float.Base using (Float; _≤ᵇ_)
open import Data.List.Base using ([]; _∷_)
open import Data.Unit.Base using (⊤; tt)
open import Relation.Nullary using (does)
open import Relation.Binary.Core using (Rel)

open import Reflection.Argument
open import Reflection.Term
open import Reflection.External
open import Reflection.TypeChecking.Monad
open import Reflection.TypeChecking.Monad.Syntax
open import Reflection.Show using (showTerm)

open import Vehicle.Utils

module Vehicle where

------------------------------------------------------------------------
-- Metadata
------------------------------------------------------------------------

VEHICLE_COMMAND : String
VEHICLE_COMMAND = "vehicle"

------------------------------------------------------------------------
-- Execution
------------------------------------------------------------------------

record EvaluateArgs : Set where
  field
    databasePath : String
    networkUUID  : String

evaluateCmd : EvaluateArgs → CmdSpec
evaluateCmd args = cmdSpec VEHICLE_COMMAND
  ( "evaluate"
  ∷ databasePath
  ∷ networkUUID
  ∷ []) ""
  where open EvaluateArgs args

-- TODO
postulate evaluate : EvaluateArgs → ∀ {m n : ℕ} → Vec Float m → Vec Float n


------------------------------------------------------------------------
-- Checking
------------------------------------------------------------------------

record CheckArgs : Set where
  field
    databasePath : String
    networkUUID  : String
    propertyUUID : String

checkCmd : CheckArgs → CmdSpec
checkCmd checkArgs = cmdSpec VEHICLE_COMMAND
  ( "check"
  ∷ databasePath
  ∷ networkUUID
  ∷ propertyUUID
  ∷ []) ""
  where open CheckArgs checkArgs

checkSuccessful : String → Bool
checkSuccessful output = "Property verified" ⊆ output

postulate valid : ∀ {a} {A : Set a} → A

`valid : Term
`valid = def (quote valid) (hArg unknown ∷ [])

checkPropertyMacro : CheckArgs → Term → TC ⊤
checkPropertyMacro args hole = do
  goal   ← inferType hole
  -- (showTerm goal)
  output ← runCmdTC (checkCmd args)
  if checkSuccessful output
    then unify hole `valid
    else typeError (strErr ("Error: " ++ output) ∷ [])

macro
  checkProperty : CheckArgs → Term → TC ⊤
  checkProperty = checkPropertyMacro
