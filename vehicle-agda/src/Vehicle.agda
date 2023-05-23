{-# OPTIONS --allow-exec #-}

open import Agda.Builtin.FromNat

open import Data.Bool.Base using (T; Bool; if_then_else_)
open import Data.String using (String; _++_; lines)
open import Data.Nat.Base using (ℕ)
open import Data.Fin using (Fin)
import Data.Fin.Literals as Fin
import Data.Nat.Literals as Nat
open import Data.Vec.Base using (Vec)
open import Data.Float.Base using (Float; _≤ᵇ_)
open import Data.List.Base using (List; []; _∷_)
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
-- Checking
------------------------------------------------------------------------

record CheckArgs : Set where
  field
    proofCache : String

validateCmd : CheckArgs → CmdSpec
validateCmd checkArgs = cmdSpec VEHICLE_COMMAND
  ( "validate"
  ∷ ("--proofCache=" ++ proofCache)
  ∷ [] ) ""
  where open CheckArgs checkArgs

checkSuccessful : String → Bool
checkSuccessful output = "verified" ⊆ output

postulate valid : ∀ {a} {A : Set a} → A

`valid : Term
`valid = def (quote valid) (hArg unknown ∷ hArg unknown ∷ [])

checkSpecificationMacro : CheckArgs → Term → TC ⊤
checkSpecificationMacro args hole = do
  goal   ← inferType hole
  output ← runCmdTC (validateCmd args)
  if checkSuccessful output
    then unify hole `valid
    else typeError (strErr ("Error: " ++ output) ∷ [])

macro
  checkSpecification : CheckArgs → Term → TC ⊤
  checkSpecification = checkSpecificationMacro

------------------------------------------------------------------------
-- Other
------------------------------------------------------------------------

instance
  finNumber : ∀ {n} -> Number (Fin n)
  finNumber {n} = Fin.number n

  natNumber : Number ℕ
  natNumber = Nat.number
