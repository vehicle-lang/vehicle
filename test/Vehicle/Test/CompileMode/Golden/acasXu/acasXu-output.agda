-- WARNING: This file was generated automatically by Vehicle
-- and should not be modified manually!
-- Metadata
--  - Agda version: 2.6.2
--  - AISEC version: 0.1.0.1
--  - Time generated: ???

{-# OPTIONS --allow-exec #-}

open import Vehicle
open import Data.Product
open import Data.Sum
open import Data.Integer as ℤ using (ℤ)
open import Data.Rational as ℚ using (ℚ)
open import Data.Fin as Fin using (Fin; #_)
open import Data.Vec.Functional
open import Relation.Binary.PropositionalEquality
open import Relation.Nullary

module acasXu-output where

InputVector : Set
InputVector = Vector ℚ 5

distanceToIntruder[Index-5] : Fin 5
distanceToIntruder[Index-5] = # 0

angleToIntruder[Index-5] : Fin 5
angleToIntruder[Index-5] = # 1

intruderHeading[Index-5] : Fin 5
intruderHeading[Index-5] = # 2

speed[Index-5] : Fin 5
speed[Index-5] = # 3

intruderSpeed[Index-5] : Fin 5
intruderSpeed[Index-5] = # 4

OutputVector : Set
OutputVector = Vector ℚ 5

clearOfConflict[Index-5] : Fin 5
clearOfConflict[Index-5] = # 0

weakLeft[Index-5] : Fin 5
weakLeft[Index-5] = # 1

weakRight : ∀ {_x0 : Set} {{_x1 : HasNatLits _x0}} → _x0
weakRight = _x3 2

strongLeft[Index-5] : Fin 5
strongLeft[Index-5] = # 3

strongRight[Index-5] : Fin 5
strongRight[Index-5] = # 4

postulate acasXu : InputVector → OutputVector

pi : ℚ
pi = ℤ.+ 392699 ℚ./ 125000

Advises : Fin 5 → (InputVector → Set)
Advises i x = ∀ (j : Fin 5) → i ≢ j → acasXu x i ℚ.< acasXu x j

IntruderDistantAndSlower : InputVector → Set
IntruderDistantAndSlower x = x distanceToIntruder[Index-5] ℚ.≥ ℤ.+ 55947691 ℚ./ 1000 × (x speed[Index-5] ℚ.≥ ℤ.+ 1145 ℚ./ 1 × x intruderSpeed[Index-5] ℚ.≤ ℤ.+ 60 ℚ./ 1)

abstract
  property1 : ∀ (x : Vector ℚ 5) → IntruderDistantAndSlower x → acasXu x clearOfConflict[Index-5] ℚ.≤ ℤ.+ 1500 ℚ./ 1
  property1 = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }

abstract
  property2 : ∀ (x : Vector ℚ 5) → IntruderDistantAndSlower x → ∃ λ (j : Fin 5) → acasXu x j ℚ.> acasXu x clearOfConflict[Index-5]
  property2 = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }

DirectlyAhead : InputVector → Set
DirectlyAhead x = (ℤ.+ 1500 ℚ./ 1 ℚ.≤ x distanceToIntruder[Index-5] × x distanceToIntruder[Index-5] ℚ.≤ ℤ.+ 1800 ℚ./ 1) × (ℚ.- (ℤ.+ 3 ℚ./ 50) ℚ.≤ x angleToIntruder[Index-5] × x angleToIntruder[Index-5] ℚ.≤ ℤ.+ 3 ℚ./ 50)

MovingTowards : InputVector → Set
MovingTowards x = x intruderHeading[Index-5] ℚ.≥ ℤ.+ 31 ℚ./ 10 × (x speed[Index-5] ℚ.≥ ℤ.+ 980 ℚ./ 1 × x intruderSpeed[Index-5] ℚ.≥ ℤ.+ 960 ℚ./ 1)

abstract
  property3 : ∀ (x : Vector ℚ 5) → DirectlyAhead x × MovingTowards x → ¬ Advises clearOfConflict[Index-5] x
  property3 = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }

MovingAway : InputVector → Set
MovingAway x = x intruderHeading[Index-5] ≡ ℤ.+ 0 ℚ./ 1 × (ℤ.+ 1000 ℚ./ 1 ℚ.≤ x speed[Index-5] × (ℤ.+ 700 ℚ./ 1 ℚ.≤ x intruderSpeed[Index-5] × x intruderSpeed[Index-5] ℚ.≤ ℤ.+ 800 ℚ./ 1))

abstract
  property4 : ∀ (x : Vector ℚ 5) → DirectlyAhead x × MovingAway x → ¬ Advises clearOfConflict[Index-5] x
  property4 = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }

NearAndApproachingFromLeft : InputVector → Set
NearAndApproachingFromLeft x = (ℤ.+ 250 ℚ./ 1 ℚ.≤ x distanceToIntruder[Index-5] × x distanceToIntruder[Index-5] ℚ.≤ ℤ.+ 400 ℚ./ 1) × ((ℤ.+ 1 ℚ./ 5 ℚ.≤ x angleToIntruder[Index-5] × x angleToIntruder[Index-5] ℚ.≤ ℤ.+ 2 ℚ./ 5) × ((ℚ.- pi ℚ.≤ x intruderHeading[Index-5] × x intruderHeading[Index-5] ℚ.≤ ℚ.- pi ℚ.+ ℤ.+ 1 ℚ./ 200) × ((ℤ.+ 100 ℚ./ 1 ℚ.≤ x speed[Index-5] × x speed[Index-5] ℚ.≤ ℤ.+ 400 ℚ./ 1) × (ℤ.+ 0 ℚ./ 1 ℚ.≤ x intruderSpeed[Index-5] × x intruderSpeed[Index-5] ℚ.≤ ℤ.+ 400 ℚ./ 1))))

abstract
  property5 : ∀ (x : Vector ℚ 5) → NearAndApproachingFromLeft x → Advises strongRight[Index-5] x
  property5 = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }

IntruderFarAway : InputVector → Set
IntruderFarAway x = (ℤ.+ 12000 ℚ./ 1 ℚ.≤ x distanceToIntruder[Index-5] × x distanceToIntruder[Index-5] ℚ.≤ ℤ.+ 62000 ℚ./ 1) × ((ℚ.- pi ℚ.≤ x angleToIntruder[Index-5] × x angleToIntruder[Index-5] ℚ.≤ ℚ.- (ℤ.+ 7 ℚ./ 10) ⊎ ℤ.+ 7 ℚ./ 10 ℚ.≤ x angleToIntruder[Index-5] × x angleToIntruder[Index-5] ℚ.≤ pi) × ((ℚ.- pi ℚ.≤ x intruderHeading[Index-5] × x intruderHeading[Index-5] ℚ.≤ ℚ.- pi ℚ.+ ℤ.+ 1 ℚ./ 200) × ((ℤ.+ 100 ℚ./ 1 ℚ.≤ x speed[Index-5] × x speed[Index-5] ℚ.≤ ℤ.+ 1200 ℚ./ 1) × (ℤ.+ 0 ℚ./ 1 ℚ.≤ x intruderSpeed[Index-5] × x intruderSpeed[Index-5] ℚ.≤ ℤ.+ 1200 ℚ./ 1))))

abstract
  property6 : ∀ (x : Vector ℚ 5) → IntruderFarAway x → Advises clearOfConflict[Index-5] x
  property6 = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }

LargeVerticalSeparation : InputVector → Set
LargeVerticalSeparation x = (ℤ.+ 0 ℚ./ 1 ℚ.≤ x distanceToIntruder[Index-5] × x distanceToIntruder[Index-5] ℚ.≤ ℤ.+ 60760 ℚ./ 1) × ((ℚ.- pi ℚ.≤ x angleToIntruder[Index-5] × x angleToIntruder[Index-5] ℚ.≤ pi) × ((ℚ.- pi ℚ.≤ x intruderHeading[Index-5] × x intruderHeading[Index-5] ℚ.≤ pi) × ((ℤ.+ 100 ℚ./ 1 ℚ.≤ x speed[Index-5] × x speed[Index-5] ℚ.≤ ℤ.+ 1200 ℚ./ 1) × (ℤ.+ 0 ℚ./ 1 ℚ.≤ x intruderSpeed[Index-5] × x intruderSpeed[Index-5] ℚ.≤ ℤ.+ 1200 ℚ./ 1))))

abstract
  property7 : ∀ (x : Vector ℚ 5) → LargeVerticalSeparation x → ¬ Advises strongLeft[Index-5] x × ¬ Advises strongRight[Index-5] x
  property7 = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }

LargeVerticalSeparationAndPreviousWeakLeft : InputVector → Set
LargeVerticalSeparationAndPreviousWeakLeft x = (ℤ.+ 0 ℚ./ 1 ℚ.≤ x distanceToIntruder[Index-5] × x distanceToIntruder[Index-5] ℚ.≤ ℤ.+ 60760 ℚ./ 1) × ((ℚ.- pi ℚ.≤ x angleToIntruder[Index-5] × x angleToIntruder[Index-5] ℚ.≤ ℚ.- (ℤ.+ 3 ℚ./ 4) ℚ.* pi) × ((ℚ.- (ℤ.+ 1 ℚ./ 10) ℚ.≤ x intruderHeading[Index-5] × x intruderHeading[Index-5] ℚ.≤ ℤ.+ 1 ℚ./ 10) × ((ℤ.+ 600 ℚ./ 1 ℚ.≤ x speed[Index-5] × x speed[Index-5] ℚ.≤ ℤ.+ 1200 ℚ./ 1) × (ℤ.+ 600 ℚ./ 1 ℚ.≤ x intruderSpeed[Index-5] × x intruderSpeed[Index-5] ℚ.≤ ℤ.+ 1200 ℚ./ 1))))

abstract
  property8 : ∀ (x : Vector ℚ 5) → LargeVerticalSeparationAndPreviousWeakLeft x → Advises clearOfConflict[Index-5] x ⊎ Advises weakLeft[Index-5] x
  property8 = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }

PreviousWeakRightAndNearbyIntruder : InputVector → Set
PreviousWeakRightAndNearbyIntruder x = (ℤ.+ 2000 ℚ./ 1 ℚ.≤ x distanceToIntruder[Index-5] × x distanceToIntruder[Index-5] ℚ.≤ ℤ.+ 7000 ℚ./ 1) × ((ℚ.- (ℤ.+ 2 ℚ./ 5) ℚ.≤ x angleToIntruder[Index-5] × x angleToIntruder[Index-5] ℚ.≤ ℚ.- (ℤ.+ 7 ℚ./ 50)) × ((ℚ.- pi ℚ.≤ x intruderHeading[Index-5] × x intruderHeading[Index-5] ℚ.≤ ℚ.- pi ℚ.+ ℤ.+ 1 ℚ./ 100) × ((ℤ.+ 100 ℚ./ 1 ℚ.≤ x speed[Index-5] × x speed[Index-5] ℚ.≤ ℤ.+ 150 ℚ./ 1) × (ℤ.+ 0 ℚ./ 1 ℚ.≤ x intruderSpeed[Index-5] × x intruderSpeed[Index-5] ℚ.≤ ℤ.+ 150 ℚ./ 1))))

abstract
  property9 : ∀ (x : Vector ℚ 5) → PreviousWeakRightAndNearbyIntruder x → Advises strongLeft[Index-5] x
  property9 = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }

IntruderFarAway2 : InputVector → Set
IntruderFarAway2 x = (ℤ.+ 36000 ℚ./ 1 ℚ.≤ x distanceToIntruder[Index-5] × x distanceToIntruder[Index-5] ℚ.≤ ℤ.+ 60760 ℚ./ 1) × ((ℤ.+ 7 ℚ./ 10 ℚ.≤ x angleToIntruder[Index-5] × x angleToIntruder[Index-5] ℚ.≤ pi) × ((ℚ.- pi ℚ.≤ x intruderHeading[Index-5] × x intruderHeading[Index-5] ℚ.≤ ℚ.- pi ℚ.+ ℤ.+ 1 ℚ./ 100) × ((ℤ.+ 900 ℚ./ 1 ℚ.≤ x speed[Index-5] × x speed[Index-5] ℚ.≤ ℤ.+ 1200 ℚ./ 1) × (ℤ.+ 600 ℚ./ 1 ℚ.≤ x intruderSpeed[Index-5] × x intruderSpeed[Index-5] ℚ.≤ ℤ.+ 1200 ℚ./ 1))))

abstract
  property10 : ∀ (x : Vector ℚ 5) → IntruderFarAway2 x → Advises clearOfConflict[Index-5] x
  property10 = checkSpecification record
    { proofCache   = "/home/matthew/Code/AISEC/vehicle/proofcache.vclp"
    }