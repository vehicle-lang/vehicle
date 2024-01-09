

module SafetyProof where

open import Algebra
import Algebra.Properties.CommutativeSemigroup as CommutativeSemigroupProperties
open import Data.List
open import Data.List.Relation.Unary.All using (All; []; _∷_)
open import Data.Fin.Patterns using (0F; 1F)
open import Data.Nat using (z≤n; s≤s)
open import Data.Integer using (+≤+; +<+; +_)
open import Data.Rational
open import Data.Rational.Properties
open import Data.Vec using (_∷_)
import Data.Vec.Functional as Vector
open import Data.Product using (_×_; _,_; uncurry)
open import Data.Sum
open import Level using (0ℓ)
open import Relation.Binary.PropositionalEquality
open import Relation.Nullary using (yes; no)

open import Vehicle.Data.Tensor
open import RationalUtils
import WindControllerSpec as Vehicle

open ≤-Reasoning

------------------------------------------------------------------------
-- Setup

toTensor : ℚ → ℚ → Tensor ℚ (2 ∷ [])
toTensor x y = Vector.fromList (x ∷ y ∷ [])

roadWidth : ℚ
roadWidth = + 3 / 1

maxWindShift : ℚ
maxWindShift = 1ℚ

maxSensorError : ℚ
maxSensorError = + 1 / 4

roadWidth≥0 : roadWidth ≥ 0ℚ
roadWidth≥0 = *≤* (+≤+ z≤n)

maxWindShift≥0 : maxWindShift ≥ 0ℚ
maxWindShift≥0 = *≤* (+≤+ z≤n)

maxSensorError≥0 : maxSensorError ≥ 0ℚ
maxSensorError≥0 = *≤* (+≤+ z≤n)

------------------------------------------------------------------------
-- Model data

record State : Set where
  constructor state
  field
    windSpeed : ℚ
    position  : ℚ
    velocity  : ℚ
    sensor    : ℚ

open State

record Observation : Set where
  constructor observe
  field
    windShift   : ℚ
    sensorError : ℚ

open Observation

------------------------------------------------------------------------
-- Model transitions

initialState : State
initialState = record
  { windSpeed = 0ℚ
  ; position  = 0ℚ
  ; velocity  = 0ℚ
  ; sensor    = 0ℚ
  }

controller : ℚ → ℚ → ℚ
controller x y = Vehicle.controller (Vehicle.normalise (toTensor x y)) 0F

nextState : Observation → State → State
nextState o s = record
  { windSpeed = newWindSpeed
  ; position  = newPosition
  ; velocity  = newVelocity
  ; sensor    = newSensor
  }
  where
  newWindSpeed = windSpeed s + windShift o
  newPosition  = position s + velocity s + newWindSpeed
  newSensor    = newPosition + sensorError o
  newVelocity  = velocity s + controller newSensor (sensor s)

finalState : List Observation → State
finalState xs = foldr nextState initialState xs

------------------------------------------------------------------------
-- Definition of correctness

nextPosition-windShift : State → ℚ
nextPosition-windShift s = position s + velocity s + windSpeed s

-- The vehicle is on the road if its position is less than the
-- width of the road.
OnRoad : State → Set
OnRoad s = ∣ position s ∣ ≤ roadWidth

-- The vehicle is in a "safe" state if it's more than the maxWindShift away
-- from the edge of the road.
SafeDistanceFromEdge : State → Set
SafeDistanceFromEdge s = ∣ nextPosition-windShift s ∣ < roadWidth - maxWindShift

-- The vehicle's previous sensor reading is accurate if it is no more than the
-- maximum error away from it's true location.
AccurateSensorReading : State → Set
AccurateSensorReading s = ∣ position s - sensor s ∣ ≤ maxSensorError

-- The vehicle's previous sensor reading was not off the road
SensorReadingNotOffRoad : State → Set
SensorReadingNotOffRoad s = ∣ sensor s ∣ ≤ roadWidth + maxSensorError

-- A state is safe if it both a safe distance from the edge and it's sensor
-- reading is accurate.
SafeState : State → Set
SafeState s = SafeDistanceFromEdge s
            × AccurateSensorReading s
            × SensorReadingNotOffRoad s

-- An observation is valid if the observed sensor error and the wind shift
-- are less than the expected maximum shifts.
ValidObservation : Observation → Set
ValidObservation o = ∣ sensorError o ∣ ≤ maxSensorError
                   × ∣ windShift   o ∣ ≤ maxWindShift

------------------------------------------------------------------------
-- Proof of correctness

-- Initial state is both a safe distance from the edge and on the road

initialState-onRoad : OnRoad initialState
initialState-onRoad = roadWidth≥0

initialState-safe : SafeState initialState
initialState-safe =
  *<* (+<+ (s≤s z≤n)) ,
  *≤* (+≤+ z≤n) ,
  *≤* (+≤+ z≤n)

-- Transitions are well-behaved

controller-lem : ∀ x y →
                 ∣ x ∣ ≤ roadWidth + maxSensorError →
                 ∣ y ∣ ≤ roadWidth + maxSensorError →
                 ∣ controller x y + 2ℚ * x - y ∣ < roadWidth - maxWindShift - 3ℚ * maxSensorError
controller-lem x y ∣x∣≤rw+εₘₐₓ ∣y∣≤rw+εₘₐₓ =
  uncurry -p<q<p⇒∣q∣<p (Vehicle.safe (toTensor x y) (λ
    { 0F → ∣p∣≤q⇒-q≤p≤q x ∣x∣≤rw+εₘₐₓ
    ; 1F → ∣p∣≤q⇒-q≤p≤q y ∣y∣≤rw+εₘₐₓ
    }))

valid⇒nextState-accurateSensor : ∀ o → ValidObservation o → ∀ s →
                                 AccurateSensorReading (nextState o s)
valid⇒nextState-accurateSensor o (ε≤εₘₐₓ , _) s = let s' = nextState o s in begin
  ∣ position s' - sensor s' ∣                     ≡⟨⟩
  ∣ position s' - (position s' + sensorError o) ∣ ≡⟨ cong ∣_∣ (p-[p+q]≡q (position s') (sensorError o)) ⟩
  ∣ sensorError o ∣                               ≤⟨ ε≤εₘₐₓ ⟩
  maxSensorError                                  ∎

valid+safe⇒nextState-onRoad : ∀ o → ValidObservation o →
                              ∀ s → SafeState s →
                              OnRoad (nextState o s)
valid+safe⇒nextState-onRoad o (_ , δw≤δwₘₐₓ) s (safeDist , _ , _) = begin
  ∣ position s + velocity s + (windSpeed s + windShift o) ∣ ≡˘⟨ cong ∣_∣ (+-assoc (position s + velocity s) (windSpeed s) (windShift o)) ⟩
  ∣ position s + velocity s + windSpeed s + windShift o   ∣ ≤⟨ ∣p+q∣≤∣p∣+∣q∣ (position s + velocity s + windSpeed s) (windShift o) ⟩
  ∣ nextPosition-windShift s ∣ + ∣ windShift o ∣            ≤⟨ +-mono-≤ (<⇒≤ safeDist) δw≤δwₘₐₓ ⟩
  roadWidth - maxWindShift + maxWindShift                   ≡⟨ p-q+q≡p roadWidth maxWindShift ⟩
  roadWidth                                                 ∎

valid+safe⇒nextState-sensorReadingNotOffRoad : ∀ s → SafeState s → ∀ o → ValidObservation o →
                                               SensorReadingNotOffRoad (nextState o s)
valid+safe⇒nextState-sensorReadingNotOffRoad s safe o valid@(ε≤εₘₐₓ , _) = begin
  ∣ position (nextState o s) + sensorError o ∣       ≤⟨ ∣p+q∣≤∣p∣+∣q∣ (position s + velocity s + (windSpeed s + windShift o)) (sensorError o) ⟩
  ∣ position (nextState o s) ∣ + ∣ sensorError o ∣   ≤⟨ +-mono-≤ (valid+safe⇒nextState-onRoad o valid s safe) ε≤εₘₐₓ ⟩
  roadWidth + maxSensorError                         ∎

valid+safe⇒nextState-safeDistanceFromEdge : ∀ o → ValidObservation o →
                                            ∀ s → SafeState s →
                                            SafeDistanceFromEdge (nextState o s)
valid+safe⇒nextState-safeDistanceFromEdge o valid@(ε-accurate , _) s safe@(safeDist , ε'-accurate , s-notOffRoad) = let
    s' = nextState o s
    y  = position  s; y' = position  s'
    v  = velocity  s; v' = velocity  s'
    w  = windSpeed s; w' = windSpeed s'
    p  = sensor    s; p' = sensor    s'
    ε  = y - p;       ε' = sensorError o
    dw = windShift   o
    dv = controller p' p
    ∣dv+2p'-p∣ = ∣ dv + 2ℚ * p' - p ∣
    s'-notOffRoad = valid+safe⇒nextState-sensorReadingNotOffRoad s safe o valid
  in begin-strict
  ∣ y' + v' + w' ∣                                                  ≡⟨ cong ∣_∣ (p+q-q≡p (y' + v' + w') _) ⟨
  ∣ y' + (v + dv) + w' + (y + ε' + ε' - p) - (y + ε' + ε' - p) ∣    ≡⟨ cong ∣_∣ (lem1 y' v dv w' y ε' p) ⟩
  ∣ dv + (y' + ε' + (y + v + w' + ε')) - p - (ε' + ε' + (y - p)) ∣  ≡⟨⟩
  ∣ dv + (p' + p') - p - (ε' + ε' + (y - p)) ∣                      ≡⟨ cong ∣_∣ (cong₂ (λ a b → dv + a - p - (b + (y - p))) (2*p≡p+p p') (2*p≡p+p ε')) ⟨
  ∣ dv + 2ℚ * p' - p - (2ℚ * ε' + (y - p)) ∣                        ≤⟨ ∣p-q∣≤∣p∣+∣q∣ (dv + 2ℚ * p' - p) (2ℚ * ε' + (y - p)) ⟩
  ∣ dv + 2ℚ * p' - p ∣ + ∣ 2ℚ * ε' + (y - p) ∣                      ≤⟨ +-monoʳ-≤ (∣ dv + 2ℚ * p' - p ∣) (∣p+q∣≤∣p∣+∣q∣ (2ℚ * ε') (y - p)) ⟩
  ∣ dv + 2ℚ * p' - p ∣ + (∣ 2ℚ * ε' ∣ + ∣ y - p ∣)                  ≡⟨ cong (λ v → ∣dv+2p'-p∣ + v) (cong (_+ _) (∣p*q∣≡∣p∣*∣q∣ 2ℚ ε')) ⟩
  ∣ dv + 2ℚ * p' - p ∣ + (2ℚ * ∣ ε' ∣ + ∣ ε ∣)                      ≤⟨ +-monoʳ-≤ (∣ dv + 2ℚ * p' - p ∣) (+-mono-≤ (*-monoʳ-≤ 2ℚ _ ε-accurate) ε'-accurate) ⟩
  ∣ dv + 2ℚ * p' - p ∣ + (2ℚ * maxSensorError + maxSensorError)     ≡⟨ cong (λ v → ∣dv+2p'-p∣ + v) (2p+p≡3p maxSensorError) ⟩
  ∣ dv + 2ℚ * p' - p ∣ + 3ℚ * maxSensorError                        <⟨ p<r-q⇒p+q<r _ _ _ (controller-lem p' p s'-notOffRoad s-notOffRoad) ⟩
  roadWidth - maxWindShift                                          ∎

safe⇒nextState-safe : ∀ s → SafeState s → ∀ o → ValidObservation o → SafeState (nextState o s)
safe⇒nextState-safe s safe o valid =
  valid+safe⇒nextState-safeDistanceFromEdge o valid s safe ,
  valid⇒nextState-accurateSensor o valid s ,
  valid+safe⇒nextState-sensorReadingNotOffRoad s safe o valid

finalState-safe : ∀ xs → All ValidObservation xs → SafeState (finalState xs)
finalState-safe []       []         = initialState-safe
finalState-safe (x ∷ xs) (px ∷ pxs) = safe⇒nextState-safe (finalState xs) (finalState-safe xs pxs) x px

finalState-onRoad : ∀ xs → All ValidObservation xs → OnRoad (finalState xs)
finalState-onRoad []       []         = initialState-onRoad
finalState-onRoad (x ∷ xs) (px ∷ pxs) = valid+safe⇒nextState-onRoad x px (finalState xs) (finalState-safe xs pxs)
