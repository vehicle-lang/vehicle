(******************************************************************************)
(* Vehicle plugin loader: provides the vehicle_validate tactic.               *)
(*                                                                            *)
(* When generated Rocq files contain                                          *)
(*   Lemma p : <type>.                                                        *)
(*   Proof. vehicle_validate "/path/to/cache". Qed.                           *)
(* this module loads the OCaml plugin that, at Qed time, invokes              *)
(*   vehicle validate --cache=/path/to/cache                                  *)
(* and closes the goal via the cache_witness axiom when validation succeeds.  *)
(*                                                                            *)
(* The axiom is the trusted base — analogous to vehicle-agda's `valid`        *)
(* postulate. It is encapsulated below so client code cannot apply it         *)
(* without going through the tactic.                                          *)
(******************************************************************************)

Declare ML Module "vehicle-rocq.plugin".

Module Private.
  Axiom cache_witness : forall A : Type, A.
End Private.

Register Private.cache_witness as vehicle.cache_witness.
