(************************************************************************)
(*                                                                       *)
(*  Vehicle plugin for Rocq: validates @property declarations by         *)
(*  invoking `vehicle validate --cache=<path>` and admitting the goal    *)
(*  via a hidden axiom when validation succeeds.                         *)
(*                                                                       *)
(*  Analogous to vehicle-agda's checkSpecification macro.                *)
(************************************************************************)

(** Build the shell command. Use CUnix.sys_command-style argv list so we
    don't have to deal with shell quoting. CUnix.run_command takes a
    single string command — we use Filename.quote on the path to be safe. *)
let validate_command cache_path =
  Printf.sprintf "vehicle validate --cache=%s" (Filename.quote cache_path)

let check_successful output =
  (* Match Agda's substring check on "verified" *)
  let target = "verified" in
  let lt = String.length target in
  let lo = String.length output in
  if lo < lt then false
  else
    let rec scan i =
      if i + lt > lo then false
      else if String.sub output i lt = target then true
      else scan (i + 1)
    in
    scan 0

(** Run `vehicle validate --cache=<path>` and return (success, output). *)
let run_validate cache_path =
  let cmd = validate_command cache_path in
  let (status, output) = CUnix.run_command cmd in
  let success =
    match status with
    | Unix.WEXITED 0 -> check_successful output
    | _ -> false
  in
  (success, output)

(** The tactic. On success, refines the current goal with an application
    of the [vehicle.cache_witness] axiom registered in Vehicle.v.
    On failure, raises a tactic-level error carrying the validator output. *)
let vehicle_validate_tac cache_path =
  Proofview.Goal.enter begin fun gl ->
    let (success, output) = run_validate cache_path in
    if success then
      let env = Proofview.Goal.env gl in
      let sigma = Proofview.Goal.sigma gl in
      let concl = Proofview.Goal.concl gl in
      let witness_ref = Rocqlib.lib_ref "vehicle.cache_witness" in
      let (sigma, witness) =
        Evd.fresh_global env sigma witness_ref
      in
      let proof = EConstr.mkApp (witness, [| concl |]) in
      Proofview.tclTHEN
        (Proofview.Unsafe.tclEVARS sigma)
        (Tactics.exact_check proof)
    else
      let msg =
        Pp.(str "Vehicle validation failed for cache "
            ++ quote (str cache_path) ++ str ":" ++ fnl ()
            ++ str output)
      in
      Tacticals.tclZEROMSG msg
  end
