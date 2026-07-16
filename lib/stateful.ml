(** Stateful property-based testing for Hegel. See [stateful.mli]. *)

module Int_table = Generators.Int_table


module Pool_gen = Generators.Make_pool (Int_table)

module Pool = struct
  type 'a t =
    { tc : Internal.test_case
    ; pool_id : int
    ; values : 'a Int_table.t
    }

  let create tc =
    let pool_id = Internal.new_pool tc in
    { tc; pool_id; values = Int_table.create 16 }
  ;;

  let add t value =
    let variable_id = Internal.pool_add t.tc ~pool_id:t.pool_id in
    Int_table.replace t.values variable_id value
  ;;

  let size t = Int_table.length t.values

  let values_consumed t =
    Pool_gen.pool_values ~pool_id:t.pool_id ~values:t.values ~consume:true
  ;;

  let values_reusable t =
    Pool_gen.pool_values ~pool_id:t.pool_id ~values:t.values ~consume:false
  ;;
end

module Rule = struct
  type 'state t =
    { name : string
    ; step : Internal.test_case -> 'state -> 'state
    }

  let create ~name ~step = { name; step }
  let name (r : 'state t) = r.name
  let _step (r : 'state t) = r.step
end

(** [run ~init ~rules ?invariants ?sexp_of_state tc] runs a stateful
    (state-machine) test with the given [rules] and [invariants] on the test
    case [tc]. *)
let run ~init ~rules ?invariants ?sexp_of_state tc =
  let invariants = Option.value invariants ~default:[] in
  let invariant_names = List.init (List.length invariants) (fun i -> Printf.sprintf "%d" i) in
  let rule_names = List.map (fun (r : 'state Rule.t) -> r.name) rules in
  let state_machine_id =
    Internal.new_state_machine
      tc
      ~rule_names
      ~invariant_names
  in
  let print_state state =
    Option.iter (fun sexp_of ->
      Internal.note tc (Printf.sprintf "state = %s" (Sexplib.Sexp.to_string_hum (sexp_of state))))
      sexp_of_state
  in
  let check_invariants ~where state =
    List.iteri (fun i inv ->
      match inv state with
      | () -> ()
      | exception e ->
        Internal.note tc (Printf.sprintf "Invariant %d violated %s." i where);
        raise e)
      invariants
  in
  print_state init;
  check_invariants ~where:"in the initial state" init;
  let loop state =
    let step_count = Internal.stateful_step_count tc in
    let max_steps = step_count in
    let rec steps s n =
      if n >= max_steps
      then s
      else (
        let rule_index = Internal.state_machine_next_rule tc ~state_machine_id in
        let name = (List.nth rules rule_index).name in
        Internal.start_span ~label:Generators.Labels.stateful_rule tc;
        Internal.note tc (Printf.sprintf "Step %d: %s" (n + 1) name);
        let s' =
          try (List.nth rules rule_index).step tc s
          with e ->
            Internal.stop_span ~discard:true tc;
            raise e
        in
        Internal.stop_span tc;
        print_state s';
        check_invariants ~where:(Printf.sprintf "after step %d" (n + 1)) s';
        steps s' (n + 1))
    in
    let (_ : 'state) = steps state 0 in
    ()
  in
  loop init
;;
