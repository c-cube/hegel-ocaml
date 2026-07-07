(** E2E tests for the [ppx_hegel_test] PPX.

    These tests exercise the full expansion of [let%hegel_test] including
    the [@@settings ...] attribute. *)

let env_var = "ANTITHESIS_OUTPUT_DIR"

let with_env_dir dir ~f =
  let prev = Stdlib.Sys.getenv_opt env_var in
  Unix.putenv env_var dir;
  Stdlib.Fun.protect
    ~finally:(fun () ->
      match prev with
      | Some v -> Unix.putenv env_var v
      | None -> Unix.putenv env_var "")
    f
;;

let with_tempdir ~f =
  let dir = Stdlib.Filename.temp_dir "hegel-ppx-test-" "" in
  Stdlib.Fun.protect
    ~finally:(fun () ->
      (try
         Stdlib.Sys.readdir dir
         |> Array.iter (fun name ->
           try Stdlib.Sys.remove (Stdlib.Filename.concat dir name) with
           | _ -> ())
       with
       | _ -> ());
      try Unix.rmdir dir with
      | _ -> ())
    (fun () -> f dir)
;;

(** [read_all path] reads the entire contents of the file at [path]. *)
let read_all path = In_channel.with_open_bin path In_channel.input_all

(** [assoc_find_exn key assoc] looks up [key] in the association list
    [assoc], raising if absent. *)
let assoc_find_exn key assoc =
  match List.find_opt (fun (k, _) -> String.equal k key) assoc with
  | Some (_, v) -> v
  | None -> failwith (Printf.sprintf "key %s not found" key)
;;

(** A simple passing test, expected to expand into a unit -> unit wrapper. *)
let%hegel_test simple_pass (tc : Hegel.test_case) =
  let _ = Hegel.draw tc (Hegel.Generators.booleans ()) in
  ()
[@@settings Hegel.settings ~test_cases:3 ()]
;;

(** A failing test. The generated wrapper will raise. *)
let%hegel_test simple_fail (tc : Hegel.test_case) =
  let _ = Hegel.draw tc (Hegel.Generators.booleans ()) in
  failwith "deliberate failure"
[@@settings Hegel.settings ~test_cases:3 ()]
;;

(** A test without an explicit [@@settings] attribute — should fall back to
    default settings. *)
let%hegel_test no_settings (tc : Hegel.test_case) =
  let _ = Hegel.draw tc (Hegel.Generators.booleans ()) in
  ()
;;

let test_passing_writes_sdk_jsonl () =
  with_tempdir ~f:(fun dir ->
    with_env_dir dir ~f:(fun () ->
      simple_pass ();
      let path = Stdlib.Filename.concat dir "sdk.jsonl" in
      Alcotest.(check bool) "sdk.jsonl exists" true (Stdlib.Sys.file_exists path);
      let lines =
        read_all path
        |> Astring.String.cuts ~empty:false ~sep:"\n"
      in
      Alcotest.(check int) "two lines (declaration + evaluation)" 2 (List.length lines);
      let decl = Yojson.Safe.from_string (List.nth lines 0) in
      let eval = Yojson.Safe.from_string (List.nth lines 1) in
      let decl_assoc =
        match decl with
        | `Assoc [ ("antithesis_assert", `Assoc fields) ] -> fields
        | _ -> Alcotest.fail "declaration not wrapped in antithesis_assert"
      in
      let eval_assoc =
        match eval with
        | `Assoc [ ("antithesis_assert", `Assoc fields) ] -> fields
        | _ -> Alcotest.fail "evaluation not wrapped in antithesis_assert"
      in
      Alcotest.(check string)
        "declaration hit"
        "false"
        (Yojson.Safe.to_string (assoc_find_exn "hit" decl_assoc));
      Alcotest.(check string)
        "evaluation hit"
        "true"
        (Yojson.Safe.to_string (assoc_find_exn "hit" eval_assoc));
      Alcotest.(check string)
        "evaluation condition is true (passing test)"
        "true"
        (Yojson.Safe.to_string (assoc_find_exn "condition" eval_assoc));
      let id = Yojson.Safe.to_string (assoc_find_exn "id" eval_assoc) in
      Alcotest.(check bool)
        "id mentions test_ppx_hegel_test"
        true
        (Astring.String.is_infix ~affix:"simple_pass in test_ppx_hegel_test" id);
      (* location.file should mention the test file. *)
      let loc = assoc_find_exn "location" eval_assoc in
      let loc_assoc =
        match loc with
        | `Assoc f -> f
        | _ -> Alcotest.fail "location not an object"
      in
      let file = Yojson.Safe.to_string (assoc_find_exn "file" loc_assoc) in
      Alcotest.(check bool)
        "location.file mentions test_ppx_hegel_test.ml"
        true
        (Astring.String.is_infix ~affix:"test_ppx_hegel_test.ml" file)))
;;

let test_failing_writes_condition_false () =
  with_tempdir ~f:(fun dir ->
    with_env_dir dir ~f:(fun () ->
      (try simple_fail () with
       | _ -> ());
      let path = Stdlib.Filename.concat dir "sdk.jsonl" in
      Alcotest.(check bool) "sdk.jsonl exists" true (Stdlib.Sys.file_exists path);
      let lines =
        read_all path
        |> Astring.String.cuts ~empty:false ~sep:"\n"
      in
      let eval = Yojson.Safe.from_string (List.nth lines 1) in
      let eval_assoc =
        match eval with
        | `Assoc [ ("antithesis_assert", `Assoc fields) ] -> fields
        | _ -> Alcotest.fail "evaluation not wrapped in antithesis_assert"
      in
      Alcotest.(check string)
        "evaluation condition is false (failing test)"
        "false"
        (Yojson.Safe.to_string (assoc_find_exn "condition" eval_assoc))))
;;

let test_no_settings_runs_with_defaults () =
  with_tempdir ~f:(fun dir ->
    with_env_dir dir ~f:(fun () ->
      no_settings ();
      let path = Stdlib.Filename.concat dir "sdk.jsonl" in
      Alcotest.(check bool) "sdk.jsonl exists" true (Stdlib.Sys.file_exists path)))
;;

let () =
  Alcotest.run
    "hegel-ppx-hegel-test"
    [ ( "ppx_hegel_test"
      , [ Alcotest.test_case
            "passing test writes sdk.jsonl"
            `Quick
            test_passing_writes_sdk_jsonl
        ; Alcotest.test_case
            "failing test writes condition:false"
            `Quick
            test_failing_writes_condition_false
        ; Alcotest.test_case
            "no [@@settings] uses defaults"
            `Quick
            test_no_settings_runs_with_defaults
        ] )
    ]
;;
