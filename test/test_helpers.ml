(** Shared test utilities used across test modules. *)

(** [unsetenv name] removes the environment variable [name] from the process
    environment. Uses the POSIX [unsetenv(3)] function via a C stub. *)
external unsetenv : string -> unit = "caml_unsetenv"

(** [mkdtemp prefix] creates a fresh temporary directory (mode 0700) under the
    system temp directory (honoring [TMPDIR]) with a name starting with
    [prefix], and returns its path. There is no [Unix.mkdtemp] in the stdlib;
    [Filename.temp_dir] provides the same create-and-return-a-unique-path
    behavior. *)
let mkdtemp prefix = Stdlib.Filename.temp_dir prefix ""

(** [with_tempdir ~prefix ~f] creates a tempdir via {!mkdtemp} under the system
    temp directory, using [prefix] as the leaf-name prefix. It passes the
    tempdir's path to [f], and removes the directory (and any flat files
    inside it) on exit — including on exception. Intended for tests whose
    tempdirs only contain top-level files; subdirectories are not recursively
    removed. *)
let with_tempdir ~prefix ~f =
  let dir = mkdtemp prefix in
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

(** [contains_substring s sub] returns [true] if [sub] appears anywhere in [s].
*)
let contains_substring s sub = Astring.String.is_infix ~affix:sub s

(** [split_lines s] splits [s] on ['\n'], dropping empty elements (so a jsonl
    file with [n] lines each terminated by ['\n'] yields exactly [n] elements).
*)
let split_lines s = Astring.String.cuts ~empty:false ~sep:"\n" s

(** Helper: check where a given command is *)
let find_cmd cmd =
  let inp_stream = Unix.open_process_in ("which " ^ cmd) in
  let output = Astring.String.trim (In_channel.input_all inp_stream) in
  let status = Unix.close_process_in inp_stream in
  match status with
  | Unix.WEXITED 0 -> output
  | Unix.WEXITED code ->
    raise (Failure (Printf.sprintf "Command failed with status: %d" code))
  | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
    raise (Failure (Printf.sprintf "Command failed with signal: %d" signal))
;;
