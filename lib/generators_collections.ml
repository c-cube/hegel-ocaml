open Generators_core

(* [validate_size_bounds ~min_size ~max_size] rejects negative or crossed
   collection size bounds. *)
let validate_size_bounds ~min_size ~max_size =
  if min_size < 0
  then raise (Invalid_argument (Printf.sprintf "min_size=%d must be non-negative" min_size));
  match max_size with
  | Some ms when ms < 0 ->
    raise (Invalid_argument (Printf.sprintf "max_size=%d must be non-negative" ms))
  | Some ms when min_size > ms ->
    raise (Invalid_argument (Printf.sprintf "Cannot have max_size=%d < min_size=%d" ms min_size))
  | _ -> ()
;;

(* [draw_association_pairs keys values ~min_size ~max_size data] drives the
   collection protocol to produce unique-keyed [(key, value)] pairs in draw
   order: the shared generation of {!assoc_lists} and {!hash_tables}.
   Duplicate keys are rejected client-side. *)
let draw_association_pairs keys values ~min_size ~max_size data =
  let coll = new_collection ~min_size ?max_size data () in
  let rec collect acc =
    if collection_more coll data
    then (
      let k = do_draw (core_of keys) data in
      if List.exists (fun (k', _) -> k' = k) acc
      then (
        collection_reject coll data;
        collect acc)
      else (
        let v = do_draw (core_of values) data in
        collect ((k, v) :: acc)))
    else List.rev acc
  in
  collect []
;;

(** [assoc_lists keys values ?min_size ?max_size ()] creates a generator
    for association lists over printable [keys] and [values]: [(key, value)]
    pairs, in generation order, whose keys are unique.

    Key-value pairs are generated one at a time via the collection protocol,
    with duplicate keys rejected client-side. *)
let assoc_lists
      (keys : ('a, printable) generator)
      (values : ('b, printable) generator)
      ?(min_size = 0)
      ?max_size
      ()
  : (('a * 'b) list, printable) generator
  =
  validate_size_bounds ~min_size ~max_size;
  let pk = printer keys
  and pv = printer values in
  let sexp_of kvs =
    Sexp.List (List.map (fun (k, v) -> Sexp.List [ pk k; pv v ]) kvs)
  in
  let core =
    Composite
      { label = Labels.map
      ; generate_fn = draw_association_pairs keys values ~min_size ~max_size
      }
  in
  Printable { core; sexp_of }
;;

(** [lists elements ?min_size ?max_size ?unique ()] creates a generator for
    lists of printable [elements].

    Elements are generated one at a time via the collection protocol inside a
    {!Labels.list} span. When [unique] is [true], duplicate elements are rejected
    client-side so the generated list contains only distinct elements. *)
let lists
      (elements : ('a, printable) generator)
      ?(min_size = 0)
      ?max_size
      ?(unique = false)
      ()
  : ('a list, printable) generator
  =
  if min_size < 0
  then raise (Invalid_argument (Printf.sprintf "min_size=%d must be non-negative" min_size));
  (match max_size with
   | Some ms when ms < 0 ->
     raise (Invalid_argument (Printf.sprintf "max_size=%d must be non-negative" ms))
   | Some ms when min_size > ms ->
     raise
       (Invalid_argument (Printf.sprintf "Cannot have max_size=%d < min_size=%d" ms min_size))
   | _ -> ());
  let elt = printer elements in
  let sexp_of xs = Sexp.List (List.map elt xs) in
  let core =
    if not unique
    then CompositeList { elements = core_of elements; min_size; max_size }
    else
      (* With uniqueness, drive the collection protocol and reject duplicates.
         The engine's own rejection limit sends StopTest when too many
         duplicates occur, which [collection_reject] converts to
         [Data_exhausted]. *)
      Composite
        { label = Labels.list
        ; generate_fn =
            (fun data ->
              let coll = new_collection ~min_size ?max_size data () in
              let rec collect acc =
                if collection_more coll data
                then (
                  let elem = do_draw (core_of elements) data in
                  if List.mem elem acc
                  then (
                    collection_reject coll data;
                    collect acc)
                  else collect (elem :: acc))
                else List.rev acc
              in
              collect [])
        }
  in
  Printable { core; sexp_of }
;;
