(** [Core.Hashtbl]-backed pool generator, for projects that already depend on
    Jane Street's [Core]. This is an optional add-on: depend on [hegel.core]
    (in addition to [hegel]) to use it; it requires [core].

    It mirrors {!Hegel.Generators.Make_pool}, specialized to [Core.Hashtbl.t]
    so callers don't need to instantiate their own table module via
    [Stdlib.Hashtbl.Make]. *)

(** [resolve_draw values ~consume id] resolves a drawn pool [id] against the
    local [values] table, removing it when [consume]. Raises
    [Hegel.Internal.Flaky_strategy] on an unknown id (an engine-contract
    violation, unreachable through the normal engine-driven path). *)
val resolve_draw : (int, 'a) Core.Hashtbl.t -> consume:bool -> int -> 'a

(** [pool_values ~pool_id ~values ~consume] builds a generator that picks a
    value from the engine pool [pool_id], resolving the drawn id against the
    local [values] table. When [consume], the picked value is removed from
    the pool. Carries no printer, so it is {!Hegel.Generators.unprintable}. *)
val pool_values
  :  pool_id:int
  -> values:(int, 'a) Core.Hashtbl.t
  -> consume:bool
  -> ('a, Hegel.Generators.unprintable) Hegel.Generators.generator
