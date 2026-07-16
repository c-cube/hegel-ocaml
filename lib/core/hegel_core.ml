let resolve_draw values ~consume variable_id =
  Hegel.Generators.resolve_draw_core
    ~find:(Core.Hashtbl.find values)
    ~remove:(Core.Hashtbl.remove values)
    ~consume
    variable_id
;;

let pool_values ~pool_id ~values ~consume =
  Hegel.Generators.values_core
    ~pool_id
    ~find:(Core.Hashtbl.find values)
    ~remove:(Core.Hashtbl.remove values)
    ~is_empty:(fun () -> Core.Hashtbl.is_empty values)
    ~consume
;;
