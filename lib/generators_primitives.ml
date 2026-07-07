open Sexplib0.Sexp_conv
open Generators_core

(** [booleans ()] creates a generator for boolean values. *)
let booleans () =
  leaf ~draw:(fun tc -> Internal.generate_boolean tc 0.5 None) ~sexp_of:sexp_of_bool
;;

(** [integers ?min_value ?max_value ()] creates a generator for integers within
    the given bounds. *)
let integers ?min_value ?max_value () =
  leaf
    ~draw:(fun tc ->
      Internal.generate_integer
        tc
        ~min_value:(Option.value min_value ~default:Int.min_int)
        ~max_value:(Option.value max_value ~default:Int.max_int))
    ~sexp_of:sexp_of_int
;;

(** [floats ?min_value ?max_value ?exclude_min ?exclude_max ?allow_nan
     ?allow_infinity ()] creates a generator for floating-point values. *)
let floats ?min_value ?max_value ?exclude_min ?exclude_max ?allow_nan ?allow_infinity () =
  leaf
    ~draw:(fun tc ->
      Internal.generate_float
        tc
        ~min_value:(Option.value min_value ~default:Float.neg_infinity)
        ~max_value:(Option.value max_value ~default:Float.infinity)
        ~allow_nan:(Option.value allow_nan ~default:false)
        ~allow_infinity:(Option.value allow_infinity ~default:false)
        ~exclude_min:(Option.value exclude_min ~default:false)
        ~exclude_max:(Option.value exclude_max ~default:false)
        ~smallest_nonzero_magnitude:Float.min_float)
    ~sexp_of:sexp_of_float
;;

(** [text ?min_size ?max_size ?codec ?min_codepoint ?max_codepoint ?categories
     ?exclude_categories ?include_characters ?exclude_characters ()]
    creates a generator for Unicode text strings. *)
let text ?min_size ?max_size ?codec ?min_codepoint ?max_codepoint ?categories
  ?exclude_categories ?include_characters ?exclude_characters () =
  leaf
    ~draw:(fun tc ->
      Internal.generate_text
        tc
        ~min_size:(Option.value min_size ~default:0)
        ~max_size
        ~codec
        ~min_codepoint:(Option.value min_codepoint ~default:0)
        ~max_codepoint:(Option.value max_codepoint ~default:0x10FFFF)
        ~categories
        ~exclude_categories
        ~include_characters
        ~exclude_characters)
    ~sexp_of:sexp_of_string
;;

(** [characters ?codec ?min_codepoint ?max_codepoint ?categories
     ?exclude_categories ?include_characters ?exclude_characters ()] creates a
    generator for single Unicode characters (as single-character UTF-8 strings). *)
let characters ?codec ?min_codepoint ?max_codepoint ?categories ?exclude_categories
  ?include_characters ?exclude_characters () =
  leaf
    ~draw:(fun tc ->
      Internal.generate_character
        tc
        ~codec
        ~min_codepoint:(Option.value min_codepoint ~default:0)
        ~max_codepoint:(Option.value max_codepoint ~default:0x10FFFF)
        ~categories
        ~exclude_categories
        ~include_characters
        ~exclude_characters)
    ~sexp_of:sexp_of_string
;;

(** [binary ?min_size ?max_size ()] creates a generator for binary byte strings. *)
let binary ?min_size ?max_size () =
  leaf
    ~draw:(fun tc ->
      Internal.generate_bytes
        tc
        ~min_size:(Option.value min_size ~default:0)
        ~max_size)
    ~sexp_of:sexp_of_string
;;

(** [just value] creates a generator that always produces [value]. The output
    type is the caller's, so the result carries no printer. *)
let just value = leaf_silent ~draw:(fun _ -> value)

(** [emails ()] creates a generator for valid email address strings. *)
let emails () =
  leaf ~draw:Internal.generate_email ~sexp_of:sexp_of_string
;;

(** [urls ()] creates a generator for valid URL strings. *)
let urls () =
  leaf ~draw:Internal.generate_url ~sexp_of:sexp_of_string
;;

(** [domains ?max_length ()] creates a generator for domain name strings. *)
let domains ?max_length () =
  (match max_length with
   | None -> ()
   | Some ml when ml < 4 || ml > 255 ->
     raise (Invalid_argument (Printf.sprintf "max_length=%d must be between 4 and 255" ml))
   | _ -> ());
  let max_length = Option.value max_length ~default:255 in
  leaf ~draw:(fun tc -> Internal.generate_domain tc ~max_length) ~sexp_of:sexp_of_string
;;

(** [format_date (year, month, day)] renders a drawn date as an ISO 8601
    [YYYY-MM-DD] string. *)
let format_date (year, month, day) = Printf.sprintf "%04d-%02d-%02d" year month day

(** [format_time (hour, minute, second, microsecond)] renders a drawn time as an
    ISO 8601 [HH:MM:SS] string, appending [.ffffff] when [microsecond] is
    non-zero. *)
let format_time (hour, minute, second, microsecond) =
  if microsecond = 0
  then Printf.sprintf "%02d:%02d:%02d" hour minute second
  else Printf.sprintf "%02d:%02d:%02d.%06d" hour minute second microsecond
;;

(** [format_datetime (date, time)] renders a drawn datetime as an ISO 8601
    [YYYY-MM-DDTHH:MM:SS[.ffffff]] string. *)
let format_datetime (date, time) = format_date date ^ "T" ^ format_time time

(** [dates ()] creates a generator for ISO 8601 date strings ([YYYY-MM-DD]),
    with year in [\[1, 9999\]] and calendar-valid month/day. *)
let dates () =
  leaf ~draw:(fun tc -> format_date (Internal.generate_date tc)) ~sexp_of:sexp_of_string
;;

(** [times ()] creates a generator for ISO 8601 time strings ([HH:MM:SS] or
    [HH:MM:SS.ffffff], the fractional part present only when microseconds are
    non-zero). *)
let times () =
  leaf ~draw:(fun tc -> format_time (Internal.generate_time tc)) ~sexp_of:sexp_of_string
;;

(** [datetimes ()] creates a generator for ISO 8601 datetime strings
    ([YYYY-MM-DDTHH:MM:SS[.ffffff]]), combining {!dates} and {!times}. *)
let datetimes () =
  leaf
    ~draw:(fun tc ->
      let date = Internal.generate_date tc in
      let time = Internal.generate_time tc in
      format_datetime (date, time))
    ~sexp_of:sexp_of_string
;;

(** [from_regex pattern ?fullmatch ()] creates a generator for strings matching
    a regular expression [pattern]. *)
let from_regex pattern ?(fullmatch = true) () =
  leaf
    ~draw:(fun tc -> Internal.generate_regex tc ~pattern ~fullmatch)
    ~sexp_of:sexp_of_string
;;
