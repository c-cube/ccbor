type t =
  | Null
  | Undefined
  | Simple of int
  | Bool of bool
  | Int of int
  | Int64 of int64
  | Float of float
  | Bytes of string
  | Text of string
  | Array of t array
  | Map of (t * t) array
  | Tag of int * t

let hex_of_string (s : string) : string =
  let i_to_hex (i : int) =
    if i < 10 then
      Char.chr (i + Char.code '0')
    else
      Char.chr (i - 10 + Char.code 'a')
  in
  let res = Bytes.create (2 * String.length s) in
  for i = 0 to String.length s - 1 do
    let n = Char.code (String.get s i) in
    Bytes.set res (2 * i) (i_to_hex ((n land 0xf0) lsr 4));
    Bytes.set res ((2 * i) + 1) (i_to_hex (n land 0x0f))
  done;
  Bytes.unsafe_to_string res

let rec pp out (self : t) =
  match self with
  | Null -> Format.pp_print_string out "null"
  | Undefined -> Format.pp_print_string out "undefined"
  | Simple i -> Format.fprintf out "simple(%d)" i
  | Bool b -> Format.pp_print_bool out b
  | Int i -> Format.fprintf out "%d" i
  | Int64 i -> Format.fprintf out "%LdL" i
  | Float f -> Format.pp_print_float out f
  | Bytes s -> Format.fprintf out "h'%s'" (hex_of_string s)
  | Text s -> Format.fprintf out "%S" s
  | Array l ->
    Format.fprintf out "[@[";
    Array.iteri
      (fun i x ->
        if i > 0 then Format.fprintf out ",@ ";
        pp out x)
      l;
    Format.fprintf out "@]]"
  | Map l ->
    Format.fprintf out "{@[";
    Array.iteri
      (fun i (k, v) ->
        if i > 0 then Format.fprintf out ",@ ";
        Format.fprintf out "@[%a:@ %a@]" pp k pp v)
      l;
    Format.fprintf out "@]}"
  | Tag (i, x) -> Format.fprintf out "%d(@[%a@])" i pp x

let show (self : t) : string = Format.asprintf "@[<h>%a@]" pp self
