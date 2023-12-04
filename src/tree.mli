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

val pp : Format.formatter -> t -> unit

val show : t -> string
