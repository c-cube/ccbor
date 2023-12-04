type t

val create : bytes -> t

val create_sub : bytes -> int -> int -> t

val create_string : string -> t

val create_string_sub : string -> int -> int -> t

type token =
  | True
  | False
  | Null
  | Undefined
  | Int of int
  | Int64 of int64
  | Simple of int
  | Float of float
  | Bytes of bytes * int * int
  | Text of bytes * int * int
  | Array of int
  | Map of int
  | Tag of int
  | Bytes_indefinite_start
  | Text_indefinite_start
  | Array_indefinite_start
  | Map_indefinite_start
  | Indefinite_end

val next_token : t -> token
(** Read the next token.
    @raise End_of_file if there is no next token. *)

val read_tree : t -> Tree.t

val skip_tree : t -> unit
(** Skip a whole subtree. *)
