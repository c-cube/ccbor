type t
(** Encoder. It accumulates bytes that form a CBOR value. *)

(** {2 Encoder} *)

val create : ?chunk_size:int -> unit -> t

val clear : t -> unit

val reset : t -> unit

val iter_chunks : t -> (bytes -> int -> int -> unit) -> unit

val to_string : t -> string
(** Build a string containing all the bytes so far. This always allocates. *)

val total_size : t -> int

(** {2 Encoding values} *)

val bool : t -> bool -> unit

val null : t -> unit

val undefined : t -> unit

val int : t -> int -> unit

val int64 : t -> int64 -> unit

val float : t -> float -> unit

val bytes : t -> bytes -> unit

val bytes_sub : t -> bytes -> int -> int -> unit

val text : t -> string -> unit

val text_sub : t -> string -> int -> int -> unit

val array_enter : t -> int -> unit

val map_enter : t -> int -> unit

val simple : t -> int -> unit

val add_tree : t -> Tree.t -> unit
(** Encode the whole subtree *)
