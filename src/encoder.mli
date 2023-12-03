type t

val create : unit -> t

val clear : t -> unit

val reset : t -> unit

val iter_chunks : t -> (bytes -> int -> int -> unit) -> unit

val total_size : t -> int

val bool : t -> bool -> unit

val null : t -> unit

val undefined : t -> unit

val int : t -> int -> unit

val int64 : t -> int64 -> unit

val bytes : t -> bytes -> int -> int -> unit

val text : t -> string -> unit

val text_sub : t -> string -> int -> int -> unit

val array_enter : t -> int -> unit

val map_enter : t -> int -> unit
