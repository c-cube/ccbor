type chunk = {
  bs: bytes;
  mutable off: int;
}
(** A chunk of data *)

type t = {
  mutable cur: chunk;  (** Current chunk, not full *)
  mutable prev_chunks: chunk list;
      (** list of previous chunks, most recent first *)
  mutable reuse_chunks: chunk list;  (** Chunks we can reuse *)
}

(** chunks of 1kiB *)
let chunk_size = 1 * 1024

module Chunk = struct
  type t = chunk

  let[@inline] size self = self.off

  (** Free space in a chunk *)
  let[@inline] free_size_ (self : t) : int = Bytes.length self.bs - self.off

  let[@inline] has_space_ (self : t) : bool = Bytes.length self.bs <> self.off

  let[@inline] create () : t = { bs = Bytes.create chunk_size; off = 0 }
end

let total_size (self : t) : int =
  List.fold_left
    (fun acc c -> acc + Chunk.size c)
    (Chunk.size self.cur) self.prev_chunks

let create () : t =
  { cur = Chunk.create (); prev_chunks = []; reuse_chunks = [] }

let[@inline] clear_chunk_ (c : Chunk.t) = c.off <- 0

let clear (self : t) =
  clear_chunk_ self.cur;
  List.iter clear_chunk_ self.prev_chunks;
  self.reuse_chunks <- self.prev_chunks;
  self.prev_chunks <- []

let reset (self : t) =
  clear_chunk_ self.cur;
  self.prev_chunks <- []

let rec list_rev_iter_ f st l =
  match l with
  | [] -> ()
  | x :: tl ->
    list_rev_iter_ f st tl;
    f st x

let iter_chunks (self : t) yield : unit =
  list_rev_iter_ (fun k c -> k c.bs 0 c.off) yield self.prev_chunks;
  yield self.cur.bs 0 self.cur.off

(* ### encoding proper ### *)

external encode_uint_with_high :
  bytes ->
  (int[@untagged]) ->
  (int[@untagged]) ->
  (int[@untagged]) ->
  (int[@untagged]) = "caml_ccbor_encode_uint_byte" "caml_ccbor_encode_uint"
  [@@noalloc]
(** [encode_int b off high i] encodes [i] at offset [off]
    in [b] with high mask [high].
    This assumes there's at least [9] bytes available
    in [b]. Returns how many bytes were written. *)

external encode_int :
  bytes -> (int[@untagged]) -> (int64[@unboxed]) -> (int[@untagged])
  = "caml_ccbor_encode_int_byte" "caml_ccbor_encode_int"
  [@@noalloc]
(** [encode_int b off i] encodes [i] at offset [off]
    in [b]. This assumes there's at least [9] bytes available
    in [b]. Returns how many bytes were written. *)

let[@inline never] alloc_new_chunk_ (self : t) : chunk =
  self.prev_chunks <- self.cur :: self.prev_chunks;
  let c =
    match self.reuse_chunks with
    | [] -> Chunk.create ()
    | c :: tl ->
      (* reuse [c] *)
      self.reuse_chunks <- tl;
      c
  in
  self.cur <- c;
  c

(** Get a chunk that can contain at least [n] bytes.
    Assumes [n] is small (smaller than chunk size) *)
let[@inline] get_space_for_small_ (self : t) (n : int) : chunk =
  let free = Chunk.free_size_ self.cur in
  if n > free then
    (* not enough space *)
    alloc_new_chunk_ self
  else
    self.cur

let[@inline] get_chunk_with_space (self : t) : chunk =
  if Chunk.has_space_ self.cur then
    self.cur
  else
    alloc_new_chunk_ self

let[@inline] add_byte (self : t) (x : char) : unit =
  let chunk = get_space_for_small_ self 1 in
  Bytes.set chunk.bs chunk.off x;
  chunk.off <- chunk.off + 1

let add_first_byte (self : t) (high : int) (low : int) : unit =
  let i = (high lsl 5) lor low in
  assert (i land 0xff == i);
  add_byte self (Char.unsafe_chr i)

external int_of_bool : bool -> int = "%identity"

let[@inline] bool self (b : bool) : unit =
  add_first_byte self 7 (20 + int_of_bool b)

let[@inline] null self = add_first_byte self 7 22

let[@inline] undefined self = add_first_byte self 7 23

let[@inline] int64 (self : t) (i : int64) : unit =
  let c = get_space_for_small_ self 9 in
  let size = encode_int c.bs c.off i in
  c.off <- c.off + size

let[@inline] int (self : t) (i : int) =
  let c = get_space_for_small_ self 9 in
  let size = encode_int c.bs c.off (Int64.of_int i) in
  c.off <- c.off + size

let add_int_unsigned (self : t) high (i : int) : unit =
  let c = get_space_for_small_ self 9 in
  let size = encode_uint_with_high c.bs c.off high i in
  c.off <- c.off + size

let rec add_bytes (self : t) bs i len : unit =
  if len > 0 then (
    let chunk = get_chunk_with_space self in
    let free = Chunk.free_size_ chunk in
    if len <= free then (
      (* can do it in one go *)
      Bytes.blit bs i chunk.bs chunk.off len;
      chunk.off <- chunk.off + len
    ) else (
      assert (free > 0);
      Bytes.blit bs i chunk.bs chunk.off free;
      chunk.off <- chunk.off + free;
      (* write the rest recursively *)
      add_bytes self bs (i + free) (len - free)
    )
  )

let bytes self bs i len : unit =
  add_int_unsigned self 2 len;
  add_bytes self bs i len

let text_sub self s i len : unit =
  add_int_unsigned self 3 len;
  add_bytes self (Bytes.unsafe_of_string s) i len

let text self s : unit = text_sub self s 0 (String.length s)

let array_enter (self : t) (n : int) : unit = add_int_unsigned self 4 n

let map_enter (self : t) (n : int) : unit = add_int_unsigned self 5 n

(* TODO: simple *)
(* TODO: float *)
(* TODO: tag *)

(*

  let rec encode_val (self : t) : unit =
    match self with
    | `Simple i ->
      if i < 24 then
        add_first_byte 7 i
      else if i <= 0xff then (
        add_first_byte 7 24;
        Buffer.add_char buf (Char.unsafe_chr i)
      ) else
        failwith "cbor: simple value too high (above 255)"
    | `Float f ->
      add_first_byte 7 27;
      (* float 64 *)
      add_i64 (Int64.bits_of_float f)
    | `Array l ->
      add_uint 4 (Int64.of_int (List.length l));
      List.iter encode_val l
    | `Map l ->
      add_uint 5 (Int64.of_int (List.length l));
      List.iter
        (fun (k, v) ->
          encode_val k;
          encode_val v)
        l
    | `Tag (t, v) ->
      add_uint 6 (Int64.of_int t);
      encode_val v
    | `Int i ->
      if i >= Int64.zero then
        add_uint 0 i
      else if Int64.(add min_int 2L) > i then (
        (* large negative int, be careful. encode [(-i)-1] via int64. *)
        add_first_byte 1 27;
        Buffer.add_int64_be buf Int64.(neg (add 1L i))
      ) else
        add_uint 1 Int64.(sub (neg i) one)
  in
  encode_val self;
  Buffer.contents buf
  *)
