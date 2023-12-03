type chunk = {
  bs: bytes;
  mutable off: int;
}
(** A chunk of data *)

(** chunks of a fixed size *)
let default_chunk_size = 256

type t = {
  mutable cur: chunk;  (** Current chunk, not full *)
  chunk_size: int;
  mutable prev_chunks: chunk list;
      (** list of previous chunks, most recent first *)
  mutable reuse_chunks: chunk list;  (** Chunks we can reuse *)
}

module Chunk = struct
  type t = chunk

  let[@inline] size self = self.off

  (** Free space in a chunk *)
  let[@inline] free_size_ (self : t) : int = Bytes.length self.bs - self.off

  let[@inline] has_space_ (self : t) : bool = Bytes.length self.bs <> self.off

  let[@inline] create ~size () : t = { bs = Bytes.create size; off = 0 }
end

let total_size (self : t) : int =
  List.fold_left
    (fun acc c -> acc + Chunk.size c)
    (Chunk.size self.cur) self.prev_chunks

let create ?(chunk_size = default_chunk_size) () : t =
  {
    cur = Chunk.create ~size:chunk_size ();
    chunk_size;
    prev_chunks = [];
    reuse_chunks = [];
  }

let[@inline] clear_chunk_ (c : Chunk.t) = c.off <- 0

let clear (self : t) =
  clear_chunk_ self.cur;
  List.iter clear_chunk_ self.prev_chunks;
  self.reuse_chunks <- self.prev_chunks;
  self.prev_chunks <- []

let[@inline] reset (self : t) =
  clear_chunk_ self.cur;
  self.prev_chunks <- []

let[@unroll 2] rec list_rev_iter_ f st l =
  match l with
  | [] -> ()
  | x :: tl ->
    list_rev_iter_ f st tl;
    f st x

let[@inline] iter_chunks (self : t) yield : unit =
  if self.prev_chunks != [] then
    list_rev_iter_ (fun k c -> k c.bs 0 c.off) yield self.prev_chunks;
  yield self.cur.bs 0 self.cur.off

module To_string_ = struct
  type state = {
    res: bytes;
    mutable off: int;
  }

  let add_chunk (self : state) (c : Chunk.t) : unit =
    Bytes.blit c.bs 0 self.res self.off c.off;
    self.off <- self.off + c.off

  let to_string (self : state) : string =
    assert (self.off = Bytes.length self.res);
    Bytes.unsafe_to_string self.res
end

let to_string (self : t) : string =
  let size = total_size self in
  let st = { To_string_.res = Bytes.create size; off = 0 } in
  if self.prev_chunks != [] then
    list_rev_iter_ To_string_.add_chunk st self.prev_chunks;
  To_string_.add_chunk st self.cur;
  To_string_.to_string st

(* ### encoding proper ### *)

external encode_uint_with_high :
  bytes ->
  (int[@untagged]) ->
  high:(int[@untagged]) ->
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
    | [] -> Chunk.create ~size:self.chunk_size ()
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

let add_int_unsigned (self : t) ~high (i : int) : unit =
  let c = get_space_for_small_ self 9 in
  let size = encode_uint_with_high c.bs c.off ~high i in
  c.off <- c.off + size

let rec add_bytes_slice_ (self : t) bs i len : unit =
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
      add_bytes_slice_ self bs (i + free) (len - free)
    )
  )

let bytes_sub self bs i len : unit =
  add_int_unsigned self ~high:2 len;
  add_bytes_slice_ self bs i len

let bytes self b = bytes_sub self b 0 (Bytes.length b)

let text_sub self s i len : unit =
  add_int_unsigned self ~high:3 len;
  add_bytes_slice_ self (Bytes.unsafe_of_string s) i len

let[@inline] text self s : unit = text_sub self s 0 (String.length s)

let[@inline] array_enter (self : t) (n : int) : unit =
  add_int_unsigned self ~high:4 n

let[@inline] map_enter (self : t) (n : int) : unit =
  add_int_unsigned self ~high:5 n

let[@inline] simple (self : t) (x : int) : unit =
  add_int_unsigned self ~high:7 x

let float (self : t) (f : float) : unit =
  add_first_byte self 7 27;
  let c = get_space_for_small_ self 8 in
  Bytes.set_int64_be c.bs c.off (Int64.bits_of_float f);
  c.off <- c.off + 8

let[@inline] tag_enter (self : t) (tag : int) : unit =
  add_int_unsigned self ~high:6 tag

let rec add_tree (self : t) (t : Tree.t) : unit =
  match t with
  | Tree.Null -> null self
  | Tree.Undefined -> undefined self
  | Tree.Simple i -> simple self i
  | Tree.Bool b -> bool self b
  | Tree.Int i -> int64 self i
  | Tree.Float f -> float self f
  | Tree.Bytes b ->
    let b = Bytes.unsafe_of_string b in
    bytes self b
  | Tree.Text s -> text self s
  | Tree.Array a ->
    array_enter self (Array.length a);
    for i = 0 to Array.length a - 1 do
      let x = Array.unsafe_get a i in
      add_tree self x
    done
  | Tree.Map m ->
    map_enter self (Array.length m);
    for i = 0 to Array.length m - 1 do
      let k, v = Array.unsafe_get m i in
      add_tree self k;
      add_tree self v
    done
  | Tree.Tag (tag, t) ->
    tag_enter self tag;
    add_tree self t
