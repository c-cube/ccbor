type chunk = {
  bs: bytes;
  mutable off: int;
}
(** A chunk of data *)

type t = {
  mutable cur: chunk;  (** Current chunk, not full *)
  mutable prev_chunks: chunk list;
      (** list of previous chunks, most recent first *)
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

let create () : t = { cur = Chunk.create (); prev_chunks = [] }

let reset (self : t) =
  self.cur.off <- 0;
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

let[@inline never] alloc_new_chunk_ (self : t) : chunk =
  self.prev_chunks <- self.cur :: self.prev_chunks;
  let c = Chunk.create () in
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

let[@inline] get_chunk (self : t) : chunk =
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

let[@inline] add_int16 (self : t) (i : int) : unit =
  let chunk = get_space_for_small_ self 2 in
  Bytes.set_int16_be chunk.bs chunk.off i;
  chunk.off <- chunk.off + 2

let[@inline] add_int32 (self : t) (i : int32) : unit =
  let chunk = get_space_for_small_ self 4 in
  Bytes.set_int32_be chunk.bs chunk.off i;
  chunk.off <- chunk.off + 4

let[@inline] add_int64 (self : t) (i : int64) : unit =
  let chunk = get_space_for_small_ self 8 in
  Bytes.set_int64_be chunk.bs chunk.off i;
  chunk.off <- chunk.off + 8

external int_of_bool : bool -> int = "%identity"

let[@inline] bool self (b : bool) : unit =
  add_first_byte self 7 (20 + int_of_bool b)

let[@inline] null self = add_first_byte self 7 22

let[@inline] undefined self = add_first_byte self 7 23

(** Add unsigned integer, including first tag byte *)
let add_int_unsigned (self : t) (high : int) (x : int) =
  assert (x >= 0);
  if x < 24 then
    add_first_byte self high x
  else if x <= 0xff then (
    add_first_byte self high 24;
    add_byte self (Char.unsafe_chr x)
  ) else if x <= 0xff_ff then (
    add_first_byte self high 25;
    add_int16 self x
  ) else if x <= 0xff_ff_ff_ff then (
    add_first_byte self high 26;
    add_int32 self (Int32.of_int x)
  ) else (
    add_first_byte self high 27;
    add_int64 self (Int64.of_int x)
  )

(** Add unsigned integer, including first tag byte *)
let add_int64_unsigned (self : t) (high : int) (x : int64) =
  assert (x >= 0L);
  if x > 0xff_ff_ff_ffL then (
    add_first_byte self high 27;
    add_int64 self x
  ) else
    add_int_unsigned self high (Int64.to_int x)

let int64 (self : t) (i : int64) : unit =
  if Int64.(add min_int 2L) > i then (
    (* large negative int, be careful. encode [(-i)-1] via int64. *)
    add_first_byte self 1 27;
    add_int64 self Int64.(neg (add 1L i))
  ) else if i >= 0L then
    add_int64_unsigned self 1 i
  else
    add_int64_unsigned self 1 Int64.(sub (neg i) one)

let int (self : t) (i : int) =
  if i < 0 then
    add_int_unsigned self 1 (-i - 1)
  else
    add_int_unsigned self 0 i

let rec add_bytes (self : t) bs i len : unit =
  if len > 0 then (
    let chunk = get_chunk self in
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
