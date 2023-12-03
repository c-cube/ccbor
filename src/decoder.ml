exception Indefinite

type t = {
  b: bytes;
  mutable i: int;
  last: int;
}

let create_sub b i len =
  if i < 0 || i + len > Bytes.length b then invalid_arg "Decoder.create_sub";
  { b; i; last = i + len }

let create b : t = create_sub b 0 (Bytes.length b)

let create_string s : t = create (Bytes.unsafe_of_string s)

let create_string_sub s i len : t = create_sub (Bytes.unsafe_of_string s) i len

let[@inline] check_non_empty (self : t) : unit =
  if self.i >= self.last then raise End_of_file

let[@inline] check_has_at_least (self : t) n : unit =
  if self.i + n > self.last then raise End_of_file

let[@inline] read_i8 (self : t) =
  check_non_empty self;
  let c = Char.code (Bytes.get self.b self.i) in
  self.i <- 1 + self.i;
  c

let[@inline] read_i16 (self : t) =
  check_has_at_least self 2;
  let c = Bytes.get_uint16_be self.b self.i in
  self.i <- self.i + 2;
  c

let[@inline] read_i32 (self : t) =
  check_has_at_least self 4;
  let c = Bytes.get_int32_be self.b self.i in
  self.i <- self.i + 4;
  c

let[@inline] read_i64 (self : t) =
  check_has_at_least self 8;
  let c = Bytes.get_int64_be self.b self.i in
  self.i <- self.i + 8;
  c

let[@inline] i64_to_int i =
  let j = Int64.to_int i in
  if Int64.(of_int j = i) then
    j
  else
    failwith "int64 does not fit in int"

let[@inline] int_is_small ~low : bool = low <= 25

(** Read an integer that fits in 16 bits *)
let[@inline] read_small_int (self : t) ~low : int =
  match low with
  | _ when low < 24 -> low
  | 24 -> read_i8 self
  | 25 -> read_i16 self
  | _ -> assert false

(** read integer value from least significant bits *)
let read_int64 (self : t) ~allow_indefinite ~low : int64 =
  match low with
  | _ when low < 0 -> failwith "cbor: invalid length"
  | _ when low <= 25 -> Int64.of_int (read_small_int self ~low)
  | 26 -> Int64.of_int32 (read_i32 self)
  | 27 -> read_i64 self
  | 28 | 29 | 30 -> failwith "cbor: invalid length"
  | 31 ->
    if allow_indefinite then
      raise_notrace Indefinite
    else
      failwith "cbor: invalid integer 31 in this context"
  | _ -> assert false

(* appendix D

   double decode_half(unsigned char *halfp) {
     unsigned half = (halfp[0] << 8) + halfp[1];
     unsigned exp = (half >> 10) & 0x1f;
     unsigned mant = half & 0x3ff;
     double val;
     if (exp == 0) val = ldexp(mant, -24);
     else if (exp != 31) val = ldexp(mant + 1024, exp - 25);
     else val = mant == 0 ? INFINITY : NAN;
     return half & 0x8000 ? -val : val;
   }
*)
let decode_f16 (half : int) : float =
  (* exponent is bits 15:10 *)
  let exp = (half lsr 10) land 0x1f in
  (* mantissa is bits 9:0 *)
  let mant = half land 0x3ff in
  let value =
    if exp = 0 then
      ldexp (float mant) (-24)
    else if exp <> 31 then
      ldexp (float (mant + 1024)) (exp - 25)
    else if mant = 0 then
      infinity
    else
      nan
  in
  if half land 0x8000 <> 0 then
    -.value
  else
    value

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

type bytes_type =
  | T_bytes
  | T_text

let read_int (self : t) ~allow_indefinite ~low : int =
  if int_is_small ~low then
    read_small_int self ~low
  else
    read_int64 self ~allow_indefinite ~low |> i64_to_int

let read_bytes (self : t) ~ty ~low : token =
  match read_int self ~allow_indefinite:true ~low with
  | exception Indefinite ->
    (match ty with
    | T_bytes -> Bytes_indefinite_start
    | T_text -> Text_indefinite_start)
  | len ->
    check_has_at_least self len;
    let tok =
      match ty with
      | T_bytes -> Bytes (self.b, self.i, len)
      | T_text -> Text (self.b, self.i, len)
    in
    self.i <- self.i + len;
    tok

let read_array (self : t) ~low : token =
  match read_int self ~allow_indefinite:true ~low with
  | len -> Array len
  | exception Indefinite -> Array_indefinite_start

let read_map (self : t) ~low : token =
  match read_int self ~allow_indefinite:true ~low with
  | len -> Map len
  | exception Indefinite -> Map_indefinite_start

let next_token (self : t) : token =
  (* roughly follow https://www.rfc-editor.org/rfc/rfc8949.html#pseudocode *)
  let c = read_i8 self in
  let high = (c land 0b111_00000) lsr 5 in
  let low = c land 0b000_11111 in
  match high with
  | 0 ->
    if int_is_small ~low then
      Int (read_small_int self ~low)
    else
      Int64 (read_int64 self ~allow_indefinite:false ~low)
  | 1 ->
    let i = read_int64 self ~allow_indefinite:false ~low in
    Int64 Int64.(sub minus_one i)
  | 2 -> read_bytes self ~ty:T_bytes ~low
  | 3 -> read_bytes self ~ty:T_text ~low
  | 4 -> read_array self ~low
  | 5 -> read_map self ~low
  | 6 ->
    let tag = read_int self ~allow_indefinite:false ~low in
    Tag tag
  | 7 ->
    (* simple or float,
       https://www.rfc-editor.org/rfc/rfc8949.html#fpnocont *)
    (match low with
    | 20 -> False
    | 21 -> True
    | 22 -> Null
    | 23 -> Undefined
    | _ when low <= 24 -> Simple (read_small_int self ~low)
    | 25 ->
      (* float16 *)
      let i = read_int self ~allow_indefinite:false ~low in
      Float (decode_f16 i)
    | 26 ->
      (* float 32 *)
      let i = read_int64 self ~allow_indefinite:false ~low in
      Float (Int32.float_of_bits (Int64.to_int32 i))
    | 27 ->
      (* float 64 *)
      let i = read_int64 self ~allow_indefinite:false ~low in
      Float (Int64.float_of_bits i)
    | 28 | 29 | 30 -> failwith "cbor: malformed"
    | 31 -> Indefinite_end
    | _ -> assert false (* unreachable *))
  | _ ->
    (* unreachable *)
    assert false

let bytes_sub_ b i len =
  if i = 0 && len = Bytes.length b then
    b
  else
    Bytes.sub b i len

let array_rev_in_place a =
  let len = Array.length a in
  if len > 0 then
    for k = 0 to (len - 1) / 2 do
      let t = a.(k) in
      a.(k) <- a.(len - 1 - k);
      a.(len - 1 - k) <- t
    done

let rec read_tree (self : t) : Tree.t =
  let module T = Tree in
  match next_token self with
  | True -> T.Bool true
  | False -> T.Bool false
  | Null -> T.Null
  | Undefined -> T.Undefined
  | Int i -> T.Int (Int64.of_int i)
  | Int64 i -> T.Int i
  | Simple i -> T.Simple i
  | Float f -> T.Float f
  | Bytes (b, i, len) -> T.Bytes (bytes_sub_ b i len |> Bytes.unsafe_to_string)
  | Text (b, i, len) -> T.Text (bytes_sub_ b i len |> Bytes.unsafe_to_string)
  | Array len ->
    let a = Array.init len (fun _ -> read_tree self) in
    T.Array a
  | Map len ->
    let m = Array.init len (fun _ -> read_kv self) in
    T.Map m
  | Tag i ->
    let v = read_tree self in
    T.Tag (i, v)
  | (Bytes_indefinite_start | Text_indefinite_start) as tok ->
    let buf = Buffer.create 32 in
    while
      match next_token self with
      | Text (b, i, len) | Bytes (b, i, len) ->
        Buffer.add_subbytes buf b i len;
        true
      | Indefinite_end -> false
      | _ -> failwith "unexpected token in indefinite text"
    do
      ()
    done;
    if tok == Bytes_indefinite_start then
      T.Bytes (Buffer.contents buf)
    else
      T.Text (Buffer.contents buf)
  | Array_indefinite_start ->
    let l = ref [] in
    while
      match read_tree self with
      | exception Indefinite -> false
      | x ->
        l := x :: !l;
        true
    do
      ()
    done;
    let a = Array.of_list !l in
    array_rev_in_place a;
    T.Array a
  | Map_indefinite_start ->
    let l = ref [] in
    let key = ref None in
    while
      match read_tree self with
      | exception Indefinite -> false
      | x ->
        (match !key with
        | None -> key := Some x
        | Some k ->
          key := None;
          l := (k, x) :: !l);
        true
    do
      ()
    done;
    let a = Array.of_list !l in
    array_rev_in_place a;
    T.Map a
  | Indefinite_end -> raise Indefinite

and read_kv (self : t) =
  let k = read_tree self in
  let v = read_tree self in
  k, v

let rec skip_tree (self : t) : unit =
  let module T = Tree in
  match next_token self with
  | True | False | Null | Undefined | Int _ | Int64 _ | Simple _ | Float _
  | Bytes _ | Text _ ->
    ()
  | Array len ->
    for _i = 1 to len do
      skip_tree self
    done
  | Map len ->
    for _i = 1 to 2 * len do
      skip_tree self
    done
  | Tag _ -> skip_tree self
  | Bytes_indefinite_start | Text_indefinite_start | Array_indefinite_start
  | Map_indefinite_start ->
    while
      match next_token self with
      | Indefinite_end -> false
      | _ -> true
    do
      ()
    done
  | Indefinite_end -> raise Indefinite
