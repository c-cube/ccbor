module Q = QCheck
module CBRef = Ccbor_reference
module C = Ccbor

let qsuite = ref []

let add_qtest ?name ?count ?long_factor gen prop =
  qsuite := Q.Test.make ?name ?count ?long_factor gen prop :: !qsuite

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

let tree_of_int64_ i =
  if Int64.(of_int (to_int i) = i) then
    C.Tree.Int (Int64.to_int i)
  else
    C.Tree.Int64 i

module Util_tree = struct
  open C.Tree

  let equal (t1 : t) (t2 : t) : bool = t1 = t2

  let rec shrink (self : t) : t Q.Iter.t =
    let open Q.Iter in
    match self with
    | Null | Undefined | Simple _ | Bool false -> empty
    | Float _ -> empty
    | Bool true -> return (Bool false)
    | Int i -> Q.Shrink.int i >|= fun i -> Int i
    | Int64 i -> Q.Shrink.int64 i >|= fun i -> tree_of_int64_ i
    | Bytes s -> Q.Shrink.string s >|= fun s -> Bytes s
    | Text s -> Q.Shrink.string s >|= fun s -> Text s
    | Array a ->
      (* return each elem, or shrink array *)
      (fun yield -> Array.iter (fun x -> yield x) a)
      <+> (Q.Shrink.array ~shrink a >|= fun a -> Array a)
    | Map m ->
      (fun yield ->
        Array.iter
          (fun (k, v) ->
            yield k;
            yield v)
          m)
      <+> ( Q.Shrink.array ~shrink:(Q.Shrink.pair shrink shrink) m >|= fun m ->
            Map m )
    | Tag (tag, v) ->
      return v
      <+> (Q.Shrink.int tag >|= fun tag -> Tag (tag, v))
      <+> (shrink v >|= fun v -> Tag (tag, v))

  let rec of_ref (r : CBRef.t) : t =
    match r with
    | `Bool b -> Bool b
    | `Simple i -> Simple i
    | `Tag (tag, v) -> Tag (tag, of_ref v)
    | `Null -> Null
    | `Array l -> Array (Array.of_list l |> Array.map of_ref)
    | `Map m -> Map (Array.of_list m |> Array.map (CCPair.map_same of_ref))
    | `Bytes s -> Bytes s
    | `Float f -> Float f
    | `Int i -> tree_of_int64_ i
    | `Undefined -> Undefined
    | `Text s -> Text s

  let rec to_ref (r : t) : CBRef.t =
    match r with
    | Bool b -> `Bool b
    | Simple i -> `Simple i
    | Tag (tag, v) -> `Tag (tag, to_ref v)
    | Null -> `Null
    | Array l -> `Array (Array.to_list l |> List.map to_ref)
    | Map m -> `Map (Array.to_list m |> List.map (CCPair.map_same to_ref))
    | Bytes s -> `Bytes s
    | Float f -> `Float f
    | Int i -> `Int (Int64.of_int i)
    | Int64 i -> `Int i
    | Undefined -> `Undefined
    | Text s -> `Text s
end

let gen_tree : C.Tree.t Q.Gen.t =
  Q.Gen.(
    let module T = C.Tree in
    sized @@ fix
    @@ fun self_ depth ->
    let base =
      [
        1, return T.Null;
        1, return T.Undefined;
        (1, float >|= fun f -> T.Float f);
        (1, bool >|= fun b -> T.Bool b);
        (2, int >|= fun x -> T.Int x);
        1, oneofl [ T.Int64 Int64.max_int; T.Int64 Int64.min_int ];
        (2, 0 -- 19 >|= fun x -> T.Simple x);
        (2, string_size ~gen:printable (0 -- 300) >|= fun s -> T.Text s);
        (2, string_size (0 -- 300) >|= fun s -> T.Bytes s);
      ]
    in
    let rec_ =
      [
        (1, array_size (0 -- 15) (self_ (depth / 2)) >|= fun a -> T.Array a);
        ( 1,
          array_size (0 -- 15) (pair (self_ 0) (self_ (depth / 3))) >|= fun m ->
          T.Map m );
        ( 1,
          0 -- 500 >>= fun tag ->
          self_ (depth - 1) >|= fun sub -> T.Tag (tag, sub) );
      ]
    in
    if depth = 0 then
      frequency base
    else
      frequency (base @ rec_))

let arb_tree : C.Tree.t Q.arbitrary =
  Q.make ~shrink:Util_tree.shrink ~print:C.Tree.show gen_tree

let () =
  add_qtest ~name:"encode_then_refdecode" ~count:1000 arb_tree (fun t ->
      let encoded =
        let enc = C.Encoder.create () in
        C.Encoder.add_tree enc t;
        C.Encoder.to_string enc
      in

      let decoded =
        let t_ref = CBRef.decode_exn encoded in
        Util_tree.of_ref t_ref
      in

      if not @@ Util_tree.equal t decoded then
        Q.Test.fail_reportf
          "initial: %a@ decoded(encoded(.)): %a@ :raw-encoded h'%s'" C.Tree.pp t
          C.Tree.pp decoded (hex_of_string encoded);

      true)

let () =
  add_qtest ~name:"encode_then_decode" ~count:1000 arb_tree (fun t ->
      let encoded =
        let enc = C.Encoder.create () in
        C.Encoder.add_tree enc t;
        C.Encoder.to_string enc
      in

      let decoded =
        let dec = C.Decoder.create_string encoded in
        C.Decoder.read_tree dec
      in

      (* Printf.eprintf "encoded: h'%s'\n%!" (hex_of_string encoded); *)
      if not @@ Util_tree.equal t decoded then
        Q.Test.fail_reportf
          "initial: %a@ decoded(encoded(.)): %a@ raw-encoded h'%s'" C.Tree.pp t
          C.Tree.pp decoded (hex_of_string encoded);

      true)

let () =
  add_qtest ~name:"refencode_then_decode" ~count:1000 arb_tree (fun t ->
      let encoded = CBRef.encode (Util_tree.to_ref t) in

      let decoded =
        let dec = C.Decoder.create_string encoded in
        C.Decoder.read_tree dec
      in

      (* Printf.eprintf "encoded: h'%s'\n%!" (hex_of_string encoded); *)
      if not @@ Util_tree.equal t decoded then
        Q.Test.fail_reportf
          "initial: %a@ decoded(refencoded(.)): %a@ raw-encoded h'%s'" C.Tree.pp
          t C.Tree.pp decoded (hex_of_string encoded);

      true)

let () =
  Option.iter
    (fun s -> QCheck_base_runner.set_seed (int_of_string s))
    (Sys.getenv_opt "SEED");
  QCheck_base_runner.run_tests_main (List.rev !qsuite)
