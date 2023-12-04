module B = Benchmark
module CB_ref = Ccbor_reference

let spf = Printf.sprintf

let rec mk_val ~n ~depth : CB_ref.t =
  let mk_rec i : CB_ref.t =
    `Map
      ((`Text "x", `Int (Int64.of_int i))
      :: (`Text "y", `Int (Int64.of_int (i * 100)))
      ::
      (if depth > 0 then
        [ `Text "sub", mk_val ~n ~depth:(depth - 1) ]
      else
        []))
  in
  `Array (List.init n mk_rec)

let rec ccbor_encode (enc : Ccbor.Encoder.t) ~n ~depth : unit =
  let module CE = Ccbor.Encoder in
  let enc_rec enc i : unit =
    if depth > 0 then
      CE.map_enter enc 3
    else
      CE.map_enter enc 2;

    CE.text enc "x";
    CE.int enc i;
    CE.text enc "y";
    CE.int enc (i * 100);
    if depth > 0 then (
      CE.text enc "sub";
      ccbor_encode enc ~n ~depth:(depth - 1)
    )
  in

  CE.array_enter enc n;
  for i = 0 to n - 1 do
    enc_rec enc i
  done

let ccbor_decode_skip (dec : Ccbor.Decoder.t) : unit =
  let module CD = Ccbor.Decoder in
  CD.skip_tree dec

module Enc = struct
  let bench ~n ~depth : B.Tree.t =
    B.Tree.(
      spf "n=%n,d=%d" n depth
      @> lazy
           (let v = mk_val ~n ~depth in
            let str = CB_ref.encode v in
            Printf.printf "size for n=%d, depth=%d: %d B\n" n depth
              (String.length str);

            let run_ref v () =
              ignore (Sys.opaque_identity (CB_ref.encode v) : string)
            in

            let enc = Ccbor.Encoder.create () in
            let run_ccbor enc () =
              Ccbor.Encoder.clear enc;
              ccbor_encode enc ~n ~depth
            in

            run_ccbor enc ();
            Printf.printf "ccbor size for n=%d, depth=%d: %d B\n" n depth
              (Ccbor.Encoder.total_size enc);

            B.throughputN ~repeat:2 3
              [ "ref", run_ref v, (); "ccbor", run_ccbor enc, () ]))

  let () =
    B.Tree.(
      register @@ "enc"
      @>> concat
            (List.map
               (fun (n, depth) -> bench ~n ~depth)
               [ 1, 1; 2, 1; 2, 2; 2, 3; 10, 1; 10, 2; 10, 3; 100, 1; 100, 2 ]))
end

module Dec = struct
  let bench ~n ~depth : B.Tree.t =
    B.Tree.(
      spf "n=%n,d=%d" n depth
      @> lazy
           (let v = mk_val ~n ~depth in
            let str = CB_ref.encode v in
            Printf.printf "size for n=%d, depth=%d: %d B\n" n depth
              (String.length str);

            let run_ref str () =
              ignore (Sys.opaque_identity (CB_ref.decode_exn str) : CB_ref.t)
            in

            let run_ccbor_tree str () : unit =
              let dec = Ccbor.Decoder.create_string str in
              ignore
                (Sys.opaque_identity
                   (Ccbor.Decoder.read_tree dec : Ccbor.Tree.t))
            in

            let run_ccbor_skip str () : unit =
              let dec = Ccbor.Decoder.create_string str in
              Sys.opaque_identity (ccbor_decode_skip dec)
            in

            B.throughputN ~repeat:2 3
              [
                "ref", run_ref str, ();
                "ccbor_tree", run_ccbor_tree str, ();
                "ccbor_skip", run_ccbor_skip str, ();
              ]))

  let () =
    B.Tree.(
      register @@ "dec"
      @>> concat
            (List.map
               (fun (n, depth) -> bench ~n ~depth)
               [ 1, 1; 2, 1; 2, 2; 2, 3; 10, 1; 10, 2; 10, 3; 100, 1; 100, 2 ]))
end

let () = B.Tree.(run_global ())
