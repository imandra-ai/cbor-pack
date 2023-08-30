type x1 =
  | A of int
  | B [@cstor "b"]
  | C of {
      x: int option;
      z: (bool * char) list; [@key "cz"]
    } [@cstor "c"]
  | D of x1 * x1 [@cstor "d"]
[@@deriving cbpack, show { with_path = false }] [@@hashcons]

type x1_alias = x1 [@@deriving cbpack, show]

type x2 = {
  a: x1_alias;
  b: x1 list list array;
  c: x2 list option;
}
[@@deriving cbpack, show { with_path = false }]
[@@cbpack.use_field_names]
[@@hashcons]

let c0 = C { x = Some 3; z = [ true, 'a'; false, 'c' ] }

let myx2 =
  let c0 = C { x = Some 3; z = [ true, 'a'; false, 'c' ] } in
  let c1 = B in
  {
    a = A 42;
    b = [| [ []; [ c0 ] ]; [ [ B; c1; c1 ]; [ c0 ]; [ c0; D (c0, c0) ] ] |];
    c = None;
  }

let myx2_big = { a = B; b = [| [ []; [ c0; c0 ] ] |]; c = Some [ myx2; myx2 ] }
;;

Format.printf "start with myx2:@.%a@." pp_x2 myx2

let s = Cbor_pack.to_string x2_to_cbpack myx2;;

Printf.printf "serialized to %S\n" s;;
Printf.printf "len: %d\n" (String.length s)

let s' = Marshal.to_string myx2 [ Marshal.No_sharing ];;

Printf.printf "len with marshal: %d\n" (String.length s')

let deser = Cbor_pack.Deser.parse_exn s;;

Format.printf "deserialized CBOR value: %a@." Cbor_pack.Deser.pp_diagnostic
  deser

let ptr0 = Cbor_pack.Deser.entry_key deser
let r = Cbor_pack.Deser.(deref deser (to_ptr ptr0));;

Format.printf "deref cbor ptr: %a@." Cbor_pack.pp_diagnostic r

let foo2 = x2_of_cbpack deser ptr0;;

Format.printf "myx2 after roundtrip:@.%a@." pp_x2 foo2;;
assert (foo2 = myx2)

let s_big = Cbor_pack.to_string x2_to_cbpack myx2_big;;

Printf.printf "len: %d\n" (String.length s_big)

let s'_big = Marshal.to_string myx2_big [ Marshal.No_sharing ];;

Printf.printf "len(big) with marshal: %d\n" (String.length s'_big)

let foo2_big = Cbor_pack.of_string_exn x2_of_cbpack s_big;;

Format.printf "deser(myx2_big):@.%a@." pp_x2 foo2_big

let cbor_big = Cbor_pack.to_cbor x2_to_cbpack myx2_big;;

Format.printf "myx2_big CBOR is:@.%a@." Cbor_pack.pp_diagnostic cbor_big;;
assert (myx2_big = foo2_big)
