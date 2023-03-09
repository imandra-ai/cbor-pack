type foo = {
  a: int;
  b: float;
}
[@@deriving cbpack, show]

let s = Cbor_pack.to_string foo_to_cbpack { a = 1; b = 2.0 };;

Printf.printf "serialized to %S\n" s;;

Printf.printf "len: %d\n" (String.length s)

let foo2 = Cbor_pack.of_string_exn foo_of_cbpack s;;

Format.printf "foo after roundtrip: %a@." pp_foo foo2
