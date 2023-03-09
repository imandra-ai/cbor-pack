type t1 =
  | F of (string[@as_bytes])
  | G of string
[@@deriving show, cbpack]

let x = Cbor_pack.to_string t1_to_cbpack (F "hello\x000world");;

Format.printf "x: %S@." x;;
Format.printf "len: %d@." (String.length x)

let t1' = Cbor_pack.of_string_exn t1_of_cbpack x;;

Format.printf "deser: %a@." pp_t1 t1'
