type t = unit [@@deriving cbpack]

let m = Cbor_pack.to_string to_cbpack ();;

Format.printf "size of empty msg is %d@." (String.length m)
