module M1 = struct
  Format.printf "without hashcons@."

  type t0 = {
    x: int; [@key "X"]
    y: bool; [@key "Y"]
  }
  [@@deriving cbpack, show]

  type t1 = { foos: t0 list } [@@deriving cbpack, show]

  let myt1 =
    let x1 = { x = 0; y = false } in
    let x2 = { x = 2; y = true } in
    { foos = [ x1; x1; x2; x1; x2 ] }
  ;;

  Format.printf "myt1: %a@." pp_t1 myt1

  let pack1 = Cbor_pack.to_cbor t1_to_cbpack myt1;;

  Format.printf "pack1: %a@." Cbor_pack.pp_diagnostic pack1

  let len1 = Cbor_pack.to_string t1_to_cbpack myt1 |> String.length;;

  Format.printf "len1: %d@." len1
end

module M2 = struct
  Format.printf "with hashcons@."

  type t0 = {
    x: int; [@key "x0"]
    y: bool; [@key "y"]
  }
  [@@deriving cbpack, show] [@@cbpack.hashcons]

  type t1 = { foos: t0 list } [@@deriving cbpack, show]

  let myt1 =
    let x1 = { x = 0; y = false } in
    let x2 = { x = 2; y = true } in
    { foos = [ x1; x1; x2; x1; x2 ] }
  ;;

  Format.printf "myt1': %a@." pp_t1 myt1

  let pack1 = Cbor_pack.to_cbor t1_to_cbpack myt1;;

  Format.printf "pack1: %a@." Cbor_pack.pp_diagnostic pack1

  let len1 = Cbor_pack.to_string t1_to_cbpack myt1 |> String.length;;

  Format.printf "len1: %d@." len1
end
