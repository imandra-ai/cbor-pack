type 'a foo = {
  x: 'a option;
  y: (int * 'a) list;
}
[@@deriving cbpack, show]

type ('a, 'b) bar = {
  f1: 'a foo list;
  f2: 'b foo list;
}
[@@deriving cbpack, show]

type bar2 = (int, bool) bar [@@deriving show, cbpack]

let b : bar2 =
  {
    f1 = [ { x = Some 1; y = [ 0, 0; 1, 1; 2, 2 ] } ];
    f2 = [ { x = None; y = [] }; { x = Some true; y = [ 0, true; 1, false ] } ];
  }
;;

Format.printf "b: %a@." pp_bar2 b

let s = Cbor_pack.to_string bar2_to_cbpack b;;

Format.printf "len=%d@." (String.length s)

let b' = Cbor_pack.of_string_exn bar2_of_cbpack s;;

Format.printf "b': %a@." pp_bar2 b';;

assert (b = b')
