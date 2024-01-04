module CP = Cbor_pack

type out = {
  char: char -> unit;
  write: bytes -> int -> int -> unit;
}

let spf = Printf.sprintf
let fpf = Printf.fprintf
let bpf = Printf.bprintf

module Buf_ = struct
  type t = {
    mutable b: bytes;
    mutable off: int;
  }

  let create () : t = { b = Bytes.create 128; off = 0 }
  let[@inline] cap self = Bytes.length self.b
  let[@inline] clear self = self.off <- 0

  let[@inline never] ensure_grow_ (self : t) n =
    assert (cap self < n);
    let new_b = Bytes.create n in
    Bytes.blit self.b 0 new_b 0 self.off;
    self.b <- new_b

  let[@inline] ensure_size_ self n =
    if self.off + n > cap self then
      ensure_grow_ self (max (cap self * 2) (self.off + n))

  let[@inline] add_char self c =
    ensure_size_ self 1;
    Bytes.set self.b self.off c;
    self.off <- self.off + 1

  let add_sub_string (self : t) s i len =
    ensure_size_ self len;
    Bytes.blit_string s i self.b self.off len;
    self.off <- self.off + len

  let[@inline] add_string self s = add_sub_string self s 0 (String.length s)
end

let hex_of_string s =
  let hex_char x =
    if x <= 9 then
      Char.chr @@ (Char.code '0' + x)
    else
      Char.chr @@ (Char.code 'a' + x - 10)
  in
  let r = Bytes.create (String.length s * 2) in
  for i = 0 to String.length s - 1 do
    Bytes.set r (i * 2) @@ hex_char @@ (Char.code s.[i] lsr 4);
    Bytes.set r ((i * 2) + 1) @@ hex_char @@ (Char.code s.[i] land 0b1111)
  done;
  Bytes.unsafe_to_string r

let add_indent (out : Buffer.t) (indent : int) =
  for _i = 1 to indent do
    Buffer.add_char out ' '
  done

(** Does [s] print ok with "%S"? *)
let string_is_printable (s : string) : bool =
  try
    let[@inline] is_ok = function
      | '\x07' .. '\x0d' -> true
      | '\x20' .. '\x7e' -> true
      | _ -> false
    in
    String.iter (fun c -> if not (is_ok c) then raise Exit) s;
    true
  with Exit -> false

let rec deref (deser : CP.Deser.state) (i : int) =
  let c = CP.Private_.deser_heap_get deser i in
  match c with
  | `Tag (6, `Int j) -> deref deser j
  | _ -> c

(** Dump immediate values, and a trivial summary for the rest *)
let rec dump_immediate (c : CP.cbor) : string =
  match c with
  | `Null -> "null"
  | `Undefined -> "undefined"
  | `Simple i -> spf "s(%d)" i
  | `Int i -> spf "%d" i
  | `Bool b -> spf "%b" b
  | `Float f -> spf "%f" f
  | `Text s ->
    if String.length s > 20 then
      spf "%S[…%d omitted]" (String.sub s 0 20) (String.length s - 20)
    else
      spf "%S" s
  | `Bytes b -> spf "bytes(…[%d omitted])" (String.length b)
  | `Array l -> spf "[…[%d omitted]]" (List.length l)
  | `Map l -> spf "{…[%d omitted]}" (List.length l)
  | `Tag (c, x) -> spf "%d(%s)" c (dump_immediate x)

let dump_bytes_summary b : string =
  let b, tail =
    if String.length b > 20 then
      String.sub b 0 20, spf "…[%dB omitted]" (String.length b - 20)
    else
      b, ""
  in
  if string_is_printable b then
    spf "bytes(%S%s)" b tail
  else
    spf "bytes(h'%s%s')" (hex_of_string b) tail

(** Dump a summary of the value, including small map/arrays *)
let rec dump_summary (deser : CP.Deser.state) depth (c : CP.cbor) : string =
  let[@inline] recurse c =
    if depth <= 0 then
      dump_immediate c
    else
      dump_summary deser (depth - 1) c
  in
  match c with
  | `Null -> "null"
  | `Undefined -> "undefined"
  | `Simple i -> spf "s(%d)" i
  | `Int i -> spf "%d" i
  | `Bool b -> spf "%b" b
  | `Float f -> spf "%f" f
  | `Text s ->
    if String.length s > 20 then
      spf "%S[…%d omitted]" (String.sub s 0 20) (String.length s - 20)
    else
      spf "%S" s
  | `Bytes b -> dump_bytes_summary b
  | `Array l ->
    (match l with
    | x :: y :: z :: (_ :: _ as tl) ->
      spf "[%s,%s,%s,…(%d)]" (recurse x) (recurse y) (recurse z)
        (List.length tl)
    | _ -> spf "[%s]" (String.concat "," @@ List.map recurse l))
  | `Map l ->
    let ppkv (k, v) = spf "%s: %s" (recurse k) (recurse v) in
    (match l with
    | kv1 :: kv2 :: kv3 :: (_ :: _ as tl) ->
      spf "{%s,%s,%s,…(%d)]" (ppkv kv1) (ppkv kv2) (ppkv kv3) (List.length tl)
    | _ -> spf "{%s}" (String.concat "," @@ List.map ppkv l))
  | `Tag (6, `Int i) -> recurse (deref deser i)
  | `Tag (c, x) -> spf "%d(%s)" c (recurse x)

let rec dump_c (deser : CP.Deser.state) (indent : int) (out : Buffer.t)
    (c : CP.cbor) : unit =
  match c with
  | `Null -> bpf out "null"
  | `Undefined -> bpf out "undefined"
  | `Simple i -> bpf out "s(%d)" i
  | `Int i -> bpf out "%d" i
  | `Bool b -> bpf out "%b" b
  | `Float f -> bpf out "%f" f
  | `Text s -> bpf out "%S" s
  | `Bytes b ->
    if string_is_printable b then
      bpf out "bytes(%S)" b
    else
      bpf out "bytes(h'%s')" (hex_of_string b)
  | `Array [] -> bpf out "[]"
  | `Array l ->
    bpf out "Array [";
    List.iteri
      (fun i x ->
        if i > 0 then bpf out ",";
        bpf out "\n%a %a" add_indent indent (dump_c deser (indent + 2)) x)
      l;
    bpf out " ]"
  | `Map [] -> bpf out "{}"
  | `Map l ->
    bpf out "Map {";
    List.iter
      (fun (x, y) ->
        bpf out "\n%a%a:\n%a%a" add_indent indent
          (dump_c deser (indent + 2))
          x add_indent (indent + 2)
          (dump_c deser (indent + 2))
          y)
      l;
    bpf out " }"
  | `Tag (6, `Int i) ->
    let pointee = deref deser i in
    bpf out "&%s @%d" (dump_summary deser 3 pointee) i
  | `Tag (c, x) -> bpf out "%d(%a)" c (dump_c deser indent) x

let dump (out : Buffer.t) (self : CP.Deser.state) : unit =
  bpf out "heap:\n";
  CP.Private_.deser_heap_iter self (fun i x ->
      bpf out "  %06d: %a\n" i (dump_c self 4) x);
  bpf out "key: %a\n" (dump_c self 2) (CP.Private_.deser_key self);
  ()

let dump_oc (oc : out_channel) self : unit =
  (* use local buffer to dump individual entries
     before writing them into [oc]. We don't buffer the whole
     output here as the value might be very large and we have the
     opportunity to wite to [oc] as we go. *)
  let buf = Buffer.create 128 in

  fpf oc "heap:\n";
  CP.Private_.deser_heap_iter self (fun i x ->
      Buffer.clear buf;
      bpf buf "  %06d: %a\n" i (dump_c self 4) x;
      Buffer.output_buffer oc buf);

  Buffer.clear buf;
  bpf buf "key: %a\n" (dump_c self 2) (CP.Private_.deser_key self);
  Buffer.output_buffer oc buf
