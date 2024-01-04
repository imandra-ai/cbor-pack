module CP = Cbor_pack
module J = Yojson.Basic

let ( let@ ) = ( @@ )
let spf = Printf.sprintf
let fpf = Printf.fprintf
let epf = Printf.eprintf
let file_is_zip f = Filename.extension f = ".zip"

module Config = struct
  type t = {
    mutable file: string;
    mutable to_json: bool;
    mutable debug: bool;
    mutable out: string;
    mutable zip_entry: string;
  }

  let create () : t =
    { file = ""; to_json = false; debug = false; out = ""; zip_entry = "" }
end

module Dump = struct
  type t =
    [ `Null
    | `Undefined
    | `Simple of int
    | `Bool of bool
    | `Int of int
    | `Float of float
    | `Bytes of string
    | `Text of string
    | `Array of t list
    | `Map of (t * t) list
    | `Tag of int * t
    ]

  let to_hex s =
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

  let add_indent (oc : out_channel) (indent : int) =
    for _i = 1 to indent do
      output_char oc ' '
    done

  let is_printable (s : string) =
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
    | `Bytes b -> spf "bytes(%d))" (String.length b)
    | `Array l -> spf "array(%d)" (List.length l)
    | `Map l -> spf "map(%d)" (List.length l)
    | `Tag (c, x) -> spf "%d(%s)" c (dump_immediate x)

  let rec dump_short (deser : CP.Deser.state) depth (c : CP.cbor) : string =
    let[@inline] recurse c =
      if depth <= 0 then
        dump_immediate c
      else
        dump_short deser (depth - 1) c
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
    | `Bytes b -> spf "bytes(%d))" (String.length b)
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

  let rec dump_c (deser : CP.Deser.state) (indent : int) (oc : out_channel)
      (c : CP.cbor) : unit =
    match c with
    | `Null -> fpf oc "null"
    | `Undefined -> fpf oc "undefined"
    | `Simple i -> fpf oc "s(%d)" i
    | `Int i -> fpf oc "%d" i
    | `Bool b -> fpf oc "%b" b
    | `Float f -> fpf oc "%f" f
    | `Text s -> fpf oc "%S" s
    | `Bytes b ->
      if is_printable b then
        fpf oc "bytes(%S)" b
      else
        fpf oc "bytes(h'%s')" (to_hex b)
    | `Array l ->
      fpf oc "array(%d)" (List.length l);
      List.iteri
        (fun i x ->
          fpf oc "\n%a- [%d] %a" add_indent indent i
            (dump_c deser (indent + 2))
            x)
        l
    | `Map l ->
      fpf oc "map(%d)" (List.length l);
      List.iter
        (fun (x, y) ->
          fpf oc "\n%a- %a:\n%a%a" add_indent indent
            (dump_c deser (indent + 2))
            x add_indent (indent + 2)
            (dump_c deser (indent + 2))
            y)
        l
    | `Tag (6, `Int i) ->
      let pointee = deref deser i in
      fpf oc "ptr(%d)\n%a(… %s)" i add_indent (indent + 2)
        (dump_short deser 3 pointee)
    | `Tag (c, x) -> fpf oc "%d(%a)" c (dump_c deser indent) x

  let dump (oc : out_channel) (self : CP.Deser.state) : unit =
    fpf oc "heap:\n";
    CP.Private_.deser_heap_iter self (fun i x ->
        fpf oc "  %d: %a\n" i (dump_c self 4) x);
    fpf oc "key: %a\n" (dump_c self 2) (CP.Private_.deser_key self);
    ()
end

let read_content (config : Config.t) : string =
  if file_is_zip config.file then (
    let zip = Zip.open_in config.file in
    let@ () = Fun.protect ~finally:(fun () -> Zip.close_in zip) in
    let entries = Zip.entries zip in
    let entry =
      if config.zip_entry <> "" then (
        try List.find (fun e -> e.Zip.filename = config.zip_entry) entries
        with Not_found ->
          failwith
            (spf "Could not find entry %S in zip file %S" config.zip_entry
               config.file)
      ) else (
        match entries with
        | [ e ] -> e
        | [] -> failwith "Zip file is empty"
        | _ ->
          failwith
            (spf
               "There are %d entries in the Zip file, use --zip-entry to \
                specify which one to show"
               (List.length entries))
      )
    in

    Zip.read_entry zip entry
  ) else (
    let ic = open_in config.file in
    let@ () = Fun.protect ~finally:(fun () -> close_in ic) in
    let len = in_channel_length ic in
    let str = Bytes.create len in
    really_input ic str 0 len;
    Bytes.unsafe_to_string str
  )

let main (config : Config.t) =
  let content = read_content config in
  if config.debug then
    epf "read %d B from %S\n%!" (String.length content) config.file;

  let cp =
    match CP.Deser.parse content with
    | Ok c -> c
    | Error err -> failwith @@ spf "Error when decoding content: %s" err
  in

  if config.to_json then
    (* TODO: turn into json *)
    failwith "TODO: json"
  else if config.out = "" then
    Dump.dump stdout cp
  else (
    let oc = open_out config.out in
    let@ () = Fun.protect ~finally:(fun () -> close_out oc) in
    Dump.dump oc cp;
    flush oc
  )

let () =
  let config = Config.create () in

  let opts =
    [
      "-d", Arg.Unit (fun () -> config.debug <- true), " debug";
      ( "--to-json",
        Arg.Unit (fun () -> config.to_json <- true),
        " output as json" );
      "-o", Arg.String (fun s -> config.out <- s), " output file";
      ( "--zip-entry",
        Arg.String (fun s -> config.zip_entry <- s),
        " entry to unpack in a .zip file" );
    ]
    |> Arg.align
  in

  Arg.parse opts
    (fun f ->
      if config.file = "" then
        config.file <- f
      else
        raise @@ Arg.Bad "please provide only one file")
    "cbpacktk <file> [opt*]";
  if config.file = "" then failwith "No file provided.";

  main config;
  ()
