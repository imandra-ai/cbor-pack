module CP = Cbor_pack
module J = Yojson.Basic
module Dump = Cbor_pack_dump

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
    Dump.dump_oc stdout cp
  else (
    let oc = open_out config.out in
    let@ () = Fun.protect ~finally:(fun () -> close_out oc) in
    Dump.dump_oc oc cp;
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
