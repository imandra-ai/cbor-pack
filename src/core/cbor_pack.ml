module Cbor = CBOR.Simple

type cbor = CBOR.Simple.t

module Vec = struct
  type t = {
    mutable a: cbor array;
    mutable sz: int;
  }

  let create () : t = { sz = 0; a = [||] }
  let length self = self.sz
  let cap self = Array.length self.a

  let get self i =
    if i >= self.sz then invalid_arg "Vec.get";
    self.a.(i)

  let grow self =
    let new_cap = max 4 (min Sys.max_array_length (2 * cap self)) in
    if new_cap = cap self then failwith "Vec cannot grow";
    let a' = Array.make new_cap `Null in
    Array.blit self.a 0 a' 0 self.sz;
    self.a <- a'

  let push self x =
    if self.sz = cap self then grow self;
    self.a.(self.sz) <- x;
    self.sz <- 1 + self.sz

  let iteri f self =
    for i = 0 to self.sz - 1 do
      f i self.a.(i)
    done

  let to_list self = List.init self.sz (fun i -> self.a.(i))

  let of_list l : t =
    let v = create () in
    List.iter (push v) l;
    v
end

(** Tag for pointers.
    [6] is not used, and fits in a single byte:
    See {{: https://www.iana.org/assignments/cbor-tags/cbor-tags.xhtml} the registry}. *)
let tag_ptr = 6

(** Main hashconsing table *)
module Cbor_table = Hashtbl.Make (struct
  type t = cbor

  let rec equal (a : t) (b : t) =
    match a, b with
    | `Null, `Null | `Undefined, `Undefined -> true
    | `Simple x, `Simple y -> x = y
    | `Bool x, `Bool y -> x = y
    | `Int x, `Int y -> x = y
    | `Float x, `Float y -> x = y
    | `Bytes x, `Bytes y -> x = y
    | `Text x, `Text y -> x = y
    | `Array x, `Array y ->
      List.length x = List.length y && List.for_all2 equal x y
    | `Map x, `Map y ->
      List.length x = List.length y
      && List.for_all2 (fun (k1, v1) (k2, v2) -> equal k1 k2 && equal v1 v2) x y
    | `Tag (x, u), `Tag (y, v) -> x = y && equal u v
    | _ -> false

  let[@inline] hash (c : cbor) : int = Hashtbl.hash_param 16 100 c
end)

module Ser = struct
  type ptr = cbor

  type state = {
    entries: Vec.t;
    hashcons: ptr Cbor_table.t;
    mutable hmap: Hmap.t;
  }

  type 'a t = state -> 'a -> cbor

  let create () : state =
    {
      entries = Vec.create ();
      hashcons = Cbor_table.create 16;
      hmap = Hmap.empty;
    }

  let[@inline] mk_ptr_ n = `Tag (tag_ptr, `Int n)
  let unit : cbor = `Null
  let[@inline] int x : cbor = `Int x
  let[@inline] bool x : cbor = `Bool x
  let[@inline] float x : cbor = `Float x
  let[@inline] list x : cbor = `Array x
  let[@inline] list_of f x = list (List.map f x)
  let[@inline] map x : cbor = `Map x
  let[@inline] string x : cbor = `Text x
  let[@inline] bytes x : cbor = `Bytes (Bytes.unsafe_to_string x)
  let[@inline] hmap self = self.hmap

  let update_hmap (self : state) f =
    let new_hmap, x = f self.hmap in
    self.hmap <- new_hmap;
    x

  let add_entry ?(hashcons = false) (self : state) (c : cbor) : ptr =
    match c with
    | `Tag (t, `Int _) when t == tag_ptr -> c (* do not add pointers *)
    | `Int _ | `Bool _ | `Null | `Undefined | `Float _ ->
      c (* do not add scalars *)
    | _ ->
      (try Cbor_table.find self.hashcons c
       with Not_found ->
         let n = Vec.length self.entries in
         Vec.push self.entries c;
         let ptr = mk_ptr_ n in
         if hashcons then Cbor_table.add self.hashcons c ptr;
         ptr)

  (** strings bigger than that will get their own entry *)
  let _hashcons_limit_str = 32

  let add_string ?(hashcons = false) self s : cbor =
    let c = string s in
    if hashcons || String.length s >= _hashcons_limit_str then
      add_entry ~hashcons:true self c
    else
      c

  let add_bytes ?(hashcons = false) self b : cbor =
    let c = bytes b in
    if hashcons || Bytes.length b >= _hashcons_limit_str then
      add_entry ~hashcons:true self c
    else
      c

  let finalize_cbor (self : state) ~key : cbor =
    map [ `Text "k", key; `Text "h", `Array (Vec.to_list self.entries) ]

  let finalize_string (self : state) ~key : string =
    Cbor.encode @@ finalize_cbor self ~key
end

let to_cbor (ser : 'a Ser.t) x : cbor =
  let st = Ser.create () in
  let key = ser st x in
  Ser.finalize_cbor st ~key

let to_string ser x =
  let st = Ser.create () in
  let key = ser st x in
  Ser.finalize_string st ~key

module Deser = struct
  type state = {
    c: cbor;
    entries: Vec.t;  (** heap *)
    key: cbor;  (** entrypoint *)
  }

  type 'a or_error = ('a, string) result
  type 'a t = state -> cbor -> 'a

  exception Error = CBOR.Error

  let error_ s = raise (Error s)
  let errorf_ s = Printf.ksprintf error_ s

  type ptr = int

  let ptr_of_int x = x

  let deref self (n : ptr) =
    if n >= Vec.length self.entries then
      error_ "cbor_pack.deser.deref: invalid index";
    Vec.get self.entries n

  let rec deref_if_ptr self (x : cbor) : cbor =
    match x with
    | `Tag (t, `Int i) when t = tag_ptr ->
      (*Format.printf "deref %d@." i;*)
      deref_if_ptr self @@ deref self i
    | _ -> x

  let to_unit state c =
    match deref_if_ptr state c with
    | `Null -> ()
    | _ -> error_ "expected null"

  let to_int_ = function
    | `Int i -> i
    | _ -> error_ "expected integer"

  let to_int state c = to_int_ @@ deref_if_ptr state c

  let to_bool state c =
    match deref_if_ptr state c with
    | `Bool x -> x
    | _ -> error_ "expected bool"

  let to_float state c =
    match deref_if_ptr state c with
    | `Float x -> x
    | _ -> error_ "expected float"

  let to_list_ = function
    | `Array x -> x
    | _ -> error_ "expected array"

  let to_list state c = to_list_ @@ deref_if_ptr state c

  let to_list_of f state c =
    match deref_if_ptr state c with
    | `Array x -> List.map (f state) x
    | _ -> error_ "expected array"

  let to_map_ = function
    | `Map l -> l
    | _ -> error_ "expected map"

  let to_map state c = to_map_ @@ deref_if_ptr state c

  let to_text state c =
    match deref_if_ptr state c with
    | `Text x -> x
    | _ -> error_ "expected text"

  let to_bytes state c =
    match deref_if_ptr state c with
    | `Bytes x -> Bytes.unsafe_of_string x
    | _ -> error_ "expected bytes"

  let to_any_tag state c =
    match deref_if_ptr state c with
    | `Tag (j, sub) -> j, sub
    | _ -> error_ "expected (any) tag"

  let to_tag_ i = function
    | `Tag (j, sub) when i = j -> sub
    | `Tag _ -> error_ "wrong tag"
    | _ -> errorf_ "expected tag %d" i

  let to_tag i state c = to_tag_ i @@ deref_if_ptr state c
  let to_ptr x : ptr = to_int_ @@ to_tag_ tag_ptr x
  let ( let+ ) self f state c = f (self state c)

  let ( let* ) self (f : _ -> _ t) state c =
    let x = self state c in
    f x state c

  let map_entry_ ~k (c : cbor) : cbor =
    let m = to_map_ c in
    try List.assoc k m with Not_found -> error_ "cannot find key in map"

  let map_entry ~k state (c : cbor) : cbor =
    let m = to_map state c in
    try List.assoc k m with Not_found -> error_ "cannot find key in map"

  let entry_key self = self.key

  let of_cbor_ c : state =
    (* see: Ser.finalize *)
    let key = map_entry_ ~k:(`Text "k") c in
    let entries = map_entry_ ~k:(`Text "h") c |> to_list_ |> Vec.of_list in
    { c; entries; key }

  let parse s : state or_error =
    try
      let c = Cbor.decode s in
      Ok (of_cbor_ c)
    with CBOR.Error s -> Error s

  let parse_exn s =
    match parse s with
    | Ok x -> x
    | Error s -> invalid_arg s

  let pp_diagnostic out self =
    Format.fprintf out "{@[<hv>h=[@[<hv>";
    Vec.iteri
      (fun i x ->
        let x =
          match x with
          | `Text s when String.length s > 100 ->
            `Text
              (Printf.sprintf "%s… (%d more bytes)" (String.sub s 0 100)
                 (String.length s - 100))
          | `Bytes s when String.length s > 50 ->
            `Bytes
              (Printf.sprintf "%S… (%d more bytes)" (String.sub s 0 50)
                 (String.length s - 50))
          | x -> x
        in
        Format.fprintf out "@[%d: %s@];@ " i (Cbor.to_diagnostic x))
      self.entries;
    Format.fprintf out "@]];@ k=%s@]}" (Cbor.to_diagnostic self.key)

  let show_diagnostic self =
    let buf = Buffer.create 32 in
    Format.fprintf (Format.formatter_of_buffer buf) "%a@?" pp_diagnostic self;
    Buffer.contents buf
end

let pp_diagnostic out cbor =
  match Deser.of_cbor_ cbor with
  | deser -> Deser.pp_diagnostic out deser
  | exception _ -> Format.pp_print_string out (Cbor.to_diagnostic cbor)

let of_cbor_exn deser h =
  let st = Deser.of_cbor_ h in
  let key = Deser.entry_key st in
  deser st key

let of_cbor deser h =
  try Ok (of_cbor_exn deser h) with Deser.Error e -> Error e

let of_string_exn deser h =
  let st = Deser.parse_exn h in
  let key = Deser.entry_key st in
  deser st key

let of_string deser h =
  try Ok (of_string_exn deser h) with Deser.Error e -> Error e
