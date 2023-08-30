# Cbor-pack

[![build](https://github.com/imandra-ai/cbor-pack/actions/workflows/main.yml/badge.svg)](https://github.com/imandra-ai/cbor-pack/actions/workflows/main.yml)

This is a serialization format that adds some structure on top of [CBOR](https://cbor.io).

The additional layer is used to encode sharing in a reasonably efficient way, by
emulating a small heap in the CBOR structure.

A cbor-pack value (or "pack"), on the wire, is the CBOR representation of:

```rust
struct Pack {
  h: Vec<Cbor>,
  k: Cbor
}
```

which can be understood as:
- `k` is the key, the entrypoint. It's the actual value.
- `h` is the heap, used to map _pointers_ to CBOR values.

A pointer, in this context, is a tagged integer `6(n)`, where `n` is the offset
of the actual value in the array `h`. Any value, including `k`, can contain such
pointers (since they are CBOR values), and deserialization will follow the
pointers automatically.

## ppx

How to use the ppx:

In dune:
```
(library
  (name foo)
  …
  (libraries … cbor-pack)
  (preprocess (pps cbor-pack-ppx)))
```

and in the code:

```ocaml
# #require "cbor-pack";;
# #require "cbor-pack-ppx";;
```

```ocaml
# type foo = {
    x: int;
    y: (string [@as_bytes]);
  } [@@deriving cbpack];;
type foo = { x : int; y : string; }
val foo_to_cbpack : Cbor_pack.Ser.state -> foo -> Cbor_pack.cbor = <fun>
val foo_of_cbpack : Cbor_pack.Deser.state -> Cbor_pack.cbor -> foo = <fun>
```

which creates two functions for serializing and deserializing.

### Example: a record

For example, in `tests/t1.ml`, we can serialize and deserialize:

```ocaml
# type foo = {
    a: int;
    b: float;
  } [@@deriving cbpack] ;;
type foo = { a : int; b : float; }
val foo_to_cbpack : Cbor_pack.Ser.state -> foo -> Cbor_pack.cbor = <fun>
val foo_of_cbpack : Cbor_pack.Deser.state -> Cbor_pack.cbor -> foo = <fun>
```

Then we can encode a value:

```ocaml
# let my_foo = { a = 1; b = 2.0 };;
val my_foo : foo = {a = 1; b = 2.}
# let c = Cbor_pack.to_cbor foo_to_cbpack my_foo;;
val c : Cbor_pack.cbor =
  `Map
    [(`Text "k", `Tag (6, `Int 0));
     (`Text "h", `Array [`Map [(`Int 0, `Int 1); (`Int 1, `Float 2.)]])]

# CBOR.Simple.to_diagnostic c |> print_endline;;
{"k": 6(0), "h": [{0: 1, 1: 2.}]}
- : unit = ()

# let s = Cbor_pack.to_string foo_to_cbpack my_foo;;
...
# String.length s;;
- : int = 21
```

and deserialize it again:

```ocaml
# let foo2 = Cbor_pack.of_string_exn foo_of_cbpack s;;
val foo2 : foo = {a = 1; b = 2.}

# my_foo = foo2;;
- : bool = true
```

### Hashconsing

Hashconsing is sharing done on the heap itself. If the same CBOR value `c` is
added twice to the heap with the hashconsing option enabled, the second
occurrence will not be added but will reuse a pointer to the first entry.

This has a cost at runtime (hashtable lookups), but can result in a
significantly smaller pack at the end. Hashconsing is generic and can work on
any type because it proceeds entirely on serialized values.

### Caching during serialization

The serializer can cache previously encoded values for types that are comparable and hashable:

```ocaml
type foo = {
  x: int;
  y: bool
} [@@deriving cbpack]

(* used inside the cache *)
module Foo = struct
  type t = foo
  let equal a b = a.x=b.x && a.y=b.y
  let hash = Hashtbl.hash
end

let key_cache_ser_foo = Cbor_pack.Ser.create_cache_key (module Foo) ;;

let foo_to_cbpack_cached: foo Cbor_pack.Ser.t =
    Cbor_pack.Ser.with_cache key_cache_ser_foo foo_to_cbpack;;
```

(Note how `foo_to_cbpack_cached` needs both a key, and the uncached serializer which is used
for values that aren't already in the cache).

Now we can encode values and introduce sharing in a way that is more efficient at runtime
that using hashconsing (values already encoded are not re-encoded at all).

```ocaml
# let l =
    let f1: foo = {x=1; y=true} in
    let f2: foo = {x=2; y=false} in
    [f1; f2; f1; f2; f1; f2; f2; f1];;
val l : foo list =
  [{x = 1; y = true}; {x = 2; y = false}; {x = 1; y = true};
   {x = 2; y = false}; {x = 1; y = true}; {x = 2; y = false};
   {x = 2; y = false}; {x = 1; y = true}]


# Cbor_pack.to_cbor Cbor_pack.Ser.(list_of foo_to_cbpack_cached) l;;
- : Cbor_pack.cbor =
`Map
  [(`Text "k",
    `Array
      [`Tag (6, `Int 0); `Tag (6, `Int 1); `Tag (6, `Int 0);
       `Tag (6, `Int 1); `Tag (6, `Int 0); `Tag (6, `Int 1);
       `Tag (6, `Int 1); `Tag (6, `Int 0)]);
   (`Text "h",
    `Array
      [`Map [(`Int 0, `Int 1); (`Int 1, `Bool true)];
       `Map [(`Int 0, `Int 2); (`Int 1, `Bool false)]])]

# Cbor_pack.to_string Cbor_pack.Ser.(list_of foo_to_cbpack_cached) l |> String.length;;
- : int = 33
```

Note that without caching we get a bigger value:

```ocaml

# Cbor_pack.to_string Cbor_pack.Ser.(list_of foo_to_cbpack) l |> String.length;;
- : int = 63
```

### Example: a tree

```ocaml
type tree =
  | Nil
  | Node of int * tree * tree
  [@@deriving cbpack] [@@hashcons];;
```


```ocaml
# let t =
    let t2 = Node (2, Nil, Nil) in
    let t3 = Node (3, t2, t2) in
    let t4 = Node (4, t3, t2) in
    Node (1, t4, t4);;
val t : tree =
  Node (1,
   Node (4, Node (3, Node (2, Nil, Nil), Node (2, Nil, Nil)),
    Node (2, Nil, Nil)),
   Node (4, Node (3, Node (2, Nil, Nil), Node (2, Nil, Nil)),
    Node (2, Nil, Nil)))
```

Note that serializing this (quite redundant) tree into CBOR would produce
a similarly-shaped tree. Here, instead, we obtain this:

```ocaml
# Cbor_pack.to_cbor tree_to_cbpack t;;
- : Cbor_pack.cbor =
`Map
  [(`Text "k", `Tag (6, `Int 3));
   (`Text "h",
    `Array
      [`Array [`Int 1; `Int 2; `Int 0; `Int 0];
       `Array [`Int 1; `Int 3; `Tag (6, `Int 0); `Tag (6, `Int 0)];
       `Array [`Int 1; `Int 4; `Tag (6, `Int 1); `Tag (6, `Int 0)];
       `Array [`Int 1; `Int 1; `Tag (6, `Int 2); `Tag (6, `Int 2)]])]

# String.length (Cbor_pack.to_string tree_to_cbpack t);;
- : int = 34
```

Without hashconsing we'd have:

```ocaml
type tree2 =
  | Nil
  | Node of int * tree2 * tree2
  [@@deriving cbpack];;

let t: tree2 =
    let t2 = Node (2, Nil, Nil) in
    let t3 = Node (3, t2, t2) in
    let t4 = Node (4, t3, t2) in
    Node (1, t4, t4);;
```

```ocaml
# Cbor_pack.to_cbor tree2_to_cbpack t;;
- : Cbor_pack.cbor =
`Map
  [(`Text "k", `Tag (6, `Int 10));
   (`Text "h",
    `Array
      [`Array [`Int 1; `Int 2; `Int 0; `Int 0];
       `Array [`Int 1; `Int 2; `Int 0; `Int 0];
       `Array [`Int 1; `Int 2; `Int 0; `Int 0];
       `Array [`Int 1; `Int 3; `Tag (6, `Int 2); `Tag (6, `Int 1)];
       `Array [`Int 1; `Int 4; `Tag (6, `Int 3); `Tag (6, `Int 0)];
       `Array [`Int 1; `Int 2; `Int 0; `Int 0];
       `Array [`Int 1; `Int 2; `Int 0; `Int 0];
       `Array [`Int 1; `Int 2; `Int 0; `Int 0];
       `Array [`Int 1; `Int 3; `Tag (6, `Int 7); `Tag (6, `Int 6)];
       `Array [`Int 1; `Int 4; `Tag (6, `Int 8); `Tag (6, `Int 5)];
       `Array [`Int 1; `Int 1; `Tag (6, `Int 9); `Tag (6, `Int 4)]])]

# String.length (Cbor_pack.to_string tree2_to_cbpack t);;
- : int = 73
```

which is more than twice as long.

### Attributes supported

- `[@ser f]` on type: custom serialize function
- `[@deser f]` on type: custom deserialize function
- `[@as_bytes]` on a string type: encode to CBOR bytes, not string.
    Should be used for non-textual data, i.e. strings not containing valid UTF-8.
- `[@cstor "x"]` on constructor: custom key for this constructor (string)
- `[@key "x"]` on record field: custom key for this field (string)
- `[@@hashcons]` on type decl: enable hashconsing for this type.
- `[@@use_field_names]` on type decl: use strings for record fields, not integer offsets

## License

MIT
