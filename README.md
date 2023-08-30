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
Line 4, characters 1-3:
Error: Syntax error
```

which creates two functions:

<!-- $MDX skip -->
```ocaml
val foo_to_cbpack : foo Cbor_pack.Ser.t
val foo_of_cbpack : foo Cbor_pack.Deser.t
```

### Example: a record

For example, in `tests/t1.ml`, we can serialize and deserialize:

```ocaml
type foo = {
  a: int;
  b: float;
}
[@@deriving cbpack]
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
```

and deserialize it again:

```ocaml
# let foo2 = Cbor_pack.of_string_exn foo_of_cbpack s;;
val foo2 : foo = {a = 1; b = 2.}

# my_foo = foo2;;
- : bool = true
```

### Hashconsing

Hashconsing is sharing done on the heap itself. If the same CBOR value `c` is added twice to the heap
with the hashconsing option enabled, the second occurrence will not be added but will reuse a
pointer to the first entry.

This has a cost at runtime (hashtable lookups), but can result in a significantly smaller pack at the end.

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
