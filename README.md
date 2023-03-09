# Cbor-pack

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
type foo = {
  x: int;
  y: (string [@as_bytes]);
} [@@deriving cbpack] ;;
```

which will create functions with the signature:

<!-- $MDX skip -->
```ocaml
val foo_to_cbpack : foo Cbor_pack.Ser.t
val foo_of_cbpack : foo Cbor_pack.Deser.t
```

### Example

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
# #install_printer Cbor_pack.pp_diagnostic;;
# let my_foo = { a = 1; b = 2.0 };;
val my_foo : foo = {a = 1; b = 2.}
# let c = Cbor_pack.to_cbor foo_to_cbpack my_foo;;
val c : Cbor_pack.cbor = {h=[0: {0: 1, 1: 2.}; ]}

# CBOR.Simple.to_diagnostic c;;
- : string = "{\"k\": 6(0), \"h\": [{0: 1, 1: 2.}]}"

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

### Attributes supported

- `[@ser f]` on type: custom serialize function
- `[@deser f]` on type: custom deserialize function
- `[@as_bytes]` on a string type: encode to CBOR bytes, not string.
    Should be used for non-textual data, i.e. strings not containing valid UTF-8.
- `[@cstor "x"]` on constructor: custom key for this constructor (string)
- `[@key "x"]` on record field: custom key for this field (string)
- `[@@hashcons]` on type decl: enable hashconsing for this type.
- `[@@nohashcons]` on type decl: disable hashconsing for this type.
- `[@@use_field_names]` on type decl: use strings for record fields, not ints
