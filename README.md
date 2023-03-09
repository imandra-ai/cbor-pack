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
of the actual value in the array `h`.

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

type foo = {
  x: int;
  y: (string [@as_bytes]);
} [@@deriving cbpack]
```

which will create functions with the signature:

```ocaml
val foo_to_cbpack : foo Cbor_pack.Ser.t
val foo_of_cbpack : foo Cbor_pack.Deser.t
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
