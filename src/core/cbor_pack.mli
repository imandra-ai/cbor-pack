(** CBOR-pack.

  CBOR pack is a data serialization scheme built on top of {{:https://cbor.io/} CBOR}.
  It introduces a notion of {i heap} (an array of CBOR values) and {i pointers} into
  this heap (CBOR values consisting of an integer wrapped in a specific tag).
  The heap is then turned into a CBOR array, so that the resulting "pack" is just a
  large CBOR value with a specific internal structure (some of which are pointers into
  the large internal {i heap}).

  When serializing a complex data structure that presents internal sharing (typically,
  with pointers/references), the heap can be used to represent that sharing directly
  in the CBOR value. This is done by serializing a value once, adding it to the {i heap}
  (which is an array); the position of the value in the heap can then be wrapped
  with tag 6 to become a {i pointer}. All other references to this value are serialized
  as pointers.
*)

type cbor = CBOR.Simple.t

val pp_diagnostic : Format.formatter -> cbor -> unit
(** Debug printer for CBOR values. *)

(** Serialization *)
module Ser : sig
  type state
  (** State used for serialization.

      It contains an in-progress heap, and a hash table for hashconsing. *)

  type 'a t = state -> 'a -> cbor
  (** Serializer for type ['a] *)

  val create : unit -> state
  (** New state. *)

  type ptr = cbor
  (** An integer + tag for CBOR *)

  val unit : cbor
  (** Build a CBOR null *)

  val int : int -> cbor
  (** Build a CBOR integer *)

  val bool : bool -> cbor
  (** Build a CBOR bool *)

  val float : float -> cbor
  (** Build a CBOR float *)

  val string : string -> cbor
  (** Build a CBOR text string (UTF8) *)

  val bytes : bytes -> cbor
  (** Build a CBOR blob (raw bytes) *)

  val list : cbor list -> cbor
  (** Build a CBOR list *)

  val map : (cbor * cbor) list -> cbor
  (** Build a CBOR map *)

  val list_of : 'a t -> 'a list t
  (** [list_of ser] encodes a list of values using [ser] for each *)

  val map_of : 'a t -> 'b t -> ('a * 'b) list t
  (** Build a map by serializing the given association list *)

  val add_entry : ?hashcons:bool -> state -> cbor -> ptr
  (** [add_entry st c] turns [c] into a heap entry and returns
      a pointer to it.
      The pointer is a small CBOR value (a tagged integer).

      @param hashcons if true, [c] is first compared to existing
      hashconsed entries (at a runtime cost) to see if we can reuse
      them instead of inserting a new value. *)

  val add_string : ?hashcons:bool -> state -> string -> cbor
  (** Same as [add_entry state (`Text s)], except that large strings
      will be hashconsed unconditionally. *)

  val add_bytes : ?hashcons:bool -> state -> bytes -> cbor
  (** Same as {!add_string} *)

  val delay : (unit -> 'a t) -> 'a t
  (** [delay f] is like [f()], but [f] is only called when needed. *)

  val fix : ('a t -> 'a t) -> 'a t
  (** [fix f] is a recursive serializer. [f] receives a serializer
      for recursive cases and must use it to implement the serialization
      for the current value. *)

  type 'a cache_key

  val create_cache_key :
    (module Hashtbl.HashedType with type t = 'a) -> 'a cache_key
  (** Create a new (generative) cache key for a hashable + comparable type.

      {b NOTE} this should be called only at module toplevel, as a constant,
      not dynamically inside a function:
      [let key = Cbor_pack.Ser.create_cache_key (module …);;].
      Indeed, this is generative, so creating multiple keys for a type
      will result in sub-par or inexistant caching. *)

  val with_cache : 'a cache_key -> 'a t -> 'a t
  (** [with_cache key enc] is the same encoder as [enc], but
      with caching. When encoding a value [x:'a],
      the cache [key] is used to detect if [x] was already
      encoded to some entry, and uses a pointer to this entry
      instead of re-serializing [x]. *)

  val finalize_cbor : state -> key:cbor -> cbor
  (** Turn the state into a pack with given [key] as entrypoint. *)

  val finalize_string : state -> key:cbor -> string
  (** Same as {!finalize_cbor} but also turns the resulting packed CBOR
      into a string. *)
end

val to_string : 'a Ser.t -> 'a -> string
(** [to_string ser x] seralizes [x] using [ser], and returns the
    pack containing the shared heap, and an entry point. *)

val to_cbor : 'a Ser.t -> 'a -> cbor
(** Same as {!to_string} but without the CBOR encoding step. *)

(** Deserialization *)
module Deser : sig
  type state
  (** Deserialization state, containing the heap of CBOR
      values. *)

  type 'a or_error = ('a, string) result

  exception Error of string
  (** Exception raised by most of the [to_xxx] functions below. *)

  type ptr

  type 'a t = state -> cbor -> 'a
  (** A deserializer takes a cbor value, and returns a value of type ['a] from it.
      @raise Error in case of error *)

  val deref : state -> ptr -> cbor
  (** Get an item via its pointer.
      @raise Invalid_argument if the pointer is invalid. *)

  val return : 'a -> 'a t

  val fail : string -> 'a
  (** Fail to decode. *)

  val failf : ('a, unit, string, 'b) format4 -> 'a
  (** Fail to decode with a formatted message. *)

  val to_unit : unit t
  val to_int : int t
  val to_bool : bool t
  val to_float : float t
  val to_list : cbor list t
  val to_list_of : 'a t -> 'a list t
  val to_map : (cbor * cbor) list t
  val to_ptr : cbor -> ptr
  val map_entry : k:cbor -> state -> cbor -> cbor

  val to_any_tag : state -> cbor -> int * cbor
  (** Deserialize an arbitrary tag *)

  val to_tag : int -> state -> cbor -> cbor
  (** Expect a particular tag. *)

  val to_text : string t
  val to_bytes : bytes t

  val deref_if_ptr : state -> cbor -> cbor
  (** If the CBOR is a pointer, dereference it (recursively) *)

  val ptr_of_int : int -> ptr
  (** Entry point *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  (** Map combinator *)

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  (** Bind combinator *)

  val delay : (unit -> 'a t) -> 'a t
  (** [delay f] is like [f()], but [f] is only called when needed. *)

  val parse : string -> state or_error

  val parse_exn : string -> state
  (** @raise Error if it fails *)

  type 'a cache_key
  (** Generative key used to cache values during decoding *)

  val create_cache_key : unit -> _ cache_key
  (** Generate a new (generative) cache key for a type.

      {b NOTE} this should be called only at module toplevel, as a constant,
      not dynamically inside a function:
      [let key: foo Cbor_pack.Deser.cache_key = Cbor_pack.Deser.create_cache_key ();;].
      Indeed, this is generative, so creating multiple keys for a type
      will result in sub-par or inexistant caching. *)

  val with_cache : 'a cache_key -> 'a t -> 'a t
  (** [with_cache key dec] is the same decoder as [dec] but
      it uses [key] to retrieve values directly from
      an internal table for entries/values that have already
      been decoded in the past. This means that a value that was
      encoded with a lot of sharing (e.g in a graph, or a large
      string using {!Ser.add_string}) will be decoded only once.
  *)

  val fix : ('a t -> 'a t) -> 'a t
  (** [fix f] is a recursive deserializer. [f] receives a deserializer
      for recursive cases and must use it to implement the deserialization
      for the current value. *)

  val entry_key : state -> cbor
  (** Entrypoint for the pack, as used in {!Ser.finalize_cbor}
      or {!Ser.finalize_string} *)

  val show_diagnostic : state -> string
  (** Show the content of the deserialized CBOR using the diagnostic notation *)

  val pp_diagnostic : Format.formatter -> state -> unit
  (** Show the content of the deserialized CBOR using the diagnostic notation *)
end

val of_cbor_exn : 'a Deser.t -> cbor -> 'a
(** [of_cbor_exn deser cbor] deserializes an object using [deser]
    from the shared heap [cbor.h], starting at [cbor.key]. *)

val of_cbor : 'a Deser.t -> cbor -> 'a Deser.or_error
(** Deserialize a pack into a value of type ['a] *)

val of_string_exn : 'a Deser.t -> string -> 'a
(** Parse CBOR and deserialize it *)

val of_string : 'a Deser.t -> string -> 'a Deser.or_error

(** Private utilities, no guarantees of stability *)
module Private_ : sig
  val deser_key : Deser.state -> cbor
  val deser_heap_iter : Deser.state -> (int -> cbor -> unit) -> unit
  val deser_heap_get : Deser.state -> int -> cbor
end
