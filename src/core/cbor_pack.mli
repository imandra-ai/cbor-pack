(** CBOR-pack *)

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

  val hmap : state -> Hmap.t
  (** Extensible hmap, to put encoder-specific state (e.g.
      a hashtable to preserve sharing for a given type) *)

  val update_hmap : state -> (Hmap.t -> Hmap.t * 'a) -> 'a
  (** [update_hmap st f] calls [f cur_hmap] on the current hmap.
      The call to [f] returns the new hmap, which is assigned in [st],
      as well as a side value that is returned. *)

  type ptr = cbor
  (** An integer + tag for CBOR *)

  val unit : cbor
  val int : int -> cbor
  val bool : bool -> cbor
  val float : float -> cbor
  val string : string -> cbor
  val bytes : bytes -> cbor
  val list : cbor list -> cbor
  val list_of : ('a -> cbor) -> 'a list -> cbor
  val map : (cbor * cbor) list -> cbor

  val add_entry : ?hashcons:bool -> state -> cbor -> ptr
  (** [add_entry st c] turns [c] as a heap entry and returns
      a pointer to it.
      @param hashcons if true, [c] is first compared to existing
      hashconsed entries (at a runtime cost) to see if we can reuse
      them instead of inserting a new value. *)

  val add_string : ?hashcons:bool -> state -> string -> cbor
  (** Same as [add_entry state (`Text s)], except that large strings
      will be hashconsed unconditionally. *)

  val add_bytes : ?hashcons:bool -> state -> bytes -> cbor
  (** Same as {!add_string} *)

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

  val to_unit : state -> cbor -> unit
  val to_int : state -> cbor -> int
  val to_bool : state -> cbor -> bool
  val to_float : state -> cbor -> float
  val to_list : state -> cbor -> cbor list
  val to_list_of : (state -> cbor -> 'a) -> state -> cbor -> 'a list
  val to_map : state -> cbor -> (cbor * cbor) list
  val to_ptr : cbor -> ptr
  val map_entry : k:cbor -> state -> cbor -> cbor

  val to_any_tag : state -> cbor -> int * cbor
  (** Deserialize an arbitrary tag *)

  val to_tag : int -> state -> cbor -> cbor
  (** Expect a particular tag. *)

  val to_text : state -> cbor -> string
  val to_bytes : state -> cbor -> bytes

  val deref_if_ptr : state -> cbor -> cbor
  (** If the CBOR is a pointer, dereference it (recursively) *)

  val ptr_of_int : int -> ptr
  (** Entry point *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val parse : string -> state or_error

  val parse_exn : string -> state
  (** @raise Error if it fails *)

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
