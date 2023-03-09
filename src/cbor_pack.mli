(** CBOR-pack *)

type cbor = CBOR.Simple.t

val pp_diagnostic : Format.formatter -> cbor -> unit
(** Debug printer for CBOR values. *)

(** Serialization *)
module Ser : sig
  type state

  type 'a t = state -> 'a -> cbor
  (** Serializer for type ['a] *)

  val create : unit -> state

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

  (* TODO: keep this?
     val with_cycle_detection : state -> 'a -> ('a -> ptr) -> ptr
  *)

  val add_entry : ?hashcons:bool -> state -> cbor -> ptr

  val add_string : state -> string -> cbor
  (** Depending on the size of the string, uses {!add_entry} or returns it
      directly using {!string} *)

  val add_bytes : state -> bytes -> cbor
  (** Same as {!add_string} *)

  val finalize_cbor : state -> key:cbor -> cbor
  val finalize_string : state -> key:cbor -> string
end

val to_string : 'a Ser.t -> 'a -> string
(** [to_string ser x] seralizes [x] using [ser], and returns the
    pack containing the shared heap, and an entry point. *)

val to_cbor : 'a Ser.t -> 'a -> cbor
(** Same as {!to_string} but without the CBOR encoding step. *)

(** Deserialization *)
module Deser : sig
  type state
  type 'a or_error = ('a, string) result

  exception Error of string

  type ptr

  type 'a t = state -> cbor -> 'a
  (** A deserializer takes a cbor value, and returns a value of type ['a] from it.
      @raise Error in case of error *)

  val deref : state -> ptr -> cbor
  (** Get an item via its pointer *)

  val to_unit : state -> cbor -> unit
  val to_int : state -> cbor -> int
  val to_bool : state -> cbor -> bool
  val to_float : state -> cbor -> float
  val to_list : state -> cbor -> cbor list
  val to_list_of : (state -> cbor -> 'a) -> state -> cbor -> 'a list
  val to_map : state -> cbor -> (cbor * cbor) list
  val to_ptr : cbor -> ptr
  val map_entry : k:cbor -> state -> cbor -> cbor

  val to_tag : int -> state -> cbor -> cbor
  (** Tag *)

  val to_text : state -> cbor -> string
  val to_bytes : state -> cbor -> bytes

  val deref_if_ptr : state -> cbor -> cbor
  (** If the CBOR is a pointer, dereference it (recursively) *)

  val ptr_of_int : int -> ptr
  (** Entry point *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  type 'a key

  val make_key : name:string -> unit -> 'a key
  val name_of_key : _ key -> string
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
(** [of_cbor_exn deser heap ~key] deserializes an object using [deser]
    from the shared heap [heap], starting at [key].
    [key] is typically a pointer. *)

val of_cbor : 'a Deser.t -> cbor -> 'a Deser.or_error
(** Deserialize a pack into a value of type ['a] *)

val of_string_exn : 'a Deser.t -> string -> 'a
(** Parse CBOR and deserialize it *)

val of_string : 'a Deser.t -> string -> 'a Deser.or_error
