open Stdint

val hash64 : bytes -> uint64
val hash64_string : string -> uint64
val hash64_ba : Base_bigstring.t -> uint64
val hash128 : bytes -> uint128
val hash128_string : string -> uint128
val hash128_ba : Base_bigstring.t -> uint128

module Seed : sig
  val hash64 : bytes -> uint64 -> uint64
  val hash64_string : string -> uint64 -> uint64
  val hash64_ba : Base_bigstring.t -> uint64 -> uint64
  val hash128 : bytes -> uint64 -> uint128
  val hash128_string : string -> uint64 -> uint128
  val hash128_ba : Base_bigstring.t -> uint64 -> uint128
end

module Secret : sig
  val hash64 : bytes -> bytes -> uint64
  val hash64_string : string -> string -> uint64
  val hash64_ba : Base_bigstring.t -> Base_bigstring.t -> uint64
  val hash128 : bytes -> bytes -> uint128
  val hash128_string : string -> string -> uint128
  val hash128_ba : Base_bigstring.t -> Base_bigstring.t -> uint128
end

module Streaming : sig
  type _ state

  val free : _ state -> unit
  val copy : 'a state -> 'a state
  val reset : _ state -> unit
  val create64 : unit -> uint64 state
  val create128 : unit -> uint128 state

  module Seed : sig
    val reset : _ state -> uint64 -> unit
    val create64 : uint64 -> uint64 state
    val create128 : uint64 -> uint128 state
  end

  module Secret : sig
    val reset : _ state -> bytes -> unit
    val reset_string : _ state -> string -> unit
    val reset_ba : _ state -> Base_bigstring.t -> unit
    val create64 : Base_bigstring.t -> uint64 state
    val create128 : Base_bigstring.t -> uint128 state
  end

  val update : _ state -> bytes -> int -> int -> unit
  val update_string : _ state -> string -> int -> int -> unit
  val update_ba : _ state -> Base_bigstring.t -> int -> int -> unit
  val digest : 'a state -> 'a
end
