open Stdint

val hash32 : ?seed:uint32 -> bytes -> uint32
val hash32_string : ?seed:uint32 -> string -> uint32
val hash32_ba : ?seed:uint32 -> Base_bigstring.t -> uint32
val hash64 : ?seed:uint64 -> bytes -> uint64
val hash64_string : ?seed:uint64 -> string -> uint64
val hash64_ba : ?seed:uint64 -> Base_bigstring.t -> uint64

module Xxh3 = Xxh3
