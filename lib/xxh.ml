open Stdint

external hash32 : bytes -> uint32 -> uint32 = "xxh32_stubs"
external hash32_string : string -> uint32 -> uint32 = "xxh32_stubs"
external hash32_ba : Base_bigstring.t -> uint32 -> uint32 = "xxh32_ba_stubs"
external hash64 : bytes -> uint64 -> uint64 = "xxh64_stubs"
external hash64_string : string -> uint64 -> uint64 = "xxh64_stubs"
external hash64_ba : Base_bigstring.t -> uint64 -> uint64 = "xxh64_ba_stubs"

let hash32 ?(seed = Uint32.zero) x = hash32 x seed
let hash32_string ?(seed = Uint32.zero) x = hash32_string x seed
let hash32_ba ?(seed = Uint32.zero) x = hash32_ba x seed
let hash64 ?(seed = Uint64.zero) x = hash64 x seed
let hash64_string ?(seed = Uint64.zero) x = hash64_string x seed
let hash64_ba ?(seed = Uint64.zero) x = hash64_ba x seed

module Xxh3 = Xxh3
