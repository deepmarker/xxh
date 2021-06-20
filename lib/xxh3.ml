open Stdint

external hash64 : bytes -> uint64 = "xxh3_64_stubs"
external hash64_string : string -> uint64 = "xxh3_64_stubs"
external hash64_ba : Base_bigstring.t -> uint64 = "xxh3_64_ba_stubs"
external hash128 : bytes -> uint128 = "xxh3_128_stubs"
external hash128_string : string -> uint128 = "xxh3_128_stubs"
external hash128_ba : Base_bigstring.t -> uint128 = "xxh3_128_ba_stubs"

module Seed = struct
  external hash64 : bytes -> uint64 -> uint64 = "xxh3_64_seed_stubs"
  external hash64_string : string -> uint64 -> uint64 = "xxh3_64_seed_stubs"

  external hash64_ba : Base_bigstring.t -> uint64 -> uint64
    = "xxh3_64_seed_ba_stubs"

  external hash128 : bytes -> uint64 -> uint128 = "xxh3_128_seed_stubs"
  external hash128_string : string -> uint64 -> uint128 = "xxh3_128_seed_stubs"

  external hash128_ba : Base_bigstring.t -> uint64 -> uint128
    = "xxh3_128_seed_ba_stubs"
end

module Secret = struct
  external hash64 : bytes -> bytes -> uint64 = "xxh3_64_secret_stubs"
  external hash64_string : string -> string -> uint64 = "xxh3_64_secret_stubs"

  external hash64_ba : Base_bigstring.t -> Base_bigstring.t -> uint64
    = "xxh3_64_secret_ba_stubs"

  external hash128 : bytes -> bytes -> uint128 = "xxh3_128_secret_stubs"

  external hash128_string : string -> string -> uint128
    = "xxh3_128_secret_stubs"

  external hash128_ba : Base_bigstring.t -> Base_bigstring.t -> uint128
    = "xxh3_128_secret_ba_stubs"
end

module Streaming = struct
  type st
  type _ state = State64 : st -> uint64 state | State128 : st -> uint128 state

  external create : unit -> st = "xxh3_create_state_stubs"
  external free : st -> unit = "xxh3_free_state_stubs" [@@noalloc]
  external copy : 'a state -> 'a state = "xxh3_copy_state_stubs"
  external reset64 : st -> int = "xxh3_64_reset_stubs" [@@noalloc]
  external reset128 : st -> int = "xxh3_128_reset_stubs" [@@noalloc]

  external reset64_seed : st -> uint64 -> int = "xxh3_64_reset_seed_stubs"
    [@@noalloc]

  external reset128_seed : st -> uint64 -> int = "xxh3_128_reset_seed_stubs"
    [@@noalloc]

  external reset64_secret : st -> bytes -> int = "xxh3_64_reset_secret_stubs"
    [@@noalloc]

  external reset128_secret : st -> bytes -> int = "xxh3_128_reset_secret_stubs"
    [@@noalloc]

  external reset64_secret_string : st -> string -> int = "xxh3_64_reset_stubs"
    [@@noalloc]

  external reset128_secret_string : st -> string -> int = "xxh3_128_reset_stubs"
    [@@noalloc]

  external reset64_secret_ba : st -> Base_bigstring.t -> int
    = "xxh3_64_reset_secret_stubs"
    [@@noalloc]

  external reset128_secret_ba : st -> Base_bigstring.t -> int
    = "xxh3_128_reset_secret_stubs"
    [@@noalloc]

  external update64 : st -> bytes -> int -> int -> int = "xxh3_64_update_stubs"
    [@@noalloc]

  external update128 : st -> bytes -> int -> int -> int
    = "xxh3_128_update_stubs"
    [@@noalloc]

  external update64_string : st -> string -> int -> int -> int
    = "xxh3_64_update_stubs"
    [@@noalloc]

  external update128_string : st -> string -> int -> int -> int
    = "xxh3_128_update_stubs"
    [@@noalloc]

  external update64_ba : st -> Base_bigstring.t -> int -> int -> int
    = "xxh3_64_update_ba_stubs"
    [@@noalloc]

  external update128_ba : st -> Base_bigstring.t -> int -> int -> int
    = "xxh3_128_update_ba_stubs"
    [@@noalloc]

  external digest64 : st -> uint64 = "xxh3_64_digest_stubs"
  external digest128 : st -> uint128 = "xxh3_128_digest_stubs"

  let or_fail1 msg f x = match f x with 0 -> () | _ -> failwith msg
  let or_fail2 msg f x y = match f x y with 0 -> () | _ -> failwith msg

  let or_fail4 msg f x y z t =
    match f x y z t with 0 -> () | _ -> failwith msg

  let free : type a. a state -> unit = function
    | State64 st -> free st
    | State128 st -> free st

  let reset : type a. a state -> unit = function
    | State64 x -> or_fail1 "reset64" reset64 x
    | State128 x -> or_fail1 "reset128" reset128 x

  let create64 () =
    let st = State64 (create ()) in
    reset st ; st

  let create128 () =
    let st = State128 (create ()) in
    reset st ; st

  module Seed = struct
    let reset : type a. a state -> uint64 -> unit =
     fun x i ->
      match x with
      | State64 x -> or_fail2 "reset64_seed" reset64_seed x i
      | State128 x -> or_fail2 "reset128_seed" reset128_seed x i

    let create64 seed =
      let st = State64 (create ()) in
      reset st seed ; st

    let create128 seed =
      let st = State128 (create ()) in
      reset st seed ; st
  end

  module Secret = struct
    let reset : type a. a state -> bytes -> unit =
     fun x i ->
      match x with
      | State64 x -> or_fail2 "reset64_secret" reset64_secret x i
      | State128 x -> or_fail2 "reset128_secret" reset128_secret x i

    let reset_string : type a. a state -> string -> unit =
     fun x i ->
      match x with
      | State64 x -> or_fail2 "reset64_secret_string" reset64_secret_string x i
      | State128 x ->
          or_fail2 "reset128_secret_string" reset128_secret_string x i

    let reset_ba : type a. a state -> Base_bigstring.t -> unit =
     fun x i ->
      match x with
      | State64 x -> or_fail2 "reset64_secret_ba" reset64_secret_ba x i
      | State128 x -> or_fail2 "reset128_secret_ba" reset128_secret_ba x i

    let create64 secret =
      let st = State64 (create ()) in
      reset_ba st secret ; st

    let create128 secret =
      let st = State128 (create ()) in
      reset_ba st secret ; st
  end

  let update : type a. a state -> bytes -> int -> int -> unit =
   fun x b p l ->
    match x with
    | State64 x -> or_fail4 "update64" update64 x b p l
    | State128 x -> or_fail4 "update128" update128 x b p l

  let update_string : type a. a state -> string -> int -> int -> unit =
   fun x b p l ->
    match x with
    | State64 x -> or_fail4 "update64_string" update64_string x b p l
    | State128 x -> or_fail4 "update128_string" update128_string x b p l

  let update_ba : type a. a state -> Base_bigstring.t -> int -> int -> unit =
   fun x b p l ->
    match x with
    | State64 x -> or_fail4 "update64_ba" update64_ba x b p l
    | State128 x -> or_fail4 "update128_ba" update128_ba x b p l

  let digest : type a. a state -> a = function
    | State64 x -> digest64 x
    | State128 x -> digest128 x
end
