open Alcotest
open Stdint
open Xxh

let of_hex32 str =
  let hx = Hex.to_string (`Hex str) in
  Uint32.of_bytes_big_endian (Bytes.unsafe_of_string hx) 0

let of_hex64 str =
  let hx = Hex.to_string (`Hex str) in
  Uint64.of_bytes_big_endian (Bytes.unsafe_of_string hx) 0

let of_hex128 str =
  let hx = Hex.to_string (`Hex str) in
  Uint128.of_bytes_big_endian (Bytes.unsafe_of_string hx) 0

let uint32 = testable Uint32.printer (fun a b -> Uint32.compare a b = 0)
let uint64 = testable Uint64.printer (fun a b -> Uint64.compare a b = 0)
let uint128 = testable Uint128.printer (fun a b -> Uint128.compare a b = 0)
let tests32 = [("", of_hex32 "02cc5d05"); ("x", of_hex32 "2ec430ea")]

let tests64 =
  [("", of_hex64 "ef46db3751d8e999"); ("x", of_hex64 "5c80c09683041123")]

let tests128 =
  [ ("", of_hex128 "99aa06d3014798d86001c324468d497f");
    ("x", of_hex128 "5c7401c0ec22eeeeeaf06c6480b2cd11") ]

let base32 () =
  List.iter
    (fun (msg, expected) -> check uint32 "" expected (Xxh.hash32_string msg))
    tests32

let base64 () =
  List.iter
    (fun (msg, expected) -> check uint64 "" expected (Xxh.hash64_string msg))
    tests64

let base128 () =
  List.iter
    (fun (msg, expected) -> check uint128 "" expected (Xxh3.hash128_string msg))
    tests128

let stream64 () =
  let module Xx = Xxh3.Streaming in
  let buf = Bytes.create 4096 in
  let st = Xx.create64 () in
  Xx.reset st ;
  for _ = 0 to 1000 do
    Xx.update st buf 0 4096
  done ;
  Format.printf "%a" Uint64.printer (Xx.digest st)

let stream128 () =
  let module Xx = Xxh3.Streaming in
  let buf = Bytes.create 4096 in
  let st = Xx.create128 () in
  Xx.reset st ;
  for _ = 0 to 1000 do
    Xx.update st buf 0 4096
  done ;
  Format.printf "%a" Uint128.printer (Xx.digest st)

let xxh32 = [test_case "base" `Quick base32]

let xxh64 =
  [test_case "base" `Quick base64; test_case "streaming" `Quick stream64]

let xxh128 =
  [test_case "base" `Quick base128; test_case "streaming" `Quick stream128]

let () = run "xxh" [("xxh32", xxh32); ("xxh64", xxh64); ("xxh128", xxh128)]
