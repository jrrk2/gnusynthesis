open Dump
open Setup
open Printf
open Xdl_parser

let rslt = Xdl_parse.parse Sys.argv.(1)

let _ = if rslt = [] then Printf.printf "failed\n" else Printf.printf "passed\n"

let _ =
  let tuplehash = Hashtbl.create 256 in
  List.map (Classify.find tuplehash) rslt;
  let olst = Classify.uniquify tuplehash in
  Classify.genpatterns olst
