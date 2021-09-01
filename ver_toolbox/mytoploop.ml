(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: toploop.ml 9166 2009-01-25 22:46:15Z weis $ *)

(* The interactive toplevel loop *)

open Path
open Lexing
(*open Format*)
open Config
open Misc
open Parsetree
open Types
open Typedtree
open Outcometree
open Toploop

(* The table of toplevel value bindings and its accessors *)

let toplevel_value_bindings =
  (Hashtbl.create 37 : (string, Obj.t) Hashtbl.t)

let getvalue name =
  try
    Hashtbl.find toplevel_value_bindings name
  with Not_found ->
    fatal_error (name ^ " unbound at toplevel")

let setvalue name v =
  Hashtbl.replace toplevel_value_bindings name v

(* Temporary assignment to a reference *)

let protect r newval body =
  let oldval = !r in
  try
    r := newval;
    let res = body() in
    r := oldval;
    res
  with x ->
    r := oldval;
    raise x

(* The interactive loop *)

exception PPerror

let use_string ppf cmdbuf =
  try
    let lb = Lexing.from_string cmdbuf in
    Location.init lb cmdbuf;
    (* Skip initial #! line if any *)
    Lexer.skip_hash_bang lb;
    let success =
      protect Location.input_name cmdbuf (fun () ->
        try
          List.iter
            (fun ph ->
              if !Clflags.dump_parsetree then Printast.top_phrase ppf ph;
              if not (execute_phrase true ppf ph) then raise Exit)
            (!parse_use_file lb);
          true
        with
        | Exit -> false
        | Sys.Break -> Format.fprintf ppf "Interrupted.@."; false
        | x -> false) in
    success
  with Not_found -> false

let loop ppf =
  Format.fprintf ppf "Unhandled syntax - entering debugger - version %s@.@." Config.version;
  let refill_lexbuf buffer len = fst(!read_interactive_input "dbg> " buffer len) in
  let looping = ref true and lb = Lexing.from_function refill_lexbuf in
  Location.input_name := "";
  Location.input_lexbuf := Some lb;
  Sys.catch_break true;
  while !looping do
    let snap = Btype.snapshot () in
    try
      Lexing.flush_input lb;
      Location.reset();
      let phr = try !parse_toplevel_phrase lb with Exit -> raise PPerror in
      if !Clflags.dump_parsetree then Printast.top_phrase ppf phr;
      ignore(execute_phrase true ppf phr)
    with
    | End_of_file -> looping := false
    | Sys.Break -> Format.fprintf ppf "Interrupted.@."; Btype.backtrack snap
    | PPerror -> ()
    | x -> Btype.backtrack snap
  done

let myfailwith str =
  Printf.fprintf stderr "%s\n" str;
  flush stderr;
  loop (Format.formatter_of_out_channel stderr)

let main_dbg quit len cnt atok =
  incr cnt;
  loop (Format.formatter_of_out_channel stderr)

(* disabled for now

let _ =
  initialize_toplevel_env ();
(*
  ignore(use_string Format.std_formatter "open Vparser open Globals open Semantics open List open Mytoploop;;");
*)
  Hashtbl.replace Setup.main_cmds "dbg" (main_dbg, "enter debugger", 1);
  Setup.failwith_ptr := myfailwith

*)
