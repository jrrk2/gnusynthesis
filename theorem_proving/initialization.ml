(* ========================================================================= *)
(* Tweak OCaml default state ready for theorem proving code.                 *)
(*                                                                           *)
(* Copyright (c) 2003-2007, John Harrison. (See "LICENSE.txt" for details.)  *)
(* ========================================================================= *)

Gc.set { (Gc.get()) with Gc.stack_limit = 16777216 };; (* Up the stack size  *)
Format.set_margin 72;;                                 (* Reduce margins     *)
open Format;;                                       (* Open formatting    *)
open Num;;                                          (* Open bignums       *)

let print_num n = print_string(string_of_num n);;      (* Avoid range limit  *)
(*
(* #install_printer print_num;; *)                           (* when printing nums *)
*)
