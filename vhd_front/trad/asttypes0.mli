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

(* $Id: asttypes0.mli 10250 2010-04-08 03:58:41Z garrigue $ *)

(* Auxiliary a.s.t. types used by parsetree0 and typedtree. *)

type constant =
    Const_int of int
  | Const_char of char
  | Const_string of string
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint

type rec_flag = Nonrecursive | Recursive | Default

type direction_flag = Upto | Downto

type private_flag = Private | Public

type mutable_flag = Immutable | Mutable

type virtual_flag = Virtual | Concrete

type override_flag = Override | Fresh

type closed_flag = Closed | Open

type label = string
