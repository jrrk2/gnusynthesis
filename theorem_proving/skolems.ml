open Prop
open Lib
open Formulas
open Prop
open Num
open Intro
open Defcnf
open Format
open Fol
open Skolem
open Dp
open Unif
open Herbrand
open Tableaux
open Prolog
(* ========================================================================= *)
(* Illustration of Skolemizing a set of formulas                             *)
(*                                                                           *)
(* Copyright (c) 2003-2007, John Harrison. (See "LICENSE.txt" for details.)  *)
(* ========================================================================= *)

let rec rename_term tm =
  match tm with
    Fn(f,args) -> Fn("old_"^f,map rename_term args)
  | _ -> tm;;

let rename_form = onformula rename_term;;

let rec skolems fms corr =
  match fms with
    [] -> [],corr
  | (p::ofms) ->
        let p',corr' = skolem (rename_form p) corr in
        let ps',corr'' = skolems ofms corr' in
        p'::ps',corr'';;

let skolemizes fms = fst(skolems fms []);;

(*
skolemizes [<<exists x y. x + y = 2>>;
            <<forall x. exists y. x + 1 = y>>];;
*)
