(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Decision

let p s = 
  let lb = Lexing.from_string s in
  match Parser0.file Lexer0.token lb with
    | f :: _ -> f
    | _ -> assert false 

let valid s = 
  assert (tautology (snd (bdd_of_formula (p s))))
let invalid s = 
  assert (not (tautology (snd (bdd_of_formula (p s)))))

let () = valid "
((b <-> c) -> (a&b&c)) & 
((c<->a)->(a&b&c)) & ((a<->b)->(a&b&c)) -> (a&b&c)"

let () = valid "~ ~(~p1 \\/ ~p2 \\/ ~p3 \\/ (p1 & p2 & p3))"

let () = valid "(a1<->a2)<->(a2<->a1)"

let () = valid " A \\/ ~A"
let () = valid " A -> ~~A"
let () = valid " A -> A"
let () = valid " ((A -> B) -> A) -> A"
let () = valid " (A -> B)-> (~B -> ~ A)"
let () = valid " ((A -> B) & A) -> B"
let () = valid " ((A -> B) & ~ B) -> ~ A"
let () = valid " ((A -> B) & (B -> C)) -> (A -> C)"
let () = valid " (A & (B \\/ C)) -> ((A & B) \\/ (A & C))"
let () = valid " ((A & B) \\/ (A & C)) -> (A & (B \\/ C))"
let () = valid " (A \\/ (B & C)) -> ((A \\/ B) & (A \\/ C))"
let () = valid " ((A \\/ B) & (A \\/ C)) -> (A \\/ (B & C))"
let () = valid " (~ A -> A) -> A "
let () = valid " ((P -> (Q & R & S)) & ~S) -> ~P"
let () = valid " (P & Q) -> (Q & P)"
let () = valid " (A & A) \\/ ~A"
let () = valid " ~~A <-> A"
let () = valid " ~(A & B) <-> (~A \\/ ~B)"
let () = valid " ~(A \\/ B) <-> (~A & ~ B)"
let () = valid " (A \\/ (B & C)) <-> ((A \\/ B) & (A \\/ C))"
let () = valid " (A & (B \\/ C)) <-> ((A & B) \\/ (A & C))"


let () = invalid " A -> (A -> ~ A)"
let () = invalid " A & ~A"
let () = invalid " (A \\/ B) & ~A & ~B"
let () = invalid " (A -> B) -> (~A -> ~B)"
let () = invalid " (A -> B) -> (B -> A)"
let () = invalid " B -> (B & A)"
let () = invalid " (A -> A) <-> A"

let () = print_endline "all tests successfully completed"

open Format

let print_count_sat s = 
  let n = count_sat (snd (bdd_of_formula (p s))) in
  printf "count_sat(%s) = %Ld@." s n

let check_count_sat s n = 
  assert(count_sat (snd (bdd_of_formula (p s))) = n)

let () = check_count_sat "A" 1L
let () = check_count_sat "A \\/ B" 3L
let () = check_count_sat "A /\\ B" 1L
let () = check_count_sat "A /\\ (B \\/ C)" 3L

let () = check_count_sat "A \\/ ~A" 2L
let () = check_count_sat "(A \\/ ~A) /\\ (B \\/ ~B)" 4L

