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
open Equal
open Trewrite
open Order
open Meson
open Completion
open Resolution
open Decidable
open Qelim
open Complexelim
open Cooper
(* ========================================================================= *)
(* Geometry theorem proving.                                                 *)
(*                                                                           *)
(* Copyright (c) 2003-2007, John Harrison. (See "LICENSE.txt" for details.)  *)
(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* List of geometric properties with their coordinate translations.          *)
(* ------------------------------------------------------------------------- *)

let coordinations =
[("collinear",
  Atom
   (R ("=",
     [Fn ("*",
       [Fn ("-", [Var "1_x"; Var "2_x"]);
        Fn ("-", [Var "2_y"; Var "3_y"])]);
      Fn ("*",
       [Fn ("-", [Var "1_y"; Var "2_y"]);
        Fn ("-", [Var "2_x"; Var "3_x"])])])));
 ("parallel",
  Atom
   (R ("=",
     [Fn ("*",
       [Fn ("-", [Var "1_x"; Var "2_x"]);
        Fn ("-", [Var "3_y"; Var "4_y"])]);
      Fn ("*",
       [Fn ("-", [Var "1_y"; Var "2_y"]);
        Fn ("-", [Var "3_x"; Var "4_x"])])])));
 ("perpendicular",
  Atom
   (R ("=",
     [Fn ("+",
       [Fn ("*",
         [Fn ("-", [Var "1_x"; Var "2_x"]);
          Fn ("-", [Var "3_x"; Var "4_x"])]);
        Fn ("*",
         [Fn ("-", [Var "1_y"; Var "2_y"]);
          Fn ("-", [Var "3_y"; Var "4_y"])])]);
      Fn ("0", [])])));
 ("lengths_eq",
  Atom
   (R ("=",
     [Fn ("+",
       [Fn ("^", [Fn ("-", [Var "1_x"; Var "2_x"]); Fn ("2", [])]);
        Fn ("^", [Fn ("-", [Var "1_y"; Var "2_y"]); Fn ("2", [])])]);
      Fn ("+",
       [Fn ("^", [Fn ("-", [Var "3_x"; Var "4_x"]); Fn ("2", [])]);
        Fn ("^", [Fn ("-", [Var "3_y"; Var "4_y"]); Fn ("2", [])])])])));
 ("is_midpoint",
  And
   (Atom
     (R ("=",
       [Fn ("*", [Fn ("2", []); Var "1_x"]);
        Fn ("+", [Var "2_x"; Var "3_x"])])),
   Atom
    (R ("=",
      [Fn ("*", [Fn ("2", []); Var "1_y"]);
       Fn ("+", [Var "2_y"; Var "3_y"])]))));
 ("is_intersection",
  And
   (Atom
     (R ("=",
       [Fn ("*",
         [Fn ("-", [Var "1_x"; Var "2_x"]);
          Fn ("-", [Var "2_y"; Var "3_y"])]);
        Fn ("*",
         [Fn ("-", [Var "1_y"; Var "2_y"]);
          Fn ("-", [Var "2_x"; Var "3_x"])])])),
   Atom
    (R ("=",
      [Fn ("*",
        [Fn ("-", [Var "1_x"; Var "4_x"]);
         Fn ("-", [Var "4_y"; Var "5_y"])]);
       Fn ("*",
        [Fn ("-", [Var "1_y"; Var "4_y"]);
         Fn ("-", [Var "4_x"; Var "5_x"])])]))));
 ("=",
  And (Atom (R ("=", [Var "1_x"; Var "2_x"])),
   Atom (R ("=", [Var "1_y"; Var "2_y"]))))];;

(* ------------------------------------------------------------------------- *)
(* Convert formula into coordinate form.                                     *)
(* ------------------------------------------------------------------------- *)

let coordinate = onatoms
  (fun (R(a,args)) ->
    let xtms,ytms = unzip
     (map (fun (Var v) -> Var(v^"_x"),Var(v^"_y")) args) in
    let xs = map (fun n -> string_of_int n^"_x") (1--length args)
    and ys = map (fun n -> string_of_int n^"_y") (1--length args) in
    subst (fpf (xs @ ys) (xtms @ ytms)) (assoc a coordinations));;

(* ------------------------------------------------------------------------- *)
(* Trivial example.                                                          *)
(* ------------------------------------------------------------------------- *)

(*
coordinate <<collinear(a,b,c) ==> collinear(b,a,c)>>;;
*)

(* ------------------------------------------------------------------------- *)
(* Verify equivalence under rotation.                                        *)
(* ------------------------------------------------------------------------- *)

let invariant (x',y') ((s:string),z) =
  let m n f =
    let x = string_of_int n^"_x" and y = string_of_int n^"_y" in
    let i = fpf ["x";"y"] [Var x;Var y] in
    (x |-> tsubst i x') ((y |-> tsubst i y') f) in
  Iff(z,subst(itlist m (1--5) undefined) z);;

let invariant_under_translation = invariant (Fn ("+", [Var "x"; Var "X"]), Fn ("+", [Var "y"; Var "Y"]))

(*
forall (grobner_decide ** invariant_under_translation) coordinations;;
*)

let invariant_under_rotation fm =
  Imp(Atom
 (R ("=",
   [Fn ("+",
     [Fn ("^", [Var "s"; Fn ("2", [])]);
      Fn ("^", [Var "c"; Fn ("2", [])])]);
    Fn ("1", [])])),
      invariant ((Fn ("-", [Fn ("*", [Var "c"; Var "x"]); Fn ("*", [Var "s"; Var "y"])]),
 Fn ("+", [Fn ("*", [Var "s"; Var "x"]); Fn ("*", [Var "c"; Var "y"])]))) fm);;

(*
forall (grobner_decide ** invariant_under_rotation) coordinations;;
*)

(* ------------------------------------------------------------------------- *)
(* And show we can always invent such a transformation to zero a y:          *)
(* ------------------------------------------------------------------------- *)

(*
real_qelim
 <<forall x y. exists s c. s^2 + c^2 = 1 /\ s * x + c * y = 0>>;;
*)

(* ------------------------------------------------------------------------- *)
(* Choose one point to be the origin and rotate to zero another y coordinate *)
(* ------------------------------------------------------------------------- *)

let originate fm =
  let a::b::ovs = fv fm in
  subst (fpf [a^"_x"; a^"_y"; b^"_y"] [zero; zero; zero])
        (coordinate fm);;

(* ------------------------------------------------------------------------- *)
(* Other interesting invariances.                                            *)
(* ------------------------------------------------------------------------- *)

let invariant_under_scaling fm =
  Imp(Not (Atom (R ("=", [Var "A"; Fn ("0", [])]))),
  invariant((Fn ("*", [Var "A"; Var "x"]), Fn ("*", [Var "A"; Var "y"]))) fm);;

let invariant_under_shearing = invariant(Fn ("+", [Var "x"; Fn ("*", [Var "b"; Var "y"])]), Var "y");;

(*
forall (grobner_decide ** invariant_under_scaling) coordinations;;

partition (grobner_decide ** invariant_under_shearing) coordinations;;
*)

(* ------------------------------------------------------------------------- *)
(* One from "Algorithms for Computer Algebra"                                *)
(* ------------------------------------------------------------------------- *)

(*
(grobner_decide ** originate)
 <<is_midpoint(m,a,c) /\ perpendicular(a,c,m,b)
   ==> lengths_eq(a,b,b,c)>>;;

(* ------------------------------------------------------------------------- *)
(* Parallelogram theorem (Chou's expository example at the start).           *)
(* ------------------------------------------------------------------------- *)

(grobner_decide ** originate)
 <<parallel(a,b,d,c) /\ parallel(a,d,b,c) /\
   is_intersection(e,a,c,b,d)
   ==> lengths_eq(a,e,e,c)>>;;

(grobner_decide ** originate)
 <<parallel(a,b,d,c) /\ parallel(a,d,b,c) /\
   is_intersection(e,a,c,b,d) /\ ~collinear(a,b,c)
   ==> lengths_eq(a,e,e,c)>>;;
*)

(* ------------------------------------------------------------------------- *)
(* Reduce p using triangular set, collecting degenerate conditions.          *)
(* ------------------------------------------------------------------------- *)

let rec pprove vars triang p degens =
  if p = zero then degens else
  match triang with
    [] -> (mk_eq p zero)::degens
  | (Fn("+",[c;Fn("*",[Var x;_])]) as q)::qs ->
        if x <> hd vars then
          if mem (hd vars) (fvt p)
          then itlist (pprove vars triang) (coefficients vars p) degens
          else pprove (tl vars) triang p degens
        else
          let k,p' = pdivide vars p q in
          if k = 0 then pprove vars qs p' degens else
          let degens' = Not(mk_eq (head vars q) zero)::degens in
          itlist (pprove vars qs) (coefficients vars p') degens';;

(* ------------------------------------------------------------------------- *)
(* Triangulate a set of polynomials.                                         *)
(* ------------------------------------------------------------------------- *)

let rec triangulate vars consts pols =
  if vars = [] then pols else
  let cns,tpols = partition (is_constant vars) pols in
  if cns <> [] then triangulate vars (cns @ consts) tpols else
  if length pols <= 1 then pols @ triangulate (tl vars) [] consts else
  let n = end_itlist min (map (degree vars) pols) in
  let p = find (fun p -> degree vars p = n) pols in
  let ps = subtract pols [p] in
  triangulate vars consts (p::map (fun q -> snd(pdivide vars q p)) ps);;

(* ------------------------------------------------------------------------- *)
(* Trivial version of Wu's method based on repeated pseudo-division.         *)
(* ------------------------------------------------------------------------- *)

let wu fm vars zeros =
  let gfm0 = coordinate fm in
  let gfm = subst(itlist (fun v -> v |-> zero) zeros undefined) gfm0 in
  if not (set_eq vars (fv gfm)) then failwith "wu: bad parameters" else
  let ant,con = dest_imp gfm in
  let pols = map (lhs ** polyatom vars) (conjuncts ant)
  and ps = map (lhs ** polyatom vars) (conjuncts con) in
  let tri = triangulate vars [] pols in
  itlist (fun p -> union(pprove vars tri p [])) ps [];;

(* ------------------------------------------------------------------------- *)
(* Simson's theorem.                                                         *)
(* ------------------------------------------------------------------------- *)

(*
let simson =
 <<lengths_eq(o,a,o,b) /\
   lengths_eq(o,a,o,c) /\
   lengths_eq(o,a,o,d) /\
   collinear(e,b,c) /\
   collinear(f,a,c) /\
   collinear(g,a,b) /\
   perpendicular(b,c,d,e) /\
   perpendicular(a,c,d,f) /\
   perpendicular(a,b,d,g)
   ==> collinear(e,f,g)>>;;

let vars =
 ["g_y"; "g_x"; "f_y"; "f_x"; "e_y"; "e_x"; "d_y"; "d_x"; "c_y"; "c_x";
  "b_y"; "b_x"; "o_x"]
and zeros = ["a_x"; "a_y"; "o_y"];;

wu simson vars zeros;;

(* ------------------------------------------------------------------------- *)
(* Try without special coordinates.                                          *)
(* ------------------------------------------------------------------------- *)

wu simson (vars @ zeros) [];;

(* ------------------------------------------------------------------------- *)
(* Pappus (Chou's figure 6).                                                 *)
(* ------------------------------------------------------------------------- *)

let pappus =
 <<collinear(a1,b2,d) /\
   collinear(a2,b1,d) /\
   collinear(a2,b3,e) /\
   collinear(a3,b2,e) /\
   collinear(a1,b3,f) /\
   collinear(a3,b1,f)
   ==> collinear(d,e,f)>>;;

let vars = ["f_y"; "f_x"; "e_y"; "e_x"; "d_y"; "d_x";
            "b3_y"; "b2_y"; "b1_y"; "a3_x"; "a2_x"; "a1_x"]
and zeros = ["a1_y"; "a2_y"; "a3_y"; "b1_x"; "b2_x"; "b3_x"];;

wu pappus vars zeros;;

(* ------------------------------------------------------------------------- *)
(* The Butterfly (figure 9).                                                 *)
(* ------------------------------------------------------------------------- *)

(****
let butterfly =
 <<lengths_eq(b,o,a,o) /\ lengths_eq(c,o,a,o) /\ lengths_eq(d,o,a,o) /\
   collinear(a,e,c) /\ collinear(d,e,b) /\
   perpendicular(e,f,o,e) /\
   collinear(a,f,d) /\ collinear(f,e,g) /\ collinear(b,c,g)
   ==> is_midpoint(e,f,g)>>;;

let vars = ["g_y"; "g_x"; "f_y"; "f_x"; "e_y"; "e_x"; "d_y"; "c_y";
            "b_y"; "d_x"; "c_x"; "b_x"; "a_x"]
and zeros = ["a_y"; "o_x"; "o_y"];;

 **** This one is costly (too big for laptop, but doable in about 300M)
 **** However, it gives exactly the same degenerate conditions as Chou

wu butterfly vars zeros;;

 ****
 ****)
*)

(*** Other examples removed from text

(* ------------------------------------------------------------------------- *)
(* Centroid (Chou, example 142).                                             *)
(* ------------------------------------------------------------------------- *)

(grobner_decide ** originate)
 <<is_midpoint(d,b,c) /\ is_midpoint(e,a,c) /\
   is_midpoint(f,a,b) /\ is_intersection(m,b,e,a,d)
   ==> collinear(c,f,m)>>;;

****)
