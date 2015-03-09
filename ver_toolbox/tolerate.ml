(*
    <vscr - Verilog converter to hls format.>
    Copyright (C) <2011,2012>  <Jonathan Richard Robert Kimmitt>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

open Globals
open Printf
open Idhash
open Vparser
open Dump
open List
open Read_library

let prefix = ref "F_"
let faultid id = ID id
let faultid' id = Flatten.pathid !prefix id

let faultsim cnt kind =
    if Hashtbl.mem modprims kind then let inst = Hashtbl.find modprims kind in
    begin
      Printf.printf "%s: %d\n" kind !cnt;
      if inst.is_seq then
         begin
         incr cnt;
         [TRIPLE (CELLPIN, ID (enterid "faultsim"), TRIPLE (BITSEL, ID (enterid "inject"), INT !cnt))]
         end
      else
         []
    end
  else
    []

let rec tolerate faultcnt syms exp :token = match exp with
| QUADRUPLE((WIRE|TRI0|TRI1) as tok, arg0, TRIPLE(arg1, rng, arg3), TLIST arg4) ->
  QUADRUPLE(tok, arg0, TRIPLE(arg1, rng, arg3),
            TLIST (List.map (fun x -> (match x with
              | DOUBLE (ID id, EMPTY) -> DOUBLE (faultid id, EMPTY)
              | _ -> unhandled stderr 51 x; EMPTY)) arg4 @
                     List.map (fun x -> (match x with
              | DOUBLE (ID id, EMPTY) -> DOUBLE (faultid' id, EMPTY)
              | _ -> unhandled stderr 54 x; EMPTY)) arg4))
| QUADRUPLE(MODINST, ID kind, params, TLIST arg3) ->
  QUADRUPLE(MODINST, faultid' kind, params,
    TLIST (List.map (function
      | TRIPLE (ID id, SCALAR, TLIST arg4) -> TRIPLE (faultid' id, SCALAR, 
        TLIST (faultsim faultcnt (!prefix^kind.id) @ List.map (function
          | TRIPLE (CELLPIN, ID cellpin, ((WIDTHNUM _ | INT _ | DECNUM _ | HEXNUM _ | BINNUM _) as const)) ->
		  let (sz,num) = Minimap.basenum const in
                  let num2 = ((1 lsl sz)-1) lxor num in
                  TRIPLE (CELLPIN, ID cellpin, DOUBLE(CONCAT, TLIST[WIDTHNUM(2, sz, num2); WIDTHNUM(2, sz, num)]))
          | TRIPLE (CELLPIN, ID cellpin, ID conn) ->
            TRIPLE (CELLPIN, ID cellpin, DOUBLE (CONCAT, TLIST [faultid' conn;faultid conn]))
          | TRIPLE (CELLPIN, ID cellpin, TRIPLE (BITSEL, ID bit, INT idx1)) ->
            TRIPLE (CELLPIN, ID cellpin, DOUBLE (CONCAT, TLIST [TRIPLE (BITSEL, faultid' bit, INT idx1);
                                                                TRIPLE (BITSEL, faultid bit, INT idx1)]))
          | TRIPLE (CELLPIN, ID cellpin, QUADRUPLE (PARTSEL, ID bit, INT idx1, INT idx2)) ->
            TRIPLE (CELLPIN, ID cellpin, DOUBLE (CONCAT, TLIST [QUADRUPLE (PARTSEL, faultid' bit, INT idx1, INT idx2);
                                                                QUADRUPLE (PARTSEL, faultid bit, INT idx1, INT idx2)]))
          | TRIPLE (CELLPIN, ID cellpin, DOUBLE(CONCAT, TLIST clst')) ->
	      let clst = Minimap.concat_flatten' syms clst' in
              TRIPLE (CELLPIN, ID cellpin,
                    DOUBLE(CONCAT,
	                   TLIST (List.flatten ([
						List.map (function
						| TRIPLE (BITSEL, ID bit, INT idx1) -> TRIPLE (BITSEL, faultid' bit, INT idx1)
						| QUADRUPLE (PARTSEL, ID bit, INT idx1, INT idx2) -> QUADRUPLE (PARTSEL, faultid' bit, INT idx1, INT idx2)
						| ID id -> faultid' id
						| (WIDTHNUM _ | INT _ | DECNUM _ | HEXNUM _ | BINNUM _) as const ->
						    let (sz,num) = Minimap.basenum const in
						    WIDTHNUM(2, sz, ((1 lsl sz)-1) lxor num)
						| z -> unhandled stderr 82 z; EMPTY) clst;
						List.map (function
						| TRIPLE (BITSEL, ID bit, INT idx1) -> TRIPLE (BITSEL, faultid bit, INT idx1)
						| QUADRUPLE (PARTSEL, ID bit, INT idx1, INT idx2) -> QUADRUPLE (PARTSEL, faultid bit, INT idx1, INT idx2)
						| ID id -> faultid id
						| (WIDTHNUM _ | INT _ | DECNUM _ | HEXNUM _ | BINNUM _) as const ->
						    let (sz,num) = Minimap.basenum const in
						    WIDTHNUM(2, sz, num)
						| z -> unhandled stderr 90 z; EMPTY) clst]))))
          | y -> unhandled stderr 91 y; EMPTY) arg4))
      | x -> unhandled stderr 92 x; EMPTY) arg3))
| QUINTUPLE(MODULE, ID arg1, TLIST arg2, TLIST arg3, THASH thash) ->
  let thash2 = (Hashtbl.create 256,Hashtbl.create 256) in
  let fsim = ID (enterid "inject") in
  faultcnt := 0;
  let arg3' = List.flatten (List.map (fun x -> (match x with
    | ID id -> [faultid' id;faultid id]
    | _ -> unhandled stderr 100 x; [])) arg3) in
  Hashtbl.iter (fun x _ -> Hashtbl.add (fst thash2) (tolerate faultcnt syms x) ()) (fst thash);
  Hashtbl.iter (fun x _ -> Hashtbl.add (snd thash2) (tolerate faultcnt syms x) ()) (snd thash);
  Hashtbl.add (fst thash2) (QUINTUPLE(INPUT, EMPTY, EMPTY, RANGE (INT !faultcnt, INT 0),TLIST [TRIPLE (fsim, EMPTY, EMPTY)])) ();

  QUINTUPLE(MODULE, ID (enterid (!prefix^arg1.id)), TLIST arg2, TLIST (fsim :: arg3'), THASH thash2)
| RANGE(INT arg1,INT arg2) -> RANGE(INT arg1,INT arg2)
| QUINTUPLE((INPUT|OUTPUT) as tok, arg1, arg2, rng, TLIST arg4) ->
  let lst = TLIST (List.flatten (List.map (fun x -> (match x with
    | TRIPLE (ID id, EMPTY, EMPTY) -> [TRIPLE(faultid' id, EMPTY, EMPTY);TRIPLE(faultid id, EMPTY, EMPTY)]
    | _ -> unhandled stderr 110 x; [])) arg4)) in
                  QUINTUPLE(tok, arg1, arg2, rng, lst)
| EMPTY -> EMPTY
| _ -> unhandled stderr 113 exp; UNKNOWN

let generate_tolerant_netlist faultcnt k = tolerate faultcnt k.symbols k.tree

let gen_tolerant_arch arch nam =
  let faultcnt = ref 0 in
  let lst = Hashtbl.find_all modprims nam in
  List.iter (fun arg -> if arg.arch = arch then
      Semantics.prescan stderr "tolerant" (generate_tolerant_netlist faultcnt arg) "Generated by gen_tolerant_arch") lst;;
