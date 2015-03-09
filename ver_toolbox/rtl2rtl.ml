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

open Setup
open Globals
open Idhash
open Vparser
open Dump
open Read_library

let width' isyms id =
  let (lft,rght,inc) = Minimap.find_width id isyms in
  if inc = 0 then SCALAR else RANGE(INT lft,INT rght)
  
let rtl2rtl_subcell width iolstref syms thash2 kind arg3 iolst paramlst =
  Hashtbl.add (snd thash2)
    (QUADRUPLE(MODINST, ID kind, paramlst, TLIST (List.map (function
      | TRIPLE (ID instid, SCALAR, TLIST arg4) ->
          TRIPLE (ID instid, SCALAR, 
                  TLIST (let pos = ref iolst in
                         let supplied = List.map (function
                           | TRIPLE (CELLPIN, ID cellpin, conn) as expr ->
                               pos := List.filter (fun itm -> ID cellpin <> itm) !pos;
                               expr
                           | (BINNUM _|OCTNUM _|DECNUM _|HEXNUM _|WIDTHNUM _|ID _
                                 |DOUBLE(NOT, _)
                                 |TRIPLE((OR|AND|BITSEL),_,_)
                                 |QUADRUPLE(PARTSEL,_,_,_)) as posarg ->
                               let hd = List.hd !pos in pos := List.tl !pos;
                               TRIPLE (CELLPIN, hd, posarg)
                           | oth -> unhandled stderr 34 oth; EMPTY) arg4 in
                         let missing = List.map (function
                           | ID id when
                               String.length id.id > 4 &&
                                 String.sub id.id (String.length id.id-4) 4 = "$reg" ->
                               let conn = enterid (String.concat "$" [kind.id;instid.id;id.id]) in
                               let intrng = width id in
                               Hashtbl.replace (fst thash2)
                                 (QUINTUPLE(OUTPUT,
                                            EMPTY,
                                            EMPTY,
                                            intrng,
                                            TLIST [(TRIPLE(ID conn, EMPTY, EMPTY))])) ();
                               iolstref := ID conn :: !iolstref;
                               TRIPLE (CELLPIN, ID id, ID conn)
                           | itm -> DOUBLE(CELLPIN, itm)) !pos in
                         supplied @ missing))
      | x -> unhandled stderr 46 x; EMPTY) arg3))) ()

let mask n0 = if n0 = 0 then 1 else let l = ref 0 and n = ref n0 in while (!n > 0) do n := !n lsr 1; l := !l + 1; done; !l

let rtl2rtl_subcell' iolstref syms thash2 kind arg3 paramlst nam top =
  match top.tree with
    | QUINTUPLE(MODULE, _, _, TLIST iolst, _) ->
        rtl2rtl_subcell (width' top.symbols) iolstref syms thash2 kind arg3 iolst paramlst
    | err -> unhandled stderr 31 err

let rec assignment_expr syms = function
  | ID id -> DOUBLE(CONCAT, TLIST (Minimap.id_flatten (Minimap.find_width id syms) id []))
  | INT n -> INT n
  | BINNUM lev -> let (sz,num) = Const.widthnum stderr 2 lev in DOUBLE(CONCAT, TLIST (Minimap.num_flatten sz num []))
  | DECNUM lev -> let (sz,num) = Const.widthnum stderr 10 lev in DOUBLE(CONCAT, TLIST (Minimap.num_flatten sz num []))
  | HEXNUM lev -> let (sz,num) = Const.widthnum stderr 16 lev in DOUBLE(CONCAT, TLIST (Minimap.num_flatten sz num []))
  | DOUBLE(NOT, expr) -> DOUBLE (NOT, assignment_expr syms expr)
  | DOUBLE(AND, expr) -> DOUBLE (AND, assignment_expr syms expr)
  | DOUBLE(OR, expr) -> DOUBLE (OR, assignment_expr syms expr)
  | DOUBLE(LPAREN, expr) -> DOUBLE(LPAREN, assignment_expr syms expr)
  | DOUBLE(PLING, expr) -> DOUBLE (PLING, assignment_expr syms expr)
  | DOUBLE(CONCAT, TLIST lst) ->
      let lst' = List.map (assignment_expr syms) lst in
      let flt = Minimap.concat_flattenable' lst' in
      let lst'' = if flt then Minimap.concat_flatten' syms lst' else lst' in
      DOUBLE(CONCAT, TLIST lst'')
  | TRIPLE(BITSEL, ID id, INT sel) -> TRIPLE(BITSEL, ID id, INT sel)
  | TRIPLE(FUNCREF, ID fid, TLIST body) -> TRIPLE(FUNCREF, ID fid, TLIST (List.map (assignment_expr syms) body))
  | TRIPLE(PLUS, left, rght) -> TRIPLE(PLUS, assignment_expr syms left, assignment_expr syms rght)
  | TRIPLE(MINUS, left, rght) -> TRIPLE(MINUS, assignment_expr syms left, assignment_expr syms rght)
  | TRIPLE(TIMES, left, rght) -> TRIPLE(TIMES, assignment_expr syms left, assignment_expr syms rght)
  | TRIPLE(AND, left, rght) -> TRIPLE(AND, assignment_expr syms left, assignment_expr syms rght)
  | TRIPLE(OR, left, rght) -> TRIPLE(OR, assignment_expr syms left, assignment_expr syms rght)
  | TRIPLE(XOR, left, rght) -> TRIPLE(XOR, assignment_expr syms left, assignment_expr syms rght)
  | TRIPLE(P_ANDAND, left, rght) -> TRIPLE(P_ANDAND, assignment_expr syms left, assignment_expr syms rght)
  | TRIPLE(P_OROR, left, rght) -> TRIPLE(P_OROR, assignment_expr syms left, assignment_expr syms rght)
  | TRIPLE(P_SLEFT, left, rght) -> TRIPLE(P_SLEFT, assignment_expr syms left, assignment_expr syms rght)
  | TRIPLE(P_SRIGHT, left, rght) -> TRIPLE(P_SRIGHT, assignment_expr syms left, assignment_expr syms rght)
  | TRIPLE(P_EQUAL, left, rght) -> TRIPLE(P_EQUAL, assignment_expr syms left, assignment_expr syms rght)
  | TRIPLE(P_NOTEQUAL, left, rght) -> TRIPLE(P_NOTEQUAL, assignment_expr syms left, assignment_expr syms rght)
  | TRIPLE(CONCAT, INT cnt, TLIST lst) -> DOUBLE(CONCAT, TLIST (Minimap.concat_flatten cnt (List.map (assignment_expr syms) lst) []))
  | QUADRUPLE(PARTSEL, ID id, INT hi, INT lo) -> DOUBLE(CONCAT, TLIST (Minimap.id_flatten (hi, lo, -1) id []))
  | QUADRUPLE(QUERY, sel, first, alternate) -> QUADRUPLE(QUERY, assignment_expr syms sel, assignment_expr syms first, assignment_expr syms alternate)
  | oth -> unhandled stderr 105 oth; EMPTY

let rtl2rtl arch thash2 syms iolstref = function
| DOUBLE((ALWAYS|INITIAL|FINAL), _) as exp -> Hashtbl.add (snd thash2) exp ();
| TRIPLE(ASSIGN, arg1, TLIST arg2) ->
    (match arg1 with 
      | DOUBLE (HASH, TLIST [INT n]) -> Printf.fprintf stderr "/* #%d Delay ignored */\n" n
      | EMPTY -> ()
      | oth -> unhandled stderr 32 oth);
    List.iter (function
      | TRIPLE(ASSIGNMENT, ID id, exp) -> let (hi,lo,inc) = Minimap.find_width id syms in
	Hashtbl.add (snd thash2)
	  (TRIPLE(ASSIGN, arg1, TLIST [TRIPLE(ASSIGNMENT, DOUBLE(CONCAT, TLIST (Minimap.id_flatten (hi,lo,inc) id [])), assignment_expr syms exp)]))()
      | TRIPLE(ASSIGNMENT, DOUBLE(CONCAT, TLIST lst), exp) ->
	  Hashtbl.add (snd thash2)
	    (TRIPLE(ASSIGN, arg1, TLIST [TRIPLE(ASSIGNMENT, DOUBLE(CONCAT, TLIST lst), assignment_expr syms exp)])) ()
      | oth -> unhandled stderr 141 oth) arg2;
| QUADRUPLE((WIRE|TRI0|TRI1) as tok, arg0, TRIPLE(arg1, srng, arg3), TLIST arg4) ->
  let intrng = Minimap.flatten_width srng syms in
  Hashtbl.replace (fst thash2) (QUADRUPLE(tok, arg0, TRIPLE(arg1, intrng, arg3),
            TLIST (List.map (fun x -> (match x with
              | DOUBLE (ID id, EMPTY) -> DOUBLE (ID id, EMPTY)
              | TRIPLE (ID id, EMPTY, expr) ->
		  let ass = assignment_expr syms expr in
		  Hashtbl.add (snd thash2)
		    (TRIPLE(ASSIGN, EMPTY, TLIST [TRIPLE(ASSIGNMENT, ID id, ass)])) ();
		  DOUBLE (ID id, EMPTY)
              | _ -> unhandled stderr 39 x; EMPTY)) arg4))) ()
| QUADRUPLE(MODINST, ID kind, paramlst, TLIST arg3) ->
  if Hashtbl.mem modprims kind.id then
    begin
      Minimap.select_sub false (rtl2rtl_subcell' iolstref syms thash2 kind arg3 paramlst) !archenv kind.id
    end
  else if Hashtbl.mem libhash kind.id then
    begin
      let libcell = Hashtbl.find libhash kind.id in
      rtl2rtl_subcell (fun _ -> SCALAR) iolstref syms thash2 kind arg3 libcell.iolst paramlst
    end
  else
    failwith ("sub-module "^kind.id^" not found")
(*
| SEXTUPLE(PARAMETER, signing, range, id, attr, expr) as param -> Hashtbl.add (fst thash2) param ()
*)
| QUADRUPLE(REG, EMPTY, srng, TLIST arg4) ->
  let intrng = Minimap.flatten_width srng syms in
  Hashtbl.replace (fst thash2) (QUADRUPLE(REG, EMPTY, intrng, TLIST arg4)) ();
| QUADRUPLE(INTEGER, EMPTY, EMPTY, TLIST arg4) ->
  Hashtbl.replace (fst thash2) (QUADRUPLE(INTEGER, EMPTY, EMPTY, TLIST arg4)) ();
| QUINTUPLE((INPUT|OUTPUT) as tok, ((EMPTY|REG) as registered), EMPTY, srng, TLIST arg4) ->
  let intrng = Minimap.flatten_width srng syms in
  let lst = TLIST (List.map (function
    | TRIPLE (ID id, EMPTY, EMPTY) -> TRIPLE(ID id, EMPTY, EMPTY)
    | x -> unhandled stderr 51 x; registered) arg4) in
  Hashtbl.replace (fst thash2) (QUINTUPLE(tok, EMPTY, EMPTY, intrng, lst)) ()
| SEPTUPLE(TASK, EMPTY, ID tid, EMPTY, TLIST iolst, TLIST body, EMPTY) as exp -> Hashtbl.add (snd thash2) exp ();
| OCTUPLE(FUNCTION, EMPTY, rng, ID fid, EMPTY, TLIST iolst, TLIST body, EMPTY) as exp -> Hashtbl.add (snd thash2) exp ();
| TRIPLE(BUF, dly, TLIST[QUADRUPLE(exp1, SCALAR, exp2, exp3)]) ->
    Hashtbl.add (snd thash2)
      (TRIPLE(ASSIGN, dly, TLIST [TRIPLE(ASSIGNMENT, assignment_expr syms exp2, assignment_expr syms exp3)])) ()
| exp -> unhandled stderr 63 exp
    
let rtl2rtl' arch thash2 syms ioref decls insts =
        Hashtbl.iter (fun x _ -> rtl2rtl arch thash2 syms ioref x) decls;
        Hashtbl.iter (fun x _ -> rtl2rtl arch thash2 syms ioref x) insts

let generate_rtl2rtl_netlist k =
  let thash2 = (Hashtbl.create 256,Hashtbl.create 256) in
  match k.tree with
    | QUINTUPLE(MODULE, ID arg1, TLIST arg2, TLIST arg3, THASH (decls,insts)) ->
        let iolstref = ref (List.rev arg3) in
        rtl2rtl' k.arch thash2 k.symbols iolstref decls insts;
        QUINTUPLE(MODULE, ID (enterid (arg1.id^ !Globals.modsuffix)), TLIST arg2,
                TLIST (List.map (function
                  | ID id -> ID id
                  | x -> unhandled stderr 78 x; EMPTY) (List.rev !iolstref)),
                THASH thash2)
    | _ -> failwith (Dump.dumpstr k.tree^" is not a module netlist")

let gen_rtl2rtl_arch arch nam =
  let lst = Count.find_arch arch nam in
  match lst with
    | arg::[] -> assert (arg.arch = arch);
        Minimap.recurse_arch (fun id subarg -> assert (subarg.arch = arch);
          Semantics.prescan stderr !Globals.archenv (generate_rtl2rtl_netlist subarg) "Generated by gen_rtl2rtl_arch") arch nam arg;
          Printf.printf "Module report %s\n" (Semantics.endscan())
    | [] -> failwith "nothing selected in gen_rtl2rtl_arch"
    | _ -> failwith "ambiguous selection in gen_rtl2rtl_arch"
