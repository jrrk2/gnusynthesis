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
open Read_library

let verbose = ref false

let other thex = function
  | WIDTHNUM (2, 1, 0) -> "Bin1'b0"^thex
  | WIDTHNUM (2, 1, 1) -> "Bin1'b1"^thex
  | BINNUM "1'b0" -> "Bin1'b0"^thex
  | BINNUM "1'b1" -> "Bin1'b1"^thex
  | TRIPLE(BITSEL, ID bit, INT idx1) -> sprintf "%s[%d]" bit.id idx1
  | TRIPLE(BITSEL, ID bit, WIDTHNUM (radix, siz, num)) -> sprintf "%s[%d]" bit.id num
  | QUADRUPLE(PARTSEL, ID part, INT idx1, INT idx2) when idx1=idx2 -> sprintf "%s[%d]" part.id idx1
  | oth -> unhandled stderr 31 oth; Count.tokenstr oth

let argsel thex syms pin lst = String.concat "" (List.map (function
  | TRIPLE (CELLPIN, ID cellpin, ID conn) when cellpin = pin ->
    if Const.shash_chain_mem syms conn then
      begin
        let (hi,lo,inc) = Minimap.find_width conn syms in
        if inc = 0 then
	  sprintf " %s" conn.id
        else if hi=lo then
	  sprintf " %s[%d]" conn.id hi
        else
	  sprintf " %s[%d:%d]" conn.id hi lo
      end
    else
      sprintf " %s" conn.id
  | TRIPLE (CELLPIN, ID cellpin, expr) when cellpin = pin -> " "^other thex expr
  | DOUBLE(CELLPIN, ID cellpin) when cellpin = pin -> sprintf "%s()" cellpin.id
  | oth -> "") lst)

let rec dump_blif_netlist thex syms comment = function
  | QUINTUPLE((INPUT|OUTPUT|INOUT) as tok, arg1, arg2, arg3, TLIST arg4) ->
    (match arg1 with 
      | EMPTY -> ()
      | REG -> ()
      | _ -> unhandled stderr 433 arg1);
    (match arg2 with 
      | EMPTY -> ()
      | _ -> unhandled stderr 436 arg2);
    let delim = ref "" and tab = ref 0 in
    let indent len : string =
      tab := !tab + len + 2;
      if !tab > 72 then (let i = !delim^"\n\t" in tab := 0; delim := ""; i) else !delim^" " in
    let conc = String.concat "" (List.map (fun x -> let y = match x with
      | TRIPLE (ID id, EMPTY, EMPTY) -> indent (String.length id.id) ^ (id.id)
      | x -> unhandled stderr 440 x; !delim in delim := ","; y) arg4) in
    sprintf "\n%s " (Ord.getstr tok) ^
    dump_blif_netlist thex syms comment arg3 ^ conc ^ ";"
  | QUADRUPLE((WIRE|TRI0|TRI1), arg0, arg1, TLIST arg4) -> ""
  | QUADRUPLE((MODINST|PRIMINST), ID kind, arg2, TLIST arg3) -> if kind = mybuf.ff.nam then
      begin
	  String.concat "" (List.map (function
	  | TRIPLE (ID id, SCALAR, TLIST arg4) -> 
	      ".latch " ^ (argsel thex syms mybuf.ff.dat arg4) ^ (argsel thex syms mybuf.ff.qout arg4) ^ " 2"
	  | oth -> unhandled stderr 345 oth; "") arg3) ^ sprintf "\n"
      end
    else
      begin
	  String.concat "" (List.map (function
	  | TRIPLE (ID id, SCALAR, TLIST arg4) -> 
	    sprintf ".gate %s" (kind.id) ^
	    String.concat "" (List.map (function
            | TRIPLE (CELLPIN, ID cellpin, ID conn) -> if Const.shash_chain_mem syms conn then
		begin
                  let (hi,lo,inc) = Minimap.find_width conn syms in
                  if inc = 0 then
                    sprintf " %s=%s" cellpin.id (conn.id)
                  else if hi=lo then
                    sprintf " %s=%s[%d]" cellpin.id (conn.id) hi
                  else
                    sprintf " %s=%s[%d:%d]" cellpin.id (conn.id) hi lo
		end
              else
		sprintf " %s=%s" cellpin.id (conn.id)
            | TRIPLE (CELLPIN, ID cellpin, expr) -> sprintf " %s=%s" cellpin.id (other thex expr)
            | DOUBLE(CELLPIN, ID cellpin) -> sprintf "%s()" cellpin.id
            | oth -> unhandled stderr 344 oth; "") (arg4))
	  | oth -> unhandled stderr 345 oth; "") arg3) ^ sprintf "\n"
      end
  | QUINTUPLE(MODULE,ID arg1, arg2, TLIST arg3, THASH targ4) ->
    let thex = Digest.to_hex (Printf.sprintf "%16.5f" (Unix.gettimeofday())) in
    let add_drive kind inst expr = dump_blif_netlist thex syms "" (QUADRUPLE(
      MODINST, ID kind.nam, EMPTY,
      TLIST [TRIPLE (ID (enterid inst), SCALAR,
		     TLIST [TRIPLE (CELLPIN,
				    ID (List.hd kind.opinlst).idpin, expr)])])) in
    let itmlst = ref (add_drive mybuf.gnd ("gnd$"^thex) (BINNUM "1'b0") ::
		      add_drive mybuf.pwr ("vcc$"^thex) (BINNUM "1'b1") :: []) in
    Hashtbl.iter (fun itm _ ->
      itmlst := (dump_blif_netlist thex syms comment itm) :: !itmlst;
      ) (snd targ4);
    let inputs = hfilter (function | QUINTUPLE(INPUT,_,_,_,_) -> true | _ -> false) (fst targ4) in
    let outputs = hfilter (function | QUINTUPLE(OUTPUT,_,_,_,_) -> true | _ -> false) (fst targ4) in
    dump_blif_header syms comment arg1 arg3 inputs outputs ^
    String.concat "" (!itmlst)
  | RANGE(INT arg1,INT arg2) -> sprintf "[%d:%d] " arg1 arg2
  | RANGE(arg1, arg2) ->
                let lft = Const.exprConstStr stderr syms arg1
                and rght = Const.exprConstStr stderr syms arg2 in
                sprintf "[%s:%s] " lft rght
  | TLIST items -> String.concat "" (List.map (dump_blif_netlist thex syms comment) items)
  | EMPTY -> ""
  | expr -> other thex expr

and dump_blif_header syms comment arg1 arg3 inputs outputs =
    let delim = ref " " and wid = ref 0 in
    let iolst arg = let ioexp = ref [] in List.iter (function
      | QUINTUPLE((INPUT|OUTPUT|INOUT), (EMPTY|REG), EMPTY, RANGE(INT lft,INT rght), TLIST vlst) ->
	List.iter (function
	| TRIPLE (ID id, EMPTY, EMPTY) ->
	  for i = lft downto rght do ioexp := sprintf "%s[%d]" id.id i :: !ioexp done
	| oth -> unhandled stderr 105 oth) vlst
      | QUINTUPLE((INPUT|OUTPUT|INOUT), (EMPTY|REG), EMPTY, EMPTY, TLIST slst) ->
	List.iter (function
	| TRIPLE (ID id, EMPTY, EMPTY) -> ioexp := id.id :: !ioexp
	| oth -> unhandled stderr 109 oth) slst
      | x -> unhandled stderr 110 x) arg; !ioexp in
    let args arg =
      String.concat "" (List.map (fun arg ->
	let io = !delim^arg in
	wid := !wid + String.length arg + 1;
	if !wid >= 80 then (wid := 1; delim := " \\\n ") else delim := " "; io) arg) in
    sprintf "# %s\n.model %s\n.inputs %s\n.outputs %s\n"
      comment arg1.id (args (iolst inputs)) (args (iolst outputs))

let rec dump_blif_netlist' comment subarg =
  if subarg.is_netlist
  then
    dump_blif_netlist "" subarg.Globals.symbols comment subarg.Globals.tree
  else
    failwith "Only netlists can be dumped with write_blif"

let write_blif_arch' arch nam file arg =
  printf "Writing module %s arch %s to file %s\n" nam arch file;
  let lst = ref [] in
  Minimap.recurse_arch (fun id subarg ->
    lst := dump_blif_netlist' ("sub-arch "^arch^" sub-module "^id^" - "^subarg.comment) subarg :: !lst) arch nam arg;
  let oc = unique_open file in
  List.iter (output_string oc) (!lst);
  close_out oc

let write_blif_arch arch nam =
  let lst = List.filter (fun arg -> arg.arch = arch) (Hashtbl.find_all modprims nam) in
  match lst with
    | [] -> failwith ("No netlists matched arch "^arch^" name "^nam)
    | _ -> List.iter (write_blif_arch' arch nam (nam^"."^arch^".blif")) lst

let write_blif nam = write_blif_arch "blif" nam
