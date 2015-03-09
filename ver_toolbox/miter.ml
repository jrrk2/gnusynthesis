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
  if inc = 0 then EMPTY else TRIPLE(INT lft,INT rght, INT inc)

let intrng = function
  | EMPTY -> EMPTY
  | TRIPLE(lft,rght,inc) -> RANGE(lft,rght)
  | _ -> failwith "internal miter error"

let miter_dir isyms id = match isyms with
  | Vparser.EndShash -> EMPTY
  | Vparser.Shash {Vparser.nxt;syms} ->
    let rslt = ref None in Vparser.TokSet.iter (function
      | OUTPUT -> rslt := Some OUTPUT
      | INPUT -> rslt := Some INPUT
      | INOUT -> failwith "INOUT not supported in miter"
      | _ -> ()) (Hashtbl.find syms id).Vparser.symattr;
    match !rslt with Some x -> x | None -> EMPTY

let miter_subcell miterlst instid width dir iolstref thash2 kind iolst =
  Hashtbl.add (snd thash2)
    (QUADRUPLE(MODINST, ID kind, EMPTY, TLIST [
      TRIPLE (ID instid, SCALAR, 
              TLIST (let pos = ref iolst in
                     let missing = List.map (function
                       | ID id -> let wid = width id in
                                  (match dir id with
                           | OUTPUT ->
                             let conn = enterid (String.concat "$" [instid.id;id.id]) in
                             Hashtbl.add (fst thash2)
                               (QUADRUPLE(WIRE,
                                          EMPTY,
                                          TRIPLE(EMPTY, intrng wid, EMPTY),
                                          TLIST [(DOUBLE(ID conn, EMPTY))])) ();
                             miterlst := (wid,conn) :: !miterlst;
                             TRIPLE(CELLPIN, ID id, ID conn)
                           | INPUT ->
                             if not (List.mem (ID id) !iolstref) then
                               begin
                                 Hashtbl.replace (fst thash2)
                                   (QUINTUPLE(INPUT,
                                              EMPTY,
                                              EMPTY,
                                              intrng wid,
                                              TLIST [(TRIPLE(ID id, EMPTY, EMPTY))])) ();
                                 iolstref := ID id :: !iolstref
                               end;
                             TRIPLE(CELLPIN, ID id, ID id)
                           | _ -> failwith "pin direction is undefined in miter")
                       | itm -> DOUBLE(CELLPIN, itm)) !pos in
                     missing))])) ()

let miter_subcell' miterlst iolstref thash2 kind nam top =
  match top.tree with
    | QUINTUPLE(MODULE, ID id, _, TLIST iolst, _) ->
        miter_subcell miterlst id (width' top.symbols) (miter_dir top.symbols) iolstref thash2 kind iolst
    | err -> unhandled stderr 31 err

let miter arch kind' thash2 iolstref =
  let miterlst = ref [] in
  let kind = enterid kind' in
  if Hashtbl.mem modprims kind.id then
    begin
      Minimap.select_sub false (miter_subcell' miterlst iolstref thash2 kind) arch kind.id;
      !miterlst
    end
  else if Hashtbl.mem libhash kind.id then
    failwith ("library cell "^kind.id^" not allowed")
  else
    failwith ("sub-module "^kind.id^" not found")
    
let miter' arch1 kind1 arch2 kind2 thash2 ioref miterid =
  let lst1 = miter arch1 kind1 thash2 ioref in
  let lst2 = miter arch2 kind2 thash2 ioref in
  let lst3 = ref [] in
  List.iter2 (fun (wid1,exp1) (wid2,exp2) -> match (wid1,wid2) with
    | (TRIPLE(INT lft1,INT rght1,INT inc1),TRIPLE(INT lft2,INT rght2,INT inc2)) ->
    begin
      let diff = enterid (exp1.id^"$"^exp2.id) in
      if (wid1 <> wid2) then failwith (Printf.sprintf "miter width mismatch signals %s %s" exp1.id exp2.id);
      Hashtbl.add (fst thash2)
        (QUADRUPLE(WIRE,
                   EMPTY,
                   TRIPLE(EMPTY, intrng wid1, EMPTY),
                   TLIST [(DOUBLE(ID diff, EMPTY))])) ();
      let asgnlst = ref [] and idx1 = ref lft1 and idx2 = ref lft2 in while !idx1 <> rght1+inc1 do
          let diff_bit = TRIPLE(BITSEL, ID diff, INT !idx1) in
          asgnlst := TRIPLE(ASSIGNMENT,
			diff_bit,
			TRIPLE(XOR,
				TRIPLE(BITSEL, ID exp1, INT !idx1),
				TRIPLE(BITSEL, ID exp2, INT !idx2))) :: !asgnlst;
	  idx1 := !idx1 + inc1;
	  idx2 := !idx2 + inc2;
	  lst3 := diff_bit :: !lst3
	  done;
      Hashtbl.replace (snd thash2)
        (TRIPLE(ASSIGN, EMPTY, TLIST !asgnlst)) ()
    end
    | (EMPTY,EMPTY) -> failwith "not implemented"
    | _ -> failwith "internal error") lst1 lst2;

  let exp = List.fold_left (fun itm arg -> TRIPLE(OR, itm, arg)) (List.hd !lst3) (List.tl !lst3) in

  Hashtbl.replace (snd thash2)
    (TRIPLE(ASSIGN, EMPTY, TLIST [TRIPLE(ASSIGNMENT, miterid, exp)])) ()
  
let generate_miter_netlist arch1 kind1 arch2 kind2 =
  let thash2 = (Hashtbl.create 256,Hashtbl.create 256) in
  let miterid = ID (enterid "miter$") in
  let iolstref = ref [miterid] in
  Hashtbl.replace (fst thash2) (QUINTUPLE(OUTPUT, EMPTY, EMPTY, EMPTY, TLIST [(TRIPLE(miterid, EMPTY, EMPTY))])) ();
  miter' arch1 kind1 arch2 kind2 thash2 iolstref miterid;
  QUINTUPLE(MODULE, ID (enterid (kind1^"$"^kind2)), EMPTY,
            TLIST (List.map (function
              | ID id -> ID id
              | x -> unhandled stderr 78 x; EMPTY) (List.rev !iolstref)),
            THASH thash2)

let gen_miter_arch arch1 nam1 arch2 nam2 =
  if nam1 = nam2 then failwith "mitered circuits must have different names";
  Semantics.prescan stderr !Globals.archenv (generate_miter_netlist arch1 nam1 arch2 nam2) "Generated by gen_miter_arch";
  Printf.printf "Module report %s\n" (Semantics.endscan())
