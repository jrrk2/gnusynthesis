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
  
let extract_subcell width iolstref syms thash2 kind arg3 iolst =
  Hashtbl.add (snd thash2)
    (QUADRUPLE(MODINST, ID kind, EMPTY, TLIST (List.map (function
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

let extract_subcell' iolstref syms thash2 kind arg3 nam top =
  match top.tree with
    | QUINTUPLE(MODULE, _, _, TLIST iolst, _) ->
        extract_subcell (width' top.symbols) iolstref syms thash2 kind arg3 iolst
    | err -> unhandled stderr 31 err

let extract debug_mode arch thash2 syms iolstref = function
| (TRIPLE(ASSIGN, arg1, TLIST arg2)) as exp ->
    (match arg1 with 
      | DOUBLE (HASH, TLIST [INT n]) -> Printf.fprintf stderr "/* #%d Delay ignored */\n" n
      | EMPTY -> ()
      | oth -> unhandled stderr 32 oth);
    Hashtbl.add (snd thash2) exp ();
| QUADRUPLE((WIRE|TRI0|TRI1) as tok, arg0, TRIPLE(arg1, srng, arg3), TLIST arg4) ->
  let intrng = Minimap.flatten_width srng syms in
  Hashtbl.replace (fst thash2) (QUADRUPLE(tok, arg0, TRIPLE(arg1, intrng, arg3),
            TLIST (List.map (fun x -> (match x with
              | DOUBLE (ID id, EMPTY) -> DOUBLE (ID id, EMPTY)
              | _ -> unhandled stderr 39 x; EMPTY)) arg4))) ()
| QUADRUPLE(MODINST, ID kind, _, TLIST arg3) ->
  if Hashtbl.mem modprims kind.id then
    begin
      Minimap.select_sub false (extract_subcell' iolstref syms thash2 kind arg3) !archenv kind.id
    end
  else if Hashtbl.mem libhash kind.id then
    begin
      let libcell = Hashtbl.find libhash kind.id in
      extract_subcell (fun _ -> SCALAR) iolstref syms thash2 kind arg3 libcell.iolst
    end
  else
    failwith ("sub-module "^kind.id^" not found")
| QUINTUPLE((INPUT|OUTPUT) as tok, EMPTY, EMPTY, srng, TLIST arg4) ->
  let intrng = Minimap.flatten_width srng syms in
  let lst = TLIST (List.map (function
    | TRIPLE (ID id, EMPTY, EMPTY) -> TRIPLE(ID id, EMPTY, EMPTY)
    | x -> unhandled stderr 51 x; EMPTY) arg4) in
  Hashtbl.replace (fst thash2) (QUINTUPLE(tok, EMPTY, EMPTY, intrng, lst)) ()
| QUADRUPLE(REG, EMPTY, srng, TLIST arg4) ->
  let intrng = Minimap.flatten_width srng syms in
  Hashtbl.replace (fst thash2) (QUADRUPLE(REG, EMPTY, intrng, TLIST arg4)) ();
  List.iter (function
    | TRIPLE (ID id', EMPTY, EMPTY) ->
        let id = enterid (id'.id^"$reg") in
        if not (debug_mode || List.mem (ID id) !iolstref) then
          begin
            Hashtbl.replace (fst thash2)
              (QUINTUPLE(OUTPUT, EMPTY, EMPTY, intrng, TLIST [(TRIPLE(ID id, EMPTY, EMPTY))])) ();
            iolstref := ID id :: !iolstref;
            Hashtbl.replace (snd thash2)
              (TRIPLE(ASSIGN, EMPTY, TLIST [TRIPLE(ASSIGNMENT, ID id, ID id')])) ();
          end
    | x -> unhandled stderr 61 x) arg4;
| DOUBLE((ALWAYS|INITIAL|FINAL), _) as exp -> Hashtbl.add (snd thash2) exp ();
| exp -> unhandled stderr 63 exp
    
let extract' debug_mode arch thash2 syms ioref decls insts =
        Hashtbl.iter (fun x _ -> extract debug_mode arch thash2 syms ioref x) decls;
        Hashtbl.iter (fun x _ -> extract debug_mode arch thash2 syms ioref x) insts

let generate_extract_netlist debug_mode k =
  let thash2 = (Hashtbl.create 256,Hashtbl.create 256) in
  match k.tree with
    | QUINTUPLE(MODULE, ID arg1, arg2, TLIST arg3, THASH (decls,insts)) ->
        let iolstref = ref (List.rev arg3) in
        extract' debug_mode k.arch thash2 k.symbols iolstref decls insts;
        QUINTUPLE(MODULE, ID (enterid (arg1.id^ !Globals.modsuffix)), arg2,
                TLIST (List.map (function
                  | ID id -> ID id
                  | x -> unhandled stderr 78 x; EMPTY) (List.rev !iolstref)),
                THASH thash2)
    | _ -> failwith (Dump.dumpstr k.tree^" is not a module netlist")

let gen_extract_arch' debug arch nam =
  let lst = Count.find_arch arch nam in
  match lst with
    | arg::[] -> assert (arg.arch = arch);
        Minimap.recurse_arch (fun id subarg -> assert (subarg.arch = arch);
          Semantics.prescan stderr !Globals.archenv (generate_extract_netlist debug subarg) "Generated by gen_extract_arch") arch nam arg;
          Printf.printf "Module report %s\n" (Semantics.endscan())
    | [] -> failwith "nothing selected in gen_extract_arch"
    | _ -> failwith "ambiguous selection in gen_extract_arch"

let gen_explicit_arch arch nam = gen_extract_arch' true arch nam
let gen_extract_arch arch nam = gen_extract_arch' false arch nam

