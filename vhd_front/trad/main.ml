(***********************************************************************)
(*                                                                     *)
(*                     Objective Caml Extension                        *)
(*                                                                     *)
(*                 Jonathan Richard Robert Kimmitt                     *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Conversion
open Rewrite
open Ctypes
open Printf

let rec qsort = function
   | [] -> []
   | pivot :: rest ->
       let is_less x = x < pivot in
       let left, right = List.partition is_less rest in
       qsort left @ [pivot] @ qsort right

let sort1 = ref []
let sort2 = ref []

let license fd = 
fprintf fd "(*\n";
fprintf fd "    <vscr - Verilog converter to abc format.>\n";
fprintf fd "    Copyright (C) <2011,2012>  <Jonathan Richard Robert Kimmitt>\n";
fprintf fd "\n";
fprintf fd "    This program is free software: you can redistribute it and/or modify\n";
fprintf fd "    it under the terms of the GNU General Public License as published by\n";
fprintf fd "    the Free Software Foundation, either version 3 of the License, or\n";
fprintf fd "    (at your option) any later version.\n";
fprintf fd "\n";
fprintf fd "    This program is distributed in the hope that it will be useful,\n";
fprintf fd "    but WITHOUT ANY WARRANTY; without even the implied warranty of\n";
fprintf fd "    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n";
fprintf fd "    GNU General Public License for more details.\n";
fprintf fd "\n";
fprintf fd "    You should have received a copy of the GNU General Public License\n";
fprintf fd "    along with this program.  If not, see <http://www.gnu.org/licenses/>.\n";
fprintf fd "*)\n";
fprintf fd "\n"

let main infile outfile outfile' asciif = (
let fd = open_in infile in
let tree = Parser0.use_file Lexer.token (Lexing.from_channel fd) in
let rslt = List.map top_phrase tree in
let tylst = (match List.hd rslt with 
  | CTOPDEF [PSTRTYPE lst] -> lst
  | _ -> []) in
let rwlst = List.map rewrite tylst in
let fd = open_out outfile in
license fd;
fprintf fd "open VhdlTypes\n";
fprintf fd "\ntype vhdintf = \n";
fprintf fd "  | VhdNone\n";
fprintf fd "  | Str of string\n";
fprintf fd "  | Char of char\n";
fprintf fd "  | Real of float\n";
fprintf fd "  | Num of Big_int.big_int\n";
fprintf fd "  | List of vhdintf list\n";
fprintf fd "  | Double of vhdintf * vhdintf\n";
fprintf fd "  | Triple of vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Quadruple of vhdintf * vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Quintuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Sextuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Septuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Octuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Nonuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Decuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Undecuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Duodecuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Tredecuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Quattuordecuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Quindecuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Sexdecuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Septendecuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Octodecuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Novemdecuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Vigenuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Unvigenuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Duovigenuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Trevigenuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Quattuorvigenuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf\n";
fprintf fd "  | Quinvigenuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf\n";  
sort1 := qsort !newconslst;
List.iter (fun nam -> fprintf fd "  | %s\n" nam) !sort1;
fprintf fd "\n\nlet rec dump_vhd_int x = Str (string_of_int x)\n";
fprintf fd "        and dump_vhd_bool x = Str (string_of_bool x)\n";
fprintf fd "        and dump_vhd_string x = Str x\n";
let cnt = ref 0 in List.iter (fun itm -> fprintf fd "(*%d*) " !cnt; incr cnt; output_string fd itm) rwlst;
close_out fd;
sort2 := qsort !buflst;
let fd' = open_out outfile' in
List.iter (output_string fd') !sort2;
List.iter (fun nam -> fprintf fd' "  | %s -> EMPTY\n" nam) !sort1;
close_out fd';
let asc = open_out asciif in
license asc;
fprintf asc "open VhdlTree\n";
fprintf asc "let asctoken = function\n";
List.iter (fun nam -> fprintf asc "  | %s -> \"%s\"\n" nam nam) !sort1;
fprintf asc "  | Quinvigenuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Quattuorvigenuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Trevigenuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Duovigenuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Unvigenuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Vigenuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Novemdecuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Octodecuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Septendecuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Sexdecuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Quindecuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Quattuordecuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Tredecuple(_, _, _, _, _, _, _, _, _, _, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Duodecuple(_, _, _, _, _, _, _, _, _, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Undecuple(_, _, _, _, _, _, _, _, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Decuple(_, _, _, _, _, _, _, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Nonuple(_, _, _, _, _, _, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Octuple (_, _, _, _, _, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Septuple (_, _, _, _, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Sextuple (_, _, _, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Quintuple (_, _, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Quadruple (_, _, _, _) -> \"unknown\"\n";
fprintf asc "  | Triple (_, _, _) -> \"unknown\"\n";
fprintf asc "  | Double (_, _) -> \"unknown\"\n";
fprintf asc "  | List _ -> \"unknown\"\n";
fprintf asc "  | Char _ -> \"unknown\"\n";
fprintf asc "  | Real _ -> \"unknown\"\n";
fprintf asc "  | Num _ -> \"unknown\"\n";
fprintf asc "  | Str _ -> \"unknown\"\n";
fprintf asc "  | VhdNone -> \"unknown\"\n";
close_out asc );;

let _ = if Array.length Sys.argv = 5 then
    main Sys.argv.(1)  Sys.argv.(2)  Sys.argv.(3)  Sys.argv.(4) 
