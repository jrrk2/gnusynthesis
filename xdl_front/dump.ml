(*
    <vscr - Verilog converter to abc format.>
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

open Printf
open Xdl_parser
open List

type uptr = UPTR of (out_channel -> int -> Xdl_parser.token -> unit) | UNIL;;

let rec dump (ch:Buffer.t) exp indent = match exp with
| TLIST lst -> List.iter (fun x -> dump ch x (indent+2)) lst
| STRING c -> bprintf ch "%s " c
| EMPTY -> bprintf ch " "
| ID str -> bprintf ch "%s " str
| ILLEGAL c -> bprintf ch "ILLEGAL character %c " c
| _ -> bprintf ch "%s " (Ord.getstr exp)

let dumpstr tok = let buf = Buffer.create 64 in dump buf tok 0; Buffer.contents buf

let rec dumpstruct' (ch:Buffer.t) = function
| TLIST lst -> bprintf ch "(List length %d) " (List.length lst)
| STRING c -> bprintf ch "(string) "
| EMPTY -> bprintf ch "(empty) "
| ID str -> bprintf ch "(id %s) " str
| ILLEGAL c -> bprintf ch "(illegal %c) " c
| oth -> bprintf ch "(token %s) " (Ord.getstr oth)

let dumpstruct tok = let buf = Buffer.create 64 in dumpstruct' buf tok; Buffer.contents buf

let (unhand_list) = ref [];;

let unhandled_dflt out_chan ln argt =
  let arg = (ln,argt) in
  if (List.mem arg !unhand_list == false) then
    begin
      unhand_list := arg :: !unhand_list;
      let buf = Buffer.create 64 in
      Printf.bprintf buf "\n**** Unhandled %d at line %d ****\n" (List.length !unhand_list) ln;
      dump buf argt 0;
      Printf.bprintf buf "\n";
      if false then
        Buffer.output_buffer out_chan buf
      else
        failwith (Buffer.contents buf)
    end

let unhandled_ptr = ref (UPTR unhandled_dflt);;

let unhandled out_chan ln arg = match !unhandled_ptr with UPTR fn -> fn out_chan ln arg | UNIL -> ();;
