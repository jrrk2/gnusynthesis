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

(* let _ = Printexc.print Vparse.parse Sys.argv.( Array.length Sys.argv - 1 );; -- Pass the last command line module *)

(* let m k x = Printf.printf "%s\n" k in Hashtbl.iter m Globals.modprims;;  -- Print all modules *)

(* Hashtbl.find Globals.modprims "test";; -- Find module "test" (displays result) *)

let dump_text out_chan m = let buf = Buffer.create 64 in
  Dump.dump buf (Hashtbl.find Globals.modprims m).Globals.tree 0; Buffer.output_buffer out_chan buf;; (* dump module as text*)

(* Hashtbl.iter (fun k x -> Printf.printf "%s\n" k) Globals.modprims;; *)

(* List.iter (fun x -> Dump.dump (x, 0)) arg;; *)

(* Hashtbl.iter (fun k x -> Printf.printf "%s\n" k) (Hashtbl.find Globals.modprims "test").symbols;; -- dump symbol names *)

let vparser args =
  let rest = Globals.mygetenv "RESTORE_LIBRARY" in
  if rest <> "" then Read_library.restore_lib rest;
  let psuccess = ref true in
  for i = 1 to ( Array.length args - 1 ) do let ix = String.rindex args.(i) '.' in
    match String.sub args.(i) ix (String.length args.(i) - ix) with
    | ".vhdl"
    | ".vhd" -> (*Printexc.print*) VhdlMain.main psuccess [args.(i)]
    | ".v"
    | ".vm"
    | ".xst"
    | ".v2a"
    | ".flat"
    | ".hana"
    | ".odin"
    | ".timesim"
    | ".synplify"
    | ".verilog" -> (*Printexc.print*) psuccess := !psuccess && Vparse.parse args.(i)
    | _ -> Printf.fprintf stderr "file %s extension not recognised - Verilog assumed\n" args.(i);
       psuccess := !psuccess && Vparse.parse args.(i)
   done;
  if (!psuccess == false) then
    Printf.printf "Not continuing due to parse errors\n";
  let report = Semantics.endscan() in
  ignore(report);
  if (Array.length args < 2) then
    Printf.printf "Usage %s verilog_file(s)\n" args.(0);

exception MyException of string * int (* exceptions can carry a value *);;

let _ =
  Printexc.record_backtrace true;
  vparser Sys.argv;
  let biggest = Count.vhier true in
  print_endline biggest;
  exit (match String.length biggest with 0 -> 1 | _ -> 0)
