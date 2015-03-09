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

let vparser args =
  let rest = Globals.mygetenv "RESTORE_LIBRARY" in
  if rest <> "" then Read_library.restore_lib rest;
  let psuccess = ref true in
  for i = 1 to ( Array.length args - 1 ) do
    psuccess := !psuccess & Vparse.parse args.(i)
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
  Globals.archenv := "flat";
  Flatten.gen_flat_arch "verilog" biggest;
  Verilogout.write_verilog_arch !archenv biggest
