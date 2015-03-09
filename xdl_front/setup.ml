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

open Xdl_parser;;
open Ord;;

type logt = Closed | Open of out_channel

  let hsiz = 64;;
  type hist = {tok:token;strt:int;stop:int;key:bool};;
  let hist_init () = {tok=EMPTY;strt=0;stop=0;key=false};;
  let histcnt = ref 0;;
  let history = Array.init hsiz (fun i -> hist_init())

type maincmd_t = (string, ((bool ref -> int -> int ref -> string array -> unit) * string * int)) Hashtbl.t

let (main_cmds:maincmd_t) = Hashtbl.create 256

let failwith_ptr = ref (fun (msg:string) -> ())

let myfailwith (msg:string) = print_endline msg; !failwith_ptr msg
