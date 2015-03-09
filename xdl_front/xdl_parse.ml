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

open Setup
open Xdl_lexer
open Xdl_parser

exception End_of_File

let str_token = function
  | IDENTIFIER str -> str
  | oth -> Ord.getstr oth

let parse arg =
  let inchan = open_in arg in
  let lexbuf = Lexing.from_channel inchan in
  try 
    start Xdl_lexer.token lexbuf;
  with e ->
    for i = 0 to hsiz-1 do let idx = (hsiz-i+(!histcnt))mod hsiz in
                     let item = (history.(idx)) in (match item.tok with
		       | EMPTY -> ()
                       | IDENTIFIER id -> Printf.printf "Backtrace %d : IDENTIFIER %s (%d-%d)\n" i id item.strt item.stop
		       | oth -> Printf.printf "Backtrace %d : %s (%d-%d)\n" i (str_token oth) item.strt item.stop) done;
  []
