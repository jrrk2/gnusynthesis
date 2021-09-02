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

open Mapselect
  
val tbl_init : Mapselect.hls_t -> tbl
val hls_select : Mapselect.mapselect
val synthdecl : Vparser.shash -> tbl -> Vparser.token -> unit
val synthbody : Vparser.shash -> tbl -> Vparser.token -> unit
val synthused : tbl -> Vparser.shash -> unit
val synth_module :
    Mapselect.hls_t ->
  Vparser.shash ->
  Idhash.idhash ->
  Vparser.token list ->
  Vparser.token list ->
  (Vparser.token, 'a) Hashtbl.t ->
  (Vparser.token, 'b) Hashtbl.t -> Vparser.token    
val gen_hls_arch : string -> string -> unit
val gen_struct_arch : string -> string -> unit
val gen_map_arch : string -> string -> unit
val verbose : bool ref

val debug_synth: Vparser.token option ref

val evoth : Vparser.token option ref
val evoth'  : ( ( Vparser.token option * Vparser.token * Vparser.token * Vparser.token) ) option ref
val evoth'' : ( ( Vparser.token option * Vparser.token * Vparser.token * Vparser.token) *
                ( Vparser.token option * Vparser.token * Vparser.token * Vparser.token) ) option ref
val evdbg : (Vparser.token * Vparser.token * Vparser.token * Vparser.token *
             Vparser.token option * Vparser.token * Vparser.token *
             Vparser.token * Vparser.token)
           option ref

val always_ev :
           Vparser.token ->
           Vparser.token * Vparser.token * Vparser.token * Vparser.token *
           Vparser.token option * Vparser.token * Vparser.token *
           Vparser.token
val always_body :
           Vparser.token -> Vparser.token * Vparser.token * Vparser.token
val always_body1 :
           Vparser.token ->
           Vparser.token option * Vparser.token * Vparser.token *
           Vparser.token

val dvlhash: (string, Vparser.token list) Hashtbl.t

