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

val startup : string array -> unit
(*
val ntkh : (string, Abclib.abc_Ntk_t * Abclib.lathash_t) Hashtbl.t
*)
val i_am_interactive : unit -> bool
val main : bool ref -> string array -> unit
(*
val networks : (string, Abclib.abc_Ntk_t * Abclib.lathash_t) Hashtbl.t -> unit
val main_mapper : 'a -> 'b -> int ref -> string array -> unit
val main_struct_hash : 'a -> 'b -> int ref -> string array -> unit
val main_unmapper : 'a -> 'b -> int ref -> string array -> unit
val main_read_hls_lib : 'a -> 'b -> int ref -> string array -> unit
*)
val main_mods : 'a -> 'b -> 'c -> 'd -> unit
val main_info : 'a -> 'b -> 'c -> 'd -> unit
val main_library_env : 'a -> 'b -> int ref -> string array -> unit
val main_write_xml_arch : 'a -> 'b -> int ref -> string array -> unit
val main_write_edif_arch : 'a -> 'b -> int ref -> string array -> unit
val main_gen_hls_arch : 'a -> 'b -> int ref -> string array -> unit
val main_write_verilog_arch : 'a -> 'b -> int ref -> string array -> unit
val main_write_decision : 'a -> 'b -> int ref -> string array -> unit
val main_discard : 'a -> 'b -> int ref -> string array -> unit
val main_find_submod : 'a -> 'b -> int ref -> string array -> unit
val main_count_arch : 'a -> 'b -> int ref -> string array -> unit
val main_xtr_count_arch : 'a -> 'b -> int ref -> string array -> unit
val main_gen_tolerant_arch : 'a -> 'b -> int ref -> string array -> unit
val main_gen_unmapped_arch : 'a -> 'b -> int ref -> string array -> unit
val main_gen_sop_arch : 'a -> 'b -> int ref -> string array -> unit
val main_gen_flat_arch : 'a -> 'b -> int ref -> string array -> unit
val main_gen_minimap_arch : 'a -> 'b -> int ref -> string array -> unit
val main_vhdparse : 'a -> int -> int ref -> string array -> unit
val main_vparse : 'a -> int -> int ref -> string array -> unit
val main_echo : 'a -> int -> int ref -> string array -> unit
val main_verbose : 'a -> int -> int ref -> string array -> unit
(*
val main_flag : 'a -> int -> int ref -> string array -> unit
*)
val main_rm : 'a -> 'b -> int ref -> string array -> unit
val main_scr : bool ref -> 'a -> int ref -> string array -> unit
(*
val main_compare : 'a -> 'b -> int ref -> string array -> unit
val main_io_read_aig : 'a -> 'b -> int ref -> string array -> unit
val main_io_read_edif : 'a -> 'b -> int ref -> string array -> unit
val main_io_write_verilog : 'a -> 'b -> int ref -> string array -> unit
val main_io_write_blif : 'a -> 'b -> int ref -> string array -> unit
val main_io_write_aig : 'a -> 'b -> int ref -> string array -> unit
*)
val main_read_blif : 'a -> 'b -> int ref -> string array -> unit
val main_read_lib : 'a -> 'b -> 'c -> 'd -> unit
val main_dump_lib : 'a -> 'b -> int ref -> string array -> unit
val main_restore_lib : 'a -> 'b -> int ref -> string array -> unit
val main_scan_lib : 'a -> 'b -> int ref -> string array -> unit
val main_blif_clk_env : 'a -> 'b -> int ref -> string array -> unit
val main_unresolved : 'a -> 'b -> int ref -> string array -> unit
val main_default_arch_env : 'a -> 'b -> int ref -> string array -> unit
val main_modsuffix_env : 'a -> int -> int ref -> string array -> unit
val main_bye : bool ref -> 'a -> int ref -> 'b -> unit
