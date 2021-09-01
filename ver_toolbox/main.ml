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

let oldtemps = ref 0.0
let verbose = ref false
let backtrace = ref false
let filt_verbose = ref false
let debug_edif = ref Vparser.EMPTY
let vhditm = ref []

type mainhash_t = (string * string, string * string * Globals.modtree) Hashtbl.t

let (ntkh:mainhash_t) = Hashtbl.create 256

let exec_cmd quit atok =
          let cnt = ref 0 and len = Array.length atok in
          if atok <> [|""|] && atok.(0).[0] <> '#' then while !cnt < len do
              let cmd = atok.(!cnt) in
              if Hashtbl.mem Setup.main_cmds cmd then
                begin
                  let (key,help,args) = Hashtbl.find Setup.main_cmds atok.(!cnt) in
                  if (len - !cnt >= args) then
                    begin
                      cnt := !cnt + args;
                      key quit len cnt atok;
                      flush_all();
                    end
                  else
                    begin
                      cnt := len;
                      Printf.printf "%s - insufficient parameters (minimum %d required)\n" cmd args
                    end
                end
              else
                begin
		  failwith (cmd^": - invalid command")
                end
            done

let remove file =
  (try Unix.unlink file with Unix.Unix_error(n,cmd,fil) -> if !verbose then Printf.printf "%s: %s error\n" fil cmd)

let hls_CmdCommandDispatch args =
  let lst1 = ref [] in Hashtbl.iter (fun (ar,nam) (x,y,z) ->
    let prefix = "blif" and suffix = ".blif" in
    let (filnam,oc) = Filename.open_temp_file prefix suffix in
    output_string oc (Write_blif.dump_blif_netlist' ("dumped for hls "^ar^":"^nam) z);
    lst1 := filnam :: !lst1;
    close_out oc;
    Printf.printf "Key %s, arch %s, nam %s, file %s\n" nam ar x filnam) ntkh;
    let prefix = "hls" and suffix = ".scr" in
    let (scrnam,oc) = Filename.open_temp_file prefix suffix in
    Printf.fprintf oc "read_library ../../v2hls/test/xilinx.genlib\n";
    Printf.fprintf oc "read_blif %s\n" (List.hd !lst1);
    Printf.fprintf oc "cec %s\n" (List.nth !lst1 1);
    Printf.fprintf oc "quit\n";
    Printf.fprintf oc "  \n";
    close_out oc;
    let cmd = Printf.sprintf "%s -f %s" (Sys.getenv "ABC") scrnam in
    Printf.printf "Command: %s\n" cmd;
    flush stdout;
    let rslt = Sys.command cmd in
    if rslt <> 0 then failwith (cmd^": returned error "^string_of_int rslt);
    List.iter remove (scrnam :: !lst1)

let hls_fn ar nam expt =
  let rlst = ref [] in
  Minimap.recurse_arch' (fun key itm -> rlst := (ar,key,itm) :: !rlst) ar expt;
  let rlst' = List.rev ((ar, nam, expt) :: !rlst) in
  List.iter (Hashtbl.add ntkh (ar,nam)) rlst'

let filter_hls fn arch =
  let find1 = ref [] in
  let got_top = String.contains arch ':' in
  let (ar,top) = if got_top then
      let idx = String.index arch ':' in
      (String.sub arch 0 idx,String.sub arch (idx+1) (String.length arch - idx - 1))
    else
      (arch,"") in
  Hashtbl.iter (fun k x ->
    if x.Globals.is_netlist && x.Globals.arch = ar && (k=top || not got_top)
    then find1 := (k,x) :: !find1) Globals.modprims;
  match !find1 with
    | [] -> failwith ("create "^ar^" "^top^" returned no modules")
    | (nam,y)::[] ->
      if not y.Globals.is_top then print_endline (ar^":"^nam^" is not the top of the hierarchy\n");
        if y.Globals.is_hier then
          begin
            let tmparch = "Auto-flattened" in
            if !filt_verbose then Verilogout.write_verilog_arch' ar nam (nam^".filter") y;
            print_endline (ar^":"^nam^" has hierarchy\n");
            let tree = Flatten.generate_flat_netlist y in
            let newsyms = Semantics.shash_create tmparch nam Vparser.EndShash 256 in
            let expt = Semantics.init_tree [] nam tree newsyms tmparch in
            if !filt_verbose then
              begin
                let file = nam^".autoflat" in
                let oc = Globals.unique_open file in
                output_string oc (Verilogout.dump_flat_netlist None expt.Globals.symbols (file) expt.Globals.tree);
                close_out oc
              end;
            Semantics.scan' stderr nam expt;
            flush_all();
          end
        else
          fn ar nam y
    | _ ->  failwith ("create "^ar^" "^top^" matched multiple modules: "^String.concat "," (List.map (fun (k,_) -> k) !find1))

let rec main quit args =
  Array.iteri (fun ix itm -> if ix > 0 && (itm="-" || String.sub itm (String.length itm-4) 4 = ".scr") then
      let ic = if itm = "-" then stdin else Vparse.my_openin itm in
      try
        while not !quit do
          if itm = "-" then print_string "prompt> ";
          flush_all();
          let toks = input_line ic and pos = ref 0 and toklst = ref [] in
          if String.length toks > 0 && toks.[0] <> '#' then print_string ("**** "^toks^" ****\n");
          flush_all();
          while String.contains_from toks !pos ' ' do
            let delim = String.index_from toks !pos ' ' in
            while !pos < delim && toks.[!pos] = ' ' do incr pos done;
            if  !pos < delim then toklst := !toklst @ [String.sub toks !pos (delim - !pos)];
            pos := delim+1;
          done;
          let atok = Array.of_list (!toklst @ [String.sub toks !pos (String.length toks - !pos)]) in
	  exec_cmd quit atok
        done
      with
        | Sys_error e -> Printf.eprintf "System error : %s" (e);
        | End_of_file -> if ic <> stdin then close_in ic else quit := true
        | Not_found -> Printf.eprintf "Not found exception";
          (*
            | e ->  (* catch all exceptions *)
            Printf.eprintf "Unexpected exception : %s" (Printexc.to_string e);
          *)
          (*If using Ocaml >= 3.11, it is possible to also print a backtrace: *)
          Printexc.print_backtrace stderr) args

and main_mods quit len cnt atok =
  Read_library.mods()

and main_info quit len cnt atok =
  let lst = ref [] in
  Hashtbl.iter (fun k (key,(help:string),args) ->
    if k <> String.sub help 0 (String.length k) then
      lst := Printf.sprintf "%s - %s" k help :: !lst
    else
      lst := help :: !lst
  ) Setup.main_cmds;
  List.iter print_endline (List.sort compare !lst)

and main_license quit len cnt atok =
  License.license atok.(!cnt-1)
(* 
and main_library_env quit len cnt atok =
  Read_library.env := atok.(!cnt-1)
and main_write_xml_arch quit len cnt atok =
  Xmlout.write_xml_arch atok.(!cnt-2) atok.(!cnt-1)
*)
and main_write_smv_arch quit len cnt atok =
  Smvout.write_smv_arch atok.(!cnt-2) atok.(!cnt-1)

and main_write_edif_arch quit len cnt atok =
  Edifout.write_edif_arch atok.(!cnt-2) atok.(!cnt-1)

and main_gen_hls_arch quit len cnt atok =
  Globals.fatal := true;
  Hls.gen_hls_arch atok.(!cnt-2) atok.(!cnt-1)

and main_gen_struct_arch quit len cnt atok =
  Globals.fatal := true;
  Hls.gen_struct_arch atok.(!cnt-2) atok.(!cnt-1)

and main_gen_map_arch quit len cnt atok =
  Globals.fatal := true;
  Hls.gen_map_arch atok.(!cnt-2) atok.(!cnt-1)

and main_gen_rtl2rtl_arch quit len cnt atok =
  Rtl2rtl.gen_rtl2rtl_arch atok.(!cnt-2) atok.(!cnt-1)

and main_gen_elaborate_arch quit len cnt atok =
  Elaborate.gen_elaborate_arch atok.(!cnt-2) atok.(!cnt-1)

and main_write_verilog_all quit len cnt atok =
  Verilogout.write_verilog_all atok.(!cnt-1)

and main_write_verilog_arch quit len cnt atok =
  Verilogout.write_verilog_arch atok.(!cnt-2) atok.(!cnt-1)

and main_write_verilog_arch_nohier quit len cnt atok =
  Verilogout.write_verilog_arch_nohier atok.(!cnt-2) atok.(!cnt-1)

and main_write_blif_arch quit len cnt atok =
  Write_blif.write_blif_arch atok.(!cnt-2) atok.(!cnt-1)

and main_write_aiger_arch quit len cnt atok =
  Aigerout.write_aiger_arch atok.(!cnt-2) atok.(!cnt-1)

and main_write_milef_arch quit len cnt atok =
  Milefout.write_milef_arch atok.(!cnt-2) atok.(!cnt-1)

and main_write_decision quit len cnt atok =
  let key = (*Vparser.enterid*) atok.(!cnt-1) in
  let fil = atok.(!cnt-1)^".dot" in
  Decision.print_to_dot (snd(Decision.bdd_of_formula (Hashtbl.find Globals.libhash key).Globals.prop)) fil;
  (match Unix.system ("dotty "^fil) with
    | Unix.WEXITED status -> Printf.printf "Exit code was %d\n" status
    | _ -> Printf.printf "Exit code unknown\n")

and main_discard quit len cnt atok =
  let nam = (*Vparser.enterid*) atok.(!cnt-1) in
  let find1 = Hashtbl.find_all Globals.modprims nam in
  List.iter (fun x -> Printf.printf "Discarding %s arch %s\n" nam x.Globals.arch; Hashtbl.remove Globals.modprims nam) find1

and main_find_submod quit len cnt atok =
  Minimap.find_submod atok.(!cnt-2) atok.(!cnt-1)

and main_erc_arch quit len cnt atok =
  Erc.erc_arch atok.(!cnt-2) atok.(!cnt-1)

and main_count_arch quit len cnt atok =
  Count.count_arch atok.(!cnt-2) atok.(!cnt-1)

and main_xtr_count_arch quit len cnt atok =
  let xtrlst = Count.xtr_count_arch atok.(!cnt-2) atok.(!cnt-1) in
  Printf.printf "Name\tarch\txtrs\n";
  List.iter (fun xcnt -> Printf.printf "%s\t%s\t%d\n" atok.(!cnt-2) atok.(!cnt-1) xcnt) xtrlst

and main_gen_tolerant_arch quit len cnt atok =
  Tolerate.gen_tolerant_arch atok.(!cnt-2) atok.(!cnt-1)

and main_gen_cnf_arch quit len cnt atok =
  Gencnf.gen_cnf_arch atok.(!cnt-2) atok.(!cnt-1)

and main_gen_sop_arch quit len cnt atok =
  Sop.gen_sop_arch atok.(!cnt-2) atok.(!cnt-1)

and main_gen_unmapped_arch quit len cnt atok =
  Sop.gen_unmapped_arch atok.(!cnt-2) atok.(!cnt-1)

and main_gen_flat_arch quit len cnt atok =
  Flatten.gen_flat_arch atok.(!cnt-2) atok.(!cnt-1)

and main_gen_minimap_arch quit len cnt atok =
  Minimap.minimap_arch atok.(!cnt-2) atok.(!cnt-1)
 
and main_gen_explicit_arch quit len cnt atok =
  Extractregs.gen_explicit_arch atok.(!cnt-2) atok.(!cnt-1)
 
and main_gen_extract_arch quit len cnt atok =
  Extractregs.gen_extract_arch atok.(!cnt-2) atok.(!cnt-1)
 
and main_gen_miter_arch quit len cnt atok =
  Miter.gen_miter_arch atok.(!cnt-4) atok.(!cnt-3) atok.(!cnt-2) atok.(!cnt-1)
 
and main_gen_smv_main_arch quit len cnt atok =
  Smvout.gen_smv_main_arch atok.(!cnt-2) atok.(!cnt-1)

and main_vhdparse quit len cnt atok =
  while !cnt < len-1
  do incr cnt;
    let ch = open_in atok.(!cnt) in
    vhditm := VhdlMain.parse_vhdl_channel ch true :: !vhditm;
    close_in ch;
    done; incr cnt

and main_vparse quit len cnt atok =
  let psuccess = ref true in
  while !cnt < len do
    print_endline ("vparse "^atok.(!cnt));
    psuccess := Vparse.parse atok.(!cnt);
  incr cnt;
  done;
  if (!psuccess == false) then
    (Printf.printf "Not continuing due to parse errors\n"; cnt := len);
  Printf.printf "Module report %s\n" (Semantics.endscan())

and main_read_aiger quit len cnt atok =
  Aigerin.read_aiger atok.(!cnt-1);
  Printf.printf "Module report %s\n" (Semantics.endscan())

and main_read_blif quit len cnt atok =
  Read_blif.read_blif atok.(!cnt-1);
  Printf.printf "Module report %s\n" (Semantics.endscan())

and main_echo quit len cnt atok =
  while !cnt < len do print_string (atok.(!cnt)^" "); incr cnt; done; print_newline()

and main_verbose quit len cnt atok =
  while !cnt < len do (match atok.(!cnt) with
    | "main" -> verbose := true
    | "hls" -> Hls.verbose := true
    | "mapping" -> Mapping.verbose := true
    | "filter" -> filt_verbose := true
    | "flatten" -> Flatten.verbose := true
    | "library" -> Read_library.verbose := true
    | "minimap" -> Minimap.verbose := true
    | "aigerin" -> Aigerin.verbose := true
    | "backtrace" -> backtrace := true; Printexc.record_backtrace true
    | "semantics" -> Semantics.verbose := true
    | "verilogout" -> Verilogout.verbose := true
    | _ -> failwith "bad option to verbose"
  ); incr cnt; done

and main_rm quit len cnt atok = remove atok.(!cnt-1)

and main_scr quit len cnt atok =
  main quit (Array.of_list(["";atok.(!cnt-1)]))

and main_compare quit len cnt atok =
  filter_hls hls_fn atok.(!cnt-2);
  filter_hls hls_fn atok.(!cnt-1);
  hls_CmdCommandDispatch [|"cec";atok.(!cnt-2);atok.(!cnt-1)|];
  flush_all()

and main_read_lib quit len cnt atok =
  Read_library.read_lib atok.(!cnt-1);
  Dump.unhand_list := [];
  Globals.fatal := true

and main_dump_lib quit len cnt atok =
  Read_library.dump_lib atok.(!cnt-1);
  Globals.fatal := true

and main_restore_lib quit len cnt atok =
  Read_library.restore_lib atok.(!cnt-1);
  Globals.fatal := true

and main_scan_lib quit len cnt atok =
  Read_library.scan_lib atok.(!cnt-1)

and main_dump_module quit len cnt atok =
  Read_library.dump_module atok.(!cnt-3) atok.(!cnt-2) atok.(!cnt-1);
  Globals.fatal := true

and main_restore_module quit len cnt atok =
  Read_library.restore_module atok.(!cnt-1);
  Globals.fatal := true

and main_blif_clk_env quit len cnt atok =
  Read_blif.blifclkenv := Vparser.ID (Vparser.enterid atok.(!cnt-1));
  Aigerin.aigerclkenv := Vparser.ID (Vparser.enterid atok.(!cnt-1))

and main_unresolved quit len cnt atok =
  Semantics.unresolved_dir := atok.(!cnt-2);
  Semantics.unresolved_ext := atok.(!cnt-1)

and main_default_arch_env quit len cnt atok =
  Globals.archenv := atok.(!cnt-1)

and main_modsuffix_env quit len cnt atok =
  if len - !cnt >= 1 then
    begin
      incr cnt;
      Globals.modsuffix := atok.(!cnt-1)
    end
  else
    begin
      Globals.modsuffix := ""
    end

and main_bye quit len cnt atok =
  incr cnt;
  print_endline "Program terminated by explicit user command";
  quit := true

and main_time quit len cnt atok =
  incr cnt;
  let temps = Sys.time() in
  Printf.printf "System time = %6.4f, incremental time = %6.4f\n" temps (temps -. !oldtemps);
  oldtemps := temps

let _ = List.iter (fun (str,help,key,args) -> Hashtbl.replace Setup.main_cmds str (key,help,args))
[
                ( "blif_clk_env", "define global blif clock name", main_blif_clk_env, 2);
                ( "compare","combinational equivalence on two AIG netlists", main_compare, 3); 
                ( "count_arch","count library references in a design", main_count_arch, 3);
                ( "erc_arch","run electrical rule checks on a design", main_erc_arch, 3);
                ( "find_submod","enumerate submod references in a design", main_find_submod, 3);
                ( "default_arch_env", "define the default architecture",main_default_arch_env, 2);
                ( "mlecho", "echo arguments", main_echo, 1);
                ( "discard","discard a netlist", main_discard, 2);
                ( "gen_hls_arch", "synthesize RTL to hierarchical logic synthesis design",main_gen_hls_arch, 3);
                ( "gen_struct_arch", "synthesize RTL to structural design",main_gen_struct_arch, 3);
                ( "gen_map_arch", "synthesize RTL to mapped design",main_gen_map_arch, 3);
                ( "gen_cnf_arch", "convert design to conjunctive normal form",main_gen_cnf_arch, 3);
                ( "gen_explicit_arch", "convert subcell connections to explicit syntax",main_gen_explicit_arch, 3);
                ( "gen_extract_arch", "extract registers to cell port level",main_gen_extract_arch, 3);
                ( "gen_flat_arch", "flatten a structural design to cell level",main_gen_flat_arch, 3);
                ( "gen_minimap_arch", "convert equation design to netlist form",main_gen_minimap_arch, 3);
                ( "gen_miter_arch", "generate miter from two similar cells",main_gen_miter_arch, 5);
                ( "gen_smv_main_arch", "generate smv main program from module",main_gen_smv_main_arch, 5);
                ( "gen_tolerant_arch", "introduce fault tolerance into a design",main_gen_tolerant_arch, 3);
                ( "gen_unmapped_arch", "remove mapping from a design",main_gen_unmapped_arch, 3);
                ( "gen_rtl2rtl_arch", "convert rtl to a semantic simpler rtl if possible",main_gen_rtl2rtl_arch, 3);
                ( "gen_elaborate_arch", "elaborate parameterised library cells into unique modules",main_gen_elaborate_arch, 3);
                ( "gen_sop_arch", "convert design to sum of products form",main_gen_sop_arch, 3);
                ( "help", "show this help", main_info, 1);
                ( "info", "show this help", main_info, 1);
		(*
                 ( "library_env", "define path to library cells",main_library_env, 2);
		 *)
                ( "modsuffix_env", "define name suffix for generated modules", main_modsuffix_env, 1);
                ( "mods", "display available modules", main_mods, 1);
                ( "time", "time the program", main_time, 1);
                ( "bye", "leave the program", main_bye, 1);
                ( "read_aiger","invoke the ASCII aiger reader", main_read_aiger, 2); 
                ( "read_blif","invoke the blif reader", main_read_blif, 2); 
                ( "read_lib", "read library cells from a directory", main_read_lib, 2);
                ( "dump_lib", "dump library cells to a dumpfile", main_dump_lib, 2);
                ( "restore_lib", "restore library cells from a dumpfile", main_restore_lib, 2);
                ( "dump_module", "dump module(s) to a dumpfile", main_dump_module, 4);
                ( "restore_module", "restore module(s) from a dumpfile", main_restore_module, 2);
                ( "rm","rm - remove a file (prior to updating)", main_rm, 2);
                ( "scan_lib", "scan a directory for suitable library cells", main_scan_lib, 2);
                ( "scr","read commands from a file", main_scr, 2);
                ( "show", "show license", main_license, 2);
                ( "unresolved", "specify directory to read for unresolved macrocells", main_unresolved, 3);
                ( "verbose", "log detailed innards of commands", main_verbose, 1);
                ( "vhdparse","parse a subset of VHDL", main_vhdparse, 2);
                ( "vparse", "parse a synthesizable subset of Verilog", main_vparse, 1);
                ( "write_decision", "write a binary decision diagram as a dot file", main_write_decision, 2);
                ( "write_aiger_arch", "write an architecture to file in aiger format",main_write_aiger_arch, 3);
		( "write_blif_arch", "write an architecture to file in Berkely logic interchange format",main_write_blif_arch, 3);
                ( "write_edif_arch", "write a structural design as EDIF netlist", main_write_edif_arch, 3);
                ( "write_milef_arch", "write an architecture to file in milef format",main_write_milef_arch, 3);
                ( "write_smv_arch", "write a module as an SMV file",main_write_smv_arch, 3);
                ( "write_verilog_all", "write all modules to individual files in Verilog format",main_write_verilog_all, 2);
                ( "write_verilog_arch", "write an architecture to file in Verilog format",main_write_verilog_arch, 3);
                ( "write_verilog_arch_nohier", "write an architecture to file in Verilog format",main_write_verilog_arch_nohier, 3);
(*
                ( "write_xml_arch", "write a netlist as an XML file",main_write_xml_arch, 3);
*)
                ( "xtr_count_arch","estimate transistor count in a design", main_xtr_count_arch, 3);
]

let i_am_interactive () =
  Unix.isatty Unix.stdin && Unix.isatty Unix.stdout

let startup args =
  let quit = ref false in
  main quit args;
  if i_am_interactive() then
    begin
      if (Array.length Sys.argv = 1) then License.copyright();
      while not !quit do try main quit [|"";"-"|] with
        | Sys_error e -> Printf.printf "System error : %s\n\n" e;
        | oth -> Printf.printf "Unknown error : %s\n" (Printexc.to_string oth);
          flush_all();
      done
    end

let _ = startup Sys.argv
