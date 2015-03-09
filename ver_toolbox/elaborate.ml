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

let dbgkind = ref []
let automap = ref ""
let dumpprefix = ref "elabprefix"
    
let width' isyms id =
  let (lft,rght,inc) = Minimap.find_width id isyms in
  if inc = 0 then SCALAR else RANGE(INT lft,INT rght)

let elaborate_subcell width iolstref syms thash2 kind arg3 iolst paramlst =
  Hashtbl.add (snd thash2)
    (QUADRUPLE(MODINST, ID kind, paramlst, TLIST (List.map (function
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

let mask n0 = if n0 = 0 then 1 else let l = ref 0 and n = ref n0 in while (!n > 0) do n := !n lsr 1; l := !l + 1; done; !l

let maptmp newkind =
  let elabtmp = !Globals.archenv in
  Globals.archenv := !automap;
  if !automap <> "" then Hls.gen_map_arch elabtmp newkind.id;
  Globals.archenv := elabtmp

let rec elaborate_subcell' used iolstref syms thash2 kind arg3 paramlst nam top =
  match top.tree with
    | QUINTUPLE(MODULE, _, TLIST plst, TLIST iolst, THASH (decls,insts)) ->
	let newparams,newkind = match paramlst with
	| EMPTY -> (plst,kind)
	| DOUBLE(HASH, TLIST lst) ->
	    let newkind = ref kind.id in
	    let map' = list_mapi 0 (fun ix arg -> match arg with
	    | SEXTUPLE(PARAMETER, signing, range, ID id, attr, expr) ->
	      if ix < List.length lst then
		let crnt=List.nth lst ix in
		(match crnt with
		| INT num -> newkind := !newkind^"_"^id.id^"_"^string_of_int num
		| oth -> Dump.unhandled stderr 83 oth);
		SEXTUPLE(PARAMETER, signing, range, ID id, attr, crnt) else arg
	    | oth -> Dump.unhandled stderr 85 oth; UNKNOWN) plst in
	    dbgkind := !newkind :: !dbgkind;
	    map',(enterid !newkind)
	| oth -> Dump.unhandled stderr 88 oth; (plst,kind) in
	if not (Count.find_arch !Globals.archenv newkind.id <> [] || newkind.id = kind.id || List.mem newkind.id !used) then
	  begin
            let modtree = QUINTUPLE(MODULE, ID newkind, TLIST newparams,
				    TLIST iolst,
				    THASH (decls, insts)) in
	    let symbols = Semantics.shash_create !Globals.archenv newkind.id top.symbols 256 in
	    let newtree = Semantics.init_tree [] (!Globals.archenv^"src") modtree symbols "" in
	    Verilogout.write_verilog_one newtree (!dumpprefix^"/"^newkind.id^"."^ !Globals.archenv) "subcell to be elaborated";	 
	    used := newkind.id :: !used;
	    List.iter (function
	      | SEXTUPLE(PARAMETER, signing, range, ID pid, attr, expr) ->
		  Semantics.enter_parameter stderr symbols pid expr EMPTY
	      | oth -> unhandled stderr 254 oth) newparams;
	    Semantics.prescan stderr !Globals.archenv (generate_elaborate_netlist used newtree) "Elaborated by gen_elaborate_arch";
	    Verilogout.write_verilog_one (List.hd (Count.find_arch !Globals.archenv newkind.id)) (!dumpprefix^"/"^newkind.id^"."^ !Globals.archenv) "elaborated subcell";
	  end;
        elaborate_subcell (width' top.symbols) iolstref syms thash2 newkind arg3 iolst EMPTY
    | err -> unhandled stderr 115 err

and isubst syms = function
| HEXNUM str -> let radix = 16 in let (sz,num) = Const.widthnum stderr radix str in INT num
| DECNUM str -> let radix = 10 in let (sz,num) = Const.widthnum stderr radix str in INT num
| OCTNUM str -> let radix = 8 in let (sz,num) = Const.widthnum stderr radix str in INT num
| BINNUM str -> let radix = 2 in let (sz,num) = Const.widthnum stderr radix str in INT num
| oth -> subst syms oth
      
and subst syms = function
| INT n -> INT n
| WIDTHNUM(radix,sz,num) -> INT num
| HEXNUM str -> let radix = 16 in let (sz,num) = Const.widthnum stderr radix str in WIDTHNUM(radix,sz,num)
| DECNUM str -> let radix = 10 in let (sz,num) = Const.widthnum stderr radix str in WIDTHNUM(radix,sz,num)
| OCTNUM str -> let radix = 8 in let (sz,num) = Const.widthnum stderr radix str in WIDTHNUM(radix,sz,num)
| BINNUM str -> let radix = 2 in let (sz,num) = Const.widthnum stderr radix str in WIDTHNUM(radix,sz,num)
| TRIPLE(PLUS, expr1, expr2) -> (match (isubst syms expr1, isubst syms expr2) with (INT num1, INT num2) -> INT ( ( + ) num1 num2) | oth1,oth2 -> TRIPLE(PLUS, oth1, oth2))
| TRIPLE(MINUS, expr1, expr2) -> (match (isubst syms expr1, isubst syms expr2) with (INT num1, INT num2) -> INT ( ( - ) num1 num2) | oth1,oth2 -> TRIPLE(MINUS, oth1, oth2))
| TRIPLE(TIMES, expr1, expr2) -> (match (isubst syms expr1, isubst syms expr2) with (INT num1, INT num2) -> INT ( ( * ) num1 num2) | oth1,oth2 -> TRIPLE(TIMES, oth1, oth2))
| TRIPLE(DIVIDE, expr1, expr2) -> (match (isubst syms expr1, isubst syms expr2) with (INT num1, INT num2) -> INT ( ( / ) num1 num2) | oth1,oth2 -> TRIPLE(DIVIDE, oth1, oth2))
| TRIPLE(P_EQUAL, expr1, expr2) -> (match (isubst syms expr1, isubst syms expr2) with (INT num1, INT num2) -> if ( ( = ) num1 num2) then INT 1 else INT 0 | oth1,oth2 -> TRIPLE(P_EQUAL, oth1, oth2))
| TRIPLE(P_NOTEQUAL, expr1, expr2) -> (match (isubst syms expr1, isubst syms expr2) with (INT num1, INT num2) -> if ( ( != ) num1 num2) then INT 1 else INT 0 | oth1,oth2 -> TRIPLE(P_NOTEQUAL, oth1, oth2))
| TRIPLE(LESS, expr1, expr2) -> (match (isubst syms expr1, isubst syms expr2) with (INT num1, INT num2) -> if ( ( < ) num1 num2) then INT 1 else INT 0 | oth1,oth2 -> TRIPLE(LESS, oth1, oth2))
| TRIPLE(GREATER, expr1, expr2) -> (match (isubst syms expr1, isubst syms expr2) with (INT num1, INT num2) -> if ( ( > ) num1 num2) then INT 1 else INT 0 | oth1,oth2 -> TRIPLE(GREATER, oth1, oth2))
| TRIPLE(P_LTE, expr1, expr2) -> (match (isubst syms expr1, isubst syms expr2) with (INT num1, INT num2) -> if ( ( <= ) num1 num2) then INT 1 else INT 0 | oth1,oth2 -> TRIPLE(P_LTE, oth1, oth2))
| TRIPLE(P_GTE, expr1, expr2) -> (match (isubst syms expr1, isubst syms expr2) with (INT num1, INT num2) -> if ( ( >= ) num1 num2) then INT 1 else INT 0 | oth1,oth2 -> TRIPLE(P_GTE, oth1, oth2))
| DOUBLE(LPAREN, expr) -> (match isubst syms expr with INT num -> INT num | oth -> DOUBLE(LPAREN, oth))
| DOUBLE(CONCAT, TLIST lst) -> DOUBLE(CONCAT, TLIST (List.map (subst syms) lst))
| ID id -> if Const.shash_chain_mem syms id then (match (Const.shash_chain_find syms id).sigattr with Sigparam pexpr -> pexpr | oth -> ID id) else ID id
| TLIST lst -> TLIST (List.map (subst syms) lst)
| RANGE(hi, lo) -> RANGE(subst syms hi, subst syms lo)
| TRIPLE(ASSIGNMENT, ID id, expr) -> TRIPLE(ASSIGNMENT, DOUBLE(CONCAT, TLIST (Minimap.id_flatten (Minimap.find_width id syms) id [])), subst syms expr)
| DOUBLE(arg1, arg2) -> DOUBLE(subst syms arg1, subst syms arg2)
| TRIPLE(arg1, arg2, arg3) -> TRIPLE(subst syms arg1, subst syms arg2, subst syms arg3)
| QUADRUPLE(arg1, arg2, arg3, arg4) -> QUADRUPLE (subst syms arg1, subst syms arg2, subst syms arg3, subst syms arg4)
| QUINTUPLE(arg1, arg2, arg3, arg4, arg5) -> QUINTUPLE (subst syms arg1, subst syms arg2, subst syms arg3, subst syms arg4, subst syms arg5)
| oth -> oth

and elaborate used arch thash2 syms iolstref = function
| DOUBLE((ALWAYS|INITIAL|FINAL), _) as exp -> Hashtbl.add (snd thash2) (subst syms exp) ();
| TRIPLE(ASSIGN, arg1, TLIST arg2) ->
    (match arg1 with 
      | DOUBLE (HASH, TLIST [INT n]) -> Printf.fprintf stderr "/* #%d Delay ignored */\n" n
      | EMPTY -> ()
      | oth -> unhandled stderr 32 oth);
    Hashtbl.add (snd thash2)
      (TRIPLE(ASSIGN, arg1, TLIST (List.map (subst syms) arg2))) ();
| QUADRUPLE((WIRE|TRI0|TRI1) as tok, arg0, TRIPLE(arg1, srng, arg3), TLIST arg4) ->
  let intrng = Minimap.flatten_width srng syms in
  Hashtbl.replace (fst thash2) (QUADRUPLE(tok, arg0, TRIPLE(arg1, subst syms srng, arg3),
            TLIST (List.map (function
              | DOUBLE (ID id, EMPTY) -> DOUBLE (ID id, EMPTY)
              | TRIPLE (ID id, EMPTY, expr) ->
		  let ass = expr in
		  Hashtbl.add (snd thash2)
		    (TRIPLE(ASSIGN, EMPTY, TLIST [TRIPLE(ASSIGNMENT, ID id, ass)])) ();
		  DOUBLE (ID id, EMPTY)
              | x -> unhandled stderr 103 x; EMPTY) arg4))) ()
| QUADRUPLE(MODINST, ID kind, paramlst, TLIST arg3) ->
  if Hashtbl.mem modprims kind.id then
    begin
      Minimap.select_sub false (elaborate_subcell' used iolstref syms thash2 kind arg3 paramlst) !archenv kind.id
    end
  else if Hashtbl.mem libhash kind.id then
    begin
      let libcell = Hashtbl.find libhash kind.id in
      let newparams,newkind = match paramlst with
      | EMPTY -> (libcell.paramlst,kind)
      | DOUBLE(HASH, TLIST lst) ->
	  let newkind = ref kind.id in
	  let map' = list_mapi 0 (fun ix arg ->
	  if ix < List.length lst then
	    let crnt=List.nth lst ix in
	    (match crnt with
	    | INT num -> newkind := !newkind^"_"^arg.pid.id^"_"^string_of_int num
	    | oth -> Dump.unhandled stderr 121 oth);
	    {pid=arg.pid;prng=arg.prng;dflt=crnt} else arg) libcell.paramlst in
	  dbgkind := !newkind :: !dbgkind;
	  map',(enterid !newkind)
      | oth -> Dump.unhandled stderr 114 oth; (libcell.paramlst,kind) in
      if not (Count.find_arch !Globals.archenv newkind.id <> [] || List.mem newkind.id !used) then
	begin
          let decls = Hashtbl.create 256 and insts = Hashtbl.create 256 in
	  let paramlst' = List.map (fun {pid=pid;prng=range;dflt=dflt} ->
	    SEXTUPLE(PARAMETER, SPECIAL, range, ID pid, EMPTY, dflt)) newparams in
	  List.iter (fun {idpin=in1;rngpin=rng} ->
	    Hashtbl.replace decls (QUINTUPLE(INPUT, EMPTY, EMPTY, rng, TLIST [TRIPLE (ID in1, EMPTY, EMPTY)])) ()) libcell.ipinlst;
	  List.iter (fun {idpin=out1;rngpin=rng} ->
	    Hashtbl.replace decls (QUINTUPLE(OUTPUT, EMPTY, EMPTY, rng, TLIST [TRIPLE (ID out1, EMPTY, EMPTY)])) ()) libcell.opinlst;
	  List.iter (fun {idpin=out1;rngpin=rng} ->
	    Hashtbl.replace decls (QUADRUPLE(REG, EMPTY, rng, TLIST [TRIPLE (ID out1, EMPTY, EMPTY)])) ()) libcell.reglst;
	  List.iter (fun arg -> Hashtbl.replace insts arg ()) libcell.tlst;
          let modtree = QUINTUPLE(MODULE, ID newkind, TLIST paramlst',
				  TLIST (List.map (function
				    | ID id -> ID id
				    | x -> unhandled stderr 78 x; EMPTY) libcell.iolst),
				  THASH (decls,insts)) in
	  let symbols = Semantics.shash_create !Globals.archenv newkind.id EndShash 256 in
	  let newtree = Semantics.init_tree [] (!Globals.archenv^"src") modtree symbols "" in
	  used := newkind.id :: !used;
	  Verilogout.write_verilog_one newtree (!dumpprefix^"/"^newkind.id^"."^newtree.arch) "subcell to be elaborated";
          List.iter (function
	    | SEXTUPLE(PARAMETER, signing, range, ID pid, attr, expr) ->
		Semantics.enter_parameter stderr symbols pid expr EMPTY
            | oth -> unhandled stderr 254 oth) paramlst';
	  Semantics.prescan stderr !Globals.archenv (generate_elaborate_netlist used newtree) "Elaborated by gen_elaborate_arch";
	  Verilogout.write_verilog_one (List.hd (Count.find_arch !Globals.archenv newkind.id)) (!dumpprefix^"/"^newkind.id^"."^ !Globals.archenv) "elaborated subcell";	  
	  
		     
	end;
      elaborate_subcell (fun _ -> SCALAR) iolstref syms thash2 newkind arg3 libcell.iolst EMPTY
    end
  else
    failwith ("sub-module "^kind.id^" not found")
| QUADRUPLE(REG, arg2, srng, TLIST arg4) ->
  Hashtbl.replace (fst thash2) (QUADRUPLE(REG, EMPTY, subst syms srng, TLIST arg4)) ();
| QUADRUPLE(INTEGER, EMPTY, EMPTY, TLIST arg4) ->
  Hashtbl.replace (fst thash2) (QUADRUPLE(INTEGER, EMPTY, EMPTY, TLIST arg4)) ();
| QUINTUPLE((INPUT|OUTPUT) as tok, ((EMPTY|REG) as registered), EMPTY, srng, TLIST arg4) ->
  let lst = List.map (function
    | TRIPLE (ID id, EMPTY, EMPTY) -> TRIPLE(ID id, EMPTY, EMPTY)
    | x -> unhandled stderr 51 x; registered) arg4 in
  Hashtbl.replace (fst thash2) (QUINTUPLE(tok, registered, EMPTY, subst syms srng, TLIST lst)) ()
| SEPTUPLE(TASK, EMPTY, ID tid, EMPTY, TLIST iolst, TLIST body, EMPTY) as exp -> Hashtbl.add (snd thash2) exp ();
| OCTUPLE(FUNCTION, EMPTY, rng, ID fid, EMPTY, TLIST iolst, TLIST body, EMPTY) as exp -> Hashtbl.add (snd thash2) exp ();
| exp -> unhandled stderr 63 exp
    
and elaborate' used arch thash2 syms ioref decls insts =
        Hashtbl.iter (fun x _ -> elaborate used arch thash2 syms ioref x) decls;
        Hashtbl.iter (fun x _ -> elaborate used arch thash2 syms ioref x) insts

and generate_elaborate_netlist used k =
  let thash2 = (Hashtbl.create 256,Hashtbl.create 256) in
  match k.tree with
    | QUINTUPLE(MODULE, ID arg1, arg2, TLIST arg3, THASH (decls,insts)) ->
        let iolstref = ref (List.rev arg3) in
        elaborate' used k.arch thash2 k.symbols iolstref decls insts;
        QUINTUPLE(MODULE, ID (enterid (arg1.id^ !Globals.modsuffix)), arg2,
                TLIST (List.map (function
                  | ID id -> ID id
                  | x -> unhandled stderr 78 x; EMPTY) (List.rev !iolstref)),
                THASH thash2)
    | _ -> failwith (Dump.dumpstr k.tree^" is not a module netlist")

let gen_elaborate_arch arch nam =
  let _ = Verilogout.mktmpdir !dumpprefix in
  let used = ref [] in  
  let lst = Count.find_arch arch nam in
  match lst with
    | arg::[] -> assert (arg.arch = arch);
(*
        let needed = ref [] in  
        Minimap.recurse_arch (fun id subarg -> assert (subarg.arch = arch);
	  print_endline id;
	  needed := (id,subarg) :: !needed) arch nam arg;
	print_endline ("Needed modules: "^String.concat " " (List.map (fun (id,subarg) -> id) !needed));
	List.iter (fun (id,subarg) ->
	  Semantics.prescan stderr !Globals.archenv (generate_elaborate_netlist used subarg) "Generated by gen_elaborate_arch"
	    ) !needed;
*)
	Semantics.prescan stderr !Globals.archenv (generate_elaborate_netlist used arg) "Generated by gen_elaborate_arch";
        Printf.printf "Module report %s\n" (Semantics.endscan());
	print_endline ("Instantiated modules: "^String.concat " " !used);
    | [] -> failwith "nothing selected in gen_elaborate_arch"
    | _ -> failwith "ambiguous selection in gen_elaborate_arch"
