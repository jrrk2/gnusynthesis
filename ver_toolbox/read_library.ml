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
open Printf
open Vparser
open Dump
open List
open Xmlm
  
let verbose = ref false
let instcnt = ref 0

let nulbuf' name tree =
  {
  nlen=String.length name;
  nam={id=name};
  decl=tree;
  len=0;
  seq=EMPTY;
  func=EMPTY;
  prop=Punknown;
  iolst=[];
  ipinlst=[];
  opinlst=[];
  reglst=[];
  wirlst=[];
  bnam=enterid "$";
  clk=enterid "$";
  clr=enterid "$";
  dat=enterid "$";
  en=enterid "$";
  qout=enterid "$";
  instcnt=0;
  tlst=[];
  instid=enterid "$";
  paramlst=[];
}

let nulbuf name tree = nulbuf' name tree

type bufset = {
  mutable buf: mybuft;
  mutable inv: mybuft;
  mutable pwr: mybuft;
  mutable gnd: mybuft;
  mutable tri: mybuft;
  mutable ff: mybuft;
  mutable ffc: mybuft;
  mutable ffce: mybuft;
  mutable logand: mybuft;
  mutable lognand: mybuft;
  mutable logor: mybuft;
  mutable logxor: mybuft;
  mutable mux: mybuft;
  mutable rednot: mybuft;
  mutable redand: mybuft;
  mutable redor: mybuft;
  mutable redxor: mybuft;
}

type rlst = (string*Globals.mybuft) list
type mlst = (string*Globals.modtree) list

let mybuf = {buf=(nulbuf "$" EMPTY);inv = (nulbuf "$" EMPTY);pwr = (nulbuf "$" EMPTY);gnd = (nulbuf "$" EMPTY);
             tri = (nulbuf "$" EMPTY);ff = (nulbuf "$" EMPTY);ffc = (nulbuf "$" EMPTY);ffce = (nulbuf "$" EMPTY);
             logand = (nulbuf "$" EMPTY);lognand = (nulbuf "$" EMPTY);logor = (nulbuf "$" EMPTY);
	     logxor = (nulbuf "$" EMPTY);mux = (nulbuf "$" EMPTY);rednot = (nulbuf "$" EMPTY);redand = (nulbuf "$" EMPTY);
             redor = (nulbuf "$" EMPTY);redxor = (nulbuf "$" EMPTY)}

let hfilter fn thash = let lst = ref [] in Hashtbl.iter (fun x _ -> if fn x then lst := x :: !lst) thash; !lst
let hfilter2 fn thash = let lst = ref [] in Hashtbl.iter (fun k x -> if fn (k,x) then lst := (k,x) :: !lst) thash; !lst;;

let rec ascfunc = function
  | Ptrue -> "1"
  | Pfalse -> "0"
  | Piff(arg1,arg2) -> ascfunc arg1^" iff "^ascfunc arg2
  | Pimp(arg1,arg2) -> ascfunc arg1^" implies "^ascfunc arg2
  | Por(arg1,arg2) -> ascfunc arg1^" | "^ascfunc arg2
  | Pand(arg1,arg2) -> ascfunc arg1^" & "^ascfunc arg2
  | Pnot arg -> "~("^ascfunc arg^")"
  | Pvar str -> Dump.dumpstr str
  | Punknown -> "unknown"
  | Prime arg -> "("^ascfunc arg^")'"
				    
let unhand_reduce = ref []

let rec reduce = function
  | ID id -> Pvar (ID id)
  | DOUBLE(LPAREN, exp) -> reduce exp
  | TRIPLE(P_OROR, arg1, arg2) -> Por(reduce arg1, reduce arg2)
  | TRIPLE(P_ANDAND, arg1, arg2) -> Pand(reduce arg1, reduce arg2)
  | other -> unhand_reduce := other :: !unhand_reduce; Pfalse

let rec genfunc arglst = function
  | BINNUM "1'b1" -> Ptrue
  | BINNUM "1'b0" -> Pfalse
  | INT 1 -> Ptrue
  | INT 0 -> Pfalse
  | BUFIF "bufif1" -> Ptrue
  | BUFIF "bufif0" -> Pfalse
  | ID id -> Pvar (ID id)
  | _ when List.length arglst = 0 -> Pfalse
  | NOT when List.length arglst = 1 -> Pnot(List.hd arglst)
  | AND when List.length arglst = 1 -> List.hd arglst
  | AND -> List.fold_left (fun arg1 arg2 -> Pand(arg1,arg2)) (List.hd arglst) (List.tl arglst)
  | OR when List.length arglst = 1 -> List.hd arglst
  | OR -> List.fold_left (fun arg1 arg2 -> Por(arg1,arg2)) (List.hd arglst) (List.tl arglst)
  | NAND -> Pnot(genfunc arglst AND)
  | NOR -> Pnot(genfunc arglst OR)
  | XOR when List.length arglst = 2 -> Por(Pand(List.nth arglst 0,Pnot (List.nth arglst 1)),
                                           Pand(Pnot (List.nth arglst 0),List.nth arglst 1))
  | XOR -> List.fold_left (fun arg1 arg2 -> genfunc [arg1;arg2] XOR) (List.hd arglst) (List.tl arglst)
  | XNOR -> Pnot(genfunc arglst XOR)
  | BUF when List.length arglst = 1 -> (List.hd arglst)
  | TRIPLE _ -> Pfalse
  | QUADRUPLE _ -> Pfalse
  | QUINTUPLE _ -> Pfalse
  | DOTTED _ -> Pfalse
  | other -> reduce other

let genfunc' kind arglst = genfunc (List.map (function
  | ID itm -> Pvar (ID itm)
  | TRIPLE(ID itm, EMPTY, EMPTY) -> Pvar (ID itm)
  | other -> Pvar other) arglst) kind

let rec subst hash = function
  | Ptrue -> Ptrue
  | Pfalse -> Pfalse
  | Piff(arg1,arg2) -> Piff(subst hash arg1, subst hash arg2)
  | Pimp(arg1,arg2) -> Pimp(subst hash arg1, subst hash arg2)
  | Por(arg1,arg2) -> Por(subst hash arg1, subst hash arg2)
  | Pand(arg1,arg2) -> Pand(subst hash arg1, subst hash arg2)
  | Pnot arg -> Pnot(subst hash arg)
  | Pvar str -> hash str
  | Punknown -> Punknown
  | Prime arg -> Prime(subst hash arg)
	
let is_member itm ipinlst = let rslt = ref false in
			 List.iter (fun {idpin=pin} -> if itm = pin then rslt := true) ipinlst;
			 !rslt

let cnv len str =
  let prop = Formula_rewrite.rewrite str in
  let func = match prop with
    | Ptrue -> BINNUM "1'b1"
    | Pfalse -> BINNUM "1'b0"
    | Pvar _ -> if len = 3 then BUFIF "bufif1" else BUF
    | Pnot (Pvar _) -> NOT
    | Pimp (Pvar _, Pvar _) -> XOR
    | Pand (Pvar _, Pvar _) -> AND
    | Por (Pvar _, Pvar _) -> OR
    | Pnot (Pimp (Pvar _, Pvar _)) -> XNOR
    | Pnot (Por (Pvar _, Pvar _)) -> NOR
    | Pnot (Pand (Pvar _, Pvar _)) -> NAND
    | Por (Pand (Pvar (s), Pvar (b)), Pand (Pvar (a), Pnot (Pvar (s')))) -> QUERY
    | oth -> (match !logfile with Open out_chan -> fprintf out_chan "Unrecognised function: %s\n" str | Closed -> ()); ASCNUM str in
   func, prop

let rec classify_cell arg (iolst',fnlst') =
      arg.iolst <- [];
      arg.paramlst <- [];
      let wires = Hashtbl.create 256 in
      List.iter (function
        | (id, "output") -> arg.opinlst <- {idpin=enterid id;rngpin=EMPTY} :: arg.opinlst;
(*
		if ff = REG then arg.reglst <- arg.reglst @ lst
*)
        | (id, "input") -> arg.ipinlst <- {idpin=enterid id;rngpin=EMPTY} :: arg.ipinlst
        | (id, ("primary_power"|"primary_ground")) -> arg.wirlst <- {idpin=enterid id;rngpin=EMPTY} :: arg.ipinlst
        | (id, other) -> arg.tlst <- ID (enterid id) :: arg.tlst) iolst';
      arg.iolst <- List.map (function {idpin = idpin; rngpin = EMPTY} -> ID idpin | _ -> EMPTY) (arg.opinlst @ arg.ipinlst);
      arg.len <- List.length arg.iolst;
      List.iter (function
        | ("Q", "IQ") -> arg.func <- MEMORY;
             arg.seq <- (if arg.len = 7 then DOUBLE(POSEDGE,IF) else POSEDGE);
             arg.clk <- {id="CK"}; arg.dat <- {id="D"};
        | (id, funcstr) -> let (func, prop) = cnv arg.len funcstr in arg.func <- func; arg.prop <- prop
        ) fnlst';
      arg.prop <- subst (function
        | ID str when Hashtbl.mem wires str -> Hashtbl.find wires str
        | other -> Pvar other) arg.prop;
      let hash1 = Hashtbl.create 255 and hash2 = Hashtbl.create 255 in
      let dir' dir = match dir with "input" -> INPUT | "output" -> OUTPUT | _ -> INOUT in
      let pinmap = List.map (fun (id, dir) -> QUINTUPLE (dir' dir, EMPTY, EMPTY, EMPTY, DOUBLE (ID {id = id}, EMPTY))) iolst' in
      arg.decl <- QUINTUPLE(MODULE, ID arg.nam, TLIST [], TLIST pinmap, THASH(hash1, hash2))

let find_buffer' rslt =
  if rslt.func=BUF && rslt.len = 2 && rslt.tlst = [] then (rslt.instid <- enterid "buf"; [rslt]) else []

let find_inverter' rslt =
  if rslt.func = NOT && rslt.len = 2 && rslt.tlst = [] then (rslt.instid <- enterid "inv"; [rslt]) else []

let find_power' rslt =
  if rslt.func = BINNUM "1'b1" && rslt.len = 1 && rslt.tlst = [] then (rslt.instid <- enterid "pwr"; [rslt]) else []

let find_ground' rslt =
  if rslt.func = BINNUM "1'b0" && rslt.len = 1 && rslt.tlst = [] then (rslt.instid <- enterid "gnd"; [rslt]) else []

let find_tristate' rslt =
  if rslt.func = BUFIF "bufif1" && rslt.len = 3 && rslt.tlst = [] then (rslt.instid <- enterid "tri"; [rslt]) else []

let find_flipflop' rslt =
  if rslt.func = MEMORY && rslt.len = 3 && rslt.seq = POSEDGE then (rslt.instid <- enterid "ff"; [rslt]) else []

let find_flipflop_c' rslt =
  if rslt.func = MEMORY && rslt.len = 4 && rslt.seq = POSEDGE then (rslt.instid <- enterid "ffc"; [rslt]) else []

let find_flipflop_ce' rslt =
  if rslt.func = MEMORY && (rslt.len = 5 || rslt.len = 7) && rslt.seq = DOUBLE(POSEDGE,IF) then (rslt.instid <- enterid "ffce"; [rslt]) else []

let find_logand' rslt =
  if rslt.func = AND && rslt.len = 3 && rslt.tlst = [] && List.length rslt.ipinlst = 2 then (rslt.instid <- enterid "and"; [rslt]) else []

let find_lognand' rslt =
  if rslt.func = NAND && rslt.len = 3 && rslt.tlst = [] && List.length rslt.ipinlst = 2 then (rslt.instid <- enterid "nand"; [rslt]) else []

let find_logor' rslt =
  if rslt.func = OR && rslt.len = 3 && rslt.tlst = [] && List.length rslt.ipinlst = 2 then (rslt.instid <- enterid "or"; [rslt]) else []

let find_logxor' rslt =
  if rslt.func = XOR && rslt.len = 3 && rslt.tlst = [] && List.length rslt.ipinlst = 2 then (rslt.instid <- enterid "xor"; [rslt]) else []

let find_mux2' rslt =
  if rslt.func = QUERY && rslt.len = 4 && rslt.tlst = [] && List.length rslt.ipinlst = 3 then (rslt.instid <- enterid "mux"; [rslt]) else []

let restore_itm fn descr =
  let buflst = ref [] in
  Hashtbl.iter (fun k x -> buflst := fn x @ !buflst) libhash;
  if !verbose then printf "%d %s detected\n" (List.length !buflst) descr;
  if List.length !buflst = 0 then failwith ("list of "^descr^" is empty");
  let entry = List.hd (List.sort Pervasives.compare !buflst) in
  if !verbose then printf "Using library %s %s %s(.%s(out),.%s(in));\n"
    descr entry.nam.Idhash.id entry.bnam.Idhash.id (List.hd entry.opinlst).idpin.Idhash.id (List.hd entry.ipinlst).idpin.Idhash.id;
  entry

let restore_lib' () =
  try
    begin
      if !verbose then printf "%d library cells detected\n" (Hashtbl.length libhash);
      mybuf.buf <- restore_itm find_buffer' "non-inverting buffer";
      mybuf.inv <- restore_itm find_inverter' "inverting buffer";
      mybuf.pwr <- restore_itm find_power' "power source";
      mybuf.gnd <- restore_itm find_ground' "ground source";
      mybuf.tri <- restore_itm find_tristate' "tri-state buffer";
      mybuf.ff <- restore_itm find_flipflop' "flip-flop";
      mybuf.ffc <- restore_itm find_flipflop_c' "flip-flop with clear";
      mybuf.ffce <- restore_itm find_flipflop_ce' "flip-flop with clear and enable";
      mybuf.logand <- restore_itm find_logand' "2-input and gate";
      mybuf.lognand <- restore_itm find_lognand' "2-input nand gate";
      mybuf.logor <- restore_itm find_logor' "2-input or gate";
      mybuf.logxor <- restore_itm find_logxor' "2-input xor gate";
      mybuf.mux <- restore_itm find_mux2' "2-input multiplexer";
    end
  with
      Failure "head" -> printf "Insufficient suitable cells detected\n"
        
let read_lib liberty =
  let rw,cellhash = File_rewrite.rewrite liberty in
  Hashtbl.iter (fun (nam:string) pattern ->
    let rslt = nulbuf nam EMPTY in classify_cell rslt pattern; Hashtbl.replace libhash nam rslt
  ) cellhash;
  restore_lib'()
    
let lib_write arg lst =
  let xml = Xmlio.Element ("gnusynthesis_library", [], List.map Xmlio.lib_trans lst) in
  Xmlio.out_tree arg (None,xml)
    
let lib_read arg =
  let _,xml = Xmlio.in_tree arg in
  let liblst = match xml with Xmlio.Element ("gnusynthesis_library", [], lst) -> lst | _ -> [] in
  List.map Xmlio.lib_untrans liblst
    
let mod_write arg lst =
  let xml = Xmlio.Element ("gnusynthesis_module", [], List.map Xmlio.mod_trans lst) in
  Xmlio.out_tree arg (None,xml)
    
let mod_read arg =
  let _,xml = Xmlio.in_tree arg in
  []
    
let dump_lib arg = 
  let (rlst:rlst ref) = ref [] in
  Hashtbl.iter (fun k x -> rlst := (k,x) :: !rlst) libhash;
  let revrlst = List.rev (!rlst) in
  if !verbose then printf "%d library cells saved\n" (List.length revrlst);
  lib_write arg revrlst

let restore_lib arg =
  let (rlst:rlst) = lib_read arg in
  Hashtbl.clear libhash;
  List.iter (fun (k,x) -> Hashtbl.replace libhash k x) rlst;
  fprintf stderr "%d library cells restored\n" (List.length rlst);
  restore_lib' ()

let dump_module arch nam arg = 
  let (lst:mlst ref) = ref [] in
  Hashtbl.iter (fun k x -> if (k = nam) && (x.arch = arch) then lst := (k,x) :: !lst) modprims;
  match !lst with
  | [] -> failwith ("No netlists matched arch "^arch^" name "^nam)
  | _ ->
    if !verbose then printf "%d modules saved\n" (List.length !lst);
    mod_write arg !lst

let restore_module arg =
  let (lst:mlst) = mod_read arg in
  List.iter (fun (k,x) -> Hashtbl.replace modprims k x) lst;
  fprintf stderr "%d modules restored\n" (List.length lst)

let scan_lib skip =
  let skipsrc = Hashtbl.create 256 in
 Globals.unresolved_list := [];
 Dump.unhand_list := []

let mods () = Printf.printf "-%16s-%10s-%8s-%8s-%8s-%8s- %s\n" "Name" "Is_netlist" "Is_behav" "Is_hier" "Is_top" "Arch" "Comment";
    Hashtbl.iter (fun k x -> if not (Hashtbl.mem libhash k) then
        Printf.printf "-%16s-%10s-%8s-%8s-%8s-%8s- %s-\n"
        k (string_of_bool x.is_netlist)
          (string_of_bool x.is_behav)
          (string_of_bool x.is_hier)
          (string_of_bool x.is_top) x.arch x.comment
    ) Globals.modprims;;
