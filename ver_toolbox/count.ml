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
open Idhash
open Vparser
open Dump

let rec dump' buf = function
| TLIST lst -> List.iter (fun x -> dump' buf x; bprintf buf "$" ) lst
| THASH thash -> Hashtbl.iter (fun x _ -> dump' buf x ) (fst thash);  Hashtbl.iter (fun x _ -> dump' buf x ) (snd thash)
| DOUBLE(tok,arg) -> dump' buf tok ; dump' buf arg  
| TRIPLE(tok, arg1, arg2) -> dump' buf tok ; dump' buf arg1 ; dump' buf arg2 
| QUADRUPLE(tok, arg1, arg2, arg3) -> dump' buf tok ; dump' buf arg1 ; dump' buf arg2 ; dump' buf arg3 
| QUINTUPLE(tok, arg1, arg2, arg3, arg4) -> dump' buf tok ; dump' buf arg1 ; dump' buf arg2 ; dump' buf arg3 ; dump' buf arg4 
| SEXTUPLE(tok, arg1, arg2, arg3, arg4, arg5) -> dump' buf tok ; dump' buf arg1 ; dump' buf arg2 ; dump' buf arg3 ; dump' buf arg4 ; dump' buf arg5 
| SEPTUPLE(tok, arg1, arg2, arg3, arg4, arg5, arg6) -> dump' buf tok ; dump' buf arg1 ; dump' buf arg2 ; dump' buf arg3 ; dump' buf arg4 ; dump' buf arg5 ; dump' buf arg6 
| RANGE(arg1,arg2) -> bprintf buf "_"; dump' buf arg1 ; bprintf buf "_"; dump' buf arg2 ; bprintf buf "_"
| ASCNUM c -> bprintf buf "%s" c
| BINNUM c -> bprintf buf "Bin%s" c
| BUFIF lev -> bprintf buf "%s" lev
| DECNUM c -> bprintf buf "Dec%s" c
| FLOATNUM flt -> bprintf buf "(%f)" flt
| HEXNUM c -> bprintf buf "Hex%s" c
| ID str -> bprintf buf "%s" (String.uppercase str.id)
| ILLEGAL c -> bprintf buf "ILLEGAL%c" c
| INTNUM c -> bprintf buf "%s" c
| PREPROC str -> bprintf buf "PRE%s" str
| WEAK strength -> bprintf buf "weak%s" strength
| WIDTHNUM(radix,sz,num) -> bprintf buf "_%d" num
| INT n -> bprintf buf "_%d" n
| EMPTY -> ()
| oth -> let tok = String.capitalize (Ord.getstr oth) in bprintf buf "%s" (String.sub tok 0 (min (String.length tok) 3))

let tokenstr tok = let buf = Buffer.create 64 in dump' buf tok; Buffer.contents buf

let rec xtrfunc' = function
  | Ptrue
  | Pfalse -> 2
  | Piff(arg1,arg2) -> xtrfunc' arg1 + xtrfunc' arg2
  | Pimp(arg1,arg2) -> xtrfunc' arg1 + xtrfunc' arg2
  | Pnot(Por(arg1,arg2)) -> xtrfunc' arg1 + xtrfunc' arg2 + 2
  | Pnot(Pand(arg1,arg2)) -> xtrfunc' arg1 + xtrfunc' arg2 + 2
  | Por(arg1,arg2) -> xtrfunc' arg1 + xtrfunc' arg2 + 2
  | Pand(arg1,arg2) -> xtrfunc' arg1 + xtrfunc' arg2 + 2
  | Pnot arg -> xtrfunc' arg + 2
  | Pvar str -> 0
  | Punknown -> 0

let xtrfunc prop = 2 + xtrfunc' prop

let xtr_count inststats =
  let totcnt = ref 0 in
  Hashtbl.iter (fun k cnt -> match k with
    | ID nam -> if Hashtbl.mem libhash nam.id then
        totcnt := !totcnt + (xtrfunc (Hashtbl.find libhash nam.id).prop) * cnt
    | _ -> ()) inststats;
  !totcnt

let rec count_flat_netlist enter = function
| TRIPLE((BUF|NOT|AND|OR|XOR|NAND|NOR|XNOR|PULLUP|NMOS|PMOS|TRAN) as kind, dly, TLIST inst) -> enter kind (List.length inst)
| TRIPLE((BUFIF lev|NOTIF lev|TRANIF lev) as kind, weaklist, TLIST inst) -> enter kind (List.length inst)
| QUADRUPLE((MODINST|PRIMINST), ID kind, arg2, TLIST inst) ->
  if Hashtbl.mem libhash kind.id then
    enter (ID kind) (List.length inst)
  else if Hashtbl.mem modprims kind.id then
    begin
    let found = (Hashtbl.find modprims kind.id) in
    if found.is_netlist then
      begin
        enter (ID kind) 0;
        count_flat_netlist enter found.tree
      end
    else
      enter (ID kind) (List.length inst)
    end
  else
    enter (ID kind) 0
| QUINTUPLE(MODULE,ID arg1, arg2, TLIST arg3, arg4) -> count_flat_netlist enter arg4;
| THASH thash ->
  Hashtbl.iter (fun itm _ -> count_flat_netlist enter itm) (fst thash);
  Hashtbl.iter (fun itm _ -> count_flat_netlist enter itm) (snd thash)
| _ -> ()

let rec count_flat_netlist' arg =
  let inststats = Hashtbl.create 256 in
  let enter tok cnt = if Hashtbl.mem inststats tok then Hashtbl.replace inststats tok ((Hashtbl.find inststats tok) + cnt)
  else Hashtbl.add inststats tok cnt in
(*
  if Hashtbl.length libhash = 0 then failwith("Cell library is empty");
*)
  count_flat_netlist enter arg.tree;
  inststats

let show_count inststats =
  let totcnt = ref 0 in
  Printf.printf "\t\tname\t ios\tprimary\tsequence\toccur\txtrs\tfunction\n";
  Hashtbl.iter (fun k cnt -> match k with
    | ID nam -> if Hashtbl.mem libhash nam.id then
        begin
        let rslt = Hashtbl.find libhash nam.id in
        let xtrcnt = xtrfunc rslt.prop in
        Printf.printf "%20s\t%2d\t%s\t%8s\t%d\t%d\t%s = %s\n"
          rslt.nam.id
          rslt.len
          (Ord.getstr rslt.func)
          (Dump.dumpstr rslt.seq)
          cnt
          (xtrfunc rslt.prop)
          (if rslt.opinlst <> [] then (List.hd rslt.opinlst).idpin.id else "?")
          (Read_library.ascfunc rslt.prop);
        totcnt := !totcnt + xtrcnt * cnt
        end
      else (* if cnt > 0 then *)
        Printf.printf "%30s\n" nam.id
    | _ -> Printf.printf "%s\t%d\n" (Dump.dumpstr k) cnt) inststats;
  Printf.printf "\t\t----\t ---\t-------\t\t\t\t----\n\t\t\t\t\t\t\t\t%d\n" !totcnt

let find_arch arch nam =
  List.filter (fun arg -> arg.arch = arch) (Hashtbl.find_all modprims nam)

let count_arch arch nam =
  let found = find_arch arch nam in
  if found <> [] then
    List.iter (fun arg -> show_count (count_flat_netlist' arg)) found
  else
    failwith ("count_arch "^arch^" "^nam^" returned an empty list")

let xtr_count_arch arch nam =
  let found = find_arch arch nam in
  if found <> [] then
    List.map (fun arg -> xtr_count (count_flat_netlist' arg)) found
  else
    failwith ("xtr_count_arch "^arch^" "^nam^" returned an empty list")

let vhier verbose =
  let thresh = ref (-1) and biggest = ref "" in
  Hashtbl.iter (fun k x ->
    if x.is_top && not (Hashtbl.mem libhash k) then
      begin
        let xcnt = xtr_count (count_flat_netlist' x) in
        if xcnt > !thresh then
          begin
          thresh := xcnt;
          biggest := k;
          end;
      end) Globals.modprims;
  if !thresh > (-1) then
    begin
      if verbose then
        begin
          Printf.fprintf stderr "biggest block = %d xtrs\n" !thresh;
          flush_all();
        end;
    end;
  !biggest
