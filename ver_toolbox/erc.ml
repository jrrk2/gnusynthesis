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

let rec erc_flat_netlist enter = function
| TRIPLE((BUF|NOT|AND|OR|XOR|NAND|NOR|XNOR|PULLUP|NMOS|PMOS|TRAN) as kind, dly, TLIST inst) -> List.iter (enter kind) inst
| TRIPLE((BUFIF lev|NOTIF lev|TRANIF lev) as kind, weaklist, TLIST inst) -> List.iter (enter kind) inst
| QUADRUPLE((MODINST|PRIMINST), ID kind, arg2, TLIST inst) ->
    List.iter (enter (ID kind)) inst
| QUINTUPLE(MODULE,ID arg1, arg2, TLIST arg3, arg4) -> erc_flat_netlist enter arg4;
| THASH thash ->
  Hashtbl.iter (fun itm _ -> erc_flat_netlist enter itm) (fst thash);
  Hashtbl.iter (fun itm _ -> erc_flat_netlist enter itm) (snd thash)
| _ -> ()

let rec erc_item trc netstats inststats ident inuse net lst =
  Printf.fprintf trc "%sNet %s:\n" ident (Count.tokenstr net);
  if List.mem net inuse then
    begin
      let err = "timing loop in:\n"^(String.concat "\n" (List.map Count.tokenstr (net::inuse))) in
      Printf.fprintf trc "%s" err;
      flush trc;
      failwith err
    end
  else let drv = ref None in List.iter (fun (typ,instid,pinref,dir,sz) ->
    Printf.fprintf trc "%s%s:%s:%s:%s:%s\n" ident (Count.tokenstr typ) instid.id pinref.id (Count.tokenstr dir) (Count.tokenstr sz);
    match dir with
    | OUTPUT | REG ->  
      (match !drv with
      | Some old -> Printf.fprintf trc "%s:%s both drive net %s\n" old.id pinref.id (Count.tokenstr net)
      | None -> drv := Some pinref)
    | INPUT ->
      List.iter (fun (net',typ',pinref',dir',sz') ->
	if (pinref != pinref') && (dir' = OUTPUT) && (String.length ident < 100) then
	  begin
	  let ident = ident^" " in
	  Printf.fprintf trc "%sNet %s:%s:%s\n" ident
	  (Count.tokenstr net') (Count.tokenstr typ') pinref'.id;
	  erc_item trc netstats inststats ident (net::inuse) net' (Hashtbl.find netstats net')
	  end
      ) (Hashtbl.find_all inststats instid);
    | _ -> failwith "Invalid direction"
    ) lst

let erc_chk_netlist ident netstats inststats =
  let trc = open_out "erc_chk.log" in
  try Hashtbl.iter (erc_item trc netstats inststats ident []) netstats; close_out trc with err -> close_out trc; raise err

let rec erc_flat_netlist' arg =
  let netstats = Hashtbl.create 256 and inststats = Hashtbl.create 256 in
  let append net pin = Hashtbl.replace netstats net (pin::if Hashtbl.mem netstats net then Hashtbl.find netstats net else []) in
  let enter typ = function
    |  TRIPLE(ID instid, SCALAR, TLIST pinlst) -> let pinhash = Hashtbl.create 256 in (match typ with
      | ID typid -> if Hashtbl.mem libhash typid.id then
	  begin
	  let prop = Hashtbl.find libhash typid.id in
	  let dir = ref INPUT in List.iter (fun pinlst ->
	    List.iter (fun itm -> match itm.rngpin with
            | RANGE(INT hi, INT lo) ->
              Hashtbl.replace pinhash itm.idpin (!dir,RANGE(INT hi,INT lo))
            | EMPTY ->
              Hashtbl.replace pinhash itm.idpin (!dir,EMPTY)
            | other -> Dump.unhandled stderr 56 other) pinlst; dir := if prop.func = MEMORY then REG else OUTPUT)
			      [prop.ipinlst;prop.opinlst]
	  end
	else if Hashtbl.mem modprims typid.id then
	  begin
	    let found = (Hashtbl.find modprims typid.id) in
	    if found.is_netlist then
	      begin
		failwith "sub-circuit of type %s (netlist) not allowed for this command"
	      end
	    else
	      failwith "sub-circuit of type %s (behavioural) not yet supported for this command"
	  end
	else
	  failwith "sub-circuit of type %s (unknown) not allowed for this command"
      | other -> Dump.unhandled stderr 56 other);
      List.iter (function
      | TRIPLE(CELLPIN, ID pinref, net) ->
	let (dir,sz) = Hashtbl.find pinhash pinref in
	append net (typ,instid,pinref,dir,sz);
	Hashtbl.add inststats instid (net,typ,pinref,dir,sz);
      | other -> Dump.unhandled stderr 58 other) pinlst
    | other -> Dump.unhandled stderr 59 other in
  erc_flat_netlist enter arg.tree;
  erc_chk_netlist "\t" netstats inststats

let erc_arch arch nam =
  let found = Count.find_arch arch nam in
  if found <> [] then
    List.iter (fun arg -> erc_flat_netlist' arg) found
  else
    failwith ("erc_arch "^arch^" "^nam^" returned an empty list")
