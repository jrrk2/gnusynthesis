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
open Read_library
open Printf
open Scanf

type aiger_Ver_SignalType_t =
    | SIG_NONE
    | SIG_INPUT
    | SIG_OUTPUT
    | SIG_INOUT
    | SIG_REG
    | SIG_WIRE

(* types of verilog gates *)
type aiger_Ver_GateType_t =
    | GATE_UNKNOWN
    | GATE_AND
    | GATE_OR
    | GATE_XOR
    | GATE_BUF
    | GATE_NAND
    | GATE_NOR
    | GATE_XNOR
    | GATE_NOT
    | FLIP_FLOP

type edge = True of (string*int) | Compl of (string*int)

type pgate = {
  arr: edge array;
}

type aigerrec = {nets:(string,edge)Hashtbl.t;
                 cons:(pgate,unit)Hashtbl.t;
                 lat:(pgate,unit)Hashtbl.t;
                 asgn:(edge,edge)Hashtbl.t;
                 mutable pis:edge list;
                 mutable pos:edge list;}

let compl = function
    | True(nam,num) -> Compl(nam,num)
    | Compl(nam,num) -> True(nam,num)

let encode_in' = function
    | True(nam,num) -> num*2
    | Compl(nam,num) -> num*2+1

let encode_out = function
    | True(nam,num) -> num*2
    | Compl(nam,num) -> failwith "internal error, output net cannot be complemented"

let rec encode' hash key = match key with
| True(nam,num) ->
  if Hashtbl.mem hash.asgn key then encode' hash (Hashtbl.find hash.asgn key)
  else if Hashtbl.mem hash.asgn (Compl(nam,num)) then compl(encode' hash (Hashtbl.find hash.asgn (Compl(nam,num))))
  else key
| Compl(nam,num) -> 
  if Hashtbl.mem hash.asgn key then encode' hash (Hashtbl.find hash.asgn key)
  else if Hashtbl.mem hash.asgn (True(nam,num)) then compl(encode' hash (Hashtbl.find hash.asgn (True(nam,num))))
  else key

let encode_in hash key =
    let redirect = encode' hash key in
    if Hashtbl.mem hash.asgn redirect then failwith "internal error";
    encode_in' redirect

let aiger_str = function
    | GATE_UNKNOWN -> "GATE_UNKNOWN"
    | GATE_AND -> "GATE_AND"
    | GATE_OR -> "GATE_OR"
    | GATE_XOR -> "GATE_XOR"
    | GATE_BUF -> "GATE_BUF"
    | GATE_NAND -> "GATE_NAND"
    | GATE_NOR -> "GATE_NOR"
    | GATE_XNOR -> "GATE_XNOR"
    | GATE_NOT -> "GATE_NOT"
    | FLIP_FLOP -> "FLIP_FLOP"

let xyzzy=Hashtbl.create 256
let gnd="%GND"
let verbose = ref false
let stdcnt = ref 0
let sparenet = ref 0

let aiger_FindOrCreateNet aigerHash pName =
  if not (Hashtbl.mem aigerHash.nets pName) then
    begin
      let num = Hashtbl.length aigerHash.nets in
(*
      printf "%d: %s\n" num pName;
*)
      Hashtbl.replace aigerHash.nets pName (True (pName,num))
    end;
    Hashtbl.find aigerHash.nets pName

let aiger_Ver_ParseFindOrCreateNetwork (pName:string) =
  if not (Hashtbl.mem xyzzy pName) then
    begin
      let nets' = Hashtbl.create 256 in
      Hashtbl.replace xyzzy pName {
    		    nets=nets';
		    cons=Hashtbl.create 256;
		    lat=Hashtbl.create 256;
		    asgn=Hashtbl.create 256;
		    pis=[];pos=[];};
    end;
  let aigerHash = Hashtbl.find xyzzy pName in
  ignore(aiger_FindOrCreateNet aigerHash gnd);
  aigerHash

let aiger_FindNet aigerHash pName =
  if not (Hashtbl.mem aigerHash.nets pName) then
    begin
      None
    end
  else
    Some (Hashtbl.find aigerHash.nets pName)

let aiger_CreatePi aigerHash pName =
  let pNet = aiger_FindOrCreateNet aigerHash pName in
  aigerHash.pis <- pNet :: aigerHash.pis;
  pNet

let aiger_FindOrCreatePi aigerHash pName =
  match aiger_FindNet aigerHash pName with
    | None -> aiger_CreatePi aigerHash pName
    | Some pi -> pi

let aiger_CreatePo aigerHash pName =
  let pNet = aiger_FindOrCreateNet aigerHash pName in
  aigerHash.pos <- pNet :: aigerHash.pos
    
let aiger_Ver_FindGateInput pGate pWord1 =
  let rec found cnt = function
    | hd::tl -> if hd.idpin.id = pWord1 then cnt else found (cnt+1) tl
    | [] -> -1 in
  found 0 (pGate.ipinlst@pGate.opinlst)

let create_signal aigerHash pWord sigType nMsb nLsb =
  (*  printf "create_signal %s\n" pWord; *)
  if ( nMsb == -1 && nLsb == -1 ) then
    begin
      if ( sigType == SIG_INPUT || sigType == SIG_INOUT ) then
        ignore(aiger_CreatePi aigerHash pWord);
      if ( sigType == SIG_OUTPUT || sigType == SIG_INOUT ) then
        aiger_CreatePo aigerHash pWord;
      if ( sigType == SIG_WIRE || sigType == SIG_REG ) then
        ignore(aiger_FindOrCreateNet aigerHash pWord)
    end
  else
    begin
      assert( nMsb >= 0 && nLsb >= 0 );
      (* add signals from Msb to Lsb *)
      let limit = if nMsb > nLsb then nMsb - nLsb + 1 else nLsb - nMsb + 1 in
      let bit = ref nMsb in for i = 0 to limit-1 do
          let buffer = sprintf "%s[%d]" pWord !bit in
          if ( sigType == SIG_INPUT || sigType == SIG_INOUT ) then
            ignore(aiger_CreatePi aigerHash buffer);
          if ( sigType == SIG_OUTPUT || sigType == SIG_INOUT ) then
            aiger_CreatePo aigerHash buffer;
          if ( sigType == SIG_WIRE || sigType == SIG_REG ) then
            ignore(aiger_FindOrCreateNet aigerHash buffer);
          if nMsb > nLsb then decr bit else incr bit;
        done
    end;
  ()

let aiger_Ver_ParseFindNet aigerHash pName =
(*  printf "aiger_Ver_ParseFindNet %s\n" pName; *)
  begin
    let pObj = aiger_FindNet aigerHash pName in
    match pObj with 
      | None -> None
      | Some _ -> pObj
  end

type alogic =
  | Atom of string
  | Not of alogic
  | And of (alogic*alogic)
  | Nor of (alogic*alogic)

let asgn aigerHash opnet ipnet = 
    Hashtbl.add aigerHash.asgn opnet ipnet

let aiger_cstart aigerHash kind arg1 arg2 =
  let op = aiger_FindOrCreateNet aigerHash ("qq_"^string_of_int (Hashtbl.length aigerHash.nets)) in
  Hashtbl.add aigerHash.cons {arr=match kind with
    | GATE_AND -> [| op; arg1 ; arg2 |]
    | GATE_NOR -> [| op; compl arg1 ; compl arg2 |]
    | oth -> failwith ("unhandled logic type "^aiger_str oth)} (); op

let rec cstart aigerHash = function
    | And (arg1,arg2) ->
        aiger_cstart aigerHash GATE_AND (cstart aigerHash arg1) (cstart aigerHash arg2)
    | Nor (arg1,arg2) ->
        aiger_cstart aigerHash GATE_NOR (cstart aigerHash arg1) (cstart aigerHash arg2)
    | Not arg -> compl (cstart aigerHash arg)
    | Atom arg -> aiger_FindOrCreateNet aigerHash arg

let rec to_aiger2 = function
  | Ptrue -> Not (Atom gnd)
  | Pfalse -> Atom gnd
  | Pvar (ID {id="1'b1"}) -> Not (Atom gnd)
  | Pvar (ID {id="1'b0"}) -> Atom gnd
  | Pvar (ID str) -> Atom (str.id)
  | Pvar _ -> failwith "unhandled formula"
  | Pnot arg -> Not (to_aiger2 arg)
  | Pand(arg1,arg2) -> collapse(And (to_aiger2 arg1, to_aiger2 arg2))
  | Por(arg1,arg2) -> collapse(Not (Nor (to_aiger2 arg1, to_aiger2 arg2)))
  | Piff (_, _)
  | Pimp (_, _)
  | Punknown -> failwith "unhandled formula"

and collapse = function
  | Not (Not arg) -> arg
  | oth -> oth
 
let aiger_Ver_ParseGate' aigerHash prop inst (connlst:(token*string*token)list) =
  let kind = prop.nam.id in
  if !verbose then printf "aiger_Ver_ParseGate' %s kind %s\n" inst kind;

  if (prop.prop = Punknown) then failwith (sprintf "Boolean function property for %s is unknown\n" kind);

  let pinhash = Hashtbl.create 256 in
  List.iter (fun (formal,conn,_) -> match formal with
    | ID cellpin ->
      Hashtbl.add pinhash cellpin conn
    | _ -> failwith "formal is not an ID"
  ) connlst;

  let mysubst = subst (function
    | ID str -> Pvar (ID (enterid(if Hashtbl.mem pinhash str then Hashtbl.find pinhash str else str.id)))
    | other -> Pvar other) prop.prop in

  let rslt2 = to_aiger2 mysubst in

  (* fill in output name *)
  List.iter (fun (formal,pWord2,_) ->
    begin
      (match formal with
        | ID id -> if Read_library.is_member id prop.opinlst then
           let opnet = aiger_FindOrCreateNet aigerHash pWord2 in
	   asgn aigerHash opnet (cstart aigerHash rslt2)
        | _ -> failwith "formal is not an ID")
    end
    ) connlst
        
let aiger_Ver_ParseGateStandard aigerHash aiger_GateType = function
| [ID op;ID arg1;ID arg2] -> 
begin
    let opnet = aiger_FindOrCreateNet aigerHash op.id in
    asgn aigerHash opnet (aiger_cstart aigerHash aiger_GateType
    	(cstart aigerHash (Atom arg1.id))
    	(cstart aigerHash (Atom arg2.id)))
end
| oth -> Dump.unhandled stderr 271 (TLIST oth)

let aiger_Ver_ParseFlopStandard aigerHash kind (connlst:(token*string*token)list) = 
  begin
    let pNetLi = ref (aiger_FindOrCreateNet aigerHash "1'b0") in
    let pNetLo = ref (aiger_FindOrCreateNet aigerHash "1'b0") in

    (* parse pairs of formal/actual inputs *)
    List.iter (fun (formal,pWord2,_) ->
    begin
        (* process one pair of formal/actual parameters *)

        (* get the actual net *)
        match aiger_Ver_ParseFindNet aigerHash pWord2 with 
          | None -> failwith (sprintf "aiger_Ver_ParseFindNet %s returned None" pWord2);
          | Some pNetActual -> (* add the fanin *)
            (match formal with
              | ID d when d.id="D" -> pNetLi := pNetActual (* data *)
              | ID q when q.id="Q" -> pNetLo := pNetActual (* out *)
              | ID id -> ()
              | _ -> failwith "formal is not an ID")
    end
    ) connlst;

    (* create the latch *)
    let arr = {arr=Array.of_list [!pNetLo;!pNetLi]} in
    Hashtbl.add aigerHash.lat arr ();
    1
end

let aiger_Ver_ParseModule pWord iolst portlst
    (insts:(string, (string * (Vparser.token * string * Vparser.token) list) list ref) Hashtbl.t) =
begin
let nMsb = (-1) and nLsb = (-1) in

    (* get the network with this name *)
    let aigerHash = aiger_Ver_ParseFindOrCreateNetwork pWord in

    (* parse the inputs/outputs/registers/wires/inouts *)
    Hashtbl.iter (fun pWord (kind,extra) ->
      (* printf "kind/actual %s/%s\n" kind pWord; *)
      match kind with
        | INPUT ->
            create_signal aigerHash pWord SIG_INPUT nMsb nLsb
        | OUTPUT ->
            create_signal aigerHash pWord SIG_OUTPUT nMsb nLsb
        | REG ->
            create_signal aigerHash pWord SIG_REG nMsb nLsb
        | WIRE ->
            create_signal aigerHash pWord SIG_WIRE nMsb nLsb
        | INOUT ->
            create_signal aigerHash pWord SIG_INOUT nMsb nLsb
        | _ -> ()) iolst;

    (* parse the remaining statements *)
    Hashtbl.iter (fun kind reflst -> match kind with
        |  "and" ->
            aiger_Ver_ParseGateStandard aigerHash GATE_AND portlst
        |  "or" ->
            aiger_Ver_ParseGateStandard aigerHash GATE_OR portlst
        |  "xor" ->
            aiger_Ver_ParseGateStandard aigerHash GATE_XOR portlst
        |  "buf" ->
            aiger_Ver_ParseGateStandard aigerHash GATE_BUF portlst
        |  "nand" ->
            aiger_Ver_ParseGateStandard aigerHash GATE_NAND portlst
        |  "nor" ->
            aiger_Ver_ParseGateStandard aigerHash GATE_NOR portlst
        |  "xnor" ->
            aiger_Ver_ParseGateStandard aigerHash GATE_XNOR portlst
        |  "not" ->
            aiger_Ver_ParseGateStandard aigerHash GATE_NOT portlst
        |  "FD"
        |  "FDC"
        |  "FDCE"
        |  "FDPE" ->
            List.iter (fun ((inst:string), (conn:(token*string*token)list)) -> ignore(aiger_Ver_ParseFlopStandard aigerHash kind conn)) !reflst
        |  "endmodule" -> ()
        | _ -> if Hashtbl.mem libhash kind then
            begin
              let pGate = Hashtbl.find libhash kind in
                  List.iter (fun (inst, conn) -> ignore(aiger_Ver_ParseGate' aigerHash pGate inst conn)) !reflst
            end
              else
                    failwith ("module instance "^(fst (List.hd !reflst))^" (kind "^kind^") is undefined")
        ) insts;
end

let write_aiger_arch' arch nam file arg =
  match arg.tree with
    | QUINTUPLE(MODULE, ID arg1, arg2, TLIST io, THASH targ4) ->
        let dlst = hfilter (function
            | QUINTUPLE((INPUT|OUTPUT|INOUT),_,_,_,_) -> true
            | _ -> not arg.is_behav) (fst targ4) in
        let osymtab = Minimap.decl_to_hls dlst arg.symbols in
        let (oinsts,_) = if arg.is_behav
          then
            (Hashtbl.create 256, Hashtbl.create 256)
          else
            Minimap.minimap_body (fun id -> ID id) (snd targ4) arg.symbols osymtab in
        let oc = unique_open file in
        aiger_Ver_ParseModule nam osymtab io oinsts;

        Hashtbl.iter (fun circ (aigerr:aigerrec) ->
	  fprintf oc "aag %d %d %d %d %d\n"
		 (Hashtbl.length aigerr.nets)
		 (List.length aigerr.pis)
		 (Hashtbl.length aigerr.lat)
		 (List.length aigerr.pos)
		 (Hashtbl.length aigerr.cons);
	
	List.iter (fun itm -> fprintf oc "%d\n" (encode_out itm)) aigerr.pis;
	Hashtbl.iter ( fun k _ ->
          fprintf oc "%d %d\n"
	  (encode_out k.arr.(0))
	  (encode_in aigerr k.arr.(1))
	   ) aigerr.lat;
	List.iter (fun itm ->
          fprintf oc "%d\n" (encode_in aigerr itm)) aigerr.pos;
	Hashtbl.iter ( fun k _ ->
          fprintf oc "%d %d %d\n"
	  (encode_out k.arr.(0))
	  (encode_in aigerr k.arr.(1))
	  (encode_in aigerr k.arr.(2))
	   ) aigerr.cons;
	let ix = ref 0 in List.iter (function
	  True (nam,_) | Compl (nam,_) -> fprintf oc "i%d %s\n" !ix nam; incr ix) aigerr.pis;
	let ix = ref 0 in Hashtbl.iter ( fun k _ -> match k.arr.(0) with
          True (nam,_) | Compl (nam,_) -> fprintf oc "l%d %s\n" !ix nam; incr ix) aigerr.lat;
	let ix = ref 0 in List.iter (function
	  True (nam,_) | Compl (nam,_) -> fprintf oc "o%d %s\n" !ix nam; incr ix) aigerr.pos;
        fprintf oc "c\n%s\n" arg.comment;
	let tmparr = Array.create (Hashtbl.length aigerr.nets) "" in
        Hashtbl.iter (fun nam e -> match e with
	  True (str,num) | Compl (str,num) -> tmparr.(num) <- nam) aigerr.nets;
        Array.iteri (fun num nam -> fprintf oc "n%d %s\n" (num*2) nam) tmparr;
        ) xyzzy;
        close_out oc;
    | _ -> failwith "cannot write non-module"

let write_aiger_arch arch nam =
  let lst = List.filter (fun arg -> arg.arch = arch) (Hashtbl.find_all modprims nam) in
  match lst with
    | [] -> failwith ("No netlists matched arch "^arch^" name "^nam)
    | _ -> List.iter (write_aiger_arch' arch nam (nam^"_"^arch^"_aiger.aag")) lst
