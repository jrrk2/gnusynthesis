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

type milef_Ver_SignalType_t =
    | SIG_NONE
    | SIG_INPUT
    | SIG_OUTPUT
    | SIG_INOUT
    | SIG_REG
    | SIG_WIRE

(* types of verilog gates *)
type milef_Ver_GateType_t =
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

type pgate = {
  gate: string;
  arr: string array;
}

type milefrec = {nets:(string,string)Hashtbl.t;
                 cons:(milef_Ver_GateType_t*int,pgate)Hashtbl.t;
                 mutable pis:string list;
                 mutable pos:string list;
                 mutable bis:string list;
                 mutable bos:string list;
                 mutable lat:string list;}

let milefname id =
  let i = ref 0 and len = String.length id in
  let valid = String.create len in
  while !i < len do (match id.[!i] with
    | 'a'..'z' -> valid.[!i] <- id.[!i]
    | 'A'..'Z' -> valid.[!i] <- id.[!i]
    | '0'..'9' when !i > 0 -> valid.[!i] <- id.[!i]
    | '_' when !i > 0 -> valid.[!i] <- '_'
    | _ -> valid.[!i] <- (if !i > 0 then '.' else 'q')); incr i
  done;
  valid

let typdump num = function
    | GATE_UNKNOWN -> "Unknown"
    | GATE_AND -> "And_"^(string_of_int num)
    | GATE_OR -> "Or_"^(string_of_int num)
    | GATE_XOR -> "Exor"
    | GATE_BUF -> "Driver"
    | GATE_NAND -> "Nand_"^(string_of_int num)
    | GATE_NOR -> "Nor_"^(string_of_int num)
    | GATE_XNOR -> "Xnor"
    | GATE_NOT -> "Inverter"
    | FLIP_FLOP -> "Dflipflop"

let xyzzy=Hashtbl.create 256

let milef_Ver_ParseFindOrCreateNetwork (pName:string) =
  if not (Hashtbl.mem xyzzy pName) then
    Hashtbl.replace xyzzy pName {nets=Hashtbl.create 256;cons=Hashtbl.create 256;pis=[];pos=[];bis=[];bos=[];lat=[];};
  Hashtbl.find xyzzy pName

let verbose = ref false
let stdcnt = ref 0
let sparenet = ref 0

let milef_FindOrCreateNet milefHash pName =
  if not (Hashtbl.mem milefHash.nets pName) then
    begin
      Hashtbl.replace milefHash.nets pName (pName)
    end;
    Hashtbl.find milefHash.nets pName

let milef_FindNet milefHash pName =
  if not (Hashtbl.mem milefHash.nets pName) then
    begin
      None
    end
  else
    Some (Hashtbl.find milefHash.nets pName)

let milef_HasBlackbox milefHash = false

let milef_CreatePi milefHash pName =
  let pNet = milef_FindOrCreateNet milefHash pName in
  milefHash.pis <- pNet :: milefHash.pis;
  pNet

let milef_FindOrCreatePi milefHash pName =
  match milef_FindNet milefHash pName with
    | None -> milef_CreatePi milefHash pName
    | Some pi -> pi

let milef_CreatePo milefHash pName =
  let pNet = milef_FindOrCreateNet milefHash pName in
  milefHash.pos <- pNet :: milefHash.pos

let milef_CreateCon milefHash inst kind len =
  let arr = {gate=inst;arr=Array.make len ("")} in
  Hashtbl.replace milefHash.cons (kind,len-1) arr;
  arr
    
let milef_Ver_FindGateInput pGate pWord1 =
  let rec found cnt = function
    | hd::tl -> if hd.idpin.id = pWord1 then cnt else found (cnt+1) tl
    | [] -> -1 in
  found 0 (pGate.ipinlst@pGate.opinlst)

let create_signal milefHash pWord sigType nMsb nLsb =
  (*  printf "create_signal %s\n" pWord; *)
  if ( nMsb == -1 && nLsb == -1 ) then
    begin
      if ( sigType == SIG_INPUT || sigType == SIG_INOUT ) then
        ignore(milef_CreatePi milefHash pWord);
      if ( sigType == SIG_OUTPUT || sigType == SIG_INOUT ) then
        milef_CreatePo milefHash pWord;
      if ( sigType == SIG_WIRE || sigType == SIG_REG ) then
        ignore(milef_FindOrCreateNet milefHash pWord)
    end
  else
    begin
      assert( nMsb >= 0 && nLsb >= 0 );
      (* add signals from Msb to Lsb *)
      let limit = if nMsb > nLsb then nMsb - nLsb + 1 else nLsb - nMsb + 1 in
      let bit = ref nMsb in for i = 0 to limit-1 do
          let buffer = sprintf "%s[%d]" pWord !bit in
          if ( sigType == SIG_INPUT || sigType == SIG_INOUT ) then
            ignore(milef_CreatePi milefHash buffer);
          if ( sigType == SIG_OUTPUT || sigType == SIG_INOUT ) then
            milef_CreatePo milefHash buffer;
          if ( sigType == SIG_WIRE || sigType == SIG_REG ) then
            ignore(milef_FindOrCreateNet milefHash buffer);
          if nMsb > nLsb then decr bit else incr bit;
        done
    end;
  0

let milef_Ver_ParseFindNet milefHash pName =
(*  printf "milef_Ver_ParseFindNet %s\n" pName; *)
  begin
    let pObj = milef_FindNet milefHash pName in
    match pObj with 
      | None ->
        if pName = "1'b0" || pName = "1'bx" then
          Some (milef_FindOrCreatePi milefHash "GND")
        else
          if ( pName = "1'b1" ) then
            Some (milef_FindOrCreatePi milefHash "VCC") 
          else
            None
      | Some _ -> pObj
  end

type mlogic =
  | Zero
  | One
  | Atom of idhash
  | Not of mlogic
  | And of mlogic list
  | Or of mlogic list
  | Xor of mlogic*mlogic

let to_milef truth = 
        Or (List.map (fun lst ->
          And (List.map (fun (decisionvar, logic) -> match decisionvar with
            | ID id -> if logic then Atom id else Not (Atom id)
            | _ -> failwith "to_milef") lst)) truth)

let milef_cstart opnet milefHash (inst:string) kind args =
  let op = match opnet with
    | None ->
        let nname num = "qq_"^string_of_int !num in
        while Hashtbl.mem milefHash.nets (nname sparenet) do
          incr sparenet;
        done;
        nname sparenet
    | Some net -> net in
  let key = (kind,List.length args) in
  Hashtbl.replace milefHash.nets op op;
  let found = List.map (fun itm -> itm.gate) (Hashtbl.find_all milefHash.cons key) in
  let inst' = inst^"_"^typdump (snd key) kind in
  let inst'' = if List.mem inst' found then
    begin
      let num = ref 0 in
      let iname num = inst^"_"^string_of_int !num^"_"^typdump (snd key) kind in
      while List.mem (iname num) found do
        incr num;
      done;
      iname num
    end
    else
      inst' in
  let arr = {gate=inst'';arr=Array.of_list (args @ [op])} in
  Hashtbl.add milefHash.cons key arr;
  op

let rec cstart opnet milefHash inst = function
    | Or [And arglst] ->
        milef_cstart opnet milefHash inst GATE_AND (List.map (cstart None milefHash inst) arglst)
    | Or arglst ->
        milef_cstart opnet milefHash inst GATE_OR (List.map (cstart None milefHash inst) arglst)
    | And [Or arglst] ->
        milef_cstart opnet milefHash inst GATE_OR (List.map (cstart None milefHash inst) arglst)
    | And arglst ->
        milef_cstart opnet milefHash inst GATE_AND (List.map (cstart None milefHash inst) arglst)
    | Not (Or arglst) ->
        milef_cstart opnet milefHash inst GATE_NOR (List.map (cstart None milefHash inst) arglst)
    | Not (And arglst) ->
        milef_cstart opnet milefHash inst GATE_NAND (List.map (cstart None milefHash inst) arglst)
    | Not arg ->
        milef_cstart opnet milefHash inst GATE_NOT (List.map (cstart None milefHash inst) [arg])
    | Xor (arg1,arg2) ->
        milef_cstart opnet milefHash inst GATE_XOR (List.map (cstart None milefHash inst) [arg1;arg2])
    | Atom arg -> (match opnet with
        | None -> arg.id
        | Some _ -> milef_cstart opnet milefHash inst GATE_BUF (List.map (cstart None milefHash inst) [Atom arg]))
    | Zero -> let zero = milef_FindOrCreatePi milefHash "GND" in (match opnet with
        | None -> zero
        | Some _ -> milef_cstart opnet milefHash inst GATE_BUF (List.map (cstart None milefHash inst) [Atom (enterid zero)]))
    | One -> let one = milef_FindOrCreatePi milefHash "VCC" in (match opnet with
        | None -> one
        | Some _ -> milef_cstart opnet milefHash inst GATE_BUF (List.map (cstart None milefHash inst) [Atom (enterid one)]))

let debughash = Hashtbl.create 256

let rec to_milef2 = function
  | Ptrue -> One
  | Pfalse -> Zero
  | Pvar (ID {id="1'b1"}) -> One
  | Pvar (ID {id="1'b0"}) -> Zero
  | Pvar (ID str) -> Atom (str)
  | Pnot arg -> Not (to_milef2 arg)
  | Por(Pand(arg1,Pnot arg2),Pand(Pnot arg3,arg4)) when arg1=arg3 && arg2=arg4 ->
      Xor(to_milef2 arg1, to_milef2 arg4)
  | Pand(arg1,arg2) -> collapse(And [to_milef2 arg1; to_milef2 arg2])
  | Por(arg1,arg2) -> collapse(Or [to_milef2 arg1; to_milef2 arg2])
  | Pimp (_, _) -> failwith "unhandled milef expression Pimp _"
  | Piff (_, _) -> failwith "unhandled milef expression Piff _"
  | Prime _ -> failwith "unhandled milef expression Prime _"
  | Punknown -> failwith "unhandled milef expression Punknown"
  | Pvar _ -> failwith "unhandled milef expression Pvar _"

and collapse = function
  | Zero -> Zero
  | One -> One
  | And ((And lst)::tl) -> collapse (And (lst @ tl))
  | And oth -> And oth
  | Or ((Or lst)::tl) -> collapse (Or (lst @ tl))
  | Or oth -> Or oth
  | Not (Not arg) -> arg
  | Atom arg -> Atom arg
  | oth -> oth
	
let milef_Ver_ParseGate' milefHash prop inst (connlst:(token*string*token)list) =
  let kind = prop.nam.id in
  if !verbose then printf "milef_Ver_ParseGate' %s kind %s\n" inst kind;

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

  let rslt2 = if false then
      begin
        let (nam, decision) = Decision.bdd_of_formula mysubst in
        let rslt = List.map (fun lst -> List.map (fun (decisionvar, logic) -> nam.(decisionvar),logic) lst) (Decision.all_sat decision) in
        to_milef rslt
      end
    else
      begin
        to_milef2 mysubst
      end in

  (* fill in output name *)
  let opnet = ref None in
  List.iter (fun (formal,pWord2,_) ->
    begin
      (match formal with
        | ID id -> if Read_library.is_member id prop.opinlst then
            opnet := Some pWord2
        | _ -> failwith "formal is not an ID")
    end
    ) connlst;

  Hashtbl.add debughash inst (!opnet,mysubst,rslt2);

  cstart !opnet milefHash inst rslt2
        
let milef_Ver_ParseGateStandard milefHash milef_GateType portlst =
begin
    (* create the component *)
    incr stdcnt;
    let arr = milef_CreateCon milefHash (string_of_int !stdcnt) milef_GateType (List.length portlst) in

    let idx = ref 0 in List.iter (function
      | ID pWord ->
    begin
        (* get the net corresponding to this output *)
        match milef_Ver_ParseFindNet milefHash pWord.id with
          | None -> failwith (sprintf "Net is missing in gate %s." pWord.id );
          | Some pNet -> 
        (* if this is the first net, add it as an output *)
        if !idx = 0 then
            arr.arr.(List.length portlst-1) <- pNet
        else
            (arr.arr.(!idx) <- pNet; incr idx)
    end
      | oth -> Dump.unhandled stderr 256 oth ) portlst;
    1
end

let milef_Ver_ParseFlopStandard milefHash kind inst (connlst:(token*string*token)list) = 
  begin
    let pNetLi = ref (milef_FindOrCreateNet milefHash "1'b0") in
    let pNetLo = ref (milef_FindOrCreateNet milefHash "1'b0") in

    (* parse pairs of formal/actual inputs *)
    let _ = List.map (fun (formal,pWord2,_) ->
    begin
        (* process one pair of formal/actual parameters *)

        (* get the actual net *)
        match milef_Ver_ParseFindNet milefHash pWord2 with 
          | None -> failwith (sprintf "milef_Ver_ParseFindNet %s returned None" pWord2);
          | Some pNetActual -> (* add the fanin *)
            (match formal with
              | ID d when d.id="D" -> pNetLi := pNetActual; (false,pNetActual,d,pWord2) (* data *)
              | ID q when q.id="Q" -> pNetLo := pNetActual; (false,pNetActual,q,pWord2) (* out *)
              | ID id -> (true,pNetActual,id,pWord2)
              | _ -> failwith "formal is not an ID")
    end
    ) connlst in

    (* create the latch *)
    let arr = {gate=inst^"_DFlipFlop";arr=Array.of_list [!pNetLi;!pNetLo]} in
    Hashtbl.add milefHash.cons (FLIP_FLOP,1) arr;
    1
end

let milef_Ver_ParseModule pWord iolst portlst
    (insts:(string, (string * (Vparser.token * string * Vparser.token) list) list ref) Hashtbl.t) =
begin
let nMsb = (-1) and nLsb = (-1) in
let retValue = ref 0 in

    (* get the network with this name *)
    let milefHash = milef_Ver_ParseFindOrCreateNetwork pWord in

    (* parse the inputs/outputs/registers/wires/inouts *)
    Hashtbl.iter (fun pWord (kind,extra) ->
      (* printf "kind/actual %s/%s\n" kind pWord; *)
      match kind with
        | INPUT ->
            retValue := create_signal milefHash pWord SIG_INPUT nMsb nLsb
        | OUTPUT ->
            retValue := create_signal milefHash pWord SIG_OUTPUT nMsb nLsb
        | REG ->
            retValue := create_signal milefHash pWord SIG_REG nMsb nLsb
        | WIRE ->
            retValue := create_signal milefHash pWord SIG_WIRE nMsb nLsb
        | INOUT ->
            retValue := create_signal milefHash pWord SIG_INOUT nMsb nLsb
        | _ -> ()) iolst;

    (* parse the remaining statements *)
    Hashtbl.iter (fun kind reflst -> match kind with
        |  "and" ->
            retValue := milef_Ver_ParseGateStandard milefHash GATE_AND portlst
        |  "or" ->
            retValue := milef_Ver_ParseGateStandard milefHash GATE_OR portlst
        |  "xor" ->
            retValue := milef_Ver_ParseGateStandard milefHash GATE_XOR portlst
        |  "buf" ->
            retValue := milef_Ver_ParseGateStandard milefHash GATE_BUF portlst
        |  "nand" ->
            retValue := milef_Ver_ParseGateStandard milefHash GATE_NAND portlst
        |  "nor" ->
            retValue := milef_Ver_ParseGateStandard milefHash GATE_NOR portlst
        |  "xnor" ->
            retValue := milef_Ver_ParseGateStandard milefHash GATE_XNOR portlst
        |  "not" ->
            retValue := milef_Ver_ParseGateStandard milefHash GATE_NOT portlst
        |  "FD"
        |  "FDC"
        |  "FDCE"
        |  "FDPE" ->
            List.iter (fun ((inst:string), (conn:(token*string*token)list)) -> ignore(milef_Ver_ParseFlopStandard milefHash kind inst conn)) !reflst
        |  "endmodule" -> ()
        | _ -> if Hashtbl.mem libhash kind then
            begin
              let pGate = Hashtbl.find libhash kind in
                  List.iter (fun (inst, conn) -> ignore(milef_Ver_ParseGate' milefHash pGate inst conn)) !reflst
            end
              else
                    failwith ("module instance "^(fst (List.hd !reflst))^" (kind "^kind^") is undefined")
        ) insts;
end

let dump_tb oc syms coding arg1 arg2 arg3 arg4 exp =
    fprintf oc "\n%s " coding;
    (match arg1 with 
      | EMPTY -> ()
      | REG -> fprintf oc "%s " (Ord.getstr arg1);
      | _ -> unhandled stderr 433 arg1);
    (match arg2 with 
      | EMPTY -> ()
      | _ -> unhandled stderr 436 arg2);
    (match arg3 with
      | RANGE(INT arg1,INT arg2) -> fprintf oc "[%d:%d] " arg1 arg2
      | RANGE(arg1, arg2) ->
                let lft = Const.exprConstStr stderr syms arg1
                and rght = Const.exprConstStr stderr syms arg2 in
                fprintf oc "[%s:%s] " lft rght
      | EMPTY -> ()
      | _ -> unhandled stderr 436 arg2);
    let delim = ref "" in List.iter (fun x -> (match x with
      | TRIPLE (ID id, EMPTY, EMPTY) -> fprintf oc "%s%s%s" !delim (if exp then "exp_" else "") (Verilogout.escaped id)
      | _ -> unhandled stderr 440 x); delim := ",") arg4;
    fprintf oc ";"

let dump_tb_header syms comment arg1 arg3 arglst ins outs =
  let oc = open_out (arg1.id^"_tb.v") in
  fprintf oc "// %s\n\nmodule %s_tb; " comment arg1.id;
  List.iter (function
    | QUINTUPLE(INPUT, arg1, arg2, arg3, TLIST arg4) -> dump_tb oc syms "reg" arg1 arg2 arg3 arg4 false
    | QUINTUPLE(INOUT, arg1, arg2, arg3, TLIST arg4) -> dump_tb oc syms "wire" arg1 arg2 arg3 arg4 false
    | QUINTUPLE(OUTPUT, arg1, arg2, arg3, TLIST arg4) -> dump_tb oc syms "wire" arg1 arg2 arg3 arg4 false
    | _ -> ()) arglst;
  List.iter (function
    | QUINTUPLE(OUTPUT, arg1, arg2, arg3, TLIST arg4) -> dump_tb oc syms "reg" arg1 arg2 arg3 arg4 true
    | _ -> ()) arglst;
  fprintf oc "\n\n%s\tdut" arg1.id;
  let delim = ref "(" and wid = ref 0 in List.iter (function
    | ID id -> let esc = Verilogout.escaped id in
               fprintf oc "%s.%s(%s)" !delim esc esc;
               wid := !wid + String.length esc + 1;
               if !wid >= 80 then (wid := 9; delim := ",\n\t") else delim := ","
    | x -> unhandled stderr 338 x) arg3;
  if !delim = "(" then fprintf oc "();" else fprintf oc ");";
  fprintf oc "\n\n";
  fprintf oc "integer i, fd;\n";
  fprintf oc "parameter inwidth = %d;\n" (List.length ins);
  fprintf oc "parameter outwidth = %d;\n" (List.length outs);
  fprintf oc "parameter maxvec = 65536;\n";
  fprintf oc "\n";
  fprintf oc "reg [inwidth+outwidth-1:0] mem[0:maxvec];\n";
  fprintf oc "reg [inwidth+outwidth-1:0] act[0:maxvec];\n\n";
  fprintf oc "initial\n";
  fprintf oc "begin\n";
  fprintf oc "  fd = $fopen(\"%s_lifting.dat\");\n" arg1.id;
  fprintf oc "  $readmemb(\"%s.mem\", mem);\n" arg1.id;
  fprintf oc "  for (i = 0; (i < maxvec) & (1'bx !== ^mem[i]); i=i+1)\n";
  fprintf oc "  begin\n";
  fprintf oc "    {%s,exp_%s} = mem[i];\n" (String.concat "," ins) (String.concat ",exp_" outs);
  fprintf oc "    $display(\"%%B,%%B\", {%s}, {exp_%s});\n" (String.concat "," ins) (String.concat ",exp_" outs);
  fprintf oc "    #1000\n";
  fprintf oc "    act[i] = {%s,%s};\n" (String.concat "," ins) (String.concat "," outs);
  fprintf oc "    $display(\"%%B,%%B\", {%s}, {%s});\n" (String.concat "," ins) (String.concat ",exp_" outs);
  fprintf oc "    $display;\n";
  fprintf oc "    if (mem[i] !== act[i]) $finish(1);\n";
  fprintf oc "`include \"%s_ordering_monitor.v\"\n" arg1.id;
  fprintf oc "  end\n";
  fprintf oc "$fclose(fd);\n";
  fprintf oc "$finish(0);\n\n";
  fprintf oc "end\n\n";
  fprintf oc "\nendmodule // %s_tb; " arg1.id;
  close_out oc

let write_milef_arch' arch nam file arg =
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
        fprintf oc "{%s}\n" arg.comment;
        milef_Ver_ParseModule nam osymtab io oinsts;
        let ins = ref [] and outs = ref [] in
        Hashtbl.iter (fun circ (milefr:milefrec) ->
          fprintf oc "Circuit %s ;\n" (milefname circ);
          let netlst = ref [] in Hashtbl.iter (fun k x -> netlst := k :: !netlst) milefr.nets;
          let delim = ref "Node " and tab = ref 0 in List.iter (fun k -> fprintf oc "%s%s" !delim (milefname k);
            tab := !tab + String.length !delim + String.length k;
            if !tab > 80 then (delim := ",\n\t "; tab := 0) else delim := ", ") (List.sort compare !netlst);
          fprintf oc " : bit;\n";
          let kindlst = ref [] in Hashtbl.iter (fun (k,arity) _ ->
            let key = (typdump arity k,k,arity) in
            if not (List.mem key !kindlst) then kindlst := key :: !kindlst) milefr.cons;
          List.iter (fun (nam,k,arity) ->
            let delim = ref "Con ( delay : 1 ) " and tab = ref 0 in List.iter (fun x ->
              fprintf oc "%s%s" !delim (milefname x.gate);
              tab := !tab + String.length !delim + String.length x.gate;
              if !tab > 80 then (delim := ",\n\t "; tab := 0) else delim := ", ") (List.sort compare (Hashtbl.find_all milefr.cons (k,arity)));
            fprintf oc ": %s ; \n" nam) (List.sort compare !kindlst);
          let delim = ref "Input " and tab = ref 0 in List.iter (fun ins' ->
                                                          ins := ins' :: !ins;
                                                          fprintf oc "%s%s" !delim (milefname ins');
                                                          tab := !tab + String.length !delim + String.length ins';
                                                           if !tab > 80 then (delim := ",\n\t "; tab := 0) else delim := ", ") (List.sort compare milefr.pis);
          fprintf oc " ;\n";
          let delim = ref "Output " and tab = ref 0 in List.iter (fun outs' ->
                                                           outs := outs' :: !outs;
                                                           fprintf oc "%s%s" !delim (milefname outs');
                                                           tab := !tab + String.length !delim + String.length outs';
                                                           if !tab > 80 then (delim := ",\n\t "; tab := 0) else delim := ", ") (List.sort compare milefr.pos);
          fprintf oc " ;\n";
          fprintf oc "Strobe STROBEALL : strb1, strb2;\n";
          fprintf oc "begin\n";
          List.iter (fun (nam,k,arity) -> List.iter ( fun x -> fprintf oc "%s " (milefname x.gate);
            let delim = ref "( " and tab = ref (String.length x.gate) in Array.iteri ( fun k itm ->
              fprintf oc "%s%s" !delim (milefname itm);
              tab := !tab + String.length !delim + String.length itm;
              if k = Array.length x.arr - 2 then delim := " ; " else delim := ", ";
              if !tab > 80 then (delim := !delim^"\n\t "; tab := 0);
            ) x.arr;
            fprintf oc " );\n") (List.sort compare (Hashtbl.find_all milefr.cons (k,arity)));
            ) (List.sort compare !kindlst);
          fprintf oc "end.\n";
        ) xyzzy;
        close_out oc;
        dump_tb_header arg.Globals.symbols (arg1.id^" testbench") arg1 io dlst (List.rev !ins) (List.rev !outs);
    | _ -> failwith "cannot write non-module"

let write_milef_arch arch nam =
  let lst = List.filter (fun arg -> arg.arch = arch) (Hashtbl.find_all modprims nam) in
  match lst with
    | [] -> failwith ("No netlists matched arch "^arch^" name "^nam)
    | _ -> List.iter (write_milef_arch' arch nam (nam^"_"^arch^".milef")) lst
