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

let verbose = ref false

let escaped id' =
  let id = id'.id in
  let escaped = ref false and i = ref 0 and len = String.length id in
  while !i < len && not !escaped do (match id.[!i] with
    | 'a'..'z' -> ()
    | 'A'..'Z' -> ()
    | '0'..'9' when !i > 0 -> ()
    | '_' -> ()
    | '$' when !i > 0 -> ()
    | _ -> escaped := true); incr i
  done;
  if !escaped then "\\"^id^" " else id

let widthnum (radix, sz, num) = match radix with
                  | 16 -> sprintf "%d'h%x" sz num
                  | 10 -> sprintf "%d'd%d" sz num
                  | 8 -> sprintf "%d'o%o" sz num
                  | 2 -> sprintf "%d'b%s" sz (Minimap.bin num sz)
                  | _ -> unhandled stderr 289 (WIDTHNUM (radix, sz, num)); string_of_int num

let rec dump_logic' = function
  | ASSIGNMENT -> " = "
  | DLYASSIGNMENT -> " <= "
  | DOUBLE(OR, arg) -> "(|("^dump_logic arg^"))"
  | DOUBLE(AND, arg) -> "(&("^dump_logic arg^"))"
  | DOUBLE(NOT, arg) -> "(~"^dump_logic arg^")"
  | DOUBLE(PLING, arg) -> "(!"^dump_logic arg^")"
  | DOUBLE(LPAREN, arg) -> "("^dump_logic arg^")"
  | DOUBLE(POSEDGE, arg) -> "posedge "^dump_logic arg
  | DOUBLE(NEGEDGE, arg) -> "negedge "^dump_logic arg
  | TRIPLE(P_EQUAL, arg1, arg2) -> "("^dump_logic arg1^" == "^dump_logic arg2^")"
  | TRIPLE(AND, arg1, arg2) -> "("^dump_logic arg1^" & "^dump_logic arg2^")"
  | TRIPLE(OR, arg1, arg2) -> "("^dump_logic arg1^" | "^dump_logic arg2^")"
  | TRIPLE(XOR, arg1, arg2) -> "("^dump_logic arg1^" ^ "^dump_logic arg2^")"
  | TRIPLE(BITSEL, arg1, arg2) -> dump_logic arg1^"["^dump_logic arg2^"]"
  | TRIPLE(IF, cond, then_clause) ->
      "if ("^dump_logic cond^")\n\t"^(match then_clause with
      | TLIST lst -> "begin\n\t"^String.concat ";\n\t" (List.map dump_logic lst)^";\n\tend\n\t"
      | _ -> dump_logic then_clause)
  | QUADRUPLE(PARTSEL, arg1, arg2, arg3) -> dump_logic arg1^"["^dump_logic arg2^":"^dump_logic arg3^"]"
  | QUADRUPLE(IF, cond, then_clause, else_clause) ->
      "if ("^dump_logic cond^")\n\t"^(match then_clause with
      | TLIST lst -> "begin\n\t"^String.concat ";\n\t" (List.map dump_logic lst)^";\n\tend"
      | _ -> dump_logic then_clause)^"\n\telse "^(match else_clause with
      | TLIST lst -> "begin\n\t"^String.concat ";\n\t" (List.map dump_logic lst)^";\n\tend"
      | _ -> dump_logic else_clause)^"\n"
  | QUADRUPLE(QUERY, cond, then_clause, else_clause) -> (match cond with
      | BINNUM "1'b0" -> dump_logic else_clause
      | _ -> "("^dump_logic cond^") ? ("^dump_logic then_clause^") : ("^dump_logic else_clause^")")
  | QUADRUPLE ((ASSIGNMENT|DLYASSIGNMENT) as asgn, qout, EMPTY, dat) ->
      dump_logic qout^dump_logic asgn^dump_logic dat
  | HEXNUM str -> str
  | DECNUM str -> str
  | OCTNUM str -> str
  | BINNUM str -> str
  | ASCNUM str -> str
  | INT n -> string_of_int n
  | ID id -> escaped id
  | WIDTHNUM args -> widthnum args
  | EMPTY -> ""
  (* synthesizable codes *)
  | TRIPLE(PLUS, arg1, arg2) -> "("^dump_logic arg1^" + "^dump_logic arg2^")"
  | TRIPLE(MINUS, arg1, arg2) -> "("^dump_logic arg1^" - "^dump_logic arg2^")"
  | TRIPLE(TIMES, arg1, arg2) -> "("^dump_logic arg1^" * "^dump_logic arg2^")"
  | TRIPLE(P_NOTEQUAL, arg1, arg2) -> "("^dump_logic arg1^" != "^dump_logic arg2^")"
  | TRIPLE(P_SLEFT, arg1, arg2) -> "("^dump_logic arg1^" << "^dump_logic arg2^")"
  | TRIPLE(P_SRIGHT, arg1, arg2) -> "("^dump_logic arg1^" >> "^dump_logic arg2^")"
  | TRIPLE(LESS, arg1, arg2) -> "("^dump_logic arg1^" < "^dump_logic arg2^")"
  | TRIPLE(P_ANDAND, arg1, arg2) -> "("^dump_logic arg1^" && "^dump_logic arg2^")"
  | TRIPLE(P_OROR, arg1, arg2) -> "("^dump_logic arg1^" || "^dump_logic arg2^")"
  | TRIPLE(P_GTE, arg1, arg2) -> "("^dump_logic arg1^" >= "^dump_logic arg2^")"
  | TRIPLE(GREATER, arg1, arg2) -> "("^dump_logic arg1^" > "^dump_logic arg2^")"
  | TRIPLE(WILDEQUAL, arg1, arg2) -> "("^dump_logic arg1^" === "^dump_logic arg2^")"
  | DOUBLE(CONCAT, TLIST clst) -> "{"^String.concat "," (List.map dump_logic clst)^"}"
  | QUADRUPLE(P_SRIGHT, arg1, shft, INT w1) -> "(("^dump_logic arg1^" >> "^dump_logic shft^")) & "^string_of_int((1 lsl w1)-1)
  | QUADRUPLE((CASE|CASEZ|CASEX) as tok, expr, EMPTY, TLIST cselst) ->
    (Ord.getstr tok)^" ("^dump_logic expr^
    ")\n\t"^String.concat "\n\t" (List.map dump_logic cselst)^"\n\tendcase"
  | TRIPLE(CASECOND, TLIST condlst, TLIST cseexpr) ->
    String.concat ",\n\t" (List.map dump_logic condlst)^": "^
      "\n\tbegin "^String.concat "\n\t;" (List.map dump_logic cseexpr)^"; end"
  | DOUBLE(DEFAULT, TLIST cseexpr) ->
    "\n\tdefault:\n\tbegin "^String.concat "\n\t;" (List.map dump_logic cseexpr)^"; end"
  | TRIPLE(FUNCREF, ID fid, TLIST arglst) ->
    fid.id^"("^String.concat ", " (List.map dump_logic arglst)^")"
  | TRIPLE(D_DISPLAY, ASCNUM str, TLIST arglst) -> "\n\t$display \"^str^\"\n"
    ^String.concat ", " (List.map dump_logic arglst)^")"
  | TRIPLE(D_DISPLAY, ASCNUM str, EMPTY) -> sprintf "\n\t$display \"%s\"\n" str
  | TLIST items -> "begin "^String.concat "; " (List.map dump_logic items)^"; end"
  | TRIPLE(CONCAT, repeat, TLIST signals) -> "{"^dump_logic repeat^"{"^String.concat "," (List.map dump_logic signals)^"}}"
  | TRIPLE(ASSIGNMENT, dst, src) -> dump_logic dst^"="^dump_logic src
  | QUINTUPLE(FOR, start, limit, step, TLIST stmts) ->
      "for ("^dump_logic start^"; "^dump_logic limit^"; "^dump_logic step^")\nbegin "^String.concat "; " (List.map dump_logic stmts)^"; end"
  | other -> unhandled stderr 74 other; "unknown"

and dump_logic expr = Stack.push (115, expr) stk; let str = dump_logic' expr in ignore (Stack.pop stk); str

let conc_dump arg1 arg2 arg3 arg4 = arg1^String.concat arg2 (List.map dump_logic arg3)^arg4

let rec concat_combinable opt = function
  | TRIPLE (BITSEL, ID bit1, INT idx1) :: TRIPLE (BITSEL, ID bit2, INT idx2) :: lst -> bit1=bit2 && idx1=idx2+1 && concat_combinable (Some idx1) (TRIPLE (BITSEL, ID bit2, INT idx2) :: lst)
  | TRIPLE (BITSEL, ID bit1, INT idx1) :: [] -> opt <> None
  | [] -> opt <> None
  | _ -> false

let rec concat_combine opt = function
  | TRIPLE (BITSEL, ID bit1, INT idx1) :: lst -> (match opt with
    | None -> concat_combine (Some idx1) lst
    | Some idx ->
	if lst <> [] then concat_combine opt lst else
	sprintf "%s[%d:%d]" (escaped bit1) idx idx1)
  | oth -> unhandled stderr 138 (TLIST oth); ""

let rec dump_flat_netlist (porthash:('a, 'b) Hashtbl.t option) syms comment = function
  | DOUBLE(AT, TLIST items) -> "@* "^dump_flat_netlist porthash syms comment (TLIST items)
  | DOUBLE(DOUBLE(AT, TLIST lst), stmts) ->
    sprintf "\n@(%s)\n\t%s\n"
      (String.concat " or " (List.map dump_logic lst))
      (match stmts with
      | TLIST lst -> conc_dump "begin\n\t" ";\n\t" lst ";\n\tend"
      | CASE -> dump_logic stmts
      | _ -> dump_logic stmts^";")
  | DOUBLE((INITIAL|FINAL|ALWAYS) as tok, TLIST items) ->
    sprintf "%s\n\t" (Ord.getstr tok)^dump_flat_netlist porthash syms comment (TLIST items)
  | DOUBLE((TABLE|SPECIFY|GENERATE), items) as oth -> unhandled stderr 149 oth; ""
  | TRIPLE(GENVAR, arg1, TLIST arg3) as oth -> unhandled stderr 150 oth; ""
  | TRIPLE(ASSIGN, dly, TLIST assignlist) -> dump_flat_netlist porthash syms comment (TLIST assignlist)
  | TRIPLE(ASSIGNMENT, dst, src) -> sprintf "\nassign %s = %s;" (dump_logic dst) (dump_logic src)
  | TRIPLE(BUF, EMPTY, TLIST[QUADRUPLE(exp1, SCALAR, exp2, exp3)]) ->
    "\nbuf\t"^dump_logic' exp1^" ("^dump_logic exp2^", "^dump_logic exp3^");"
  | TRIPLE((BUF|NOT|AND|OR|XOR|NAND|NOR|XNOR|PULLUP|NMOS|PMOS|TRAN), dly, TLIST instances) as oth -> unhandled stderr 153 oth; ""
  | TRIPLE((BUFIF lev|NOTIF lev|TRANIF lev), weaklist, TLIST instances) as oth -> unhandled stderr 154 oth; ""
  | TRIPLE(IF, _, _) as tif -> dump_logic tif
  | QUADRUPLE(IF, _, _, _) as qif -> dump_logic qif
  | QUADRUPLE(ASSIGNMENT, dst, EMPTY, src) -> sprintf "\nassign %s = %s;" (dump_logic dst) (dump_logic src)
(*
  | SEXTUPLE(PARAMETER, signing, range, ID id, attr, expr) ->
    let delim = ref "" in
    "\nparameter "^
    dump_flat_netlist porthash syms "" range ^
    sprintf "%s%s = %s;\n" !delim (escaped id) (dump_logic expr)
*)
  | QUINTUPLE((INPUT|OUTPUT|INOUT) as tok, arg1, arg2, arg3, TLIST arg4) ->
    let xreg = (match arg1 with 
      | EMPTY -> ""
      | REG -> " reg"
      | _ -> unhandled stderr 433 arg1; "") in
    (match arg2 with 
      | EMPTY -> ()
      | _ -> unhandled stderr 436 arg2);
    (match porthash with None -> () | Some tbl -> List.iter (function
      | TRIPLE (ID id, EMPTY, EMPTY) -> Hashtbl.replace tbl id
	    (Ord.getstr tok ^ xreg ^ dump_flat_netlist porthash syms comment arg3 ^ " " ^ (escaped id) ^ ";\n")
      | x -> unhandled stderr 440 x) arg4);
      ""
  | QUADRUPLE((WIRE|REG|TRI0|TRI1|SUPPLY0|SUPPLY1|REAL|INTEGER|EVENT) as tok, arg0, arg1, TLIST arg4) as itm ->
    let noredundant = List.filter (function
      | QUADRUPLE(ID id, EMPTY, _, EMPTY)
      | DOUBLE (ID id, _) | TRIPLE (ID id, _, _) ->
        if Const.shash_chain_mem syms id
        then let myset = (Const.shash_chain_find syms id).Vparser.symattr in
             not (Vparser.TokSet.mem INPUT myset or (Vparser.TokSet.mem OUTPUT myset && (tok <> REG)))
        else true
      | oth -> unhandled stderr 445 oth; true) arg4 in
    if noredundant <> [] then
      begin
        let delim=ref "\t" and tab = ref 0 in
        let indent len : string =
          tab := !tab + len + 2;
          if !tab > 72 then (let i = !delim^"\n\t" in tab := 0; delim := ""; i) else !delim^" " in
        (match arg0 with 
          | REG -> ()
          | EMPTY -> ()
          | _ -> unhandled stderr 447 itm);
        sprintf "\n%s " (Ord.getstr tok)^
        (match arg1 with 
          | EMPTY -> ""
          | RANGE _ -> dump_flat_netlist porthash syms comment arg1;
          | TRIPLE(EMPTY, rng, EMPTY) -> dump_flat_netlist porthash syms comment rng;
          | _ -> unhandled stderr 450 arg1; "") ^
        String.concat "" (List.map (function
          | QUADRUPLE(ID id, EMPTY, _, EMPTY)
          | DOUBLE (ID id, _) | TRIPLE (ID id, _, _) ->
            let it = indent (String.length id.id) ^ escaped id in
	    delim := ","; it
          | oth -> unhandled stderr 454 oth; "") noredundant) ^
        sprintf ";\n"
      end
    else ""
  | QUADRUPLE(CASE, expr, EMPTY, TLIST cselst) ->
    sprintf "case (%s)\n\t%s\n\tendcase\n" (dump_logic expr)
    (String.concat "\n\t" (List.map dump_logic cselst))
  | QUADRUPLE((MODINST|PRIMINST), ID kind, params, TLIST arg3) ->
    let delim = ref "" in
    sprintf "\n%s " (escaped kind) ^
    (match params with
      | DOUBLE(HASH, TLIST paramlst) -> "#("^String.concat ", " (List.map (function 
          | INT n -> string_of_int n
          | oth -> unhandled stderr 217 oth; "") paramlst)^")"
      | EMPTY -> ""
      | oth -> unhandled stderr 219 oth; "") ^
    String.concat "" (List.map (fun x -> let outer = match x with
      | TRIPLE (ID id, SCALAR, TLIST arg4) -> 
        let delim2 = ref "\n\t" in
        sprintf "%s\n\t%s (" !delim (escaped id) ^
	String.concat "" (List.map (fun y -> let lst = match y with
          | TRIPLE (CELLPIN, ID cellpin, ID conn) -> if Const.shash_chain_mem syms conn then
              begin
                let (hi,lo,inc) = Minimap.find_width conn syms in
                if inc = 0 then
                  sprintf "%s.%s(%s)" !delim2 cellpin.id (escaped conn)
                else if hi=lo then
                  sprintf "%s.%s(%s[%d])" !delim2 cellpin.id (escaped conn) hi
                else
                  sprintf "%s.%s(%s[%d:%d])" !delim2 cellpin.id (escaped conn) hi lo
              end
            else
              sprintf "%s.%s(%s)" !delim2 cellpin.id (escaped conn)              
          | TRIPLE (CELLPIN, ID cellpin, TRIPLE (BITSEL, ID bit, INT idx1)) ->
            sprintf "%s.%s(%s[%d])" !delim2 cellpin.id (escaped bit) idx1
          | TRIPLE (CELLPIN, ID cellpin, DOUBLE(CONCAT, TLIST clst)) ->
	      if concat_combinable None clst then sprintf "%s.%s(%s)" !delim2 cellpin.id (concat_combine None clst) else 
	      let delim3 = ref "" in
	      sprintf "%s.%s({" !delim2 cellpin.id ^
              String.concat "" (List.map (fun z -> let lst1 = match z with
              | TRIPLE (BITSEL, ID bit, INT idx1) -> sprintf "%s\n%s[%d]" !delim3 (escaped bit) idx1
              | ID id -> sprintf "%s%s" !delim3 (escaped id)
              | WIDTHNUM args -> sprintf "%s%s" !delim3 (widthnum args)
              | BINNUM arg -> sprintf "%s%s" !delim3 arg
              | QUADRUPLE(PARTSEL, ID part, thi, tlo) ->
                  let lft = Const.exprConstStr stderr syms thi
                  and rght = Const.exprConstStr stderr syms tlo in
                  if !verbose then printf "Part Select %s:%s\n" lft rght;
                  sprintf "%s%s[%s:%s]" !delim3 (escaped part) lft rght
	      | DOUBLE(CONCAT, TLIST lst') ->
		  if concat_combinable None lst' then !delim3^concat_combine None lst'
		  else String.concat "" (let lst'' = List.map (function
		    | TRIPLE(BITSEL, ID id', INT sel) -> sprintf "%s\n%s[%d]" !delim3 (escaped id') sel
		    | oth -> unhandled stderr 257 oth; "") lst' in delim3 := ","; lst'')
              | _ -> unhandled stderr 258 z; "" in delim3 := ","; lst1) clst) ^ "})"
          | TRIPLE (CELLPIN, ID cellpin, expr) -> sprintf "%s.%s(%s)" !delim2 cellpin.id (dump_logic expr)
          | DOUBLE(CELLPIN, ID cellpin) -> sprintf "%s.%s()" !delim2 cellpin.id
          (* deprecated connect by position *)
          | ID id -> sprintf "%s%s" !delim2 id.id
          | BINNUM lev -> sprintf "%s%s" !delim2 lev
          | TRIPLE((BITSEL|AND|OR),_,_)
          | QUADRUPLE((PARTSEL),_,_,_) -> sprintf "%s%s" !delim2 (dump_logic y)
          | _ -> unhandled stderr 344 y; "" in delim2 := ",\n\t"; lst) (List.sort compare arg4)) ^ sprintf ")"
      | _ -> unhandled stderr 345 x; "" in delim := ","; outer) arg3) ^ sprintf ";\n"
  | QUINTUPLE(MODULE,ID arg1, TLIST arg2, TLIST arg3, THASH targ4) ->
      let itmlst = ref [] and sortlst = ref [] and porthash = Some (Hashtbl.create 256) in
      Hashtbl.iter (fun itm _ -> sortlst := itm :: !sortlst) (snd targ4);
      List.iter (fun itm -> itmlst := dump_flat_netlist porthash syms comment itm :: !itmlst) (List.sort compare !sortlst);
      let head = dump_flat_header porthash syms comment arg1 arg2 arg3 (hfilter (function _ -> true) (fst targ4)) in
      let conc = ref [] in
      (match porthash with None -> () | Some tbl -> Hashtbl.iter (fun k x -> conc := x :: !conc) tbl);
      head ^
      String.concat "" !conc ^
      String.concat "" (List.sort compare !itmlst) ^
      sprintf "\nendmodule\n\n"
  | QUINTUPLE(PRIMITIVE,ID arg1, EMPTY, TLIST primargs, TLIST arg4) as oth -> unhandled stderr 293 oth;""
  | SEPTUPLE(TASK, EMPTY, ID taskname, EMPTY, TLIST args, TLIST stmts, EMPTY) ->
      sprintf "       task %s;\n" taskname.id^
      sprintf "\n"^
      dump_flat_netlist porthash syms comment (TLIST args)^
      sprintf "      \n"^
      sprintf "      begin\n"^
      dump_flat_netlist porthash syms comment (TLIST stmts)^
      sprintf "      ;\n      end\n"^
      sprintf "\n"^
      sprintf "   endtask // %s\n" taskname.id
  | OCTUPLE(FUNCTION, EMPTY, range, ID funcname, EMPTY, TLIST args, TLIST stmts, EMPTY) ->
      sprintf "   function "^
      dump_flat_netlist porthash syms comment range^
      sprintf " %s;\n" funcname.id^
      sprintf "\n"^
      dump_flat_netlist porthash syms comment (TLIST args)^
      sprintf "      \n"^
      sprintf "      begin\n"^
      dump_flat_netlist porthash syms comment (TLIST stmts)^
      sprintf "      ;\n      end\n"^
      sprintf "\n"^
      sprintf "   endfunction // %s\n" funcname.id
  | RANGE(arg1, arg2) -> "["^dump_logic arg1^":"^dump_logic arg2^"]"
  | TLIST items -> String.concat "" (List.map (dump_flat_netlist porthash syms comment) items)
  | ASCNUM str -> sprintf "// %s" str
  | EMPTY -> ""
  | err -> unhandled stderr 359 err; Ord.getstr err
	
and dump_flat_header porthash syms comment arg1 arg2 arg3 arglst =
    let itmlst = ref [] and merge = Hashtbl.create 256 in
    let delim = ref "(" and wid = ref 0 in
    List.iter (function
    | QUADRUPLE(WIRE as tok, EMPTY, TRIPLE (EMPTY, rng, EMPTY), TLIST lst)
    | QUINTUPLE((INPUT|OUTPUT|INOUT) as tok, EMPTY, EMPTY, rng, TLIST lst) ->
      if Hashtbl.mem merge (tok,rng) then
	Hashtbl.replace merge (tok,rng) (Hashtbl.find merge (tok,rng) @ lst)
      else
	Hashtbl.add merge (tok,rng) lst
    | itm -> itmlst := (dump_flat_netlist porthash syms comment itm) :: !itmlst) arglst;
    Hashtbl.iter (fun (tok,rng) lst -> let itm = match tok with
    | WIRE -> QUADRUPLE(tok, EMPTY, TRIPLE (EMPTY, rng, EMPTY), TLIST (List.sort compare lst))
    | INPUT | OUTPUT | INOUT -> QUINTUPLE(tok, EMPTY, EMPTY, rng, TLIST (List.sort compare lst))
    | _ -> unhandled stderr 328 tok; EMPTY in
    itmlst := (dump_flat_netlist porthash syms comment itm) :: !itmlst) merge;
    let delim = ref "#(parameter " in
    let params = String.concat "" (List.map (function
      | SEXTUPLE(PARAMETER, signing, range, ID id, attr, expr) ->
	  dump_flat_netlist porthash syms "" range ^
	  (let exp = !delim ^ escaped id ^" = "^ dump_logic expr in
	      wid := !wid + String.length exp + 1;
	      if !wid >= 80 then (wid := 9; delim := ",\n\t") else delim := ",";exp)
      | oth -> unhandled stderr 353 oth; "") arg2) in
    let params = params^(if !delim.[0] = ',' then ")\n\t" else "") in
    delim := "(";
    let iolst = List.map (function
      | QUINTUPLE((INPUT|OUTPUT|INOUT), (EMPTY|REG), EMPTY, (EMPTY|RANGE(_,_)), DOUBLE(ID id, EMPTY))
      | ID id -> escaped id
      | x -> unhandled stderr 338 x; "unhand") arg3 in
    let args = String.concat "" (
      List.map (fun arg ->
	let io = !delim^arg in
	wid := !wid + String.length arg + 1;
	if !wid >= 80 then (wid := 9; delim := ",\n\t") else delim := ","; io) (List.sort compare iolst)) in
    "// "^comment^"\n\nmodule "^arg1.id^" "^params^" " ^ args ^ if !delim = "(" then "();" else ");\n"^
    String.concat "" (List.sort compare !itmlst)

let write_verilog_arch' arch nam file arg =
  printf "Writing module %s arch %s to file %s\n" nam arch file;
  let lst = ref [] in
  Minimap.recurse_arch (fun id subarg ->
    lst := dump_flat_netlist None subarg.Globals.symbols ("sub-arch "^arch^" sub-module "^id^" - "^subarg.comment) subarg.tree :: !lst) arch nam arg;
  let oc = unique_open file in
  List.iter (output_string oc) (List.sort compare !lst);
  close_out oc

let write_verilog_arch arch nam =
  let lst = List.filter (fun arg -> arg.arch = arch) (Hashtbl.find_all modprims nam) in
  match lst with
    | [] -> failwith ("No netlists matched arch "^arch^" name "^nam)
    | _ -> List.iter (write_verilog_arch' arch nam (nam^"."^arch)) lst

let write_verilog_arch_nohier arch nam =
  let lst = List.filter (fun arg -> arg.arch = arch) (Hashtbl.find_all modprims nam) in
  match lst with
    | [] -> failwith ("No netlists matched arch "^arch^" name "^nam)
    | _ -> List.iter (fun subarg ->
	let oc = unique_open (nam^"."^arch) in
	output_string oc (dump_flat_netlist None subarg.Globals.symbols ("sub-arch "^arch^" sub-module "^nam^" - "^subarg.comment) subarg.tree);
	close_out oc) lst

let write_verilog_one arg outfile comment =
  let fd = open_out outfile in
  output_string fd (dump_flat_netlist None arg.symbols comment arg.tree);
close_out fd

let mktmpdir lib =
  let pid = string_of_int (Unix.getpid()) in
  let mask = (-1-(Unix.umask 0)) in
  Unix.mkdir pid mask;
  (try Unix.rename lib (pid^"/prev") with Unix.Unix_error _ -> print_endline ("failed renaming "^lib^" to "^pid));
  Unix.rename pid lib;
  mask
	    
let write_verilog_all lib =
  let mask = mktmpdir lib in
  let used = ref [] in
  Hashtbl.iter (fun id arg ->
    if not (List.mem arg.arch !used) then
      begin
	Unix.mkdir (lib^"/"^arg.arch) mask;
	used := arg.arch :: !used
      end;
    let tm = Unix.localtime arg.datestamp in
    let datestr = sprintf "%.2d%.2d%.2d%.2d%.2d%.2d" (tm.Unix.tm_year+1900) tm.Unix.tm_mon tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec in
    let outfile = lib^"/"^arg.arch^"/"^id ^ "." ^ arg.arch ^ "." ^ datestr in
    if !verbose then print_endline outfile;
    write_verilog_one arg outfile "write_verilog_all") modprims
