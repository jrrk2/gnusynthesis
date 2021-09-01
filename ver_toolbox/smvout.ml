(*
    <v2smv - Verilog converter to smv format.>
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
open Printf
open Idhash
open Vparser
open List
open Dump

let instcnt = ref 0;;
let namtab = Hashtbl.create 256;;

let mask n0 = if n0 = 0 then 1 else let l = ref 0 and n = ref n0 in while (!n > 0) do n := !n lsr 1; l := !l + 1; done; !l

let bin n0 = let b = ref "0" and n = ref n0 in while (!n > 0) do b := !b^(if !n mod 2 = 1 then "1" else "0"); n := !n lsr 1; done; !b

let uniq olds s = let cnt = ref 0 and u = ref s and matched = ref (Hashtbl.mem namtab olds) in
  while not !matched && Hashtbl.mem namtab !u do
    matched := Hashtbl.find namtab !u = olds;
    Printf.printf "%s -> %s\n" !u (if !matched then "true" else "false");
    if not !matched then
      (incr cnt; u := s^"_"^(string_of_int !cnt))
  done;
  if not !matched then (Hashtbl.replace namtab olds !u);
  Hashtbl.find namtab olds

let valid s' = let s = s'.id in
let l = String.length s and truncate = ref false in
let v = String.create l in for i = 0 to l-1 do match s.[i] with
| '_' -> v.[i] <- s.[i];
| 'A'..'Z' -> v.[i] <- s.[i];
| 'a'..'z' -> v.[i] <- s.[i];
| '0'..'9' -> if i > 0 then v.[i] <- s.[i] else v.[i] <- '_';
| '$' -> if i > 0 then v.[i] <- s.[i] else v.[i] <- '_';
| '#' -> if i > 0 then v.[i] <- s.[i] else v.[i] <- '_';
| '\\' -> if i > 0 then v.[i] <- s.[i] else v.[i] <- '_';
| '-' -> if i > 0 then v.[i] <- s.[i] else v.[i] <- '_';
| ' ' -> if i == l-1 then truncate := true else v.[i] <- '_';
| _ -> v.[i] <- '_';
done;
uniq s (Bytes.to_string (if !truncate then Bytes.sub v 0 (l-1) else v))

let uniq_tab () = Hashtbl.clear namtab;
List.iter (fun e -> Hashtbl.replace namtab e ("v"^e); Hashtbl.replace namtab ("v"^e) e)
["ABF";
"ABG";
"AF";
"AG";
"array";
"ASSIGN";
"A";
"AX";
"boolean";
"bool";
"BU";
"case";
"count";
"COMPASSION";
"COMPUTE";
"COMPWFF";
"CONSTANTS";
"CONSTRAINT";
"CTLSPEC";
"CTLWFF";
"DEFINE";
"EBF";
"EBG";
"EF";
"EG";
"esac";
"E";
"EX";
"FAIRNESS";
"FALSE";
"F";
"G";
"H";
"init";
"INIT";
"integer";
"INVARSPEC";
"INVAR";
"in";
"IN";
"ISA";
"IVAR";
"JUSTICE";
"LTLSPEC";
"LTLWFF";
"MAX";
"MIN";
"MODULE";
"mod";
"next";
"of";
"O";
"process";
"PSLSPEC";
"PSLWFF";
"real";
"self";
"SIMPWFF";
"SPEC";
"S";
"TRANS";
"TRUE";
"T";
"union";
"U";
"VAR";
"V";
"word1";
"word";
"xnor";
"xor";
"X";
"Y";
"Z"]

let is_mem syms id = match syms with
| EndShash -> false
| Shash {nxt;syms} -> if Hashtbl.mem syms id then TokSet.mem MEMORY (Hashtbl.find syms id).symattr else false

let defkind syms (id:idhash) = match syms with
| EndShash -> "DEFINE"
| Shash {nxt;syms} -> let sym = Hashtbl.find syms id in if TokSet.mem OUTPUT sym.symattr || TokSet.mem WIRE sym.symattr then "ASSIGN" else "DEFINE"

let find_dir syms id = match syms with
| EndShash -> EMPTY
| Shash {nxt;syms} -> let sym = Hashtbl.find syms id in if TokSet.mem OUTPUT sym.symattr then OUTPUT else if TokSet.mem INPUT sym.symattr then INPUT else EMPTY

let find_wire_width sym gsyms = match gsyms with
| EndShash -> (-1,-1,-1)
| Shash {nxt;syms} -> if Hashtbl.mem syms sym then
  (match (Hashtbl.find syms sym).width with
  | RANGE range -> Const.iwidth stderr gsyms (RANGE range)
  | SCALAR | EMPTY | UNKNOWN -> (0,0,0)
  | _ -> (-1,-1,-1))
else
  (-1,-1,-1)

let show_set oc s = let str = ref "" in TokSet.iter (fun e -> str := (str_token e)^" "^(!str)) s; !str;;

let sym_width syms sym = match sym.sigattr with
| Sigparam pexpr -> (match Const.exprConst stderr syms pexpr with
  | WIDTHNUM(radix,sz,num) -> sz
  | INT n -> mask n
  | _ -> 1)
| Sigarray x -> (match sym.width with
  | SCALAR -> 1
  | EMPTY -> 1
  | RANGE(INT lft, INT rght) -> (lft-rght+1)
  | RANGE(expr1, expr2) -> let lft = Const.exprConstStr stderr syms expr1 and
			       rght = Const.exprConstStr stderr syms expr2 in
			       ((int_of_string lft) - (int_of_string rght) + 1)
  | _ -> unhandled stderr 72 sym.width; 1)
| Signamed blk -> 1
| Sigfunc fn -> 1
| Sigtask tsk -> 1
| Sigundef -> 1

let sym_detail oc syms sym =
TokSet.iter (fun t -> match t with
| SUBCCT -> ()
| SUBMODULE -> ()
| INTEGER -> bprintf oc.(0) "\nVAR %s: word[32];" (valid sym.path)
| REG -> if not (TokSet.mem OUTPUT sym.symattr) then bprintf oc.(0) "\nVAR %s: word [%d];" (valid sym.path) (sym_width syms sym )
| IMPLICIT -> ()
| WIRE -> if not ((TokSet.mem INPUT sym.symattr) || (TokSet.mem OUTPUT sym.symattr)) then bprintf oc.(0) "\nVAR %s: word [%d];" (valid sym.path) (sym_width syms sym )
| MEMORY -> bprintf oc.(0) "\n-- VAR %s: array of --" (valid sym.path)
| IOPORT -> ()
| MODULE -> ()
| PARAMETER -> ()
| OUTPUT -> ()
| INPUT -> ()
| PARAMUSED -> ()
| TASK -> ()
| TASKUSED -> ()
| NAMED -> ()
| FUNCASSIGNED -> ()
| FUNCTION -> ()
| _ -> unhandled stderr 71 t) sym.symattr;;

let symwidth syms id = (match syms with
    | Shash symr -> if Hashtbl.mem symr.syms id then sym_width syms (Hashtbl.find symr.syms id) else 1
    | EndShash -> failwith "Empty symbol table")

let rec defn syms ch exp owidth:int = match exp with
      | INT n -> if owidth < 0 then (bprintf ch "%d " n; mask n)
                 else if n >= 0 then (let w = max owidth (mask n) in bprintf ch "0d%d_%d " w n; w)
                 else (let n1 = (1 lsl owidth-1) land n in bprintf ch "0d%d_%d " owidth n1; owidth)
      | HEXNUM str -> let radix = 16 in let (sz,num) = Const.widthnum stderr radix str in let w = max owidth sz in bprintf ch "0h%d_%x " w num; w
      | DECNUM str -> let radix = 10 in let (sz,num) = Const.widthnum stderr radix str in let w = max owidth sz in bprintf ch "0d%d_%d " w num; w
      | OCTNUM str -> let radix = 8 in let (sz,num) = Const.widthnum stderr radix str in let w = max owidth sz in bprintf ch "0o%d_%o " w num; w
      | BINNUM str -> let radix = 2 in let (sz,num) = Const.widthnum stderr radix str in let w = max owidth sz in bprintf ch "0b%d_%s " w (bin num); w
      | EMPTY -> bprintf ch "() "; 0
      | ID arg -> let w = symwidth syms arg in if w >= owidth then bprintf ch "%s " (valid arg)
          else bprintf ch "(0d%d_0 :: %s)" (owidth-w) (valid arg); max owidth w
      | DOUBLE(LPAREN, arg1) -> bprintf ch "( "; let w = defn syms ch arg1 owidth in bprintf ch ") "; w
      | DOUBLE(MINUS, arg1) -> bprintf ch "-"; defn syms ch arg1 owidth
      | DOUBLE(P_TILDE, arg1) -> bprintf ch "! "; defn syms ch arg1 owidth
      | DOUBLE(PLING, arg1) -> bprintf ch "! "; defn syms ch arg1 owidth
      | DOUBLE(P_CARET, arg1) -> bprintf ch "^ "; defn syms ch arg1 owidth
      | DOUBLE(P_NOR, arg1) -> bprintf ch "!(|("; ignore(defn syms ch arg1 owidth); bprintf ch ")) "; 1
      | TRIPLE(P_AMPERSAND, arg1, arg2) -> let w1 = defn syms ch arg1 owidth in bprintf ch "& "; let w2 = defn syms ch arg2 owidth in max w1 w2
      | TRIPLE(P_VBAR, arg1, arg2) -> bprintf ch "( "; let w1 = defn syms ch arg1 owidth in bprintf ch ") | ( ";
          let w2 = defn syms ch arg2 owidth in bprintf ch ") "; max w1 w2
      | TRIPLE(P_OROR, arg1, arg2) -> bprintf ch "( "; let w1 = defn syms ch arg1 owidth in bprintf ch ") | ( ";
          let w2 = defn syms ch arg2 owidth in bprintf ch ") "; max w1 w2
      | TRIPLE(P_ANDAND, arg1, arg2) -> bprintf ch "( "; let w1 = defn syms ch arg1 owidth in bprintf ch ") & ( ";
          let w2 = defn syms ch arg2 owidth in bprintf ch ") "; max w1 w2
      | TRIPLE(PLUS, ID arg1, INT arg2) -> let w = symwidth syms arg1 in bprintf ch "%s + 0d%d_%d " (valid arg1) w arg2; w
      | TRIPLE(PLUS, arg1, arg2) -> let w1 = defn syms ch arg1 owidth in bprintf ch "+ "; let w2 = defn syms ch arg2 (max w1 owidth) in max w1 w2
      | TRIPLE(MINUS, INT arg1, INT arg2) -> defn syms ch (INT (arg1-arg2)) owidth
      | TRIPLE(MINUS, arg1, arg2) -> let w1 = defn syms ch arg1 owidth in bprintf ch "- "; let w2 = defn syms ch arg2 owidth in max w1 w2
      | TRIPLE(TIMES, INT arg1, ID arg2) -> if symwidth syms arg2 > 1 then
          (let w1 = defn syms ch (INT arg1) owidth in bprintf ch "* "; let w2 = defn syms ch (ID arg2) owidth in max w1 w2)
        else
          (let w = max owidth (mask arg1) in bprintf ch "( bool( %s ) ? 0d%d_%d : 0d%d_0) " arg2.id w arg1 w; w)
      | TRIPLE(TIMES, arg1, arg2) -> let w1 = defn syms ch arg1 owidth in bprintf ch "* "; let w2 = defn syms ch arg2 owidth in max w1 w2
      | TRIPLE(DIVIDE, arg1, arg2) -> let w1 = defn syms ch arg1 owidth in bprintf ch "/ "; let w2 = defn syms ch arg2 owidth in max w1 w2
      | TRIPLE(P_SLEFT, arg1, arg2) -> let w1 = defn syms ch arg1 owidth in bprintf ch "<< "; let w2 = defn syms ch arg2 owidth in max w1 w2
      | TRIPLE(P_SRIGHT, arg1, arg2) -> let w1 = defn syms ch arg1 owidth in bprintf ch ">> "; let w2 = defn syms ch arg2 owidth in max w1 w2
      | TRIPLE(P_CARET, arg1, arg2) -> let w1 = defn syms ch arg1 owidth in bprintf ch "xor "; let w2 = defn syms ch arg2 owidth in max w1 w2
      | TRIPLE(P_EQUAL, ID arg1, INT arg2) -> let w = symwidth syms arg1 in bprintf ch "word1(%s = 0d%d_%d) " arg1.id w arg2; 1
      | TRIPLE(P_EQUAL, arg1, arg2) -> bprintf ch "word1( "; ignore(defn syms ch arg1 owidth); bprintf ch "= "; ignore(defn syms ch arg2 owidth); bprintf ch ") "; 1
      | TRIPLE(P_CASEEQUAL, arg1, arg2) -> bprintf ch "word1( "; ignore(defn syms ch arg1 owidth); bprintf ch "=== "; ignore(defn syms ch arg2 owidth); bprintf ch ") "; 1
      | TRIPLE(P_NOTEQUAL, arg1, arg2) -> bprintf ch "word1( "; ignore(defn syms ch arg1 owidth); bprintf ch "!= "; ignore(defn syms ch arg2 owidth); bprintf ch ") "; 1
      | TRIPLE(P_CASENOTEQUAL, arg1, arg2) -> bprintf ch "word1( "; ignore(defn syms ch arg1 owidth); bprintf ch "!== "; ignore(defn syms ch arg2 owidth); bprintf ch ") "; 1
      | TRIPLE(P_GTE, ID arg1, INT arg2) -> let w = symwidth syms arg1 in bprintf ch "word1(%s >= 0d%d_%d) " (valid arg1) w arg2; 1
      | TRIPLE(P_GTE, arg1, arg2) -> bprintf ch "word1( "; let w1 = defn syms ch arg1 owidth in bprintf ch ">= "; ignore(defn syms ch arg2 (max w1 owidth)); bprintf ch ") "; 1
      | TRIPLE(P_LTE, ID arg1, INT arg2) -> let w = symwidth syms arg1 in bprintf ch "word1(%s <= 0d%d_%d) " (valid arg1) w arg2; 1
      | TRIPLE(P_LTE, arg1, arg2) -> bprintf ch "word1( "; ignore(defn syms ch arg1 owidth); bprintf ch "<= "; ignore(defn syms ch arg2 owidth); bprintf ch ") "; 1
      | TRIPLE(GREATER, ID arg1, INT arg2) -> let w = symwidth syms arg1 in bprintf ch "word1( %s > 0d%d_%d) " (valid arg1) w arg2; w
      | TRIPLE(GREATER, arg1, arg2) -> bprintf ch "word1( "; ignore(defn syms ch arg1 owidth); bprintf ch "> "; ignore(defn syms ch arg2 owidth); bprintf ch ") "; 1
      | TRIPLE(LESS, arg1, arg2) -> bprintf ch "word1( "; ignore(defn syms ch arg1 owidth); bprintf ch "< "; ignore(defn syms ch arg2 owidth); bprintf ch ") "; 1
      | TRIPLE(BITSEL, ID id, DOUBLE(LPAREN,sel)) | TRIPLE(BITSEL, ID id, sel) -> if is_mem syms id then
          (bprintf ch "%s[" (valid id); ignore(defn syms ch sel (-1)); bprintf ch "] "; 1)
        else
          (idxtrans syms ch id sel sel; 1)
      | QUADRUPLE(PARTSEL, ID id, INT hi, INT lo) -> bprintf ch "%s[%d:%d] " (valid id) hi lo; hi-lo+1
      | QUADRUPLE(PARTSEL, ID id, hi, lo) -> let lft = Const.exprConstStr stderr syms hi and
			       rght = Const.exprConstStr stderr syms lo in
			       bprintf ch "%s[%s:%s] " (valid id) lft rght;
                               ((int_of_string lft) - (int_of_string rght) + 1)
      | QUADRUPLE(QUERY, arg1, arg2, arg3) -> bprintf ch "bool( "; ignore(defn syms ch arg1 1); bprintf ch ") ?\n( "; 
          let w2 = defn syms ch arg2 owidth in bprintf ch ") :\n( ";
          let w3 = defn syms ch arg3 owidth in bprintf ch ") "; max w2 w3
(* behavioural codes *)
      | TLIST lst -> List.iter (fun arg -> ignore(defn syms ch arg 0)) lst; owidth
      | DOTTED lst -> let delim = ref "" in List.iter (
          fun arg -> bprintf ch "%s " !delim; delim := ".";
          match arg with
            | ID id -> bprintf ch "%s" id.id;
            | TRIPLE (BITSEL, ID mem, addr) -> bprintf ch "%s[" mem.id; ignore(defn syms ch addr 1); bprintf ch "]";
            | _ -> unhandled stderr 157 arg) lst; owidth
      | DOUBLE(D_STOP, EMPTY) -> bprintf ch "$stop;"; owidth
      | DOUBLE(D_DISPLAY, EMPTY) -> bprintf ch "$display;"; owidth
      | DOUBLE(DOUBLE (AT, TLIST [DOUBLE ((NEGEDGE|POSEDGE) as dir, ID clk)]), lste) -> bprintf ch "@%s %s " (Ord.getstr dir) (valid clk);
          (match lste with EMPTY -> () | TLIST lst -> List.iter (fun arg -> ignore(defn syms ch arg 0)) lst | _ -> ()); owidth
      | DOUBLE(DEFAULT, itm) -> bprintf ch "\n-- default "; defn syms ch itm 0
      | TRIPLE(CASECOND, TLIST lst, itm) -> bprintf ch "\n-- casecond "; List.iter (fun arg ->ignore(defn syms ch arg 0)) lst; 
          defn syms ch itm 0
      | TRIPLE(D_READMEMB, ASCNUM fmt, ID prg) -> bprintf ch "$readmemb(%s,%s); " fmt prg.id; 1
      | TRIPLE(D_FOPEN, ID fd, ASCNUM nam) -> bprintf ch "%s = $fopen(%s); " fd.id nam; 1
      | TRIPLE(D_DISPLAY, ASCNUM fmt, disp) -> bprintf ch "$display %s " fmt; (match disp with
          | TLIST lst -> List.iter (fun arg ->ignore(defn syms ch arg 0)) lst; bprintf ch "\n-- "
          | EMPTY -> ()
          | _ -> unhandled stderr 269 disp); owidth
      | TRIPLE(D_WRITE, ASCNUM fmt, wrt) -> bprintf ch "$write %s " fmt; (match wrt with
          | TLIST lst -> List.iter (fun arg ->ignore(defn syms ch arg 0)) lst; bprintf ch "\n-- "
          | EMPTY -> ()
          | _ -> unhandled stderr 269 wrt); owidth
      | TRIPLE(IF, expr, thenc) -> bprintf ch "if "; ignore(defn syms ch expr 0); ignore(defn syms ch thenc 0); owidth
      | QUADRUPLE(IF, expr, thenc, elsec) -> bprintf ch "if "; ignore(defn syms ch expr 0);
          ignore(defn syms ch thenc 0);
          ignore(defn syms ch thenc 0); owidth
      | QUADRUPLE((CASE|CASEZ|CASEX), expr, EMPTY, TLIST lst) -> bprintf ch "case "; ignore(defn syms ch expr 0);
          List.iter (fun arg ->ignore(defn syms ch arg 0)) lst; owidth
      | QUADRUPLE(ASSIGNMENT, target, EMPTY, expr) -> ignore(defn syms ch target 0);
          bprintf ch "= "; ignore(defn syms ch expr 0); bprintf ch "\n-- "; 1
      | QUINTUPLE(FOR, TRIPLE(ASSIGNMENT,ID idstart, start), test, TRIPLE(ASSIGNMENT,ID idinc,inc), lste) ->
          bprintf ch "for ( %s = " (valid idstart); ignore(defn syms ch start 0); bprintf ch "; "; ignore(defn syms ch test 0); 
          bprintf ch "; %s = " (valid idinc); ignore(defn syms ch inc 0); bprintf ch ") ";
          (match lste with EMPTY -> () | TLIST lst -> List.iter (fun arg -> ignore(defn syms ch arg 0)) lst | _ -> ()); owidth
      | DOUBLE(DOUBLE (HASH, FLOATNUM f), stmt) -> bprintf ch "#%f " f;
          defn syms ch stmt 0
      | TRIPLE(D_FDISPLAY, ID fd, TLIST lst) -> bprintf ch "$fdisplay %s " fd.id;
          List.iter (fun arg -> ignore(defn syms ch arg 0)) lst; owidth
      | ASCNUM str -> bprintf ch "%s" str; owidth
      | DOUBLE(CONCAT, TLIST lst2) -> let delim = ref "(" in List.iter (fun arg -> bprintf ch "%s" !delim; delim := ") :: ("; ignore(defn syms ch arg 0)) lst2;
          bprintf ch ") "; owidth
      | TRIPLE(CONCAT, INT cnt, TLIST lst2) -> let delim = ref "(" in for i = 1 to cnt do List.iter (fun arg -> bprintf ch "%s" !delim; delim := ") :: ("; ignore(defn syms ch arg 0)) lst2 done;
          bprintf ch ") "; owidth
      | TRIPLE (FUNCREF, ID funcid, TLIST flst) -> bprintf ch "\n-- funcref %s " funcid.id; List.iter (fun arg ->ignore(defn syms ch arg 0)) flst;
          owidth
      | TRIPLE (TASKREF, ID taskid, EMPTY) -> bprintf ch "\n-- taskref %s() " taskid.id; owidth
      | TRIPLE (TASKREF, ID taskid, TLIST flst) -> bprintf ch "\n-- taskref %s " taskid.id; List.iter (fun arg ->ignore(defn syms ch arg 0)) flst;
          owidth
      | _ -> unhandled stderr (-(fst (Stack.top stk))) (DOUBLE(exp, snd (Stack.top stk))); 1

and idxtrans syms ch id sel1 sel2 = let (hi,lo,inc) = find_wire_width id syms in
  let sel1m = if (lo > 0) then TRIPLE(MINUS,sel1,INT lo) else sel1 in
  let sel2m = if (lo > 0) then TRIPLE(MINUS,sel2,INT lo) else sel2 in
  bprintf ch "%s[" (valid id);
  ignore(defn syms ch sel1m (-1));
  bprintf ch ": ";
  ignore(defn syms ch sel2m (-1));
  bprintf ch "] "

let dpartsel syms ch id hi lo exp =
  let sym = (match syms with
    | Shash symr -> Hashtbl.find symr.syms id
    | EndShash -> failwith "Empty symbol table") in
match sym.sigattr with
| Sigarray attrs -> (
match sym.width with
| RANGE range -> let (left, right, inc) = Const.iwidth stderr syms sym.width in
  if not ((TokSet.mem IMPLICIT sym.symattr)||(TokSet.mem MEMORY sym.symattr)) then
  ( let i = ref hi in try while (if inc > 0 then !i <= lo else !i >= lo) do
    if (!i > left) || (!i < right) then bprintf ch "Trying to access %s[%d:%d] with index [%d]\n" (valid sym.path) left right !i
    else (
      TokSet.iter (fun l -> match l with TLIST _ -> bprintf ch "Trying to multiply define index [%d] of %s[%d:%d]\n" !i (valid sym.path) left right | _ -> ()) attrs.(!i);
      attrs.(!i) <- TokSet.add (if !i = hi then TLIST [exp] else TLIST [EMPTY]) attrs.(!i));
    i := !i + inc
    done
  with Invalid_argument("index out of bounds") -> bprintf ch "Trying to access %s with index [%d:%d]\n" (valid sym.path) left right);
  ( let i = ref left and complete = ref true in (try while (if inc > 0 then !i <= right else !i >= right) do
    (let subexp = ref VOID in
    TokSet.iter (fun l -> match l with TLIST [e] -> subexp := e | _ -> ()) attrs.(!i); if !subexp = VOID then complete := false);
    i := !i + inc
    done
  with Invalid_argument("index out of bounds") -> bprintf ch "Trying to access %s with index [%d:%d]\n" (valid sym.path) left right);
  if !complete then
  ( bprintf ch "%s %s := " (defkind syms id) (valid id);
    let i = ref left in let delim = ref "( " in while (if inc > 0 then !i <= right else !i >= right) do
    TokSet.iter (fun l -> match l with TLIST [e] -> if e <> EMPTY then (bprintf ch "%s" !delim; ignore(defn syms ch e 0); delim := ") ::\n( ") | _ -> ()) attrs.(!i);
    i := !i + inc
    done; bprintf ch ");\n"))

| SCALAR | EMPTY | UNKNOWN->
    bprintf ch "Trying to access scalar %s with a partsel\n" (valid id)
| _ -> unhandled stderr 313 sym.width)
| _ -> bprintf ch "Trying to access invalid type %s with a partsel\n" (valid id)

let rec rewrite syms ch edg clk pred expr hashtab = match expr with
| TLIST lst -> List.iter (fun itm -> rewrite syms ch edg clk pred itm hashtab) lst
| QUADRUPLE((IF|QUERY), cond, then_clause, else_clause) -> 
    rewrite syms ch edg clk (if pred <> EMPTY then TRIPLE(P_AMPERSAND,cond,pred) else cond) then_clause hashtab;
    rewrite syms ch edg clk (let ncond = DOUBLE(PLING,cond) in if pred <> EMPTY then TRIPLE(P_AMPERSAND,ncond,pred) else ncond) else_clause hashtab;
| TRIPLE(IF, cond, then_clause) ->
    rewrite syms ch edg clk (if pred <> EMPTY then TRIPLE(P_AMPERSAND,cond,pred) else cond) then_clause hashtab;
| QUADRUPLE((CASE|CASEZ|CASEX), cexpr, EMPTY, TLIST lst) -> List.iter (fun itm -> match itm with
  | TRIPLE(CASECOND, TLIST lst, itm) -> let clst = ref EMPTY in List.iter (fun citem -> let c = TRIPLE(P_EQUAL,cexpr,citem) in
      clst := (if !clst <> EMPTY then TRIPLE(P_VBAR,!clst,c) else c)) lst;
    rewrite syms ch edg clk (if pred <> EMPTY then TRIPLE(P_AMPERSAND,!clst,pred) else !clst) itm hashtab
  | DOUBLE(DEFAULT, itm) -> rewrite syms ch edg clk pred itm hashtab
  | _ -> unhandled stderr 362 itm) lst
| QUADRUPLE((ASSIGNMENT|DLYASSIGNMENT), target, dly, next) ->
    let hashit dest next = if Hashtbl.mem hashtab dest then Hashtbl.replace hashtab dest ((pred, next) :: (Hashtbl.find hashtab dest))
                      else Hashtbl.add hashtab dest [(pred, next)] in
    (match dly with
	| EMPTY -> ()
	| DOUBLE(HASH, FLOATNUM f) -> ()
	| _ -> unhandled stderr 369 dly);
    (match target with
	| ID dest -> hashit dest next
	| TRIPLE(BITSEL, ID dest, INT idx) -> let (hi,lo,inc) = find_wire_width dest syms and fld = QUADRUPLE(QUERY, next, BINNUM "1'b1", BINNUM "1'b0") in 
	  if idx = lo then hashit dest (DOUBLE(CONCAT, TLIST [QUADRUPLE(PARTSEL, ID dest, INT hi, INT (idx+1)); fld]))
	  else if idx < hi then hashit dest (DOUBLE(CONCAT, TLIST [QUADRUPLE(PARTSEL, ID dest, INT hi, INT (idx+1));
							    fld; QUADRUPLE(PARTSEL, ID dest, INT (idx-1), INT lo)]))
	  else if idx = hi then hashit dest (DOUBLE(CONCAT, TLIST [fld; QUADRUPLE(PARTSEL, ID dest, INT (hi-1), INT lo)]))
	  else bprintf ch "Bit select of %s[%d:%d] is out of range\n" dest.id hi lo
	| _ -> unhandled stderr 375 target)
| QUINTUPLE(NAMED, ID name, TLIST params, itm, EMPTY) -> rewrite syms ch edg clk pred itm hashtab
| TRIPLE(TASKREF, ID taskid, tasks) -> ()
| TRIPLE(D_DISPLAY, ASCNUM fmt, disp) -> ()
| DOUBLE (D_STOP, EMPTY) -> ()
| DOUBLE (D_FINISH, EMPTY) -> ()
| DOUBLE(D_DISPLAY, EMPTY) -> ()
| EMPTY -> ()
| _ -> unhandled stderr 370 expr

and edge syms ch edg clk itm = let hashtab = Hashtbl.create 256 
        and cdump arg1 arg2 owidth = bprintf ch "bool( "; ignore(defn syms ch arg1 1); bprintf ch ") ?\n( "; 
          ignore(defn syms ch arg2 owidth); bprintf ch ") :\n " in
        rewrite syms ch edg clk EMPTY itm hashtab; Hashtbl.iter (fun dest arg ->
          bprintf ch "ASSIGN next(%s) := " (valid dest); let w = symwidth syms dest in match arg with
            | [(EMPTY,next)] -> ignore(defn syms ch next w); bprintf ch ";\n"
            | _ -> List.iter (fun (cond,next) -> cdump cond next w) arg; bprintf ch "%s;\n" (valid dest)) hashtab

let rec initial syms ch stmt delim = let mydump ch itm = bprintf ch "-- "; ignore(defn syms ch itm 0); bprintf ch "\n" in match stmt with
  | TLIST lst -> List.iter (fun itm -> match itm with
    | QUADRUPLE((ASSIGNMENT|DLYASSIGNMENT), ID dest, EMPTY, next) -> let w = symwidth syms dest in
       bprintf ch "ASSIGN %s(%s) := " !delim (valid dest); ignore(defn syms ch next w); bprintf ch "; -- w=%d\n" w;
    | QUINTUPLE(FOR, TRIPLE(ASSIGNMENT,ID idstart, start), test, TRIPLE(ASSIGNMENT,ID idinc,inc), clause) -> mydump ch itm
    | TRIPLE(WHILE, expr, stmt) -> bprintf ch "-- while "; mydump ch expr; bprintf ch "-- do "; mydump ch stmt
    | TRIPLE(D_READMEMB, ASCNUM str, ID prog) -> mydump ch itm
    | TRIPLE(D_FOPEN, fd, ASCNUM txt) -> mydump ch itm
    | DOUBLE(D_FINISH, EMPTY) -> bprintf ch "-- finish\n"
    | DOUBLE(DOUBLE (AT, TLIST [DOUBLE (POSEDGE, ID clk)]), atstmt) -> mydump ch itm; delim := "next"; initial syms ch atstmt delim
    | DOUBLE(D_MONITOR, TLIST ((ASCNUM str) :: lst)) -> bprintf ch "-- $monitor(%s" str;
       List.iter (fun itm ->  bprintf ch ", "; ignore(defn syms ch itm 0)) lst; bprintf ch ");\n";
    | DOUBLE (DOUBLE (HASH, FLOATNUM f), dlystmt) -> initial syms ch dlystmt delim
    | _ -> unhandled stderr 260 itm) lst
  | EMPTY -> ()
  | _ -> initial syms ch (TLIST [stmt]) delim

let process_modinst oc kind find_dir symwidth primargs syms arg3 =
  List.iter (fun x -> (match x with
    | TRIPLE(ID idinst, SCALAR, TLIST arg4) -> 
        bprintf oc.(0) "VAR %s: %s(" (valid idinst) kind.id;
        let argpos = Array.create (List.length primargs) ("",EMPTY,0,EMPTY)
        and ix = ref 0 in List.iter (fun arg -> match arg with
          | ID id -> argpos.(!ix) <- (id.id,find_dir id,symwidth id,EMPTY); incr ix
          | _ -> unhandled stderr 341 arg) primargs;
        let argcnt = ref 0 in List.iter (fun y -> if !argcnt < Array.length argpos then (match y with
        | TRIPLE(CELLPIN, ID cellpin, conn) -> let found = ref false in
	   Array.iteri (fun ix ((e:string),dir,w,_) -> if e = cellpin.id then (found := true; argpos.(ix) <- (e,dir,w,conn))) argpos;
	   if not !found then failwith (sprintf "Cell-pin %s not found in idinst %s of type %s" cellpin.id idinst.id kind.id)
        | _ ->  let (e,dir,w,_) = argpos.(!argcnt) in argpos.(!argcnt) <- (e,dir,w,y); incr argcnt) else failwith "surplus modinst args") arg4;
        Array.iteri (fun ix (e,dir,w,y) -> let delim2 = if ix < (List.length primargs)-1 then "," else "" in (match y with
        | EMPTY -> (*unconnected pin *)
             bprintf oc.(0) "uncon_%s_%s%s -- %s (unconnected)\n\t" idinst.id e delim2 (Ord.getstr dir);
             bprintf oc.(2) "VAR uncon_%s_%s: word[%d];\n" idinst.id e w
        | TRIPLE(BITSEL, ID expr, INT sel) ->
           if dir <> OUTPUT then
             (idxtrans syms oc.(0) expr (INT sel) (INT sel); bprintf oc.(0) "%s -- %s\n\t" delim2 (Ord.getstr dir))
           else
             (bprintf oc.(0) "tmpstr%d%s -- %s (was " (!instcnt) delim2 (Ord.getstr dir); idxtrans syms oc.(0) expr (INT sel) (INT sel);
              bprintf oc.(0) ")\n\t"; bprintf oc.(2) "VAR tmpstr%d: word[%d];\n" (!instcnt) 1;
              dpartsel syms oc.(3) expr sel sel (ID (enterid ("tmpstr"^(string_of_int !instcnt))));
              incr instcnt)
        | QUADRUPLE(PARTSEL, ID expr, INT hi, INT lo) ->
	   if dir <> OUTPUT then
	     (idxtrans syms oc.(0) expr (INT hi) (INT lo); bprintf oc.(0) "%s -- %s\n\t" delim2 (Ord.getstr dir))
           else
             (bprintf oc.(0) "tmpstr%d%s -- %s (was " (!instcnt) delim2 (Ord.getstr dir); idxtrans syms oc.(0) expr (INT hi) (INT lo);
              bprintf oc.(0) ")\n\t"; bprintf oc.(2) "VAR tmpstr%d: word[%d];\n" (!instcnt) (hi-lo+1);
              dpartsel syms oc.(3) expr hi lo (ID (enterid ("tmpstr"^(string_of_int !instcnt))));
              incr instcnt)
        | DOUBLE(CONCAT, TLIST clst) ->
	   if dir <> OUTPUT then
	     (
	      let delim3 = ref "(" in List.iter (fun z -> (match z with
	        | TRIPLE (BITSEL, ID bit, INT idx1) -> bprintf oc.(0) "%s" !delim3; idxtrans syms oc.(0) bit (INT idx1) (INT idx1)
	        | ID id -> bprintf oc.(0) "%s%s" !delim3 (valid id)
	        | _ -> unhandled stderr 216 z); delim3 := ") :: (") clst;
                       bprintf oc.(0) ")%s -- %s\n\t" delim2 (Ord.getstr dir)
	     )
           else
             (bprintf oc.(0) "concat_%s_%s%s -- %s\n\t" idinst.id e delim2 (Ord.getstr dir);
              bprintf oc.(1) "VAR concat_%s_%s: word[%d];\n" idinst.id e w;
	      let ix = ref (w-1) in List.iter (fun z -> (match z with
	        | TRIPLE (BITSEL, ID bit, INT idx1) -> bprintf oc.(1) "%s %s[%d] := concat_%s_%s[%d:%d]\n"
                    (defkind syms bit) (valid bit) idx1 idinst.id e !ix !ix; decr ix
	        | ID id -> let sw = symwidth id in 
		    bprintf oc.(1) "%s %s := concat_%s_%s[%d:%d];\n" (defkind syms id) (valid id) idinst.id e !ix (!ix-sw+1);
		    ix := !ix - sw
	        | _ -> unhandled stderr 216 z)) clst
             )
        | _ -> ignore(defn syms oc.(0) y 0); bprintf oc.(0) "%s -- %s\n\t" delim2 (Ord.getstr dir))) argpos; bprintf oc.(0) ");\n"
    | _ -> unhandled stderr 218 x)) arg3

let rec flt syms (oc:Buffer.t array) exp (lev:bool) = Stack.push (420, exp) stk; (match exp with
| TLIST lst -> List.iter (fun x -> flt syms oc x lev) lst
| THASH thash ->
  Hashtbl.iter (fun x _ -> flt syms oc x lev) (fst thash);
  Hashtbl.iter (fun x _ -> flt syms oc x lev) (snd thash)
| DOUBLE(INITIAL,stmt) -> let delim = ref "init" in initial syms oc.(0) stmt delim
| DOUBLE(ALWAYS,stmt) -> (match stmt with
  | DOUBLE(DOUBLE(AT, TLIST [DOUBLE ((POSEDGE|NEGEDGE) as edg, clk)]), TLIST lst) -> List.iter (fun itm -> edge syms oc.(0) edg clk itm) lst
  | DOUBLE(DOUBLE(AT, TLIST [DOUBLE ((POSEDGE|NEGEDGE) as edg, clk)]), itm) ->  edge syms oc.(0) edg clk itm
  | DOUBLE(DOUBLE(AT, TLIST [DOUBLE ((POSEDGE|NEGEDGE) as edg, clk); DOUBLE ((POSEDGE|NEGEDGE), clr)]), itm) ->
		 edge syms oc.(0) edg clk itm	   
  | DOUBLE(DOUBLE(AT, TLIST [DOUBLE ((POSEDGE|NEGEDGE) as edg, clk); DOUBLE ((POSEDGE|NEGEDGE), clr); DOUBLE ((POSEDGE|NEGEDGE), pre)]), itm)
     -> edge syms oc.(0) edg clk itm	   
  | TLIST lst -> List.iter (fun itm -> bprintf oc.(0) "-- "; ignore(defn syms oc.(0) itm 0); bprintf oc.(0) "\n") lst
  | DOUBLE(DOUBLE(HASH, FLOATNUM f), dlystmt) -> (match dlystmt with
    | QUADRUPLE((ASSIGNMENT|DLYASSIGNMENT), ID dest, EMPTY, next) -> let w = symwidth syms dest in
       bprintf oc.(0) "ASSIGN next(%s) := " (valid dest); ignore(defn syms oc.(0) next w); bprintf oc.(0) "; -- w=%d\n" w;
    | _ -> unhandled stderr 442 dlystmt)
  | _ -> unhandled stderr 443 stmt)
| TRIPLE(EQUALS, arg1, arg2) -> bprintf oc.(0) "\n";  flt syms oc arg1 lev; bprintf oc.(0) "= "; flt syms oc arg2 lev
| TRIPLE(IF, arg1, arg2) -> bprintf oc.(0) "\n";  bprintf oc.(0) "if ( "; flt syms oc arg1 lev; bprintf oc.(0) ") "; flt syms oc arg2 lev
| TRIPLE(PLUS, arg1, arg2) -> bprintf oc.(0) "\n";  flt syms oc arg1 lev; bprintf oc.(0) "+ "; flt syms oc arg2 lev
| TRIPLE(ASSIGN, arg1, TLIST arg2) ->
  bprintf oc.(0) "\n";  
  (match arg1 with 
    | DOUBLE(HASH, TLIST [INT n]) -> bprintf oc.(0) "-- Verilog Delay #%d ignored\n" n
    | DOUBLE(HASH, FLOATNUM f) -> bprintf oc.(0) "-- Verilog delay #%f ignored\n" f
    | EMPTY -> ()
    | _ -> unhandled stderr 454 arg1);
  List.iter (fun x -> (match x with
    | TRIPLE (ASSIGNMENT, ID id, exp) -> let w = symwidth syms id in
        bprintf oc.(1) "%s %s := " (defkind syms id) (valid id);
        ignore(defn syms oc.(1) exp w);
	bprintf oc.(1) ";\n"
    | TRIPLE (ASSIGNMENT, QUADRUPLE (PARTSEL, ID id, INT hi, INT lo), exp) -> dpartsel syms oc.(1) id hi lo exp
    | TRIPLE (ASSIGNMENT, TRIPLE (BITSEL, ID id, INT sel), exp) ->  dpartsel syms oc.(1) id sel sel exp
    | _ -> unhandled stderr 462 x)) arg2
| TRIPLE(NOT, arg1, TLIST arg2) ->
  bprintf oc.(0) "\n";  
  (match arg1 with 
    | DOUBLE(HASH, TLIST [INT n]) -> bprintf oc.(0) "-- Verilog Delay #%d ignored\n" n
    | DOUBLE(HASH, FLOATNUM f) -> bprintf oc.(0) "-- Verilog delay #%f ignored\n" f
    | EMPTY -> ()
    | _ -> unhandled stderr 469 arg1);
  List.iter (fun x -> (match x with
    | QUADRUPLE (ID gateid, SCALAR, ID dest, exp) -> let w = symwidth syms dest in
        bprintf oc.(1) "%s %s := !(" (defkind syms dest) (valid dest);
        ignore(defn syms oc.(1) exp w);
	bprintf oc.(1) ");\n"
    | _ -> unhandled stderr 477 x)) arg2
| TRIPLE((AND|OR|XOR) as op, arg1, TLIST arg2) ->
  bprintf oc.(0) "\n";  
  (match arg1 with 
    | DOUBLE(HASH, TLIST [INT n]) -> bprintf oc.(0) "-- Verilog Delay #%d ignored\n" n
    | DOUBLE(HASH, FLOATNUM f) -> bprintf oc.(0) "-- Verilog delay #%f ignored\n" f
    | EMPTY -> ()
    | _ -> unhandled stderr 469 arg1);
  List.iter (fun x -> (match x with
    | QUADRUPLE (ID gateid, SCALAR, ID dest, TLIST exp) -> let w = symwidth syms dest in
        bprintf oc.(1) "%s %s := " (defkind syms dest) (valid dest);
        let delim = ref "( " in List.iter(fun itm ->
          bprintf oc.(1) "%s " !delim;
          ignore(defn syms oc.(1) itm w);
          delim := ") "^(match op with AND -> "&" | OR -> "|" | XOR -> "xor" | _ -> failwith (Dump.dumpstr op))^" (") exp;
	bprintf oc.(1) ");\n"
    | _ -> unhandled stderr 477 x)) arg2
| QUADRUPLE(EQUALS, arg1, arg2, arg3) -> bprintf oc.(0) "\n";  flt syms oc arg1 lev; bprintf oc.(0) "= "; flt syms oc arg2 lev; flt syms oc arg3 lev
| QUADRUPLE(IF, arg1, arg2, arg3) -> bprintf oc.(0) "\n";
    bprintf oc.(0) "if ( ";
    flt syms oc arg1 lev;
    bprintf oc.(0) ") ";
    flt syms oc arg2 lev;
    bprintf oc.(0) "else ";
    flt syms oc arg3 lev
| QUADRUPLE((WIRE|TRI0|TRI1), arg0, TRIPLE(arg1, rng, arg3), TLIST arg4) -> List.iter (fun x -> match x with
    | DOUBLE(ID _, EMPTY) -> ()
    | TRIPLE(ID id, EMPTY, exp) -> let w = symwidth syms id in
        bprintf oc.(1) "%s %s := " (defkind syms id) (valid id);
        ignore(defn syms oc.(1) exp w);
	bprintf oc.(1) ";\n"
    | _ -> unhandled stderr 480 x) arg4
(* convert memory declarations *)
| QUADRUPLE(REG, EMPTY, rng, TLIST lst) -> begin
    let width = (match rng with
      | RANGE(left, right) -> let lft = Const.exprConstStr stderr syms left and
			       rght = Const.exprConstStr stderr syms right in
			       ((int_of_string lft) - (int_of_string rght) + 1)
      | EMPTY -> 1
      | TRIPLE(EMPTY,EMPTY,EMPTY) -> 1
      | _ ->  unhandled stderr 319 rng; 1) in
    List.iter (fun x -> match x with
      | TRIPLE(ID id, arg5, EMPTY) -> (match arg5 with
          | EMPTY -> ()
          | TLIST [RANGE (expr1, expr2)] -> let hi = Const.exprConstStr stderr syms expr1 and
			       lo = Const.exprConstStr stderr syms expr2 in
                               bprintf oc.(1) "VAR %s: array %s .. %s of word[%d];\n" (valid id) (min lo hi) (max lo hi) width;
          | _ -> unhandled stderr 578 arg5);
      | DOUBLE(id, EMPTY) -> ()
      | _ -> unhandled stderr 580 x) lst; end
| QUADRUPLE(INTEGER, EMPTY, rng, lst) -> ()
| QUADRUPLE(PARAMETER, EMPTY, range, decls) -> let def syms id = 
    let arg = Const.exprConst stderr syms (ID id) in (match arg with
      | INT n -> bprintf oc.(1) "DEFINE %s := 0d%d_%d;\n" (valid id) (mask n) n
      | WIDTHNUM(r,w,n) -> ( match r with
        | 16 -> bprintf oc.(1) "DEFINE %s := 0h%d_%x;\n" (valid id) w n
        | 10 -> bprintf oc.(1) "DEFINE %s := 0d%d_%d;\n" (valid id) w n
        | 8 -> bprintf oc.(1) "DEFINE %s := 0o%d_%o;\n" (valid id) w n
        | 2 -> bprintf oc.(1) "DEFINE %s := 0b%d_%s;\n" (valid id) w (bin n)
        | _ -> unhandled stderr 590 arg)
      | _ -> unhandled stderr 591 arg) in
    ( match decls with
      | TLIST arg9 ->  List.iter (fun x -> match x with TRIPLE(ID id, arg5, arg6) -> def syms id | _ -> unhandled stderr 238 x) arg9
      | EMPTY -> ()
      | TRIPLE (ID id, EMPTY, arg6) -> def syms id
      | _ -> unhandled stderr 596 decls)
| QUADRUPLE(MODINST, ID kind, EMPTY, TLIST arg3) ->
  bprintf oc.(0) "\n";
  if Hashtbl.mem Globals.modprims kind.id then
    begin
      let kindhash = Hashtbl.find Globals.modprims kind.id in 
      let primargs = match kindhash.Globals.tree with QUINTUPLE((MODULE|PRIMITIVE),ID arg1, _, TLIST primargs, _) -> primargs | _ -> failwith "modinst" in
      let isyms = kindhash.Globals.symbols in
      process_modinst oc kind (find_dir isyms) (symwidth isyms) primargs syms arg3
    end
  else if Hashtbl.mem Globals.libhash kind.id then
    begin
      let kindhash = Hashtbl.find Globals.libhash kind.id in 
      let primargs = kindhash.Globals.iolst in
      let find_dir' id =
        if Read_library.is_member id kindhash.Globals.opinlst then OUTPUT
        else if Read_library.is_member id kindhash.Globals.ipinlst then INPUT else EMPTY in
      let symwidth' id = 1 in
      process_modinst oc kind find_dir' symwidth' primargs syms arg3
    end
  else failwith ("modinst kind "^kind.id^" is not found")
| QUINTUPLE(MODULE, ID arg1, EMPTY, TLIST arg3, arg4) ->
  bprintf oc.(0) "\nMODULE %s" (if lev then "main" else valid(arg1));
  if lev && arg3 <> [] then fprintf stderr "Main circuit \"%s\" has ports - not supported by smv\n" arg1.id;
  let delim = ref "(" in List.iter (fun x -> (match x with
    | ID id -> bprintf oc.(0) "%s%s" !delim (valid id)
    | _ -> flt syms oc x lev); delim := ",") arg3; if !delim <> "(" then bprintf oc.(0) ")";
  (match syms with
    | Shash symr -> Hashtbl.iter (fun _ sym -> sym_detail oc syms sym) symr.syms
    | EndShash -> ());
  bprintf oc.(0) "\n";
  flt syms oc arg4 lev
| QUINTUPLE((INPUT|OUTPUT), arg1, arg2, rng, TLIST arg4) -> ()
| DOTTED lst -> List.iter (fun itm -> match itm with
  | ID s -> bprintf oc.(0) "%s " (valid s)
  | TRIPLE (BITSEL, ID id1, TRIPLE (DIVIDE, ID id2, INT n)) -> bprintf oc.(0) "%s[%s/%d] " (valid id1) (valid id2) n
  | _ -> unhandled stderr 177 itm) lst
| RANGE (TRIPLE (MINUS, ID id1, INT n1), INT n2) -> bprintf oc.(0) "[%s-%d:%d] " (valid id1) n1 n2
| SEPTUPLE(TASK, EMPTY, ID nam, EMPTY, params, body, EMPTY) -> bprintf oc.(0) "-- task %s\n" nam.id; ( (* match params with
  | TLIST tlst -> List.iter (fun itm -> match itm with
    | QUINTUPLE((INPUT|OUTPUT|INOUT) as dir, EMPTY, EMPTY, rng, TLIST iolst) 
    | QUADRUPLE((WIRE|REG|INTEGER) as dir, EMPTY, rng, TLIST iolst) -> List.iter (fun io -> match io with
      | TRIPLE (ID id, EMPTY, EMPTY) -> bprintf oc.(0) "-- taskargs %s %s\n" (Ord.getstr dir) id
      | _ -> unhandled stderr 549 io) iolst
    | _ -> unhandled stderr 550 itm) tlst
  | EMPTY -> ()
  | _ -> unhandled stderr 552 params);
  (match body with
  | TLIST tasks -> List.iter (fun itm -> match itm with
    | EMPTY -> ()
    | _ -> bprintf oc.(0) "-- taskbody "; ignore(defn syms oc.(0) itm 0); bprintf oc.(0) "\n") tasks
  | EMPTY -> ()
  | _ -> bprintf oc.(0) "-- taskbody "; ignore(defn syms oc.(0) body 0); *) bprintf oc.(0) "\n")
| OCTUPLE(FUNCTION, EMPTY, rng, ID nam, EMPTY, params, body, EMPTY) -> bprintf oc.(0) "-- function %s\n" nam.id; ( (*match params with 
  | TLIST tlst -> List.iter (fun itm -> match itm with
    | QUINTUPLE((INPUT|OUTPUT|INOUT) as dir, EMPTY, EMPTY, rng, TLIST iolst) 
    | QUADRUPLE((WIRE|REG|INTEGER) as dir, EMPTY, rng, TLIST iolst) -> List.iter (fun io -> match io with
      | TRIPLE (ID id, EMPTY, EMPTY) -> bprintf oc.(0) "-- funargs %s %s\n" (Ord.getstr dir) id
      | _ -> unhandled stderr 562 io) iolst
    | _ -> unhandled stderr 563 itm) tlst
  | EMPTY -> ()
  | _ -> unhandled stderr 568 params);
  (match body with
  | TLIST tasks -> List.iter (fun itm -> match itm with
    | EMPTY -> ()
    | _ -> bprintf oc.(0) "-- funbody "; ignore(defn syms oc.(0) itm 0); bprintf oc.(0) "\n") tasks
  | EMPTY -> ()
  | _ -> bprintf oc.(0) "-- funbody "; ignore(defn syms oc.(0) body 0); *) bprintf oc.(0) "\n")
| _ -> unhandled stderr 575 exp); ignore(Stack.pop stk)

let write_smv_arch' arch nam file arg =
  let op = unique_open file in
  Minimap.recurse_arch (fun id subarg ->
    let oc = Array.init 10 (fun n -> Buffer.create 4096) in
    uniq_tab();
    flt subarg.Globals.symbols oc subarg.Globals.tree (id=nam);
    for i = 0 to 9 do Buffer.output_buffer op oc.(i) done) arch nam arg;
  close_out op

let write_smv_arch arch nam =
  let lst = List.filter (fun arg -> arg.arch = arch) (Hashtbl.find_all modprims nam) in
  match lst with
    | [] -> failwith ("No netlists matched arch "^arch^" name "^nam)
    | _ -> List.iter (write_smv_arch' arch nam (nam^"_"^arch^".smv")) lst

let width' isyms id =
  let (lft,rght,inc) = Minimap.find_width id isyms in
  if inc = 0 then EMPTY else RANGE(INT lft,INT rght)
  
let smv_main_dir isyms id = match isyms with
  | Vparser.EndShash -> EMPTY
  | Vparser.Shash {Vparser.nxt;syms} ->
    let rslt = ref None in Vparser.TokSet.iter (function
      | OUTPUT -> rslt := Some OUTPUT
      | INPUT -> rslt := Some INPUT
      | INOUT -> failwith "INOUT not supported in smv_main"
      | _ -> ()) (Hashtbl.find syms id).Vparser.symattr;
    match !rslt with Some x -> x | None -> EMPTY

let smv_main_subcell smv_mainlst instid width dir thash2 kind iolst =
  Hashtbl.add (snd thash2)
    (QUADRUPLE(MODINST, ID kind, EMPTY, TLIST [
      TRIPLE (ID instid, SCALAR, 
              TLIST (let pos = ref iolst in
                     let missing = List.map (function
                       | ID id -> let intrng = width id in (match dir id with
                           | OUTPUT ->
                             let conn = enterid (String.concat "$" [instid.id;id.id]) in
                             Hashtbl.add (fst thash2)
                               (QUADRUPLE(WIRE,
                                          EMPTY,
                                          TRIPLE(EMPTY, intrng, EMPTY),
                                          TLIST [(DOUBLE(ID conn, EMPTY))])) ();
                             smv_mainlst := (intrng,conn) :: !smv_mainlst;
                             TRIPLE(CELLPIN, ID id, ID conn)
                           | INPUT ->
                               begin
                                 Hashtbl.replace (fst thash2)
                                   (QUADRUPLE(REG,
                                              EMPTY,
                                              intrng,
                                              TLIST [(DOUBLE(ID id, EMPTY))])) ();
                               end;
                             TRIPLE(CELLPIN, ID id, ID id)
                           | _ -> failwith "pin direction is undefined in smv_main")
                       | itm -> DOUBLE(CELLPIN, itm)) !pos in
                     missing))])) ()

let smv_main_subcell' smv_mainlst thash2 kind nam top =
  match top.tree with
    | QUINTUPLE(MODULE, ID id, _, TLIST iolst, _) ->
        smv_main_subcell smv_mainlst id (width' top.symbols) (smv_main_dir top.symbols) thash2 kind iolst
    | err -> unhandled stderr 31 err

let smv_main arch kind' thash2 =
  let smv_mainlst = ref [] in
  let kind = enterid kind' in
  if Hashtbl.mem modprims kind.id then
    begin
      Minimap.select_sub false (smv_main_subcell' smv_mainlst thash2 kind) arch kind.id;
      !smv_mainlst
    end
  else if Hashtbl.mem libhash kind.id then
    failwith ("library cell "^kind.id^" not allowed")
  else
    failwith ("sub-module "^kind.id^" not found")

let generate_smv_main_netlist arch kind =
  let thash2 = (Hashtbl.create 256,Hashtbl.create 256) in
  let _ = smv_main arch kind thash2 in
  QUINTUPLE(MODULE, ID (enterid ("main")), EMPTY,
            TLIST [],
            THASH thash2)

let gen_smv_main_arch arch nam =
  Semantics.prescan stderr !Globals.archenv (generate_smv_main_netlist arch nam) "Generated by gen_smv_main_arch";
  Printf.printf "Module report %s\n" (Semantics.endscan())
