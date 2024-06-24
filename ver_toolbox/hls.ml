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
open Printf
open Globals
open Idhash
open Vparser
(*
open Vconvert
*)
open VhdlTree
open Read_library
open Mapselect
  
let verbose = ref false

type tfn = Vparser.token -> Vparser.token -> Vparser.token list
(*
type tbl = {fst:(Vparser.token, unit) Hashtbl.t;
            snd:(Vparser.token, unit) Hashtbl.t;
            gentab:(string, unit) Hashtbl.t;
            namtab:(string, string) Hashtbl.t;
            defnhash:(Vparser.token*int, Vparser.token*int) Hashtbl.t;
            tused:(idhash,Vparser.token) Hashtbl.t;
	    edges : (string, (string*string*bool) list) Hashtbl.t;
	    combs : (string, (string*string*bool) list) Hashtbl.t;
	    edgdmp : (string, Vparser.token*Vparser.token*
	      (Vparser.token*int)*(Vparser.token*int)*(Vparser.token*int)*(Vparser.token*int)) Hashtbl.t;
	    combdmp : (string, Vparser.token*Vparser.token*
	      (Vparser.token*int)*(Vparser.token*int)*(Vparser.token*int)*(Vparser.token*int)) Hashtbl.t}
      *)
    
type uptr = UPTR of (out_channel -> int -> Vparser.token -> unit) | UNIL;;

let instid' tbl =
  let loop = ref true and instcnt = ref 0 and inst = ref "" in
  while !loop do
    inst := sprintf "V2hls$%X" !instcnt;
    if Hashtbl.mem tbl.gentab !inst then incr instcnt
    else
      begin
        Hashtbl.replace tbl.gentab !inst ();
        loop := false
      end;
  done;
    ID (enterid (Minimap.valid tbl.namtab !inst))

let is_mem syms id = match syms with
| EndShash -> false
| Shash {nxt;syms} -> if Hashtbl.mem syms id then TokSet.mem MEMORY (Hashtbl.find syms id).symattr else false

let find_dir syms id = match syms with
| EndShash -> EMPTY
| Shash {nxt;syms} -> let sym = Hashtbl.find syms id in if TokSet.mem OUTPUT sym.symattr then OUTPUT else if TokSet.mem INPUT sym.symattr then INPUT else EMPTY

let show_set (tbl:tbl) s = let str = ref "" in TokSet.iter (fun e -> str := (str_token e)^" "^(!str)) s; !str;;

(*
let declared (tbl:tbl) path = let found = ref false in Hashtbl.iter (fun k _ -> match k with
  | QUADRUPLE(WIRE, EMPTY, TRIPLE(EMPTY, rng, EMPTY), TLIST[DOUBLE (pth, EMPTY)]) when pth = path ->
    found := true
      | _ -> ()) tbl.fst;
  !found                         
*)

let defkind (tbl:tbl) gsyms id w =
if !verbose then printf "139 v2hls.defkind %s %d\n" id.id w;
 match gsyms with
| EndShash -> Minimap.declare tbl.namtab gsyms id (w-1) 0
| Shash {nxt;syms} -> if Hashtbl.mem syms id then
    (let sym = Hashtbl.find syms id in
     if not (TokSet.mem OUTPUT sym.symattr || TokSet.mem WIRE sym.symattr)
     then Minimap.declare tbl.namtab gsyms id (w-1) 0)
  else
    Minimap.declare tbl.namtab gsyms id (w-1) 0

and hadd str =
  let str' = str.id^"$" in
(*
  if !verbose then printf "hadd %s = %s" str str'; print_newline();
*)
  enterid str'

let sym_detail (tbl:tbl) syms sym =
TokSet.iter (fun t -> match t with
| SUBCCT -> ()
| SUBMODULE -> ()
| INTEGER -> Minimap.declare tbl.namtab syms sym.path 31 0
| REG -> (* if not (TokSet.mem OUTPUT sym.symattr) then *)
    begin
      Minimap.declare tbl.namtab syms sym.path (Minimap.sym_width syms sym - 1) 0;
      Minimap.declare tbl.namtab syms (hadd sym.path) (Minimap.sym_width syms sym - 1) 0
    end
| IMPLICIT -> ()
| WIRE -> if not ((TokSet.mem INPUT sym.symattr) || (TokSet.mem OUTPUT sym.symattr)) then
    Minimap.declare tbl.namtab syms sym.path (Minimap.sym_width syms sym - 1) 0
| MEMORY -> () (* plugh_str (tbl:tbl) " -- VAR s: array of --"  (Minimap.valid (tbl:tbl) sym.path) *)
| IOPORT -> ()
| MODULE -> ()
| PARAMETER -> ()
| OUTPUT -> ()
| INPUT -> ()
| INOUT -> ()
| PARAMUSED -> ()
| TASK -> ()
| TASKUSED -> ()
| NAMED -> ()
| FUNCASSIGNED -> ()
| FUNCTION -> ()
| _ -> Dump.unhandled stderr 177 t) sym.symattr

let shorten syms wid = function
  | ID id -> DOUBLE(CONCAT, TLIST (Minimap.list_shorten wid (Minimap.id_flatten (Minimap.find_width id syms) id [])))
  | INT num -> DOUBLE(CONCAT, TLIST (Minimap.list_shorten wid (Minimap.num_flatten wid num [])))
  | WIDTHNUM (radix, wid, num) -> DOUBLE(CONCAT, TLIST (Minimap.list_shorten wid (Minimap.num_flatten wid num [])))
  | QUADRUPLE (PARTSEL, _, _, _) as part -> DOUBLE(CONCAT, TLIST (Minimap.list_shorten wid (Minimap.concat_flatten' syms [part])))
  | TRIPLE (BITSEL, ID id, INT sel) -> TRIPLE (BITSEL, ID id, INT sel)
  | DOUBLE(CONCAT, TLIST lst) ->
      let lst' = Minimap.list_shorten wid lst in
(*
      assert(wid=List.length lst');
*)
      DOUBLE(CONCAT, TLIST lst')
  | oth -> Dump.unhandled stderr 454 oth; oth

let dvlhash = Hashtbl.create 255

let guessdir = function
| "A" -> INPUT
| "B" -> INPUT
| "C" -> INPUT
| "CK" -> INPUT
| "D" -> INPUT
| "DEF" -> INPUT
| "RN" -> INPUT
| "S" -> INPUT
| "Q" -> OUTPUT
| "Y" -> OUTPUT
| oth -> failwith ("guessdir: "^oth)

let dvladd syms synthnam pins stmts =
  let params = [] in
  let arg3,decls = List.split ( List.map (function
     | TRIPLE(CELLPIN, ID id, conn) ->
        let dir = guessdir id.id in
        let wid = Minimap.elwidth syms conn in
        ID id, QUINTUPLE(dir, EMPTY, EMPTY, RANGE(INT (wid-1), INT 0), TLIST[TRIPLE(ID id, EMPTY, EMPTY)])
     | oth -> failwith "dvladd") pins) in           
  let arg4 = Hashtbl.create 255, Hashtbl.create 255 in
  List.iter (fun itm -> Hashtbl.add (fst arg4) itm ()) decls;
  List.iter (fun itm -> Hashtbl.add (snd arg4) itm ()) stmts;
  let modul = QUINTUPLE(MODULE, ID (enterid synthnam), TLIST params, TLIST arg3, THASH arg4) in
  Semantics.prescan stderr !archenv modul "Generated by gen_hls_arch"

let cacheid (lst,pins,syms) =
let pth = String.concat "_" lst in
if not (Hashtbl.mem dvlhash pth) then (
let stmts = match List.hd lst with
  | "DVL_ARI_ADD" -> TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID {id = "Y"}, TRIPLE(PLUS, ID {id = "A"}, ID {id = "B"}))]) :: []
  | "DVL_ARI_MUL" -> TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID {id = "Y"}, TRIPLE(TIMES, ID {id = "A"}, ID {id = "B"}))]) :: []
  | "DVL_ARI_SUB" -> TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID {id = "Y"}, TRIPLE(MINUS, ID {id = "A"}, ID {id = "B"}))]) :: []
  | "DVL_BUF" -> TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID {id = "Y"}, ID {id = "A"})]) :: []
  | "DVL_BW_AND" -> TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID {id = "Y"}, TRIPLE(AND, ID {id = "A"}, ID {id = "B"}))]) :: []
  | "DVL_BW_NOT" -> TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID {id = "Y"}, DOUBLE(PLING, ID {id = "A"}))]) :: []
  | "DVL_BW_OR" -> TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID {id = "Y"}, TRIPLE(OR, ID {id = "A"}, ID {id = "B"}))]) :: []
  | "DVL_BW_XOR" -> TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID {id = "Y"}, TRIPLE(XOR, ID {id = "A"}, ID {id = "B"}))]) :: []
  | "DVL_DFF_SYNC" -> DOUBLE (ALWAYS, TLIST [DOUBLE (DOUBLE (AT, TLIST
             [DOUBLE (POSEDGE, ID {id = "CK"});
              DOUBLE (NEGEDGE, ID {id = "RN"})]),
          TLIST [QUADRUPLE (IF, ID {id = "RN"},
              TLIST [QUADRUPLE
                 (DLYASSIGNMENT, ID {id = "Q"}, EMPTY,
                  HEXNUM "32'h0")],
              TLIST [QUADRUPLE
                 (DLYASSIGNMENT, ID {id = "Q"}, EMPTY,
                  ID {id = "D"})])])]) :: []
  | "DVL_EQ" -> TRIPLE
     (ASSIGN, EMPTY,
      TLIST
       [TRIPLE
         (ASSIGNMENT, ID {id = "Y"},
          DOUBLE
           (PLING,
            DOUBLE
             (LPAREN,
              DOUBLE
               (OR,
                DOUBLE
                 (LPAREN,
                  TRIPLE
                   (XOR, ID {id = "A"}, ID {id = "B"}))))))]) :: []
  | "DVL_GEQ" -> TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID {id = "Y"}, DOUBLE (LPAREN, TRIPLE (P_GTE, ID {id = "A"}, ID {id = "B"})))]) :: []
  | "DVL_GREATER" -> TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID {id = "Y"}, DOUBLE (LPAREN, TRIPLE (GREATER, ID {id = "A"}, ID {id = "B"})))]) :: []
  | "DVL_LEQ" -> TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID {id = "Y"}, DOUBLE (LPAREN, TRIPLE (P_LTE, ID {id = "A"}, ID {id = "B"})))]) :: []
  | "DVL_LESS" -> TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID {id = "Y"}, DOUBLE (LPAREN, TRIPLE (LESS, ID {id = "A"}, ID {id = "B"})))]) :: []
  | "DVL_LOG_AND" -> TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID {id = "Y"}, TRIPLE(P_ANDAND, ID {id = "A"}, ID {id = "B"}))]) :: []
  | "DVL_LOG_NOT" -> TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID {id = "Y"}, DOUBLE(PLING, ID {id = "A"}))]) :: []
  | "DVL_LOG_OR" -> TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID {id = "Y"}, TRIPLE(P_OROR, ID {id = "A"}, ID {id = "B"}))]) :: []
  | "DVL_MUX" -> TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID {id = "Y"},
QUADRUPLE (QUERY, ID {id = "S"}, ID {id = "B"}, ID {id = "A"}))]) :: []
  | "DVL_NEQ" -> TRIPLE
     (ASSIGN, EMPTY,
      TLIST
       [TRIPLE
         (ASSIGNMENT, ID {id = "Y"},
            DOUBLE
             (LPAREN,
              DOUBLE
               (OR,
                DOUBLE
                 (LPAREN,
                  TRIPLE
                   (XOR, ID {id = "A"}, ID {id = "B"})))))]) :: []
  | "DVL_RED_AND" -> TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID {id = "Y"}, DOUBLE(AND, ID {id = "A"}))]) :: []
  | "DVL_RED_OR" -> TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID {id = "Y"}, DOUBLE(OR, ID {id = "A"}))]) :: []
  | "DVL_SH_L" -> TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID {id = "Y"}, TRIPLE(P_SLEFT, ID {id = "A"}, ID {id = "B"}))]) :: []
  | "DVL_SH_R" -> TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID {id = "Y"}, TRIPLE(P_SRIGHT, ID {id = "A"}, ID {id = "B"}))]) :: []
  | oth -> failwith oth in
Hashtbl.add dvlhash pth stmts;
dvladd syms pth pins stmts);
enterid pth

let rec defn syms (tbl:tbl) exp owidth =
  if !verbose then printf "v2hls.defn %s %d\n" (Dump.dumpstr exp) owidth;
  if Hashtbl.mem tbl.defnhash (exp,owidth) then
    Hashtbl.find tbl.defnhash (exp,owidth) else
    let (rslt,wid) = (match exp with
      | DOUBLE(CONCAT, TLIST arg) ->
          let w = ref 0 in let arg' = List.map (fun itm ->
            let (itm',w') = defn syms (tbl:tbl) itm 0 in
            w := !w + w';
            itm') arg in tbl.map_hls.map_select.map_unaryplus exp (tbl:tbl) syms (DOUBLE(CONCAT, TLIST arg'),!w)
      | DOUBLE(CONCAT, arg) -> failwith "internal error - concat called with non-list argument"
      | ID arg -> Minimap.lfill (ID (enterid (Minimap.valid tbl.namtab arg.id)),Minimap.symwidth syms arg) owidth
      | INT n -> if owidth < 0 then (WIDTHNUM(10,Minimap.mask n,n), Minimap.mask n)
        else if n >= 0 then let w = max owidth (Minimap.mask n) in (WIDTHNUM(10,w,n), w)
        else let n1 = (1 lsl owidth-1) land n in (WIDTHNUM(10,owidth,n1), owidth)
      | HEXNUM str -> let radix = 16 in let (sz,num) = Const.widthnum stderr radix str in
                                        let w = max owidth sz in (WIDTHNUM(radix,w,num), w)
      | DECNUM str -> let radix = 10 in let (sz,num) = Const.widthnum stderr radix str in
                                        let w = max owidth sz in (WIDTHNUM(radix,w,num), w)
      | OCTNUM str -> let radix = 8 in let (sz,num) = Const.widthnum stderr radix str in
                                       let w = max owidth sz in (WIDTHNUM(radix,w,num), w)
      | BINNUM str -> let radix = 2 in let (sz,num) = Const.widthnum stderr radix str in
                                       let w = max owidth sz in (WIDTHNUM(radix,w,num), w)
      | EMPTY -> (EMPTY, 0)
      | DOUBLE(LPAREN, arg) -> let (e,w) = defn syms (tbl:tbl) arg owidth in (e, w)
      | DOUBLE(MINUS, arg) -> let (e,w) = defn syms (tbl:tbl) arg owidth in minus exp tbl syms (WIDTHNUM(2, w, 0),w) (e, w)
      | DOUBLE(NOT, arg) -> tbl.map_hls.map_select.map_tilde exp (tbl:tbl) syms (defn syms (tbl:tbl) arg owidth)
      | DOUBLE((PLING|P_NOR), arg) -> tbl.map_hls.map_select.map_pling exp (tbl:tbl) syms (defn syms (tbl:tbl) arg owidth)
      | DOUBLE(AND, arg) -> tbl.map_hls.map_select.map_redand exp (tbl:tbl) syms (defn syms (tbl:tbl) arg owidth)
      | DOUBLE(OR, arg) -> tbl.map_hls.map_select.map_redor exp (tbl:tbl) syms (defn syms (tbl:tbl) arg owidth)
      | TRIPLE(AND, arg1, arg2) -> let e1 = defn syms (tbl:tbl) arg1 owidth in
                                   let e2 = defn syms (tbl:tbl) arg2 (max (snd e1) owidth) in
                                   tbl.map_hls.map_select.map_logand exp (tbl:tbl) syms e1 e2
      | TRIPLE(OR, arg1, arg2) -> let e1 = defn syms (tbl:tbl) arg1 owidth in
                                  let e2 = defn syms (tbl:tbl) arg2 (max (snd e1) owidth) in
                                  tbl.map_hls.map_select.map_logor exp (tbl:tbl) syms e1 e2
      | TRIPLE(XOR, arg1, arg2) -> let e1 = defn syms (tbl:tbl) arg1 owidth in
                                   let e2 = defn syms (tbl:tbl) arg2 (max (snd e1) owidth) in
                                   tbl.map_hls.map_select.map_logxor exp (tbl:tbl) syms e1 e2
      | TRIPLE(P_OROR, arg1, arg2) -> let e1 = defn syms (tbl:tbl) arg1 owidth in
                                      let e2 = defn syms (tbl:tbl) arg2 owidth in
                                      tbl.map_hls.map_select.map_log_por exp (tbl:tbl) syms e1 e2
      | TRIPLE(P_ANDAND, arg1, arg2) -> let e1 = defn syms (tbl:tbl) arg1 owidth in
                                        let e2 = defn syms (tbl:tbl) arg2 owidth in
                                        tbl.map_hls.map_select.map_log_pand exp (tbl:tbl) syms e1 e2
      | TRIPLE(PLUS, INT arg1, INT arg2) -> defn syms (tbl:tbl) (INT (arg1+arg2)) owidth
      | TRIPLE(MINUS, INT arg1, INT arg2) -> defn syms (tbl:tbl) (INT (arg1-arg2)) owidth
      | TRIPLE(TIMES, INT arg1, INT arg2) -> defn syms (tbl:tbl) (INT (arg1*arg2)) owidth
      | TRIPLE((PLUS|MINUS|TIMES) as op, ID arg1, INT arg2) -> 
          let w = Minimap.symwidth syms arg1 in (op' tbl  op)
                                     exp (tbl:tbl) syms (ID (enterid (Minimap.valid tbl.namtab arg1.id)), w) (INT arg2, Minimap.mask arg2)
      | TRIPLE((PLUS|MINUS|TIMES) as op, INT arg1, ID arg2) -> 
          let w = Minimap.symwidth syms arg2 in (op' tbl  op)
                                     exp (tbl:tbl) syms (INT arg1, Minimap.mask arg1) (ID (enterid (Minimap.valid tbl.namtab arg2.id)), w)
      | TRIPLE((PLUS|MINUS) as op, arg1, arg2) -> let w1 = defn syms (tbl:tbl) arg1 owidth in
                                                  let w2 = defn syms (tbl:tbl) arg2 owidth in
                                                  (op' tbl  op) exp (tbl:tbl) syms w1 w2
      | TRIPLE((TIMES) as op, arg1, arg2) -> let w1 = defn syms (tbl:tbl) arg1 0 in
                                                  let w2 = defn syms (tbl:tbl) arg2 0 in
                                                  (op' tbl  op) exp (tbl:tbl) syms w1 w2
      | TRIPLE(DIVIDE, arg1, arg2) -> let (e1,w1) = defn syms (tbl:tbl) arg1 owidth in
                                      let (e2,w2) = defn syms (tbl:tbl) arg2 owidth in
                                      (TRIPLE(DIVIDE,e1, e2), max w1 w2)
      | TRIPLE(P_SLEFT, arg1, arg2) -> let e1 = defn syms (tbl:tbl) arg1 owidth in
                                       let e2 = defn syms (tbl:tbl) arg2 0 in
                                       tbl.map_hls.map_sleft exp (tbl:tbl) syms e1 e2
      | TRIPLE(P_SRIGHT, arg1, arg2) -> let e1 = defn syms (tbl:tbl) arg1 owidth in
                                        let e2 = defn syms (tbl:tbl) arg2 0 in
                                        tbl.map_hls.map_sright exp (tbl:tbl) syms e1 e2
      | TRIPLE(P_EQUAL, ID arg1, INT arg2) -> let w = Minimap.symwidth syms arg1 in equal exp (tbl:tbl) syms (ID arg1,w) (INT arg2,Minimap.mask arg2)
      | TRIPLE(P_EQUAL, arg1, arg2) -> tbl.map_hls.map_select.map_equal exp (tbl:tbl) syms (defn syms (tbl:tbl) arg1 owidth) (defn syms (tbl:tbl) arg2 owidth)
      | TRIPLE(WILDEQUAL, arg1, arg2) -> (match arg2 with
	| BINNUM str -> let (sz,_) = Const.widthnum stderr 2 str in tbl.map_hls.map_select.map_wildequal exp (tbl:tbl) syms (defn syms (tbl:tbl) arg1 owidth) (arg2,sz)
	| _ -> tbl.map_hls.map_select.map_wildequal exp (tbl:tbl) syms (defn syms (tbl:tbl) arg1 owidth) (defn syms (tbl:tbl) arg2 owidth))
(*
      | TRIPLE(P_CASEEQUAL, arg1, arg2) -> let (e1,w1) = defn syms (tbl:tbl) arg1 owidth in
                                           let (e2,w2) = defn syms (tbl:tbl) arg2 owidth in
                                           (TRIPLE(P_CASEEQUAL,e1, e2), 1)
      | TRIPLE(P_CASENOTEQUAL, arg1, arg2) -> let (e1,w1) = defn syms (tbl:tbl) arg1 owidth in
                                              let (e2,w2) = defn syms (tbl:tbl) arg2 owidth in
                                              (TRIPLE(P_CASENOTEQUAL,e1, e2), 1)
*)
      | TRIPLE(P_NOTEQUAL, arg1, arg2) -> let w1 = defn syms (tbl:tbl) arg1 owidth in
                                          let w2 = defn syms (tbl:tbl) arg2 (max (snd w1) owidth) in
				          tbl.map_hls.map_select.map_noteq exp (tbl:tbl) syms w1 w2
      | TRIPLE(P_GTE, ID arg1, INT arg2) -> let w = Minimap.symwidth syms arg1 in gte exp (tbl:tbl) syms (ID arg1,w) (INT arg2,Minimap.mask arg2)
      | TRIPLE(P_GTE, arg1, arg2) -> gte exp (tbl:tbl) syms (defn syms (tbl:tbl) arg1 owidth) (defn syms (tbl:tbl) arg2 owidth)
      | TRIPLE(P_LTE, arg1, arg2) ->   let w1 = defn syms (tbl:tbl) arg1 owidth in
                                       let w2 = defn syms (tbl:tbl) arg2 (max (snd w1) owidth) in
				       tbl.map_hls.map_select.map_lte exp (tbl:tbl) syms w1 w2
      | TRIPLE(GREATER, arg1, arg2) -> let w1 = defn syms (tbl:tbl) arg1 owidth in
                                       let w2 = defn syms (tbl:tbl) arg2 (max (snd w1) owidth) in
				       tbl.map_hls.map_select.map_greater exp (tbl:tbl) syms w1 w2
      | TRIPLE(LESS, arg1, arg2) ->    let w1 = defn syms (tbl:tbl) arg1 owidth in
                                       let w2 = defn syms (tbl:tbl) arg2 (max (snd w1) owidth) in
				       tbl.map_hls.map_select.map_less exp (tbl:tbl) syms w1 w2
      | QUADRUPLE(P_SRIGHT, arg1, shft, INT w1) -> let (arg,_) = defn syms (tbl:tbl) arg1 w1 in
						   let w2 = defn syms (tbl:tbl) shft w1 in
					   tbl.map_hls.map_sright exp (tbl:tbl) syms (arg,w1) w2
      | QUADRUPLE(PARTSEL, ID id, hi, lo) ->
          let lft = Const.exprConstStr stderr syms hi and
	      rght = Const.exprConstStr stderr syms lo in
          let ihi = int_of_string lft and ilo = int_of_string rght in
          (QUADRUPLE(PARTSEL, ID (enterid (Minimap.valid tbl.namtab id.id)), INT ihi, INT ilo), ihi - ilo + 1)
      | TRIPLE(BITSEL, ID id, INT sel) -> (TRIPLE(BITSEL, ID (enterid (Minimap.valid tbl.namtab id.id)), INT sel), 1)
      | TRIPLE(BITSEL, ID id, ID sel) -> printf "BITSEL %s[%s] not (yet) supported\n" id.id sel.id; (WIDTHNUM(0,1,0),1)
      | TRIPLE(BITSEL, INT num, INT sel) -> (Flatten.const_shift num sel, 1)
      | QUADRUPLE(QUERY, arg1, arg2, arg3) -> let e1 = defn syms (tbl:tbl) arg1 1 in
                                              let e2 = defn syms (tbl:tbl) arg2 owidth in
                                              let e3 = defn syms (tbl:tbl) arg3 owidth in
                                              tbl.map_hls.map_select.map_mux2 exp (tbl:tbl) syms e1 e2 e3
      | WIDTHNUM(radix,sz,num) -> (WIDTHNUM(radix,sz,num),sz)
      | other -> Dump.unhandled stderr 370 other; (other,1)) in
    (Hashtbl.add tbl.defnhash (exp,owidth) (rslt,wid); (rslt,wid))

and op' tbl = function
  | PLUS -> tbl.map_hls.map_select.map_plus
  | MINUS -> tbl.map_hls.map_select.map_minus
  | TIMES -> tbl.map_hls.map_times
  | oth -> failwith (Dump.dumpstr oth)

and idxtrans syms (tbl:tbl) id sel1 sel2 = let (hi,lo,inc) = Minimap.find_width id syms in
                                           let sel1m = if (lo > 0) then TRIPLE(MINUS,sel1,INT lo) else sel1 in
                                           let sel2m = if (lo > 0) then TRIPLE(MINUS,sel2,INT lo) else sel2 in
                                           QUADRUPLE(PARTSEL, ID (enterid (Minimap.valid tbl.namtab id.id)), fst(defn syms (tbl:tbl) sel1m (-1)), fst(defn syms (tbl:tbl) sel2m (-1)))

and replace' hashtab syms (tbl:tbl) pred = function
  | QUADRUPLE((IF|QUERY|PARTSEL) as op, arg1, hi, lo) ->
      QUADRUPLE(op,
                replace' hashtab syms (tbl:tbl) pred arg1,
                replace' hashtab syms (tbl:tbl) pred hi,
                replace' hashtab syms (tbl:tbl) pred lo)
  | TRIPLE((PLUS|P_SLEFT|P_SRIGHT|P_GTE) as op, arg1, arg2) -> 
      TRIPLE(op,
             replace' hashtab syms (tbl:tbl) pred arg1,
             replace' hashtab syms (tbl:tbl) pred arg2)
  | ID id ->
    (*
      if !verbose then printf "considering %s (len=%d)" id (Hashtbl.length hashtab);
      print_newline();
      Hashtbl.iter (fun k _ -> if !verbose then printf "entry %s" k; print_newline()) hashtab;
    *)
      if Hashtbl.mem hashtab id && (let (predicate,rep,blocking) = List.hd (Hashtbl.find hashtab id) in blocking && (pred=predicate))
      then
        let h = hadd id in (if !verbose then printf "replacing %s with %s\n" id.id h.id; ID h)
      else
        ID id
  | DOUBLE(LPAREN, arg1) -> replace' hashtab syms (tbl:tbl) pred arg1
  | INT n -> WIDTHNUM(10,Minimap.mask n,n)
  | oth -> if !verbose then printf "Replace %s\n" (Count.tokenstr oth); oth

and rewrite syms (tbl:tbl) dest clk pred expr hashtab = match expr with
  | QUINTUPLE(NAMED, ID name, TLIST params, itm, EMPTY) -> rewrite syms (tbl:tbl) dest clk pred itm hashtab
  | TLIST lst -> List.iter (fun itm -> rewrite syms (tbl:tbl) dest clk pred itm hashtab) lst
  | QUADRUPLE((IF|QUERY), cond, then_clause, else_clause) -> 
      rewrite syms (tbl:tbl) dest clk (if pred <> EMPTY then TRIPLE(AND,cond,pred) else cond) then_clause hashtab;
      rewrite syms (tbl:tbl) dest clk (let ncond = DOUBLE(PLING,cond) in if pred <> EMPTY then TRIPLE(AND,ncond,pred) else ncond) else_clause hashtab;
  | DOUBLE(LPAREN, expr) -> rewrite syms (tbl:tbl) dest clk pred expr hashtab
  | TRIPLE((PLUS|MINUS), lft, rght) -> List.iter (fun itm -> rewrite syms (tbl:tbl) dest clk pred itm hashtab) [lft;rght]
  | TRIPLE(IF, cond, then_clause) -> 
      rewrite syms (tbl:tbl) dest clk (if pred <> EMPTY then TRIPLE(AND,cond,pred) else cond) then_clause hashtab;
  | QUADRUPLE((CASE|CASEZ|CASEX), cexpr, EMPTY, TLIST lst) ->
    let oldpred = ref pred in List.iter (function
      | TRIPLE(CASECOND, TLIST lst, itm) ->
	let clst = ref EMPTY in List.iter (fun citem ->
	  let c = TRIPLE(WILDEQUAL,cexpr,citem) in
          clst := (if !clst <> EMPTY then TRIPLE(OR,!clst,c) else c)) lst;
	let newpred = if !oldpred <> EMPTY then TRIPLE(AND,!clst,!oldpred) else !clst in
        rewrite syms (tbl:tbl) dest clk newpred itm hashtab;
	oldpred := let ncond = DOUBLE(PLING,!clst) in
		   if !oldpred <> EMPTY then TRIPLE(AND,ncond,!oldpred) else ncond
      | DOUBLE(DEFAULT, itm) -> rewrite syms (tbl:tbl) dest clk !oldpred itm hashtab
      | itm -> Dump.unhandled stderr 362 itm) lst
  | QUADRUPLE((ASSIGNMENT|DLYASSIGNMENT) as op, target, dly, next) ->
      let hashit dest next =
        let rep = replace' hashtab syms (tbl:tbl) pred next in
      (*
        if !verbose then printf "next %s replaced by %s" (tokenstr next) (Count.tokenstr rep); print_newline();
      *)
        if Hashtbl.mem hashtab dest then Hashtbl.replace hashtab dest ((pred, rep, op = ASSIGNMENT) :: (Hashtbl.find hashtab dest))
        else Hashtbl.add hashtab dest [(pred, rep, op = ASSIGNMENT)];
    (*
      if !verbose then printf "hashing %s (len=%d)" dest (Hashtbl.length hashtab); print_newline()
    *) 
      in
      (match dly with
	| EMPTY -> ()
	| DOUBLE(HASH, FLOATNUM f) -> ()
	| _ -> Dump.unhandled stderr 369 dly);
      (match target with
	| ID dest -> hashit dest next
	| TRIPLE(BITSEL, ID dest, INT idx) ->
            let (hi,lo,inc) = Minimap.find_width dest syms in
	    if idx = lo then hashit dest (DOUBLE(CONCAT, TLIST [QUADRUPLE(PARTSEL, ID dest, INT hi, INT (idx+1)); next]))
	    else if idx < hi then hashit dest (DOUBLE(CONCAT, TLIST [QUADRUPLE(PARTSEL, ID dest, INT hi, INT (idx+1));
							             next; QUADRUPLE(PARTSEL, ID dest, INT (idx-1), INT lo)]))
	    else if idx = hi then hashit dest (DOUBLE(CONCAT, TLIST [next; QUADRUPLE(PARTSEL, ID dest, INT (hi-1), INT lo)]))
	    else printf "Bit select of %s[%d:%d] is out of range\n" dest.id hi lo
	| TRIPLE(BITSEL, ID dest, ID idx) ->
(*
            let (hi,lo,inc) = Minimap.find_width dest syms in
*)
	    printf "Bit select of %s[%s] is not (yet) supported\n" dest.id idx.id
	| QUADRUPLE(PARTSEL, ID dest, INT thi, INT tlo) ->
            let (hi,lo,inc) = Minimap.find_width dest syms in
	    if tlo = lo then hashit dest (DOUBLE(CONCAT, TLIST [QUADRUPLE(PARTSEL, ID dest, INT hi, INT (tlo+1)); next]))
	    else if thi < hi then hashit dest (DOUBLE(CONCAT, TLIST [QUADRUPLE(PARTSEL, ID dest, INT hi, INT (thi+1));
							             next; QUADRUPLE(PARTSEL, ID dest, INT (tlo-1), INT lo)]))
	    else if thi = hi then hashit dest (DOUBLE(CONCAT, TLIST [next; QUADRUPLE(PARTSEL, ID dest, INT (tlo-1), INT lo)]))
	    else printf "Bit select of %s[%d:%d] is out of range\n" dest.id hi lo
	| DOUBLE(CONCAT, TLIST clst) ->
	  let progress = ref 0 in
	  List.iter (function
	  | ID dest ->
	    let progress2 = Minimap.elwidth syms (ID dest) in
	    let part = QUADRUPLE ( P_SRIGHT, next, INT !progress, INT progress2 ) in
	    hashit dest part;
	    if !verbose then printf "Assign to concat {%s} width %d offset %d\n" dest.id progress2 !progress;
	    progress := !progress + progress2
	  | oth -> Dump.unhandled stderr 413 target) (List.rev clst);
	| _ -> Dump.unhandled stderr 465 target)
  | TRIPLE(TASKREF, ID taskid, tasks) -> ()
  | TRIPLE((D_DISPLAY|D_WRITE), ASCNUM fmt, disp) -> ()
  | DOUBLE(D_STOP, EMPTY) -> ()
  | DOUBLE(D_FINISH, EMPTY) -> ()
  | DOUBLE(D_DISPLAY, EMPTY) -> ()
  | EMPTY -> ()
  | HEXNUM _ -> ()
  | ID _ -> ()
  | _ -> Dump.unhandled stderr 474 expr

and trunc (src,swid) dwid = 
  if swid < dwid
  then fst(Minimap.lfill (src,swid) dwid)
  else if swid > dwid
  then QUADRUPLE(PARTSEL, src, INT (dwid-1), INT 0)
  else src

and monadic_buffer (tbl:tbl) syms path wid (dst,src) =
  let out = Minimap.netid "buf" tbl.gentab tbl.namtab in
  Minimap.declare tbl.namtab syms out (wid-1) 0;  
  assert (wid>0);
(*
  assert (tbl.map_hls.map_id=Map_hls);
*)
  let src' = shorten syms wid src in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), src');
                                               TRIPLE(CELLPIN, ID (enterid "Y"), dst)] in
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_BUF" :: string_of_int wid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (path out, SCALAR,
                                            TLIST
                                              pins)])) ()

and monadic_flipflop (tbl:tbl) syms path wid (dst,src) = function
    | [DOUBLE(POSEDGE,clk)] ->
       let out = Minimap.netid "ff" tbl.gentab tbl.namtab in
       let pins = [TRIPLE(CELLPIN, ID (enterid "D"), src);
						  TRIPLE(CELLPIN, ID (enterid "CK"), clk);
						  TRIPLE(CELLPIN, ID (enterid "Q"), dst)] in
	Minimap.declare tbl.namtab syms out (wid-1) 0;
	assert (wid>0);
	Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_DFF_SYNC" :: string_of_int wid :: [], pins, syms)), EMPTY,
                                       TLIST
					 [TRIPLE
                                             (path out, SCALAR,
                                              TLIST
						pins)])) ()
    | [DOUBLE(POSEDGE,clk);TRIPLE(POSEDGE,clr, clr_val)] ->
	let out = Minimap.netid "ff" tbl.gentab tbl.namtab in
	Minimap.declare tbl.namtab syms out (wid-1) 0;  
	assert (wid>0);
        let pins = [TRIPLE(CELLPIN, ID (enterid "D"), src);
						  TRIPLE(CELLPIN, ID (enterid "CK"), clk);
						  TRIPLE(CELLPIN, ID (enterid "RN"), clr);
						  TRIPLE(CELLPIN, ID (enterid "DEF"), clr_val);
						  TRIPLE(CELLPIN, ID (enterid "Q"), dst)] in
	Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_DFF_SYNC" :: string_of_int wid :: [], pins, syms)), EMPTY,
                                       TLIST
					 [TRIPLE
                                             (path out, SCALAR,
                                              TLIST
						pins)])) ()
    | edglst ->
        Dump.unhandled stderr 495 (TLIST edglst);
	let dumpedge edglst :string =
	  let buf = Buffer.create 64 in
	  List.iter (function
	    | DOUBLE(edg,net) -> bprintf buf "%s:%s " (Ord.getstr edg) (Count.tokenstr net)
	    | TRIPLE(edg,net,clrval) -> bprintf buf "%s:%s:%s " (Ord.getstr edg) (Count.tokenstr net) (Count.tokenstr clrval) 
	    | x -> Dump.unhandled stderr 506 x; failwith "inMinimap.valid tbl.namtab edgelist") edglst;
	  Buffer.contents buf in
        failwith ("ff with control logic "^(dumpedge edglst)^" not implemented")

and bus_flipflop (tbl:tbl) syms edglst (dst,dwid) (src,swid) =
  if !verbose then printf "assign_next %s w=%d\n" (Count.tokenstr dst) dwid;
  monadic_flipflop (tbl:tbl) syms (Minimap.validid tbl.namtab) dwid (dst,src) edglst

and buffer_signal (tbl:tbl) syms path = function
  | TRIPLE (ASSIGNMENT, dst, src) ->
      let w = Minimap.elwidth syms dst in
      if (w>0) then
	tbl.map_hls.map_select.map_buffer (tbl:tbl) syms path w (dst,src)
      else
	  Dump.unhandled stderr 504 dst;
  | oth -> Dump.unhandled stderr 505 oth

and define exp (tbl:tbl) syms dst wid src =
  defkind (tbl:tbl) syms dst wid;
  if !verbose then printf "799 v2hls.buffer_signal %s %s\n" dst.id (Count.tokenstr src);
  buffer_signal (tbl:tbl) syms (Minimap.validid tbl.namtab) (TRIPLE(ASSIGNMENT,ID dst,src))

and assign exp (tbl:tbl) syms (dst:token) (src,wid) =
  (match dst with 
  | ID id -> defkind (tbl:tbl) syms id wid
  | TRIPLE(BITSEL, ID id, INT n) -> defkind (tbl:tbl) syms id (n+1)
  | _ -> Dump.unhandled stderr 805 dst);
  if !verbose then printf "806 v2hls.buffer_signal %s %s\n" (Dump.dumpstr dst) (Count.tokenstr src);
  buffer_signal (tbl:tbl) syms (Minimap.validid tbl.namtab) (TRIPLE(ASSIGNMENT,dst,src))

and plus exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = (max lwid rwid)+1 in
let out = Minimap.netid "sum" tbl.gentab tbl.namtab in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), lft);
                                               TRIPLE(CELLPIN, ID (enterid "B"), rght);
                                               TRIPLE(CELLPIN, ID (enterid "Y"), ID out)] in
  Minimap.declare tbl.namtab syms out (wid-1) 0;
  assert (lwid>0 && rwid>0);
  assert (tbl.map_hls.map_id=Map_hls);
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_ARI_ADD" :: string_of_int wid :: string_of_int lwid :: string_of_int rwid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (ID out,wid)

and minus exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = (max lwid rwid)+1 in
let out = Minimap.netid "dif" tbl.gentab tbl.namtab in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), lft);
                                               TRIPLE(CELLPIN, ID (enterid "B"), rght);
                                               TRIPLE(CELLPIN, ID (enterid "Y"), ID out)] in
  Minimap.declare tbl.namtab syms out (wid-1) 0;
  assert (lwid>0 && rwid>0);
  assert (tbl.map_hls.map_id=Map_hls);
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_ARI_SUB" :: string_of_int wid :: string_of_int lwid :: string_of_int rwid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (ID out,wid)

and times' exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
let out = Minimap.netid "mul" tbl.gentab tbl.namtab in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), lft);
                                               TRIPLE(CELLPIN, ID (enterid "B"), rght);
                                               TRIPLE(CELLPIN, ID (enterid "Y"), ID out)] in
  Minimap.declare tbl.namtab syms out (lwid-1) 0;
  assert (lwid>0 && rwid>0);
  assert (tbl.map_hls.map_id=Map_hls);
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_ARI_MUL" :: string_of_int lwid :: string_of_int lwid :: string_of_int rwid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (ID out,lwid)

and times exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let pout = Array.make lwid (TIMES,0) in
  let sum = Array.make lwid (TIMES,0) in
  let rght' = List.rev (Minimap.list_shorten lwid (Minimap.list_pad lwid (Minimap.concat_flatten' syms [lft]))) in
  list_iteri 0 (fun i select ->
    pout.(i) <- tbl.map_hls.map_select.map_logand exp (tbl:tbl) syms (DOUBLE(CONCAT, TLIST (Array.to_list (Array.make rwid select))),rwid) (rght,rwid);
    sum.(i) <- if i > 0 
    then
      let shft = (DOUBLE (CONCAT, TLIST [fst pout.(i); WIDTHNUM(2,i,0)]), rwid+i) in
      tbl.map_hls.map_select.map_plus exp tbl syms shft sum.(i-1)
    else
      pout.(0)) rght';
  sum.(lwid-1)

and sleft exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let pout = Array.make rwid (P_SLEFT, 0) in
  let rght' = List.rev (Minimap.list_shorten rwid (Minimap.list_pad rwid (Minimap.concat_flatten' syms [rght]))) in
  list_iteri 0 (fun i select ->
    if !verbose then printf "sleft %s %s\n" (Dump.dumpstr select) (Dump.dumpstr lft);
    let src = if i = 0 then (lft,lwid) else pout.(i-1) in
    let increased = DOUBLE (CONCAT, TLIST [fst src; WIDTHNUM(2,1 lsl i,0)]) in
    pout.(i) <- tbl.map_hls.map_select.map_mux2 exp (tbl:tbl) syms (select,1) (increased, (snd src) + (1 lsl i)) src) rght';
  pout.(rwid-1)

and sright exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let pout = Array.make rwid (P_SRIGHT, 0) in
  let rght' = List.rev (Minimap.list_shorten rwid (Minimap.list_pad rwid (Minimap.concat_flatten' syms [rght]))) in
  list_iteri 0 (fun i select ->
    if !verbose then printf "sright %s %s\n" (Dump.dumpstr select) (Dump.dumpstr lft);
    let src = if i = 0 then (lft,lwid) else pout.(i-1) in
    let src' = Minimap.num_flatten (1 lsl rwid) 0 [] @ Minimap.concat_flatten' syms [fst src] in
    let src'' = Minimap.list_pad lwid (Minimap.list_shorten lwid (List.rev(Minimap.list_shorten (List.length src' - (1 lsl i)) (List.rev src')))) in
    let reduced = DOUBLE (CONCAT, TLIST src''), lwid in
    pout.(i) <- tbl.map_hls.map_select.map_mux2 exp (tbl:tbl) syms (select,1) reduced src) rght';
  pout.(rwid-1)

and wildequal exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let out = Minimap.netid "weq" tbl.gentab tbl.namtab in
    let pins = [TRIPLE(CELLPIN, ID (enterid "A"), lft);
                                               TRIPLE(CELLPIN, ID (enterid "B"), rght);
                                               TRIPLE(CELLPIN, ID (enterid "Y"), ID out)] in
  Minimap.declare tbl.namtab syms out 0 0;
  assert (wid>0);
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_EQ" :: string_of_int lwid :: string_of_int rwid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (ID out,1)

and sleft' exp (tbl:tbl) syms (lft,lwid) (rght,rwid) = match rght with
  | WIDTHNUM(radix,sz,num) -> (DOUBLE (CONCAT, TLIST [lft; WIDTHNUM(2,num,0)]), lwid+num)
  | INT n -> (DOUBLE (CONCAT, TLIST [lft; WIDTHNUM(2,n,0)]), lwid+n)
  | _ ->
      let wid = 1 + max lwid rwid in
      let out = Minimap.netid "shl" tbl.gentab tbl.namtab in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), lft);
                                               TRIPLE(CELLPIN, ID (enterid "B"), rght);
                                               TRIPLE(CELLPIN, ID (enterid "Y"), ID out)] in
      Minimap.declare tbl.namtab syms out (wid-1) 0;
      assert (lwid>0 && rwid>0);
      assert (tbl.map_hls.map_id=Map_hls);
      Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_SH_L" :: string_of_int wid :: string_of_int lwid :: string_of_int rwid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (ID out,wid)

and sright' exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let out = Minimap.netid "shr" tbl.gentab tbl.namtab in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), lft);
                                               TRIPLE(CELLPIN, ID (enterid "B"), rght);
                                               TRIPLE(CELLPIN, ID (enterid "Y"), ID out)] in
  Minimap.declare tbl.namtab syms out (lwid-1) 0;
  assert (lwid>0 && rwid>0);
  assert (tbl.map_hls.map_id=Map_hls);
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_SH_R" :: string_of_int lwid :: string_of_int lwid :: string_of_int rwid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (ID out,lwid)

and mux2 exp (tbl:tbl) syms (sel,_) (hi,hwid) (lo,lwid) =
  let wid = max hwid lwid in
  let out = Minimap.netid "mux" tbl.gentab tbl.namtab in
  Minimap.declare tbl.namtab syms out (wid-1) 0;
  assert (hwid>0 && lwid>0);
  let lo' = Minimap.lfill' tbl.namtab syms wid lo in
  let hi' = Minimap.lfill' tbl.namtab syms wid hi in
  let out' = shorten syms wid (ID out) in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), lo');
                                               TRIPLE(CELLPIN, ID (enterid "B"), hi');
                                               TRIPLE(CELLPIN, ID (enterid "S"), sel);
                                               TRIPLE(CELLPIN, ID (enterid "Y"), out')] in
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_MUX" :: string_of_int wid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (out',wid)

and redand exp (tbl:tbl) syms (src,wid) =
  let out = Minimap.netid "rand" tbl.gentab tbl.namtab in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), src);
                                               TRIPLE(CELLPIN, ID (enterid "Y"), ID out)] in
  Minimap.declare tbl.namtab syms out 0 0;
  assert (wid>0);
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_RED_AND" :: string_of_int wid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (ID out,1)

and redor exp (tbl:tbl) syms (src,wid) =
  let out = Minimap.netid "ror" tbl.gentab tbl.namtab in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), src);
                                               TRIPLE(CELLPIN, ID (enterid "Y"), ID out)] in
  Minimap.declare tbl.namtab syms out 0 0;
  assert (wid>0);
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_RED_OR" :: string_of_int wid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (ID out,1)

and pling exp (tbl:tbl) syms (src,wid) =
  let out = Minimap.netid "not" tbl.gentab tbl.namtab in
  Minimap.declare tbl.namtab syms out 0 0;
  assert (wid>0);
  let src' = shorten syms wid src in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), src');
                                               TRIPLE(CELLPIN, ID (enterid "Y"), ID out)] in
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_LOG_NOT" :: string_of_int wid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (ID out,1)

and unaryplus exp (tbl:tbl) syms (src,wid) =
  let out = Minimap.netid "idn" tbl.gentab tbl.namtab in
  Minimap.declare tbl.namtab syms out (wid-1) 0;
  assert (wid>0);
(*
  assert (tbl.map_hls.map_id=Map_hls);
*)
  let src' = shorten syms wid src in
  let out' = shorten syms wid (ID out) in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), src');
                                               TRIPLE(CELLPIN, ID (enterid "Y"), out')] in
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_BUF" :: string_of_int wid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (out',wid)

and tilde exp (tbl:tbl) syms (src,wid) =
  let out = Minimap.netid "til" tbl.gentab tbl.namtab in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), src);
                                               TRIPLE(CELLPIN, ID (enterid "Y"), ID out)] in
  Minimap.declare tbl.namtab syms out (wid-1) 0;
  assert (wid>0);
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_BW_NOT" :: string_of_int wid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (ID out,wid)


and logand exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = min lwid rwid in
  let out = Minimap.netid "land" tbl.gentab tbl.namtab in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), lft);
                                               TRIPLE(CELLPIN, ID (enterid "B"), rght);
                                               TRIPLE(CELLPIN, ID (enterid "Y"), ID out)] in
  Minimap.declare tbl.namtab syms out (wid-1) 0;
  assert (wid>0);
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_BW_AND" :: string_of_int wid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (ID out,wid)

and logor exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let out = Minimap.netid "lor" tbl.gentab tbl.namtab in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), lft);
                                               TRIPLE(CELLPIN, ID (enterid "B"), rght);
                                               TRIPLE(CELLPIN, ID (enterid "Y"), ID out)] in
  Minimap.declare tbl.namtab syms out (wid-1) 0;
  assert (wid>0);
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_BW_OR" :: string_of_int wid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (ID out,wid)

and logxor exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let out = Minimap.netid "lxor" tbl.gentab tbl.namtab in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), lft);
                                               TRIPLE(CELLPIN, ID (enterid "B"), rght);
                                               TRIPLE(CELLPIN, ID (enterid "Y"), ID out)] in
  Minimap.declare tbl.namtab syms out (wid-1) 0;
  assert (wid>0);
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_BW_XOR" :: string_of_int wid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (ID out,wid)

and log_pand exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let out = Minimap.netid "pand" tbl.gentab tbl.namtab in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), lft);
                                               TRIPLE(CELLPIN, ID (enterid "B"), rght);
                                               TRIPLE(CELLPIN, ID (enterid "Y"), ID out)] in
  Minimap.declare tbl.namtab syms out (wid-1) 0;
  assert (wid>0);
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_LOG_AND" :: string_of_int wid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (ID out,1)

and log_por exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let out = Minimap.netid "por" tbl.gentab tbl.namtab in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), lft);
                                               TRIPLE(CELLPIN, ID (enterid "B"), rght);
                                               TRIPLE(CELLPIN, ID (enterid "Y"), ID out)] in
  Minimap.declare tbl.namtab syms out (wid-1) 0;
  assert (wid>0);
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_LOG_OR" :: string_of_int wid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (ID out,1)

and equal exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let out = Minimap.netid "equ" tbl.gentab tbl.namtab in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), lft);
                                               TRIPLE(CELLPIN, ID (enterid "B"), rght);
                                               TRIPLE(CELLPIN, ID (enterid "Y"), ID out)] in
  Minimap.declare tbl.namtab syms out 0 0;
  assert (wid>0);
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_EQ" :: string_of_int lwid :: string_of_int rwid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (ID out,1)

and noteq exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let out = Minimap.netid "neq" tbl.gentab tbl.namtab in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), lft);
                                               TRIPLE(CELLPIN, ID (enterid "B"), rght);
                                               TRIPLE(CELLPIN, ID (enterid "Y"), ID out)] in
  Minimap.declare tbl.namtab syms out 0 0;
  assert (wid>0);
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_NEQ" :: string_of_int lwid :: string_of_int rwid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (ID out,1)

and gte exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let out = Minimap.netid "gte" tbl.gentab tbl.namtab in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), lft);
                                               TRIPLE(CELLPIN, ID (enterid "B"), rght);
                                               TRIPLE(CELLPIN, ID (enterid "Y"), ID out)] in
  Minimap.declare tbl.namtab syms out 0 0;
  assert (wid>0);
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_GEQ" :: string_of_int lwid :: string_of_int rwid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (ID out,1)

and greater exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let out = Minimap.netid "grt" tbl.gentab tbl.namtab in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), lft);
                                               TRIPLE(CELLPIN, ID (enterid "B"), rght);
                                               TRIPLE(CELLPIN, ID (enterid "Y"), ID out)] in
  Minimap.declare tbl.namtab syms out 0 0;
  assert (wid>0);
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_GREATER" :: string_of_int lwid :: string_of_int rwid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (ID out,1)

and less exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let out = Minimap.netid "les" tbl.gentab tbl.namtab in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), lft);
                                               TRIPLE(CELLPIN, ID (enterid "B"), rght);
                                               TRIPLE(CELLPIN, ID (enterid "Y"), ID out)] in
  Minimap.declare tbl.namtab syms out 0 0;
  assert (wid>0);
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_LESS" :: string_of_int lwid :: string_of_int rwid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (ID out,1)

and lte exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let out = Minimap.netid "lte" tbl.gentab tbl.namtab in
  let pins = [TRIPLE(CELLPIN, ID (enterid "A"), lft);
                                               TRIPLE(CELLPIN, ID (enterid "B"), rght);
                                               TRIPLE(CELLPIN, ID (enterid "Y"), ID out)] in
  Minimap.declare tbl.namtab syms out 0 0;
  assert (wid>0);
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID (cacheid ("DVL_LEQ" :: string_of_int lwid :: string_of_int rwid :: [], pins, syms)), EMPTY,
                                     TLIST
                                       [TRIPLE
                                           (instid' tbl, SCALAR,
                                            TLIST
                                              pins)])) (); (ID out,1)
    
and cdump' tbl syms dmp dest' itm arg1 arg2 owidth alt =
  let arg1' = defn syms (tbl:tbl) arg1 1 in
  let arg2' = defn syms (tbl:tbl) arg2 owidth in
  let rslt = mux2 itm (tbl:tbl) syms arg1' arg2' alt in
  Hashtbl.add dmp dest' (arg1, arg2, arg1', arg2', alt, rslt);
  rslt

and assign_comb comb dmp hashtab syms tbl itm dest dest' arg w =
    assign itm (tbl:tbl) syms (ID (enterid dest'))
      (match List.rev arg with
	| [] -> failwith "empty list in always_comb"
        | (EMPTY,next,blocking) :: r ->
	  if not comb then printf "%s is not sequential in always\n" dest';
	  List.fold_left (fun alt (cond,next',blocking) -> cdump' tbl syms dmp dest' itm cond next' w alt)
	    (defn syms (tbl:tbl) next w) r
        | r -> let next = if comb then WIDTHNUM(2, w, 0) else ID dest in
	       if comb then printf "%s not combinational in always_comb\n" dest';
	  List.fold_left (fun alt (cond,next',blocking) -> cdump' tbl syms dmp dest' itm cond next' w alt)
	    (defn syms (tbl:tbl) next w) r
      )

and edge hashtab syms (tbl:tbl) edglst itm =
  rewrite syms (tbl:tbl) "" edglst EMPTY itm hashtab; Hashtbl.iter (fun dest arg ->
    Hashtbl.add tbl.edges dest.id (List.map (fun (cond,nxt,blk) -> (Verilogout.dump_logic cond,Verilogout.dump_logic nxt,blk)) arg);
    let w = Minimap.symwidth syms dest
    and dest'=Minimap.valid tbl.namtab (hadd dest).id in
    if !verbose then printf "edge %s w=%d\n" dest.id w;
    bus_flipflop (tbl:tbl) syms edglst (Minimap.validid tbl.namtab dest,w) (ID (enterid dest'),w);
    assign_comb false tbl.edgdmp hashtab syms tbl itm dest dest' arg w) hashtab

and always_comb hashtab syms (tbl:tbl) itm =
  rewrite syms (tbl:tbl) "" [] EMPTY itm hashtab; Hashtbl.iter (fun dest arg ->
    Hashtbl.add tbl.combs dest.id (List.map (fun (cond,nxt,blk) -> (Verilogout.dump_logic cond,Verilogout.dump_logic nxt,blk)) arg);
    let w = Minimap.symwidth syms dest
    and dest'=Minimap.valid tbl.namtab (hadd dest).id in
    if !verbose then printf "always_comb %s w=%d\n" dest.id w;
    assert (w>0);
    tbl.map_hls.map_select.map_buffer (tbl:tbl) syms (Minimap.validid tbl.namtab) w (Minimap.validid tbl.namtab dest,ID (enterid dest'));
    assign_comb true tbl.combdmp hashtab syms tbl itm dest dest' arg w) hashtab

let mydump' syms (tbl:tbl) txt itm = Hashtbl.add tbl.snd (ASCNUM (txt^" "^Count.tokenstr (fst (defn syms (tbl:tbl) itm 0)))) ()

let rec initial syms (tbl:tbl) stmt delim =
  match stmt with
  | TLIST lst -> List.iter (fun itm -> match itm with
    | QUADRUPLE((ASSIGNMENT|DLYASSIGNMENT), ID dest, EMPTY, next) ->
      let w = Minimap.symwidth syms dest in
      assign itm (tbl:tbl) syms (Minimap.validid tbl.namtab dest) (defn syms (tbl:tbl) next w)
    | QUINTUPLE(FOR, TRIPLE(ASSIGNMENT,ID idstart, start), test, TRIPLE(ASSIGNMENT,ID idinc,inc), clause) -> mydump' syms (tbl:tbl) "for" itm
    | TRIPLE(WHILE, expr, stmt) -> mydump' syms (tbl:tbl) "-- while " expr; mydump' syms (tbl:tbl) "-- do " stmt
    | TRIPLE(D_READMEMB, ASCNUM str, ID prog) -> mydump' syms (tbl:tbl) "-- readmemb" itm
    | TRIPLE(D_FOPEN, fd, ASCNUM txt) -> mydump' syms (tbl:tbl) "-- fopen" itm
    | DOUBLE(D_FINISH, EMPTY) -> mydump' syms (tbl:tbl) "-- finish " EMPTY
    | DOUBLE(DOUBLE (AT, TLIST [DOUBLE (POSEDGE, ID clk)]), atstmt) -> mydump' syms (tbl:tbl) "at" itm; delim := "next";
      initial syms (tbl:tbl) atstmt delim
    | DOUBLE(D_MONITOR, TLIST ((ASCNUM str) :: lst)) -> mydump' syms (tbl:tbl) ("-- $monitor"^str) (TLIST lst);
    | DOUBLE(DOUBLE (HASH, FLOATNUM f), dlystmt) -> initial syms (tbl:tbl) dlystmt delim
    | _ -> Dump.unhandled stderr 260 itm) lst
  | EMPTY -> ()
  | _ -> initial syms (tbl:tbl) (TLIST [stmt]) delim

let map_mod_inst tbl syms kind modlst primargs find swidth modid params = 
  begin
  let inst = List.map (function
    | TRIPLE(ID idinst, SCALAR, TLIST arg4) -> 
        let argpos = Array.make (List.length primargs) (enterid "$",EMPTY,0,EMPTY)
        and ix = ref 0 in List.iter (function
	  | QUINTUPLE((INPUT|OUTPUT|INOUT), (EMPTY|REG), EMPTY, (EMPTY|RANGE(_,_)), DOUBLE(ID id, EMPTY))
          | ID id -> argpos.(!ix) <- (id,find id,swidth id,EMPTY); incr ix
          | oth -> Dump.unhandled stderr 341 oth) primargs;
        let argcnt = ref 0 in List.iter (fun y -> if !argcnt < Array.length argpos
          then ((match y with
            | TRIPLE(CELLPIN, ID cellpin, conn) ->
                let found = ref false in
	        Array.iteri (fun ix (e,dir,w,_) -> if e = cellpin then (found := true; argpos.(ix) <- (e,dir,w,conn))) argpos;
	        if not !found then
                  begin
                    Array.iter(fun (arg1,arg2,arg3,arg4) ->
                      printf "%s %s %d %s\n" arg1.id (Dump.dumpstr arg2) arg3 (Dump.dumpstr arg4)) argpos;
                    failwith (sprintf "Cell-pin %s not found in idinst %s of type %s" cellpin.id idinst.id kind.id)
                  end
            | _ -> let (e,dir,w,_) = argpos.(!argcnt) in argpos.(!argcnt) <- (e,dir,w,y)); incr argcnt)
          else
            failwith "surplus modinst args") arg4;
        let plst = ref [] in Array.iteri (fun ix (e,dir,w,y) -> plst := TRIPLE(CELLPIN, Minimap.validid tbl.namtab e, match y with
          | EMPTY (*unconnected pin *)
          | DOUBLE (CELLPIN, ID _) (* explicit unconnect *) ->
              if !verbose then printf "Cell-pin %s unconnected in idinst %s of type %s\n" e.id idinst.id kind.id;
              let uncon = sprintf "uncon_%s_%s_%d" idinst.id e.id w in
              Minimap.declare tbl.namtab syms (enterid uncon) (w-1) 0;
              ID (enterid uncon)
          | TRIPLE(BITSEL, ID expr, INT sel) ->
              (idxtrans syms (tbl:tbl) expr (INT sel) (INT sel))
          | QUADRUPLE(PARTSEL, ID expr, INT hi, INT lo) ->
	      (idxtrans syms (tbl:tbl) expr (INT hi) (INT lo))
          | DOUBLE(CONCAT, TLIST clst) ->
	    if dir <> OUTPUT then
	      (
	        TLIST (List.map (function
	          | TRIPLE (BITSEL, ID bit, INT idx1) -> idxtrans syms (tbl:tbl) bit (INT idx1) (INT idx1)
	          | ID id -> Minimap.validid tbl.namtab id
	          | oth -> Dump.unhandled stderr 1212 oth; UNKNOWN) clst)
	      )
            else
              (let concat = enterid (sprintf "concat_%s_%s" idinst.id e.id) in
	       let ix = ref (w-1) in List.iter (function
	         | TRIPLE (BITSEL, ID bit, INT idx1) as b -> assign y (tbl:tbl) syms b (TRIPLE(BITSEL,ID concat,INT !ix),1); decr ix
	         | ID id ->
                   let sw = swidth id in
                   assign y (tbl:tbl) syms (ID id) (QUADRUPLE(PARTSEL,ID concat,INT !ix, INT (!ix-sw+1)),sw); ix := !ix - sw
	         | oth -> Dump.unhandled stderr 1221 oth) clst;
               ID concat
              )
          | (INT _|BINNUM _|OCTNUM _|DECNUM _|HEXNUM _|WIDTHNUM _|ID _) ->
              fst(defn syms (tbl:tbl) y 0)
	  | DOUBLE((AND|OR|XOR|NOT|LPAREN|PLING), _) ->
              fst(defn syms (tbl:tbl) y 0)
	  | TRIPLE((PLUS|MINUS|P_EQUAL|P_ANDAND|P_OROR|OR|AND|P_GTE),_,_) ->
              fst(defn syms (tbl:tbl) y 0)
	  | QUADRUPLE(QUERY,_,_,_) ->
              fst(defn syms (tbl:tbl) y 0)
          | oth -> Dump.unhandled stderr 1208 oth; UNKNOWN) :: !plst) argpos;
        TRIPLE(ID idinst, SCALAR, TLIST !plst) 
    | oth -> Dump.unhandled stderr 1210 oth; UNKNOWN) modlst in
  Hashtbl.add tbl.snd (QUADRUPLE(MODINST, ID modid, params, TLIST inst)) ()
end
    
let rec synthdecl syms (tbl:tbl) = function
| QUADRUPLE((WIRE|TRI0|TRI1), arg0, TRIPLE(arg1, rng, arg3), TLIST arg4) -> List.iter (function
    | DOUBLE(ID _, _) -> ()
    | TRIPLE(ID id, EMPTY, exp) -> let w = Minimap.symwidth syms id in
        assign exp (tbl:tbl) syms (Minimap.validid tbl.namtab id) (defn syms (tbl:tbl) exp w)
    | x -> Dump.unhandled stderr 480 x) arg4
(* convert memory declarations *)
| QUADRUPLE(REG, EMPTY, rng, TLIST lst) -> begin
    let width = (match rng with
      | RANGE(left, right) -> let lft = Const.exprConstStr stderr syms left and
			       rght = Const.exprConstStr stderr syms right in
			       ((int_of_string lft) - (int_of_string rght) + 1)
      | EMPTY -> 1
      | TRIPLE(EMPTY,EMPTY,EMPTY) -> 1
      | TRIPLE(EMPTY,RANGE(INT lft, INT rght),EMPTY) -> (lft - rght + 1)
      | _ ->  Dump.unhandled stderr 319 rng; 1) in
    List.iter (fun x -> match x with
      | TRIPLE(ID id, arg5, EMPTY) -> (match arg5 with
          | EMPTY -> ()
          | TLIST [RANGE (expr1, expr2)] ->
              let hi = Const.exprConstStr stderr syms expr1
              and lo = Const.exprConstStr stderr syms expr2 in
              mydump' syms (tbl:tbl) (sprintf "VAR %s: array %s .. %s of word[%d]" (Minimap.valid tbl.namtab id.id) (min lo hi) (max lo hi) width) EMPTY
          | _ -> Dump.unhandled stderr 1328 arg5);
      | DOUBLE(id, EMPTY) -> ()
      | _ -> Dump.unhandled stderr 1330 x) lst; end
| QUADRUPLE(INTEGER, EMPTY, rng, lst) -> ()
| QUINTUPLE((INPUT|OUTPUT) as dir, arg1, arg2, rng, TLIST arg4) -> Hashtbl.add tbl.fst (QUINTUPLE(dir, arg1, arg2, rng, TLIST arg4)) ()
(*
| DOTTED lst -> List.iter (fun itm -> match itm with
  | ID s -> plugh_str (tbl:tbl) "s "  (Minimap.valid tbl.namtab s)
  | TRIPLE (BITSEL, ID id1, TRIPLE (DIVIDE, ID id2, INT n)) -> plugh_str_str_dec (tbl:tbl) "s[s/d] "  (Minimap.valid tbl.namtab id1) (Minimap.valid tbl.namtab id2) n
  | _ -> Dump.unhandled stderr 177 itm) lst
| RANGE (TRIPLE (MINUS, ID id1, INT n1), INT n2) -> plugh_str_dec_dec (tbl:tbl) "[s-d:d] "  (Minimap.valid tbl.namtab id1) n1 n2
| ABC str -> plugh_str (tbl:tbl) "s"  str
*)
| SEPTUPLE(TASK, EMPTY, ID nam, EMPTY, params, body, EMPTY) -> mydump' syms (tbl:tbl) ("-- task"^nam.id) EMPTY; ( (* match params with
  | TLIST tlst -> List.iter (fun itm -> match itm with
    | QUINTUPLE((INPUT|OUTPUT|INOUT) as dir, EMPTY, EMPTY, rng, TLIST iolst) 
    | QUADRUPLE((WIRE|REG|INTEGER) as dir, EMPTY, rng, TLIST iolst) -> List.iter (fun io -> match io with
      | TRIPLE (ID id, EMPTY, EMPTY) -> plugh_str_str (tbl:tbl) "-- taskargs s s "  (Ord.getstr dir) id
      | _ -> Dump.unhandled stderr 549 io) iolst
    | _ -> Dump.unhandled stderr 550 itm) tlst
  | EMPTY -> ()
  | _ -> Dump.unhandled stderr 552 params);
  (match body with
  | TLIST tasks -> List.iter (fun itm -> match itm with
    | EMPTY -> ()
    | _ -> plugh (tbl:tbl) "-- taskbody " ; ignore(defn syms (tbl:tbl) itm 0); plugh" ") tasks
  | EMPTY -> ()
  | _ -> plugh (tbl:tbl) "-- taskbody " ; ignore(defn syms (tbl:tbl) body 0); *))
| OCTUPLE(FUNCTION, EMPTY, rng, ID nam, EMPTY, params, body, EMPTY) -> mydump' syms (tbl:tbl) ("-- function"^nam.id) EMPTY; ( (*match params with 
  | TLIST tlst -> List.iter (fun itm -> match itm with
    | QUINTUPLE((INPUT|OUTPUT|INOUT) as dir, EMPTY, EMPTY, rng, TLIST iolst) 
    | QUADRUPLE((WIRE|REG|INTEGER) as dir, EMPTY, rng, TLIST iolst) -> List.iter (fun io -> match io with
      | TRIPLE (ID id, EMPTY, EMPTY) -> plugh_str_str (tbl:tbl) "-- funargs s s "  (Ord.getstr dir) id
      | _ -> Dump.unhandled stderr 562 io) iolst
    | _ -> Dump.unhandled stderr 563 itm) tlst
  | EMPTY -> ()
  | _ -> Dump.unhandled stderr 568 params);
  (match body with
  | TLIST tasks -> List.iter (fun itm -> match itm with
    | EMPTY -> ()
    | _ -> plugh (tbl:tbl) "-- funbody " ; ignore(defn syms (tbl:tbl) itm 0); plugh" ") tasks
  | EMPTY -> ()
  | _ -> plugh (tbl:tbl) "-- funbody " ; ignore(defn syms (tbl:tbl) body 0); *))
| e -> Dump.unhandled stderr 615 e

let hls_select = {
  map_redor = redor ;
  map_tilde = tilde ;
  map_unaryplus = unaryplus ;
  map_pling = pling ;
  map_equal = equal ;
  map_wildequal = wildequal ;
  map_noteq = noteq ;
  map_gte = gte ;
  map_lte = lte ;
  map_less = less ;
  map_greater = greater ;
  map_logand = logand ;
  map_logor = logor ;
  map_log_pand = log_pand ;
  map_log_por = log_por ;
  map_logxor = logxor ;
  map_plus = plus ;
  map_minus = minus ;
  map_mux2 = mux2 ;
  map_buffer = monadic_buffer ;
  map_flipflop = bus_flipflop ;
  map_redand = redand ;
}

let map_hls = {
  map_id = Map_hls;
  map_select = hls_select;
  map_times = times' ;
  map_sleft = sleft' ;
  map_sright = sright' ;
}
    
let map_struct = {
  map_id = Map_struct;
  map_select = hls_select;
  map_times = times ;
  map_sleft = sleft ;
  map_sright = sright ;
}
    
let map_xilinx = {
  map_id = Map_xilinx;
  map_select = Mapping.map_select;
  map_times = times ;
  map_sleft = sleft ;
  map_sright = sright ;
}

let evoth = ref None
let evoth' = ref None
let evoth'' = ref None
let evdbg = ref None

let rec always_body = function
| QUADRUPLE ((ASSIGNMENT|DLYASSIGNMENT) as dly, dest, _, clrval) -> (dly,dest,clrval)
| TLIST (itm :: []) -> always_body itm
| TLIST (itm :: TLIST [EMPTY] :: tl) -> always_body itm
| oth -> evoth := Some oth; failwith "always_body"
		   
let rec always_body1 = function
| QUADRUPLE ((ASSIGNMENT|DLYASSIGNMENT) as dly, dest, _, _) as itm -> (None,dly,dest,itm)
| TRIPLE (IF, clken, (QUADRUPLE ((ASSIGNMENT|DLYASSIGNMENT) as dly1, dest1, _, _) as itm)) -> (Some clken, dly1, dest1, itm)
| TRIPLE (IF, en, if_clause) ->
    (match always_body1 if_clause with
       | (None, (ASSIGNMENT|DLYASSIGNMENT as dly), dest, if_val) -> (Some en, dly, dest, TRIPLE (IF, en, if_val))
       | oth -> evoth' := Some oth; failwith "always_body1'")
| QUADRUPLE (IF, clr, if_clause, else_clause) ->
(match always_body1 if_clause, always_body1 else_clause with
       | (None, (ASSIGNMENT|DLYASSIGNMENT as dly), dest, if_val),
         (None, (ASSIGNMENT|DLYASSIGNMENT as dly'), dest', else_val) ->
         (None, dly, dest, QUADRUPLE (IF, clr, if_val, else_val))
       | (None, (ASSIGNMENT|DLYASSIGNMENT as dly), dest, if_val),
         (Some en, (ASSIGNMENT|DLYASSIGNMENT as dly'), dest', else_val) ->
         (Some en, dly, dest, QUADRUPLE (IF, clr, if_val, else_val))
       | (None, (ASSIGNMENT|DLYASSIGNMENT as dly), dest, if_val),
         (_, _, _, (QUADRUPLE(IF, _, _, _) as else_val)) ->
         (None, dly, dest, QUADRUPLE (IF, clr, if_val, else_val))
      | oth -> evoth'' := Some oth; failwith "always_body1''")
| TLIST (itm :: []) -> always_body1 itm
| TLIST (itm :: TLIST [EMPTY] :: tl) -> always_body1 itm
| oth -> evoth := Some oth; failwith "always_body1"

let rec always_ev = function
| QUADRUPLE (IF, clr1, body, body') ->
    let (dly, dest, clrval) = always_body body in
    let (clken, dly1, dest1, itm) = always_body1 body' in
           (clr1, dly, dest, clrval, clken, dly1, dest1, itm) 
| TLIST (itm :: []) -> always_ev itm
| oth -> evoth := Some oth; failwith "always_ev"

let rec synthbody syms (tbl:tbl) = function
| DOUBLE(INITIAL,stmt) -> let delim = ref "init" in initial syms (tbl:tbl) stmt delim
| DOUBLE(ALWAYS,stmts) ->
    let ident = Dump.dumpstr stmts in
    if !verbose then printf "always %s\n" ident;
    let syms2 = Semantics.shash_create "always" ident syms 256 in
    let hashtab = Hashtbl.create 256 in (match stmts with

  | TLIST [DOUBLE(DOUBLE(AT, TLIST [DOUBLE ((POSEDGE|NEGEDGE) as edg, clk)]), itm)] ->
    if !verbose then printf "pattern matched %s\n" (Dump.dumpstr stmts);
    edge hashtab syms2 (tbl:tbl) [DOUBLE(edg,clk)] itm
  | TLIST [DOUBLE(DOUBLE(AT, TLIST [DOUBLE ((POSEDGE|NEGEDGE) as edg, clk);
                             DOUBLE ((POSEDGE|NEGEDGE) as edg1, clr)]), ev_stmts)] -> 
    let (clr1, dly, dest, clrval, clken, dly1, dest1, itm) = always_ev ev_stmts in
    if !verbose then printf "pattern matched %s\n" (Dump.dumpstr itm);
    evdbg := Some (clr1, dly, dest, clrval, clken, dly1, dest1, ev_stmts, itm);
    let clken' = match clken with Some TRIPLE (P_EQUAL, clken, HEXNUM "1'h1") -> (* DOUBLE(dly,clken) :: *) [] | None -> [] in
    if clr=clr1 && dly=dly1 && dest=dest1 then edge hashtab syms2 (tbl:tbl) (DOUBLE(edg,clk) :: TRIPLE(edg1,clr,clrval) :: clken') itm
    else failwith (sprintf "malformed always block %s != %s or %s != %s or %s != %s"
                     (Count.tokenstr clr)
                     (Count.tokenstr clr1)
                     (Count.tokenstr dly)
                     (Count.tokenstr dly1)
                     (Count.tokenstr dest)
                     (Count.tokenstr dest1))
  | TLIST [DOUBLE(DOUBLE(HASH, FLOATNUM f), dlystmt)] -> (match dlystmt with
    | QUADRUPLE((ASSIGNMENT|DLYASSIGNMENT), ID dest, EMPTY, next) -> let w = Minimap.symwidth syms dest in
       assign dlystmt (tbl:tbl) syms (ID dest) (defn syms (tbl:tbl) next w)
    | _ -> Dump.unhandled stderr 686 dlystmt)
  | TLIST [DOUBLE(DOUBLE (AT, TLIST deplist), dlystmt)] ->
    if !verbose then printf "pattern matched %s\n" (Dump.dumpstr dlystmt);
    always_comb hashtab syms2 (tbl:tbl) dlystmt
  | TLIST [DOUBLE (AT, TLIST dlystmt)] ->
    if !verbose then List.iter (fun arg -> printf "always_comb matched %s\n" (Dump.dumpstr arg)) dlystmt;
    always_comb hashtab syms2 (tbl:tbl) (TLIST dlystmt)
  | TLIST lst ->mydump' syms (tbl:tbl) "-- tlist" (TLIST lst)
(*
  | TLIST [DOUBLE(AT, TLIST (TLIST stmtlst :: tail))] ->
    if !verbose then printf "pattern matched stmtlst %s\n" (String.concat ";" (List.map Dump.dumpstr stmtlst));
*)
  | _ -> Dump.unhandled stderr 687 stmts)
| TRIPLE(ASSIGN, arg1, TLIST arg2) ->
  (match arg1 with 
    | DOUBLE(HASH, TLIST [INT n]) -> printf "-- Verilog Delay #%d ignored "  n
    | DOUBLE(HASH, FLOATNUM f) -> printf "-- Verilog delay #%f ignored "  f
    | EMPTY -> ()
    | _ -> Dump.unhandled stderr 1138 arg1);
  List.iter (fun x -> (match x with
    | TRIPLE (ASSIGNMENT, ID id, exp) ->
        assign exp (tbl:tbl) syms (Minimap.validid tbl.namtab id) (defn syms (tbl:tbl) exp (Minimap.symwidth syms id));
    | TRIPLE (ASSIGNMENT, QUADRUPLE (PARTSEL, ID id, INT hi, INT lo), exp) ->
      let src = defn syms (tbl:tbl) exp ((max hi lo)-(min hi lo)+1) in
      defkind (tbl:tbl) syms (enterid (Minimap.valid tbl.namtab id.id)) (Minimap.symwidth syms id);
      buffer_signal (tbl:tbl) syms (Minimap.validid tbl.namtab) (TRIPLE (ASSIGNMENT, QUADRUPLE (PARTSEL, ID id, INT hi, INT lo), (fst src)))
    | TRIPLE (ASSIGNMENT, TRIPLE (BITSEL, ID id, INT sel), exp) ->
      let src = defn syms (tbl:tbl) exp 1 in
      defkind (tbl:tbl) syms (enterid (Minimap.valid tbl.namtab id.id)) (Minimap.symwidth syms id);
      buffer_signal (tbl:tbl) syms (Minimap.validid tbl.namtab) (TRIPLE (ASSIGNMENT, TRIPLE (BITSEL, ID id, INT sel), (fst src)))
    | TRIPLE (ASSIGNMENT, DOUBLE (CONCAT, TLIST lst), exp) ->
      let owid = Minimap.cwidth syms lst in
      let src = defn syms (tbl:tbl) exp owid in
      List.iter (function
	| ID id -> defkind (tbl:tbl) syms (enterid (Minimap.valid tbl.namtab id.id)) (Minimap.symwidth syms id);
	| TRIPLE(BITSEL, ID id, INT sel) -> defkind (tbl:tbl) syms (enterid (Minimap.valid tbl.namtab id.id)) (sel+1);
	| oth -> Dump.unhandled stderr 1480 oth) lst;
      buffer_signal (tbl:tbl) syms (Minimap.validid tbl.namtab) (TRIPLE (ASSIGNMENT, DOUBLE (CONCAT, TLIST lst), (fst src)))
    | _ -> Dump.unhandled stderr 1144 x)) arg2
| QUADRUPLE(MODINST, ID kind, params, TLIST modlst) -> if Hashtbl.mem modprims kind.id then
 begin
  let kindhash = Hashtbl.find modprims kind.id in 
  let primargs = match kindhash.tree with QUINTUPLE((MODULE|PRIMITIVE),ID arg1, _, TLIST args, _) -> args | _ -> failwith "modinst" in
  let isyms = kindhash.symbols in
  let modid = enterid (kind.id^ !modsuffix) in
  map_mod_inst tbl syms kind modlst primargs (find_dir isyms) (Minimap.symwidth isyms) modid params
 end
  else if Hashtbl.mem libhash kind.id then
    begin
      let hash = Hashtbl.find libhash kind.id in
      let find_dir id = if Read_library.is_member id hash.ipinlst then INPUT else if Read_library.is_member id hash.opinlst then OUTPUT else EMPTY in
      let symwidth id = 1 in
      let modid = kind in
      map_mod_inst tbl syms kind modlst (List.map (fun id -> ID id.idpin) (hash.ipinlst @ hash.opinlst)) find_dir symwidth modid params
    end
  else
    failwith ("module instance "^kind.id^" is neither a defined sub-module nor a library cell")
(* these are direct instantiation of Verilog primitives at the top level *)
| TRIPLE(NOT, EMPTY, TLIST [QUADRUPLE (EMPTY, SCALAR, ID id, exp')]) -> let exp = DOUBLE(NOT, exp') in
  assign exp (tbl:tbl) syms (Minimap.validid tbl.namtab id) (defn syms (tbl:tbl) exp (Minimap.symwidth syms id));
| TRIPLE((OR|AND) as op, EMPTY, TLIST[QUADRUPLE(EMPTY, SCALAR, ID id, TLIST (hd::lst))]) ->
  let exp = List.fold_right (fun itm arg -> TRIPLE(op, itm, arg)) lst hd in
  assign exp (tbl:tbl) syms (Minimap.validid tbl.namtab id) (defn syms (tbl:tbl) exp (Minimap.symwidth syms id));
| e -> Dump.unhandled stderr 1203 e

let rec used syms tused lst = List.iter (function
  | INT _ -> ()
  | EMPTY -> ()
  | BINNUM _ -> ()
  | WIDTHNUM _ -> ()
  | ID id ->
      begin
	if Const.shash_chain_mem syms id
	then
	  begin
	    let (hi',lo',inc') = Minimap.find_width id syms in
	    if Hashtbl.mem tused id then
		  (match Hashtbl.find tused id with
		    | EMPTY -> if inc' <> 0 then Hashtbl.replace tused id (RANGE(INT hi', INT lo'))
		    | RANGE(INT hi, INT lo) -> Hashtbl.replace tused id (RANGE(INT (max hi hi'), INT (min lo lo')))
		    | oth -> Dump.unhandled stderr 1254 oth)
		else
		  Hashtbl.add tused id (if inc' <> 0 then RANGE(INT hi', INT lo') else EMPTY)
	  end
	else if not (Hashtbl.mem tused id) then Hashtbl.add tused id EMPTY
      end
  | DOUBLE(NOT, arg) -> used syms tused [arg]
  | TRIPLE(BITSEL, ID id, INT n) -> if Hashtbl.mem tused id then
      (match Hashtbl.find tused id with
        | EMPTY -> Hashtbl.replace tused id (RANGE(INT n, INT n))
        | RANGE(INT hi, INT lo) -> Hashtbl.replace tused id (RANGE(INT (max hi n), INT (min lo n)))
        | oth -> Dump.unhandled stderr 1254 oth)
    else
      Hashtbl.add tused id (RANGE(INT n, INT n))
  | QUADRUPLE(PARTSEL, ID id, lft, rght) -> let (hi, lo) =
    (match (Const.exprConst stderr syms lft,Const.exprConst stderr syms rght) with
    | INT left, INT right -> (max left right, min left right)
    | oth -> Dump.unhandled stderr 1260 (DOUBLE oth); (-1,-1)) in
    if Hashtbl.mem tused id then
      (match Hashtbl.find tused id with
        | EMPTY -> Hashtbl.replace tused id (RANGE(INT hi, INT lo))
        | RANGE(INT oldhi, INT oldlo) -> Hashtbl.replace tused id (RANGE(INT (max hi oldhi), INT (min lo oldlo)))
        | oth -> Dump.unhandled stderr 1265 oth)
    else
      Hashtbl.add tused id (RANGE(INT hi, INT lo))
  | TRIPLE((AND|OR), arg1, arg2) -> used syms tused [arg1;arg2]
  | TRIPLE(REG, ID q_out, arg) when q_out.id="q_out" -> used syms tused [arg]
  | DOUBLE (CONCAT, TLIST ilst) -> used syms tused ilst
  | TRIPLE(BITSEL, _, _) -> ()
  | HEXNUM _ -> ()
  | other -> Dump.unhandled stderr 1270 other) lst

let synthused (tbl:tbl) syms =
  Hashtbl.iter (fun k _ -> match k with
    | QUADRUPLE(MODINST, ID kind, paramlst, TLIST arg3) ->
      List.iter (function
        | TRIPLE (ID id, SCALAR, TLIST arg4) ->
          List.iter (function
            | TRIPLE (CELLPIN, ID cellpin, conn) -> used syms tbl.tused [conn]
            | err -> Dump.unhandled stderr 1297 err) arg4
        | oth -> Dump.unhandled stderr 1298 oth) arg3
    | oth ->Dump.unhandled stderr 1263 oth) tbl.snd;
  Hashtbl.iter (fun pth rng ->
    Hashtbl.add tbl.fst (QUADRUPLE(WIRE, EMPTY, TRIPLE(EMPTY, rng, EMPTY), TLIST[DOUBLE (ID pth, EMPTY)])) ();
    ) tbl.tused
    
let prepare1 syms tbl cached = function
              | ID id
              | DOUBLE (ID id, EMPTY) ->
                  (match !cached with
                    | (EMPTY, _, _, _, _) -> ()
                    | ((INPUT|OUTPUT|INOUT) as dir, arg1, arg2, intrng, _) ->
		      Hashtbl.add tbl.tused id intrng;
                      Hashtbl.add tbl.fst (QUINTUPLE(dir, arg1, arg2, intrng,
                                                       TLIST [TRIPLE(Minimap.validid tbl.namtab id, EMPTY, EMPTY)])) ()
                    | oth -> Dump.unhandled stderr 1372 (QUINTUPLE oth));
                  Minimap.validid tbl.namtab id
              | QUINTUPLE((INPUT|OUTPUT|INOUT) as dir, arg1, arg2, srng, DOUBLE (ID id, EMPTY)) ->
                  let intrng = Minimap.flatten_width srng syms in
                  cached := (dir, arg1, arg2, intrng, TLIST [TRIPLE(Minimap.validid tbl.namtab id, EMPTY, EMPTY)]);
		  Hashtbl.add tbl.tused id intrng;
                  Hashtbl.add tbl.fst (QUINTUPLE !cached) ();
                  Minimap.validid tbl.namtab id
              | oth -> Dump.unhandled stderr 1372 oth; UNKNOWN

let prepare2 syms tbl k _ = match k with
              | QUINTUPLE((INPUT|OUTPUT) as tok, arg1, arg2, srng, TLIST arg4) ->
                  let intrng = Minimap.flatten_width srng syms in
                  let lst = List.map (function
                    | TRIPLE (ID id, EMPTY, EMPTY) ->
		      Hashtbl.add tbl.tused id intrng;
		      TRIPLE(Minimap.validid tbl.namtab id, EMPTY, EMPTY)
                    | oth -> Dump.unhandled stderr 1381 oth; EMPTY) arg4 in
                  Hashtbl.add tbl.fst (QUINTUPLE(tok, arg1, arg2, intrng, TLIST lst)) ()
              | QUADRUPLE((WIRE|TRI0|TRI1) (*as kind*), EMPTY, TRIPLE(EMPTY, RANGE(hi,lo), EMPTY), TLIST arg4) ->
                  List.iter (fun x -> match x with
                  | TRIPLE(ID id, EMPTY, _)
                  | DOUBLE(ID id, EMPTY) ->
                      used syms tbl.tused [QUADRUPLE(PARTSEL,ID id, hi, lo)];
                  | _ -> Dump.unhandled stderr 1495 x) arg4
              | _ -> ()		    

let dump_internals tbl arg1 =
	    (* dump internals *)
	    let dmpnam = arg1.id^ !modsuffix ^ ".dmp" in
	    let dmp = open_out dmpnam in
	    printf "Dumping %s\n" dmpnam;
	    let lab = ref "edges" in List.iter (fun arg ->
	      Hashtbl.iter (fun nam lst ->
		List.iter (fun (itm1,itm2,itm3) ->
		  fprintf dmp "%s %s cond: %s\n" !lab nam itm1;
		  fprintf dmp "%s %s next: %s\n" !lab nam itm2;
		  fprintf dmp "%s %s blk: %s\n" !lab nam (string_of_bool itm3)) lst) arg; lab := "combs") [tbl.edges;tbl.combs];
	    let lab = ref "edge" in List.iter (fun arg ->
	      Hashtbl.iter (fun nam (arg1,arg2,arg3,arg4,arg5,arg6) ->
		fprintf dmp "%s %s: {%s} {%s} {%s} {%s} {%s} {%s}\n"
		  !lab nam
		  (Dump.dumpstr arg1)
		  (Dump.dumpstr arg2)
		  (Dump.dumpstr (fst arg3))
		  (Dump.dumpstr (fst arg4))
		  (Dump.dumpstr (fst arg5))
		  (Dump.dumpstr (fst arg6))) arg; lab := "comb") [tbl.edgdmp;tbl.combdmp];
	    close_out dmp
  
let tbl_init select = {
  fst=Hashtbl.create 256;
  snd=Hashtbl.create 256;
  gentab = Hashtbl.create 256;
  namtab = Hashtbl.create 256;
  defnhash = Hashtbl.create 256;
  tused = Hashtbl.create 256;
  edges = Hashtbl.create 256;
  combs = Hashtbl.create 256;
  edgdmp = Hashtbl.create 256;
  combdmp = Hashtbl.create 256;
  map_hls = select;
}

let synth_module select syms synthnam params arg3 arg4 arg5 =    
  let tbl = tbl_init select in
  let cached = ref (EMPTY, EMPTY, EMPTY, EMPTY, EMPTY) in
  let ports = List.map (prepare1 syms tbl cached) arg3 in
  (match syms with
  | Shash symr -> Hashtbl.iter (fun _ sym -> sym_detail (tbl:tbl) syms sym) symr.syms
  | EndShash -> ());
  Hashtbl.iter (prepare2 syms tbl) arg4;

  List.iter (function
    | SEXTUPLE(PARAMETER, signing, range, ID id, attr, expr) ->
	let arg = Const.exprConst stderr syms (ID id) in (match arg with
	| INT n -> define exp (tbl:tbl) syms (enterid (Minimap.valid tbl.namtab id.id)) (Minimap.mask n) (INT n)
	| WIDTHNUM(r,w,n) -> ( match r with
	  | 16 -> define exp (tbl:tbl) syms (enterid (Minimap.valid tbl.namtab id.id)) w (INT n)
	  | 10 -> define exp (tbl:tbl) syms (enterid (Minimap.valid tbl.namtab id.id)) w (INT n)
	  | 8 -> define exp (tbl:tbl) syms (enterid (Minimap.valid tbl.namtab id.id)) w (INT n)
	  | 2 -> define exp (tbl:tbl) syms (enterid (Minimap.valid tbl.namtab id.id)) w (INT n)
	  | _ -> Dump.unhandled stderr 1340 arg)
	| _ -> Dump.unhandled stderr 1341 arg)
    | oth -> Dump.unhandled stderr 1346 oth) params;
  
  Hashtbl.iter (fun x _ -> synthdecl syms (tbl:tbl) x ) arg4;
  Hashtbl.iter (fun x _ -> synthbody syms (tbl:tbl) x ) arg5;
  
  synthused tbl syms;
  if !verbose then dump_internals tbl synthnam;
  
  QUINTUPLE(MODULE, ID synthnam, TLIST [], TLIST ports, THASH (tbl.fst,tbl.snd))

let debug_synth = ref None

let rec generate_submod select nam itm =
  if mybuf.buf.nam.id <> "" && 
    mybuf.ff.nam.id <> "" && 
    mybuf.ffc.nam.id <> "" && 
    mybuf.ffce.nam.id <> "" && 
    mybuf.inv.nam.id <> "" && 
    mybuf.logand.nam.id <> "" && 
    mybuf.logor.nam.id <> "" && 
    mybuf.logxor.nam.id <> "" && 
    mybuf.mux.nam.id <> "" then begin
      match itm.tree with
        | QUINTUPLE(MODULE, ID arg1, TLIST params, TLIST arg3, THASH (arg4,arg5)) ->
	    let arch = !archenv in
	    let synthnam = enterid(arg1.id^ !modsuffix) in
	    let modul = synth_module select itm.symbols synthnam params arg3 arg4 arg5 in
	    debug_synth := Some modul;
            if Count.find_arch arch synthnam.id <> []
            then print_endline("generate_submod select "^arch^" "^nam^" result is ambiguous");
            Semantics.prescan stderr arch modul "Generated by gen_hls_arch"
        | oth -> Dump.unhandled stderr 1384 oth
    end
  else failwith ("One or more necessary library cells are missing for synthesis")

let gen_hls_arch' (select:mapid) arch nam arg =
  Minimap.recurse_arch (fun nam subarg ->
    if subarg.is_behav then
      begin
	match subarg.tree with
        | QUINTUPLE(MODULE, ID arg1, TLIST params, TLIST arg3, THASH arg4) ->
          Printf.printf "gen_blackbox_arch %s\n" nam;
          let arch = !archenv in
          let synthnam = enterid(arg1.id^ !modsuffix) in
          let modul = QUINTUPLE(MODULE, ID synthnam, TLIST params, TLIST arg3, THASH arg4) in
          if Count.find_arch arch synthnam.id <> []
          then failwith("gen_hls_arch' select "^arch^" "^nam^" result is ambiguous");
          Semantics.prescan stderr arch modul "Generated by gen_hls_arch"
        | oth -> Dump.unhandled stderr 1384 oth
      end
    else
      begin
	let sel, map = match select with Map_hls -> "hls",map_hls | Map_struct -> "struct",map_struct | Map_xilinx -> "map",map_xilinx in
        Printf.printf "gen_%s_arch %s\n" sel nam;
        generate_submod map nam subarg;
        Printf.printf "Module %s report %s\n" nam (Semantics.endscan())
      end) arch nam arg
  
let gen_hls_arch arch nam =
  let lst = List.filter (fun arg -> arg.arch = arch) (Hashtbl.find_all modprims nam) in
  match lst with
    | [] -> failwith ("No modules matched arch "^arch^" name "^nam)
    | _ -> List.iter (gen_hls_arch' Map_hls arch nam) lst
	  
let gen_struct_arch arch nam =
  let lst = List.filter (fun arg -> arg.arch = arch) (Hashtbl.find_all modprims nam) in
  match lst with
    | [] -> failwith ("No modules matched arch "^arch^" name "^nam)
    | _ -> List.iter (gen_hls_arch' Map_struct arch nam) lst
	  
let gen_map_arch arch nam =
  let lst = List.filter (fun arg -> arg.arch = arch) (Hashtbl.find_all modprims nam) in
  match lst with
    | [] -> failwith ("No modules matched arch "^arch^" name "^nam)
    | _ -> List.iter (gen_hls_arch' Map_xilinx arch nam) lst
