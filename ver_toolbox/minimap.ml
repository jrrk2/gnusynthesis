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
open Idhash
open Vparser
open Globals
open Dump
open Printf
open Scanf
open Read_library

type osymtab = (string,token*bool)Hashtbl.t

let magic = "xyzzy_"
let verbose = ref false

let sort_date lst =
  let datecmp a b = if a.datestamp < b.datestamp then -1 else 1 in
  List.sort datecmp lst

let rec select_sub recurse fn (arch:string) id =
  let all_lst = Hashtbl.find_all modprims id in
  let subarglst = List.filter (fun arg -> arg.arch = arch) all_lst in
  match subarglst with
    | [] -> (match sort_date all_lst with
        | [] -> failwith ("sub-module "^id^" is not found")
        | subarg::tl ->
	    if tl <> [] then print_endline ("arch "^arch^" module "^id^" is not found, otherwise ("^String.concat ";" (List.map (fun arg -> arg.arch) all_lst)^") is newest");
	    if recurse then recurse_arch' fn arch subarg; fn id subarg)
    | subarg::[] -> if recurse then recurse_arch' fn arch subarg; fn id subarg
    | lst -> let slst = sort_date lst in
      print_endline ("sub-module "^id^" is ambiguous: "^String.concat ";" (List.map (fun arg -> arg.arch) lst));
      let subarg = List.hd slst in if recurse then recurse_arch' fn arch subarg; fn id subarg

and recurse_tree fn = function
| TRIPLE((BUF|NOT|AND|OR|XOR|NAND|NOR|XNOR|PULLUP|NMOS|PMOS|TRAN) as kind, dly, TLIST inst) -> fn kind (List.length inst)
| TRIPLE((BUFIF lev|NOTIF lev|TRANIF lev) as kind, weaklist, TLIST inst) -> fn kind (List.length inst)
| QUADRUPLE((MODINST|PRIMINST), ID kind, arg2, TLIST inst) ->
  if not (Hashtbl.mem libhash kind.id) then
    fn (ID kind) (List.length inst)
| QUINTUPLE(MODULE,ID arg1, arg2, TLIST arg3, arg4) -> recurse_tree fn arg4;
| THASH thash ->
  Hashtbl.iter (fun itm _ -> recurse_tree fn itm) (fst thash);
  Hashtbl.iter (fun itm _ -> recurse_tree fn itm) (snd thash)
| _ -> ()

and recurse_arch' fn arch arg =
  recurse_tree (fun k cnt -> match k with
    | ID id -> select_sub true (fn) arch id.id
    | _ -> ()) arg.tree

and recurse_arch'' arch nam arg =
  let hierlst = ref [] in
  let add2lst k cnt = let itm = (k,cnt) in
    if not (List.mem itm !hierlst) then hierlst := itm :: !hierlst in
  recurse_arch' add2lst arch arg;
  select_sub true add2lst arch nam;
  !hierlst

and recurse_arch fn arch nam arg =
  List.iter (fun (k,cnt) -> fn k cnt) (List.rev (recurse_arch'' arch nam arg))

let find_submod arch nam =
  match Count.find_arch arch nam with
    | [] -> failwith ("find_submod "^arch^" "^nam^" returned an empty list")
    | arg::[] -> let delim = ref '\n' in
                 recurse_arch (fun id subarg -> Printf.printf "%c%s" !delim id; delim := ' ') arch nam arg;
                 print_newline()
    | _ ->  failwith ("find_submod "^arch^" "^nam^" is ambiguous")

let instid siglst syms' rslt =
  let incinst rslt =
    rslt.instcnt <- rslt.instcnt + 1;
    enterid ("v2hls$"^rslt.instid.id^(string_of_int rslt.instcnt)) in
  match syms' with
  | Vparser.EndShash -> ID (incinst rslt)
  | Vparser.Shash {Vparser.nxt;syms} ->
      let inuse = ref true and nam = ref (enterid "$") in
      while !inuse do
        nam := incinst rslt;
        inuse := Hashtbl.mem syms !nam;
        if not !inuse then Hashtbl.add syms !nam {
          symattr=TokSet.empty;
          width = EMPTY;
          path = !nam;
          sigattr = Sigundef;
          localsyms = EndShash
        }
      done;
      ID !nam

let rec bin n0 wid = (if n0 > 1 || wid > 1 then bin (n0 lsr 1) (wid-1) else "")^(if n0 mod 2 = 1 then "1" else "0")

let mask n0 = if n0 = 0 then 1 else let l = ref 0 and n = ref n0 in while (!n > 0) do n := !n lsr 1; l := !l + 1; done; !l

let sym_width syms sym = match sym.sigattr with
| Sigparam pexpr -> (match Const.exprConst stderr syms pexpr with
  | WIDTHNUM(radix,sz,num) -> sz
  | INT n -> 32
  | _ -> 1)
| Sigarray x -> (match sym.width with
  | SCALAR -> 1
  | EMPTY -> 1
  | RANGE(INT lft, INT rght) -> (lft-rght+1)
  | RANGE(expr1, expr2) -> let lft = Const.exprConstStr stderr syms expr1 and
			       rght = Const.exprConstStr stderr syms expr2 in
			       ((int_of_string lft) - (int_of_string rght) + 1)
  | _ -> Dump.unhandled stderr 72 sym.width; 1)
| Signamed blk -> 1
| Sigfunc fn -> 1
| Sigtask tsk -> 1
| Sigundef -> 1

let symwidth syms id =
  if Const.shash_chain_mem syms id
  then
    begin
      let w = sym_width syms (Const.shash_chain_find syms id) in
      if !verbose then Printf.printf "symbol %s, width = %d\n" id.id w;
      w
    end
  else failwith (sprintf "Symbol %s not found\n" id.id)

let rec elwidth syms = function
      | ID id -> symwidth syms id
      | BINNUM lev -> 1
      | IDSTR _ -> 1
      | WIDTHNUM(radix,sz,num) -> sz
      | TLIST lst -> cwidth syms lst
      | DOUBLE(CONCAT, arg) -> elwidth syms arg
      | TRIPLE(BITSEL, ID _, idx) -> 1
      | QUADRUPLE(PARTSEL, ID dest, hi, lo) ->
	let lft = Const.exprConstStr stderr syms hi and
	    rght = Const.exprConstStr stderr syms lo in
	((int_of_string lft) - (int_of_string rght) + 1)
      | DOUBLE((LPAREN|NOT), arg) -> elwidth syms arg
      | TRIPLE(OR, arg1, arg2) -> max (elwidth syms arg1) (elwidth syms arg2)
      | TRIPLE(AND, arg1, arg2) -> min (elwidth syms arg1) (elwidth syms arg2)
      | EMPTY -> printf("Error: empty element\n"); 1
      | TRIPLE(P_OROR, _, _) -> 1
      | TRIPLE(P_ANDAND, _, _) -> 1
      | TRIPLE(BITSEL, DOUBLE(CONCAT, _), idx) -> 1
      | HEXNUM num -> fst (Const.widthnum stderr 16 num)
      | oth -> Dump.unhandled stderr 200 oth; 1

and cwidth syms lst = let w = ref 0 in List.iter (fun itm -> w := !w + elwidth syms itm) lst; !w

let rec log2 n = if n <= 1 then 0 else 1 + log2(n asr 1)

let basenum = function
  | INT num -> (32, num)
  | WIDTHNUM(rdx,sz,num) -> (sz,num)
  | BINNUM num -> Const.widthnum stderr 2 num
  | OCTNUM num -> Const.widthnum stderr 8 num
  | DECNUM num -> Const.widthnum stderr 10 num
  | HEXNUM num -> Const.widthnum stderr 16 num
  | oth -> unhandled stderr 164 oth; (1,-1)

let find_width symnam syms = 
if !verbose then
  begin
    printf "find_width %s\n" symnam.id;
    let symnxt = ref syms and found = ref false and fail = ref false in
    while not !found && not !fail do
      (match !symnxt with
      | Shash symr ->
          Printf.printf "Searching symbol table %s:%s\n" symr.stabarch symr.stabnam;
          if Hashtbl.mem symr.syms symnam then found := true else symnxt := symr.nxt
      | EndShash -> fail := true);
    done
  end;
if Const.shash_chain_mem syms symnam then let sym = Const.shash_chain_find syms symnam in (
  match sym.Vparser.sigattr with
   | Vparser.Sigparam x -> ((fst (basenum x))-1,0,-1)
   | Vparser.Sigarray _ ->
       (match sym.Vparser.width with
         | RANGE range -> Const.iwidth stderr syms (RANGE range)
         | SCALAR | EMPTY | UNKNOWN -> (0,0,0)
         | w -> Dump.unhandled stderr 142 w; (-1,-1,-1))
   | _ -> failwith "unhandled sigattr")
else (flush_all(); failwith (sprintf "find_width: symbol %s not found\n" symnam.id) (*; (-1,-1,-1) *) )

let rec list_shorten wid lst =
  if List.length lst <= wid then lst
  else list_shorten wid (List.tl lst)

let rec list_pad wid lst =
  if List.length lst >= wid then lst
  else list_pad wid (BINNUM "1'b0" :: lst)

let rec concat_flatten cnt lst cum =
  if cnt > 0 then concat_flatten (cnt-1) lst (cum@lst)
  else
    cum

let rec num_flatten cnt num cum =
  if cnt > 0 then num_flatten (cnt-1) (num asr 1) (BINNUM (if num land 1 > 0 then "1'b1" else "1'b0") :: cum)
  else
    cum

let rec id_flatten (sel,lo,inc) id cum =
  if !verbose then print_endline (id.id^"["^string_of_int sel^":"^string_of_int lo^":"^string_of_int inc^"]");
  if inc = 0 then [ID id] else
  if sel*inc <= lo*inc then id_flatten (sel+inc,lo,inc) id (cum@[TRIPLE(BITSEL, ID id, INT sel)])
  else
    cum

let rec concat_flattenable' = function
  | DOUBLE(CONCAT, TLIST lst') :: lst -> concat_flattenable' lst' && concat_flattenable' lst
  | TRIPLE (BITSEL, _, _) :: lst -> concat_flattenable' lst
  | ID _ :: lst -> concat_flattenable' lst
  | BINNUM "1'b0" :: lst -> concat_flattenable' lst
  | BINNUM "1'b1" :: lst -> concat_flattenable' lst
  | [] -> true
  | _ -> false
	
let rec concat_flatten' syms = function
  | DOUBLE(CONCAT, TLIST lst') :: lst -> concat_flatten' syms lst' @ concat_flatten' syms lst
  | QUADRUPLE (PARTSEL, ID id, INT hi', INT lo') :: lst -> let (hi,lo,inc) = find_width id syms in (id_flatten (hi',lo',inc) id []) @ concat_flatten' syms lst
  | TRIPLE (BITSEL, _, _) as itm :: lst -> itm :: concat_flatten' syms lst
  | ID id :: lst -> let (hi,lo,inc) = find_width id syms in (id_flatten (hi,lo,inc) id []) @ concat_flatten' syms lst
  | BINNUM "1'b0" as itm :: lst -> itm :: concat_flatten' syms lst
  | BINNUM "1'b1" as itm :: lst -> itm :: concat_flatten' syms lst
  | WIDTHNUM(radix,width,num) :: lst -> num_flatten width num [] @ concat_flatten' syms lst
  | BINNUM str :: lst -> let (width,num) = Const.widthnum stderr 2 str in num_flatten width num [] @ concat_flatten' syms lst
  | DECNUM str :: lst -> let (width,num) = Const.widthnum stderr 10 str in num_flatten width num [] @ concat_flatten' syms lst
  | HEXNUM str :: lst -> let (width,num) = Const.widthnum stderr 16 str in num_flatten width num [] @ concat_flatten' syms lst
  | OCTNUM str :: lst -> let (width,num) = Const.widthnum stderr 8 str in num_flatten width num [] @ concat_flatten' syms lst
  | INT num :: lst -> num_flatten 32 num [] @ concat_flatten' syms lst
  | [] -> []
  | oth -> Dump.unhandled stderr 230 (TLIST oth); []

let ass_id = ref 0
let addinst' syms w olst exp =
  QUADRUPLE(MODINST,
	    ID (enterid "DVL_BUF"),
	    DOUBLE(HASH, TLIST [INT w]),
	    TLIST[TRIPLE(ID (enterid ("assign"^string_of_int !ass_id)), SCALAR, TLIST [TRIPLE(CELLPIN, ID (enterid "Y"),
											      DOUBLE(CONCAT, TLIST olst));
											TRIPLE(CELLPIN, ID(enterid "A"),
											       exp)])])

let rec wdump syms = function
  | ID id -> let (lft,rght,inc) = find_width id syms in sprintf "%s[%d:%d]" id.id lft rght
  | WIDTHNUM(radix,sz,num) -> sprintf "widthnum(%d,%d,%d)" radix sz num
  | DOUBLE(CONCAT, arg) -> "{"^wdump syms arg^"}"
  | TLIST lst -> String.concat ";" (List.map (wdump syms) lst)
  | oth -> Dump.dumpstr oth

let assert_failure = ref None

let assert_unit_width syms lst = List.iter (fun arg ->
  let wid = elwidth syms arg in
  if wid <> 1 then
    begin
      assert_failure := Some (syms,arg);
      failwith ("unit_width failure "^wdump syms arg)
    end) lst;
  if Hashtbl.length libhash = 0 then failwith "Cell-library not loaded"

let flatten_width tok gsyms = match gsyms with
| Vparser.EndShash -> EMPTY
| Vparser.Shash {Vparser.nxt;syms} -> match tok with
  | RANGE range -> let (hi,lo,inc) = Const.iwidth stderr gsyms (RANGE range) in
                   RANGE(INT hi, INT lo)
  | scalar -> scalar

let insert_buf syms output input =
  assert_unit_width syms [input;output];
  [QUADRUPLE(MODINST, ID mybuf.buf.nam, EMPTY,
            TLIST
              [TRIPLE
                  (instid [input;output] syms mybuf.buf, SCALAR,
                   TLIST
                     [TRIPLE (CELLPIN, ID (List.hd mybuf.buf.ipinlst).idpin, input);
                      TRIPLE (CELLPIN, ID (List.hd mybuf.buf.opinlst).idpin, output)])])]

let insert_inv syms output input =
  assert_unit_width syms [input;output];
  [QUADRUPLE(MODINST, ID mybuf.inv.nam, EMPTY,
            TLIST
              [TRIPLE
                  (instid [input;output] syms mybuf.inv, SCALAR,
                   TLIST
                     [TRIPLE (CELLPIN, ID (List.hd mybuf.inv.ipinlst).idpin, input);
                      TRIPLE (CELLPIN, ID (List.hd mybuf.inv.opinlst).idpin, output)])])]

let insert_rednot syms output input =
  assert_unit_width syms [input;output];
  [QUADRUPLE(MODINST, ID mybuf.inv.nam, EMPTY,
            TLIST
              [TRIPLE
                  (instid [input;output] syms mybuf.rednot, SCALAR,
                   TLIST
                     [TRIPLE (CELLPIN, ID (List.hd mybuf.inv.ipinlst).idpin, input);
                      TRIPLE (CELLPIN, ID (List.hd mybuf.inv.opinlst).idpin, output)])])]

let insert_redand syms output input =
  assert_unit_width syms [input;output];
  [QUADRUPLE(MODINST, ID mybuf.inv.nam, EMPTY,
            TLIST
              [TRIPLE
                  (instid [input;output] syms mybuf.redand, SCALAR,
                   TLIST
                     [TRIPLE (CELLPIN, ID (List.hd mybuf.inv.ipinlst).idpin, input);
                      TRIPLE (CELLPIN, ID (List.hd mybuf.inv.opinlst).idpin, output)])])]

let insert_redor syms output input =
  assert_unit_width syms [input;output];
  [QUADRUPLE(MODINST, ID mybuf.inv.nam, EMPTY,
            TLIST
              [TRIPLE
                  (instid [input;output] syms mybuf.redor, SCALAR,
                   TLIST
                     [TRIPLE (CELLPIN, ID (List.hd mybuf.inv.ipinlst).idpin, input);
                      TRIPLE (CELLPIN, ID (List.hd mybuf.inv.opinlst).idpin, output)])])]

let insert_pwr syms output =
  assert_unit_width syms [output];
  [QUADRUPLE(MODINST, ID mybuf.pwr.nam, EMPTY,
            TLIST
              [TRIPLE
                  (instid [output] syms mybuf.pwr, SCALAR,
                   TLIST
                     [TRIPLE (CELLPIN, ID (List.hd mybuf.pwr.opinlst).idpin, output)])])]

let insert_gnd syms output =
  assert_unit_width syms [output];
  [QUADRUPLE(MODINST, ID mybuf.gnd.nam, EMPTY,
            TLIST
              [TRIPLE
                  (instid [output] syms mybuf.gnd, SCALAR,
                   TLIST
                     [TRIPLE (CELLPIN, ID (List.hd mybuf.gnd.opinlst).idpin, output)])])]

let insert_tri syms output input enable =
  assert_unit_width syms [input;output;enable];
  [QUADRUPLE(MODINST, ID mybuf.tri.nam, EMPTY,
            TLIST
              [TRIPLE
                  (instid [input;output;enable] syms mybuf.tri, SCALAR,
                   TLIST
                     [TRIPLE (CELLPIN, ID (List.nth mybuf.tri.ipinlst 1).idpin, enable);
                      TRIPLE (CELLPIN, ID (List.hd mybuf.tri.ipinlst).idpin, input);
                      TRIPLE (CELLPIN, ID (List.hd mybuf.tri.opinlst).idpin, output)])])]

let insert_and syms out lft rght = 
  assert_unit_width syms [out;lft;rght];
  [QUADRUPLE(MODINST, ID mybuf.logand.nam, EMPTY,
             TLIST
               [TRIPLE
                   (instid [out;lft;rght] syms mybuf.logand, SCALAR,
                    TLIST
                      [TRIPLE(CELLPIN, ID (List.hd mybuf.logand.ipinlst).idpin, lft);
                       TRIPLE(CELLPIN, ID (List.nth mybuf.logand.ipinlst 1).idpin, rght);
                       TRIPLE(CELLPIN, ID (List.hd mybuf.logand.opinlst).idpin, out)])])]

and insert_or syms out lft rght = 
  assert_unit_width syms [out;lft;rght];
  [QUADRUPLE(MODINST, ID mybuf.logor.nam, EMPTY,
             TLIST
               [TRIPLE
                   (instid [out;lft;rght] syms mybuf.logor, SCALAR,
                    TLIST
                      [TRIPLE(CELLPIN, ID (List.hd mybuf.logor.ipinlst).idpin, lft);
                       TRIPLE(CELLPIN, ID (List.nth mybuf.logor.ipinlst 1).idpin, rght);
                       TRIPLE(CELLPIN, ID (List.hd mybuf.logor.opinlst).idpin, out)])])]

and insert_xor syms out lft rght =
  assert_unit_width syms [out;lft;rght];
 [QUADRUPLE(MODINST, ID mybuf.logxor.nam, EMPTY,
                                         TLIST
                                           [TRIPLE
                                               (instid [out;lft;rght] syms mybuf.logxor, SCALAR,
                                                TLIST
                                                  [TRIPLE(CELLPIN, ID (List.hd mybuf.logxor.ipinlst).idpin, lft);
                                                   TRIPLE(CELLPIN, ID (List.nth mybuf.logxor.ipinlst 1).idpin, rght);
                                                   TRIPLE(CELLPIN, ID (List.hd mybuf.logxor.opinlst).idpin, out)])])]

and insert_mux syms sel out hi lo =
  assert_unit_width syms [sel;out;hi;lo];
  [QUADRUPLE(MODINST, ID mybuf.mux.nam, EMPTY,
             TLIST
               [TRIPLE
                   (instid [sel;out;hi;lo] syms mybuf.mux, SCALAR,
                    TLIST
                      [TRIPLE(CELLPIN, ID (List.nth mybuf.mux.ipinlst 2).idpin, sel);
                       TRIPLE(CELLPIN, ID (List.nth mybuf.mux.ipinlst 1).idpin, hi);
                       TRIPLE(CELLPIN, ID (List.nth mybuf.mux.ipinlst 0).idpin, lo);
                       TRIPLE(CELLPIN, ID (List.hd mybuf.mux.opinlst).idpin, out)])])]

let libcells () = Printf.printf "\t\t\t  name\t io_count\tprimary\n";
    Hashtbl.iter (fun k x -> if x.func <> EMPTY then Printf.printf "%30s\t%2d\t%s\n" k x.len (Ord.getstr x.func)) libhash;;

let hls_net_id osymtab =
  let nam id = sprintf "%s%d" magic id in
  let hls_id = ref (Hashtbl.length osymtab) in
  while Hashtbl.mem osymtab (nam !hls_id) do
    incr hls_id
  done;
  Hashtbl.replace osymtab (nam !hls_id) (WIRE,true);
  IDSTR (nam !hls_id)

let rec hls_triadic syms osymtab fn (ilst:token list ref) exp1 exp2 exp3 =
  let sel = cnv_asgn syms osymtab ilst exp1
  and lft = cnv_asgn syms osymtab ilst exp2
  and rght = cnv_asgn syms osymtab ilst exp3
  and netid = hls_net_id osymtab in
  ilst := fn sel netid lft rght @ !ilst;
  netid
and hls_dyadic syms osymtab fn ilst exp1 exp2 =
  let lft = cnv_asgn syms osymtab ilst exp1
  and rght = cnv_asgn syms osymtab ilst exp2
  and netid = hls_net_id osymtab in
  ilst := fn netid lft rght @ !ilst;
  netid
and hls_unary syms osymtab fn ilst exp =
  let lft = cnv_asgn syms osymtab ilst exp
  and netid = hls_net_id osymtab in
  ilst := fn netid lft @ !ilst;
  netid
and cnv_concat syms osymtab ilst (clst:token list) = function
  | ID dest ->
  let (lft,rght,inc) = find_width dest syms in
  let idx = ref lft in List.iter (function
    | IDSTR whole ->
            ilst := insert_buf syms
              (TRIPLE(BITSEL, ID dest, INT !idx))
              (IDSTR whole) @ !ilst;
            idx := !idx + inc;
    | oth -> Dump.unhandled stderr 336 oth) clst
  | oth -> Dump.unhandled stderr 337 oth
and cnv_asgn syms osymtab ilst = function
      | DOUBLE(LPAREN, exp) -> cnv_asgn syms osymtab ilst exp
      | DOUBLE(NOT, exp) -> hls_unary syms osymtab (insert_inv syms) ilst exp
      | DOUBLE(PLING, exp) -> hls_unary syms osymtab (insert_rednot syms) ilst exp
      | DOUBLE(AND, exp) -> hls_unary syms osymtab (insert_redand syms) ilst exp
      | DOUBLE(OR, exp) -> hls_unary syms osymtab (insert_redor syms) ilst exp
      | TRIPLE(AND, exp1, exp2) -> hls_dyadic syms osymtab (insert_and syms) ilst exp1 exp2
      | TRIPLE(OR, exp1, exp2) -> hls_dyadic syms osymtab (insert_or syms) ilst exp1 exp2
      | TRIPLE(XOR, exp1, exp2) -> hls_dyadic syms osymtab (insert_xor syms) ilst exp1 exp2
      | QUADRUPLE(QUERY, exp1, exp2, exp3) -> hls_triadic syms osymtab (insert_mux syms) ilst exp1 exp2 exp3
(*
      | TRIPLE(BITSEL, ID exp1, INT exp2) -> hls_Hop_TBD exp1
      | BINNUM num -> hls_Hop_TBD num
      | ID id -> hls_Hop_TBD id
*)
      | TRIPLE(P_OROR, exp1, exp2) -> hls_dyadic syms osymtab (insert_or syms) ilst exp1 exp2
      | TRIPLE(P_ANDAND, exp1, exp2) -> hls_dyadic syms osymtab (insert_and syms) ilst exp1 exp2
      | ID id -> ID id
      | IDSTR id -> ID (enterid id)
      | TRIPLE(BITSEL, ID exp1, INT exp2) -> TRIPLE(BITSEL, ID exp1, INT exp2)
      | oth -> Dump.unhandled stderr 357 oth; oth

let create_modinst'' pathid ilst syms (osymtab:osymtab) oinsts id = function
  | UNKNOWN -> (enterid "$","",UNKNOWN)
  | (TRIPLE (CELLPIN, ID cellpin, ID conn)) ->
     let (hi,lo,inc) = find_width conn syms in
      let bus = if hi=lo then sprintf "%s[%d]" conn.id hi else
	   sprintf "%s[%d:%d]" conn.id hi lo in
      if !verbose then printf "create_modinst1'' .%s(%s[%d:%d:%d])\n" cellpin.id conn.id hi lo inc;
      if Hashtbl.mem osymtab conn.id then
        (cellpin, conn.id, if inc = 0 then pathid conn else TRIPLE(BITSEL, pathid conn, INT hi))
      else if Hashtbl.mem osymtab bus then
        (cellpin, bus, TRIPLE(BITSEL, pathid conn, INT hi))
      else failwith ("wire "^conn.id^" not found")
  | TRIPLE (CELLPIN, ID cellpin, (TRIPLE(BITSEL, ID conn, (INT idx|WIDTHNUM(_,_,idx))))) ->
     let (hi,lo,inc) = find_width conn syms in
     if !verbose then printf "create_modinst2'' %s\n" cellpin.id;
      let bus = sprintf "%s[%d]" conn.id idx in
      if Hashtbl.mem osymtab bus then
        (cellpin, bus, TRIPLE(BITSEL, pathid conn, INT idx))
      else if Hashtbl.mem osymtab conn.id && idx = hi && idx = lo then
        (cellpin, conn.id, TRIPLE(BITSEL, pathid conn, INT idx))
      else failwith ("wire "^bus^" not found")
  | TRIPLE (CELLPIN, ID cellpin, QUADRUPLE(PARTSEL, ID conn, lft, rght)) ->
    if !verbose then printf "create_modinst3'' %s\n" cellpin.id;
      let (hi,lo,inc) = find_width conn syms in
      let (selhi,sello,dir) = Const.iwidth stderr syms (RANGE(lft,rght)) in
      if selhi=sello && (selhi >= 0) && (sello >= 0) && (hi >= 0) && (lo >= 0) then
        begin
          let bus = sprintf "%s[%d]" conn.id selhi in
          if Hashtbl.mem osymtab bus then
            (cellpin, bus,  TRIPLE(BITSEL, pathid conn, INT selhi))
          else if Hashtbl.mem osymtab conn.id && selhi = hi && sello = lo then
            (cellpin, conn.id,  TRIPLE(BITSEL, pathid conn, INT selhi))
          else failwith ("wire "^bus^" not found")
        end
      else if (selhi >= 0) && (sello >= 0) && (hi >= 0) && (lo >= 0) then
        begin          let idx = ref hi
          and buf = Buffer.create 64
          and delim = ref '{' in
                           while !idx <> lo + inc do
                             bprintf buf "%c%s[%d]" !delim conn.id !idx;
                             delim := ',';
                             idx := !idx + inc
                           done;
                           (cellpin, Buffer.contents buf^"}", ID conn)
        end
      else failwith(sprintf "cellpin connection of width [%d:%d] out of [%d:%d] not supported" selhi sello hi lo)
  | TRIPLE (CELLPIN, ID cellpin, BINNUM lev) ->
    if !verbose then printf "create_modinst4'' %s\n" cellpin.id;
    (cellpin, lev, BINNUM lev)
  | TRIPLE (CELLPIN, ID cellpin, (WIDTHNUM(radix,sz,num) as wn)) ->
    if !verbose then printf "create_modinst5'' %s\n" cellpin.id;
    (cellpin, sprintf "%d'b%d" sz num, wn)
  | ILLEGAL _ ->
    (enterid "$","",UNKNOWN)
  | TRIPLE (CELLPIN, ID cellpin, arg) ->
    if !verbose then printf "create_modinst6'' %s\n" cellpin.id;
      let (conn,subst) = match cnv_asgn syms osymtab ilst arg with
        | ID id -> (id, pathid id)
        | IDSTR id' -> let id = enterid id' in (id, ID id)
        | EMPTY -> 
            (match hls_net_id osymtab with
              | IDSTR id' -> let id = enterid id' in (id, ID id)
              | oth -> unhandled stderr 1493 oth; (enterid "$",EMPTY))
        | oth -> unhandled stderr 1444 oth; (enterid "$",oth) in
      (cellpin,conn.id,subst)
  | DOUBLE (CELLPIN, ID cellpin) ->
    if !verbose then printf "create_modinst7'' %s\n" cellpin.id;
    (cellpin, "1'b0", EMPTY)
  | EMPTY -> failwith "cellpin cannot be empty"
  | err -> unhandled stderr 1446 err; (enterid "$",id,err)

let create_modinst' pathid ilst syms osymtab oinsts kind is_out reglst = function
      | TRIPLE (ID id, SCALAR, TLIST arg4) ->
        let pinlst = ref [] in 
        let instenc = ref id.id in
        List.iter (fun itm ->
          if !verbose then printf "create_modinst' %s\n" (Dump.dumpstr itm);
          let (cellpin,conn,tok) = create_modinst'' pathid ilst syms osymtab oinsts id.id itm in
          if !verbose then printf "returned (%s,%s,%s)\n" cellpin.id conn (Dump.dumpstr tok);
          pinlst := (ID cellpin,conn,tok) :: !pinlst;
          if is_out cellpin then 
            List.iter (fun itm -> instenc := itm.idpin.id ^"_"^ conn ^"_"^ !instenc) reglst) arg4;
        if Hashtbl.mem oinsts kind then
          let klst = Hashtbl.find oinsts kind in
          klst := (!instenc,!pinlst) :: !klst
        else
          Hashtbl.add oinsts kind (ref [!instenc,!pinlst])
      | err -> unhandled stderr 312 err

let create_modinst pathid ilst syms osymtab oinsts kind instlst =
  if Hashtbl.mem libhash kind then
    let prop = Hashtbl.find libhash kind in
    let is_out cellpin = Read_library.is_member cellpin prop.opinlst in
    List.iter (create_modinst' pathid ilst syms osymtab oinsts kind is_out prop.reglst) instlst
  else if Hashtbl.mem modprims kind then
    let subcell = Hashtbl.find modprims kind in
    (match subcell.tree with
    | QUINTUPLE(MODULE, ID arg1, arg2, TLIST arg3, THASH (decls,insts)) ->
      let op = ref [] in Hashtbl.iter (fun k _ -> match k with
          | QUINTUPLE(OUTPUT, arg1, arg2, rng, TLIST arg4) ->
            List.iter (function
              | TRIPLE (ID id, _, _) -> op := id :: !op
              | err -> unhandled stderr 328 err) arg4
          | _ -> ()) decls;
      let is_out cellpin = List.mem cellpin !op in
      List.iter (create_modinst' pathid ilst syms osymtab oinsts kind is_out []) instlst
    | _ -> failwith (kind^" is not a module netlist"))
  else
    (printf "modinst %s not found\n" kind)

let create_ilst fn = function
    | QUADRUPLE(MODINST, ID kind, EMPTY, TLIST arg3) -> fn kind.id arg3
    | oth -> unhandled stderr 1453 oth

let create_asgn pathid syms osymtab oinsts out src =
      let lst = ref [] in
      (match src with 
       | DOUBLE(CONCAT, TLIST clst) ->
       	 cnv_concat syms osymtab lst (List.map (cnv_asgn syms osymtab lst) clst) out
       | _ -> lst := insert_buf syms out (cnv_asgn syms osymtab lst src) @ !lst);
      let lst'' = ref [] in List.iter (create_ilst (create_modinst pathid lst syms osymtab oinsts)) !lst;
      assert(!lst'' = [])

let minimap_body''' pathid syms osymtab ilst oinsts = function
    | QUADRUPLE(MODINST, ID kind, EMPTY, TLIST arg3) -> create_modinst pathid ilst syms osymtab oinsts kind.id arg3

    | TRIPLE(ASSIGN, _, TLIST alst) ->
      List.iter (function
	| TRIPLE(ASSIGNMENT, ID out, src) -> create_asgn pathid syms osymtab oinsts (ID out) src
	| TRIPLE(ASSIGNMENT, (TRIPLE (BITSEL, ID out, INT outidx) as dest), src) ->
          create_asgn pathid syms osymtab oinsts dest src
        | oth -> unhandled stderr 1420 oth) alst
    | oth -> unhandled stderr 1421 oth

let minimap_body'' pathid ohash syms osymtab ilst oinsts =
  let lst'' = ref [] in List.iter (create_ilst (create_modinst pathid lst'' syms osymtab oinsts)) !ilst;
  assert(!lst'' = []);
  Hashtbl.iter (fun kind reflst ->
    Hashtbl.add ohash
      (QUADRUPLE(MODINST, ID (enterid kind), EMPTY, TLIST (List.map (fun (inst, connlst) ->
        TRIPLE(ID (enterid inst), SCALAR,
               TLIST (List.map (fun (formal,str,conn) -> TRIPLE(CELLPIN, formal, conn)) connlst))
       )!reflst ))) ()) oinsts

let minimap_body' pathid ohash hash syms osymtab =
  let ilst = ref [] and oinsts = Hashtbl.create 256 in
  Hashtbl.iter (fun x _ -> minimap_body''' pathid syms osymtab ilst oinsts x) hash;
  minimap_body'' pathid ohash syms osymtab ilst oinsts;
  (oinsts,ohash)

let minimap_body pathid hash syms osymtab = minimap_body' pathid (Hashtbl.create 256) hash syms osymtab
    
let decl_to_hls dlst syms =
        let (osymtab:osymtab) = Hashtbl.create 256 in
        List.iter (function
          | QUINTUPLE((INPUT|OUTPUT|INOUT) as dir, arg1, arg2, rng, TLIST arg4) ->
            List.iter (function
              | TRIPLE (ID id, EMPTY, EMPTY) ->  (match rng with
                  | EMPTY -> Hashtbl.replace osymtab id.id (dir,false)
                  | RANGE(left, right) ->
                    let hi = Const.exprConstStr stderr syms left and
		        lo = Const.exprConstStr stderr syms right in
                    for i = int_of_string lo to int_of_string hi do
                      Hashtbl.replace osymtab (sprintf "%s[%d]" id.id i) (dir,false) done
                  | err -> unhandled stderr 484 err)
              | err -> unhandled stderr 485 err) arg4
          | QUADRUPLE((WIRE|TRI0|TRI1) as kind, arg0, TRIPLE(arg1, rng, arg3), TLIST arg4) -> List.iter (function
              | DOUBLE (ID id, EMPTY) -> (match rng with
                  | EMPTY ->
                      if not (Hashtbl.mem osymtab id.id) then (* dont override an input/output with a wire *)
                        Hashtbl.replace osymtab id.id (kind,false)
                  | RANGE(INT hi, INT lo) -> for i = lo to hi do let nam = sprintf "%s[%d]" id.id i in
                      if not (Hashtbl.mem osymtab nam) then (* dont override an input/output with a wire *)
                        Hashtbl.replace osymtab nam (kind,false) done
                  | err -> unhandled stderr 494 err)
              | err -> unhandled stderr 495 err) arg4
          | SEXTUPLE(PARAMETER, signage, range, id, attr, expr) -> () (* value should already be known *)
          | oth -> unhandled  stderr 496 oth) dlst;
        osymtab

let minimap_netlist pathid k =
  match k.tree with
    | QUINTUPLE(MODULE, ID arg1, arg2, TLIST arg3, THASH (decls,insts)) ->
      let osymtab = decl_to_hls (hfilter (function _ -> true) decls) k.symbols in
      let (oinsts,ohash) = minimap_body pathid insts k.symbols osymtab in
      let odecls = Hashtbl.copy decls in
      let extlst = ref [] in
      Hashtbl.iter (fun k (kind,extra) -> if extra then extlst := DOUBLE(ID (enterid k), EMPTY) :: !extlst) osymtab;
      Hashtbl.add odecls (QUADRUPLE(WIRE, EMPTY, TRIPLE(EMPTY, EMPTY, EMPTY), TLIST !extlst)) ();
      QUINTUPLE(MODULE, ID (enterid (arg1.id^ !Globals.modsuffix)), arg2,
                TLIST arg3,
                THASH (odecls, ohash))
    | _ -> failwith (Dump.dumpstr k.tree^" is not a module netlist")

let minimap_arch arch nam =
  let lst = Hashtbl.find_all modprims nam in
  List.iter (fun arg -> if arg.arch = arch then
      Semantics.prescan stderr !Globals.archenv (minimap_netlist (fun id -> ID id) arg) "Generated by minimap_arch";
      ) lst;
Printf.printf "Module report %s\n" (Semantics.endscan())

let uniq namtab (olds:string) (s:string) = let cnt = ref 0 and u = ref s and matched = ref (Hashtbl.mem namtab olds) in
  while not !matched && Hashtbl.mem namtab !u do
    matched := (Hashtbl.find namtab !u) = olds;
    Printf.printf "%s -> %s " !u (if !matched then "true" else "false");
    if not !matched then
      (incr cnt; u := s^"_"^(string_of_int !cnt))
  done;
  if not !matched then (Hashtbl.replace namtab olds !u);
  Hashtbl.find namtab olds

let valid namtab s =
  let l = String.length s and truncate = ref false in
  let v = Bytes.create l in for i = 0 to l-1 do match s.[i] with
    | '_' -> Bytes.set v i s.[i];
    | 'A'..'Z' -> Bytes.set v i s.[i];
    | 'a'..'z' -> Bytes.set v i s.[i];
    | '0'..'9' -> if i > 0 then Bytes.set v i s.[i] else Bytes.set v i '_';
    | '$' -> if i > 0 then Bytes.set v i s.[i] else Bytes.set v i '_';
    | '#' -> if i > 0 then Bytes.set v i s.[i] else Bytes.set v i '_';
    | '\\' -> if i > 0 then Bytes.set v i s.[i] else Bytes.set v i '_';
    | '-' -> if i > 0 then Bytes.set v i s.[i] else Bytes.set v i '_';
    | ' ' -> if i == l-1 then truncate := true else Bytes.set v i '_';
    | _ -> Bytes.set v i '_';
    done;
  uniq namtab s (Bytes.to_string (if !truncate then Bytes.sub v 0 (l-1) else v))

let validid namtab id = ID (enterid (valid namtab id.id))
    
let netid prefix gentab namtab  =
  let loop = ref true and netcnt = ref 0 and net = ref "" in
  while !loop do
    net := sprintf "v2%s$%d" prefix !netcnt;
    if Hashtbl.mem gentab !net then incr netcnt
    else
      begin
        Hashtbl.replace gentab !net ();
        loop := false
      end;
  done;
    enterid (valid namtab !net)
    
let declare namtab syms p hi lo =
  if !verbose then printf "131 v2hls.declare %s[%d:%d]\n" p.id hi lo;
  let pth = valid namtab p.id in
  let pth' = enterid pth in
  if not (Const.shash_chain_mem syms pth') then
    begin
      if !verbose then printf "135 v2hls.declare %s[%d:%d]\n" pth hi lo;
      Semantics.enter_a_sym stderr syms pth' WIRE (RANGE(INT hi,INT lo)) Semantics.Create;
    end

let lfill' namtab syms owidth = function
  | ID arg ->
    let w = symwidth syms arg in
    if w < owidth then DOUBLE(CONCAT,TLIST [WIDTHNUM(2,owidth-w,0);ID (enterid (valid namtab arg.id))])
    else (ID arg)
  | INT num -> WIDTHNUM(10,max (mask num) owidth,num)
  | WIDTHNUM(radix,sz,num) -> WIDTHNUM(radix,max sz owidth,num)
  | QUADRUPLE(PARTSEL, ID id, INT hi, INT lo) ->
    let w = hi-lo+1 in
    if (w < owidth) then DOUBLE(CONCAT,TLIST [WIDTHNUM(2,owidth-w,0);QUADRUPLE(PARTSEL, ID id, INT hi, INT lo)])
    else QUADRUPLE(PARTSEL, ID id, INT hi, INT lo)
  | TRIPLE(BITSEL, ID id, sel) ->
    if owidth > 1 then DOUBLE(CONCAT,TLIST [WIDTHNUM(2,owidth-1,0);TRIPLE(BITSEL, ID id, sel)])
    else TRIPLE(BITSEL, ID id, sel)
  | DOUBLE(CONCAT,TLIST lst) -> let w = cwidth syms lst in
    if w < owidth then DOUBLE(CONCAT,TLIST (WIDTHNUM(2,owidth - w,0) :: lst))
    else DOUBLE(CONCAT,TLIST lst)
  | TRIPLE(P_ANDAND, lft, rght) ->
    if owidth > 1 then DOUBLE(CONCAT,TLIST [WIDTHNUM(2,owidth-1,0);TRIPLE(P_ANDAND, lft, rght)])
    else TRIPLE(P_ANDAND, lft, rght)
  | BINNUM str -> let (sz,_) = Const.widthnum stderr 2 str in
		  if sz = owidth then BINNUM str else (Dump.unhandled stderr 209 (BINNUM str); EMPTY)
  | QUADRUPLE(P_SRIGHT, arg, INT shft, INT w) ->
    if (w < owidth) then DOUBLE(CONCAT,TLIST [WIDTHNUM(2,owidth-w,0);QUADRUPLE(P_SRIGHT, arg, INT shft, INT w)])
    else QUADRUPLE(P_SRIGHT, arg, INT shft, INT w)
  | oth -> Dump.unhandled stderr 210 oth; EMPTY

let lfill (arg,w) owidth =
  if w >= owidth
  then (arg,w)
  else
    begin
      if !verbose then printf "lfill %s from %d to %d\n" (Count.tokenstr arg) w owidth;
      (DOUBLE(CONCAT,TLIST [WIDTHNUM(2,owidth-w,0);arg]),owidth)
    end

