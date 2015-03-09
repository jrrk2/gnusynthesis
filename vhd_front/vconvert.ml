(*
    <vscr - Verilog converter to abc format.>
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

open Buffer
open Printf
open Vparser
open VhdlTree
open Combined

let unkpkg1 = ref []
let unkpkg2 = ref []
let unkpkg3 = ref []
let unkpkg4 = ref []
let errlst = ref []
let pkglst = ref []
let pkgtab = Hashtbl.create 256

let rec log2 = function
  | 0 -> 0
  | 1 -> 0
  | n -> 1 + log2(n/2)

let rec rngfun = function
| RANGE(INT hi, INT lo) -> RANGE(INT hi, INT lo)
| DOUBLE(ID kind, BINNUM eval) -> 
    let str = String.uppercase kind.Idhash.id in
    if Hashtbl.mem pkgtab str then rngfun (Hashtbl.find pkgtab str) else EMPTY
| ID kind -> if Hashtbl.mem pkgtab kind.Idhash.id then rngfun (Hashtbl.find pkgtab (String.uppercase kind.Idhash.id)) else EMPTY
| _ -> EMPTY

let rec convert = function
  | Quintuple (Str "5",
               Str ("std_logic_vector"|"STD_LOGIC_VECTOR"),
               Str init,
               Str id,
               Triple (Str "218", Num hi, Num lo)) ->
               let rng = RANGE(INT (Big_int.int_of_big_int hi), INT (Big_int.int_of_big_int lo)) in
                     QUADRUPLE(REG, EMPTY, rng, TLIST [ TRIPLE (ID (enterid id), EMPTY, EMPTY) ])
  | Quintuple (Str "5",
               Str ("std_logic_vector"|"STD_LOGIC_VECTOR"),
               Str init,
               List lst0,
               List [
                 List [
                   Triple (Str "218", Num hi, Num lo)
                 ]
               ]) -> let rng = RANGE(INT (Big_int.int_of_big_int hi), INT (Big_int.int_of_big_int lo)) in
                     QUADRUPLE(REG, EMPTY, rng, TLIST (List.map (function
                       | Str id -> TRIPLE (ID (enterid id), EMPTY, EMPTY)
                       | _ -> EMPTY) lst0))
  | Quintuple(Str "7",str0,Num num0,Num num1,List lst0) -> let rng = RANGE(INT (log2 (Big_int.int_of_big_int num1)), INT 0) in
                     QUADRUPLE(REG, EMPTY, rng, TLIST (List.map (function
                       | Str id -> TRIPLE (ID (enterid id), EMPTY, EMPTY)
                       | _ -> EMPTY) lst0))
  | Quintuple (Str "7", str0, num0, num1, arg) -> convert (Quintuple (Str "7", str0, num0, num1, List [arg]))
  | Quadruple (Str "10",str0,chr0,List lst0) -> QUADRUPLE(REG, EMPTY, EMPTY, TLIST (List.map (function
      | Str id -> TRIPLE (ID (enterid id), EMPTY, EMPTY)
      | _ -> EMPTY) lst0))
  | Quadruple (Str "10",str0,chr0,Str id) -> convert (Quadruple (Str "10",str0,chr0,List [Str id]))
  | Triple (Str "11", Str logic, List lst0) -> let idlst = List.map (function
        | Str id -> DOUBLE (ID (enterid id), EMPTY)
        | _ -> EMPTY) lst0 in
    if Hashtbl.mem pkgtab (String.uppercase logic) then
      QUADRUPLE(WIRE, EMPTY, TRIPLE(EMPTY, rngfun (Hashtbl.find pkgtab (String.uppercase logic)), EMPTY), TLIST idlst)
    else
      QUADRUPLE(WIRE, EMPTY, TRIPLE(EMPTY,EMPTY,EMPTY), TLIST idlst)
   | Triple (Str "11", Str logic, oth) -> convert (Triple (Str "11", Str logic, List [oth]))
   | Quintuple (Str "29", Str c, Str id, List [], List lst0) -> let modref = enterid id in ();
    QUADRUPLE(MODINST, ID modref, EMPTY,
              TLIST [TRIPLE (ID (enterid c), SCALAR, TLIST (List.map (function
                | Double (Str "240", Str port) -> ID (enterid port)
                | _ -> EMPTY) lst0))]);
  | Quintuple (Str "33", Str "false",
               List senslst,
               List [],
               List procstmts) -> TLIST (List.map (function
                 | Triple (Str "115", Str outq, Double (Str "276", Str intq)) ->
                     TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID (enterid outq), ID (enterid intq))])
                 | others -> convert others) procstmts)
  | Quintuple(Str "35",	Str str0, Str outp, Str str2, List [Triple (Str "257", first,
       Double (Str "77", cond1));Triple (Str "257", second, cond2)]) ->
    TRIPLE(ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID (enterid outp), QUADRUPLE(QUERY, convert cond1, convert first, convert second))])
  | Quintuple (Str "36", Str "false", Str outp, Str "false", List lst0) ->
    TRIPLE(ASSIGN, EMPTY, TLIST (List.map (fun arg -> TRIPLE (ASSIGNMENT, ID (enterid outp), convert arg)) lst0))
  | Triple (Str "46", Str ("std_logic"|"STD_LOGIC"), List lst0) -> QUINTUPLE(INPUT, EMPTY, EMPTY, EMPTY, TLIST (List.map (function
      | Str id -> TRIPLE (ID (enterid id), EMPTY, EMPTY)
      | _ -> EMPTY) lst0))
  | Quadruple (Str "50",
               Str ("std_logic_vector"|"STD_LOGIC_VECTOR"),
               List lst0,
               List [
                 List [
                   Triple (Str "218", Num hi, Num lo)
                 ]
               ]) -> let rng = RANGE(INT (Big_int.int_of_big_int hi), INT (Big_int.int_of_big_int lo)) in
                     QUINTUPLE(OUTPUT, EMPTY, EMPTY, rng, TLIST (List.map (function
                       | Str id -> TRIPLE (ID (enterid id), EMPTY, EMPTY)
                       | _ -> EMPTY) lst0))
  | Triple (Str "51", Str "std_logic", List lst0) -> QUINTUPLE(OUTPUT, EMPTY, EMPTY, EMPTY, TLIST (List.map (function
      | Str id -> TRIPLE (ID (enterid id), EMPTY, EMPTY)
      | _ -> EMPTY) lst0))
  | Double(Str "56", Str str0) -> ID (enterid str0)
  | Double(Str "77", exp0) -> convert exp0
  | Sextuple(Str "78", Str nam, lst0, intf, lst2, lst3) ->
    let iolst = (match convert intf with
      | TLIST lst -> lst
      | _ -> []) in
    TRIPLE(IOPORT,
           TLIST (List.concat (List.map (function
             | QUINTUPLE(dir, EMPTY, EMPTY, rng, TLIST lst) -> List.map (function
                 | TRIPLE (ID id, EMPTY, EMPTY) -> ID (id)
                 | err -> EMPTY) lst
             | err -> []) iolst)),
           TLIST (iolst))
  | Quintuple (Str "79", Str pkgid, List [], List [], List lst) -> TLIST (List.map (function
      | Quadruple (Str "63", Str id, Quadruple (Str "311", Str "integer", Num n, Str "nb_record"), List lst) ->
        TLIST (List.map (function
          | Quadruple (Str "309", Str "std_logic_vector", Str bus, rng) -> TRIPLE(VECTORED, ID (enterid bus), convert rng)
          | Triple (Str "312", Str bus, Str id) -> TRIPLE(VECTORED, ID (enterid id), ID (enterid bus))
          | Triple (Str "316", Str bus, Str id) -> TRIPLE(VECTORED, ID (enterid id), ID (enterid bus))
          | Quadruple (Str "317", Str "std_logic_vector", Str bus, rng) -> TRIPLE(VECTORED, ID (enterid bus), convert rng)
          | Triple (Str "318", Str bus, Str id) -> TRIPLE(VECTORED, ID (enterid id), ID (enterid bus))
          | err -> unkpkg1 := err :: !unkpkg1; EMPTY) lst)
      | Quadruple (Str "63", Str id, List [], List lst) -> TLIST (List.map (function
          | Triple (Str "312", Str bus, Str id) -> TRIPLE(VECTORED, ID (enterid id), ID (enterid bus))
          | Triple (Str "316", Str bus, Str id) -> TRIPLE(VECTORED, ID (enterid id), ID (enterid bus))
          | Triple (Str "318", Str bus, Str id) -> TRIPLE(VECTORED, ID (enterid id), ID (enterid bus))
          | err -> unkpkg2 := err :: !unkpkg2; EMPTY) lst)
      | Quadruple (Str "70", Str bus, Str "std_logic_vector", rng) -> TRIPLE(VECTORED, ID (enterid bus), convert rng)
      | Triple (Str "72", Str bus, Str logic) -> TRIPLE(VECTORED, ID (enterid bus), ID (enterid logic))
      | Sextuple (Str "73", Str id, Str arg1, Str arg2, chr0, chr1) -> EMPTY
      | Quadruple (Str "74", Str "std_logic", Str "resolved", Str "std_ulogic") -> EMPTY
      | Quadruple (Str "75", Str vector, Str logic, Double (Str "133", Str "NATURAL")) -> EMPTY
      | Triple (Str "76", Str "std_ulogic", List lst) -> TLIST (List.map (function
          | Double (Str "20", Char ch) -> INT (int_of_char ch)
          | err -> unkpkg3 := err :: !unkpkg3; EMPTY) lst)
      | Quadruple (Str "326", Str kind, Str eval, Str nam) -> QUADRUPLE(VECTORED, ID (enterid nam), ID (enterid kind), BINNUM eval)
      | Sextuple (Str ("327"|"328"), Str id, Str logic, arg1, List [], List []) -> convert arg1
      | err -> unkpkg4 := err :: !unkpkg4; EMPTY) lst)
    
  | Quintuple(Str "87", Str arch, Str nam, lst0, lst1) -> QUADRUPLE(COVER, ID (enterid arch), convert lst0, convert lst1)
  | Quadruple(Str "93", lst0, lst1, exp0) -> QUADRUPLE(IF, convert exp0, convert lst0, convert lst1)
  | Quadruple(Str "94", lst0, exp0, exp1) -> QUADRUPLE(IF, convert exp0, convert lst0, convert exp1)
  | Triple(Str "95", List stmts, Double (Str "77",
                                         Quadruple (Str "152", Str clk1, Char '1',
                                                    Triple (Str "255", Str clk2, Str "event")))) when clk1=clk2 ->
    DOUBLE(ALWAYS, DOUBLE(DOUBLE (AT, TLIST [DOUBLE (POSEDGE, ID (enterid clk1))]), TLIST (List.map convert stmts)))
  | Triple(Str "95", stmts, exp0) -> TRIPLE(IF, convert exp0, convert stmts)
  | Triple(Str "115", Str id, src) -> QUADRUPLE (ASSIGNMENT, ID (enterid id), EMPTY, convert src)
  | Quadruple (Str "117", Str nam, Triple(Str "218", Num hi, Num lo), src) ->
    QUADRUPLE (ASSIGNMENT, QUADRUPLE(PARTSEL, ID (enterid nam), INT (Big_int.int_of_big_int hi),
                                     INT (Big_int.int_of_big_int lo)),
               EMPTY, convert src)
  | Quadruple (Str "117", Str nam, Double(Str "237", Num sel), src) ->
    QUADRUPLE (ASSIGNMENT, TRIPLE(BITSEL,
                                  ID (enterid nam),
                                  INT (Big_int.int_of_big_int sel)),
               EMPTY,
               convert src)
  | Triple(Str "142", exp2, exp1) -> TRIPLE(IF, convert exp1, convert exp2)
  | Triple(Str "171", str0, str1) -> TRIPLE(P_EQUAL, convert str0, convert str1)
  | Quadruple(Str "200",Str str0,Str str1,exp0) -> TRIPLE(OR, convert exp0,
                                                          TRIPLE(P_EQUAL, ID (enterid str0), ID (enterid str1)))
  | Quintuple(Str "201", left1, left2, right1, right2) -> TRIPLE(OR,
                                                                 TRIPLE(P_EQUAL, convert left1, convert right1),
                                                                 TRIPLE(P_EQUAL, convert left2, convert right2))
  | Triple (Str "218", Num hi, Num lo) -> RANGE(INT (Big_int.int_of_big_int hi), INT (Big_int.int_of_big_int lo))
  | Double (Str "237", num0) -> ASCNUM "*BITSEL*"
  | Double (Str "270", chr0) -> convert chr0
  | Double (Str "275", Str init) -> BINNUM ((string_of_int (String.length init))^"'b"^init)
  | Double (Str "276", Str inp) -> ID (enterid inp)
  | Double (Str "277", Str inp) -> DOUBLE(PLING, ID (enterid inp))
  | Triple (Str "281", Str nam, Double(Str "237", Num sel)) -> DOUBLE(PLING, TRIPLE(BITSEL, ID (enterid nam), INT (Big_int.int_of_big_int sel)))
  | Triple (Str "292", Str nam, Triple(Str "218", Num hi, Num lo)) -> QUADRUPLE(
    PARTSEL,
    ID (enterid nam),
    INT (Big_int.int_of_big_int hi),
    INT (Big_int.int_of_big_int lo))
  | Triple (Str "285", Str inp1, Str inp2) -> TRIPLE(AND, ID (enterid inp1), ID (enterid inp2))
  | Triple (Str "295", Str inp1, Str inp2) -> TRIPLE(OR, ID (enterid inp1), ID (enterid inp2))
  | Triple (Str "296", Str inp1, Str inp2) -> TRIPLE(OR, ID (enterid inp1), ID (enterid inp2))
  | Triple (Str "298", Str inp1, Str inp2) -> TRIPLE(XOR, ID (enterid inp1), ID (enterid inp2))
  | Triple (Str "305", Str str0, src) -> TRIPLE (ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, ID (enterid str0), convert src)])
  | Quintuple (Str "306", Str "ns", Num n, List [], Str "55") -> EMPTY
  | Triple (Str "312", Str logic, Str id) ->
    if Hashtbl.mem pkgtab (String.uppercase logic) then
      QUINTUPLE(INPUT, EMPTY, EMPTY, rngfun (Hashtbl.find pkgtab (String.uppercase logic)), TLIST [TRIPLE (ID (enterid id), EMPTY, EMPTY)])
    else
      QUINTUPLE(INPUT, EMPTY, EMPTY, EMPTY, TLIST [TRIPLE (ID (enterid id), EMPTY, EMPTY)])
  | Quadruple (Str "317", Str ("std_logic_vector"|"STD_LOGIC_VECTOR"), Str id, (Triple (Str "218", Num hi, Num lo) as rng)) ->
    QUINTUPLE(OUTPUT,
              EMPTY,
              EMPTY,
              convert rng,
              TLIST [TRIPLE (ID (enterid id), EMPTY, EMPTY)])
  | Triple (Str "318", Str logic, Str id) ->
    if Hashtbl.mem pkgtab (String.uppercase logic) then
      QUINTUPLE(OUTPUT, EMPTY, EMPTY, rngfun (Hashtbl.find pkgtab (String.uppercase logic)), TLIST [TRIPLE (ID (enterid id), EMPTY, EMPTY)])
        else
      QUINTUPLE(OUTPUT, EMPTY, EMPTY, EMPTY, TLIST [TRIPLE (ID (enterid id), EMPTY, EMPTY)])
  | Double(VhdPhysicalPrimary, Triple (VhdPhysicalInteger, Str num, Str "ns")) -> INTNUM num
  | Double(VhdAggregatePrimary, List lst1) -> ASCNUM "VhdAggregatePrimary"
  | Double(VhdNotFactor, dotted) -> DOUBLE(PLING, convert dotted)
  | Triple(VhdAndLogicalExpression, relation0, relation1) -> TRIPLE(AND, convert relation0, convert relation1)
  | Triple(VhdNandLogicalExpression, relation0, relation1) -> TRIPLE(convert VhdNandLogicalExpression, convert relation0, convert relation1)
  | Triple(VhdNorLogicalExpression, relation0, relation1) -> TRIPLE(convert VhdNorLogicalExpression, convert relation0, convert relation1)
  | Triple(VhdOrLogicalExpression, relation0, relation1) -> TRIPLE(OR, convert relation0, convert relation1)
  | Triple(VhdShiftLeftLogicalExpression, simple_expression0, simple_expression1) ->
    TRIPLE(convert VhdShiftLeftLogicalExpression, convert simple_expression0, convert simple_expression1)
  | Triple(VhdShiftRightLogicalExpression, simple_expression0, simple_expression1) ->
    TRIPLE(convert VhdShiftRightLogicalExpression, convert simple_expression0, convert simple_expression1)
  | Triple(VhdXnorLogicalExpression, relation0, relation1) -> TRIPLE(convert VhdXnorLogicalExpression, convert relation0, convert relation1)
  | Triple(VhdXorLogicalExpression, relation0, relation1) -> TRIPLE(convert VhdXorLogicalExpression, convert relation0, convert relation1)
  | Triple(VhdNameParametersPrimary, name0, parameters1) -> TRIPLE(convert VhdNameParametersPrimary, convert name0, convert parameters1)

  | Triple(Str signal, Triple (Str "", Str signal2, abc), def) ->
    QUADRUPLE (ID (enterid signal), ID (enterid signal2), convert abc, convert def)
  | Triple(Str id, olst, ilst) -> TRIPLE(ID (enterid id), convert olst, convert ilst)

  | Quinvigenuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> failwith "unexpected tuple size 25"
  | Quattuorvigenuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> failwith "unexpected tuple size 24"
  | Trevigenuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> failwith "unexpected tuple size 23"
  | Duovigenuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> failwith "unexpected tuple size 22"
  | Unvigenuple(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19,arg20,arg21) ->
    UNVIGENUPLE(convert arg1,
                convert arg2,
                convert arg3,
                convert arg4,
                convert arg5,
                convert arg6,
                convert arg7,
                convert arg8,
                convert arg9,
                convert arg10,
                convert arg11,
                convert arg12,
                convert arg13,
                convert arg14,
                convert arg15,
                convert arg16,
                convert arg17,
                convert arg18,
                convert arg19,
                convert arg20,
                convert arg21)
  | Vigenuple (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> failwith "unexpected tuple size 20"
  | Novemdecuple (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> failwith "unexpected tuple size 19"
  | Octodecuple (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> failwith "unexpected tuple size 18"
  | Septendecuple (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> failwith "unexpected tuple size 17"
  | Sexdecuple (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> failwith "unexpected tuple size 16"
  | Quindecuple(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15) -> QUINDECUPLE(convert arg1,
                                                                                                                 convert arg2,
                                                                                                                 convert arg3,
                                                                                                                 convert arg4,
                                                                                                                 convert arg5,
                                                                                                                 convert arg6,
                                                                                                                 convert arg7,
                                                                                                                 convert arg8,
                                                                                                                 convert arg9,
                                                                                                                 convert arg10,
                                                                                                                 convert arg11,
                                                                                                                 convert arg12,
                                                                                                                 convert arg13,
                                                                                                                 convert arg14,
                                                                                                                 convert arg15)
  | Quattuordecuple(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14) -> QUATTUORDECUPLE(convert arg1,
                                                                                                                   convert arg2,
                                                                                                                   convert arg3,
                                                                                                                   convert arg4,
                                                                                                                   convert arg5,
                                                                                                                   convert arg6,
                                                                                                                   convert arg7,
                                                                                                                   convert arg8,
                                                                                                                   convert arg9,
                                                                                                                   convert arg10,
                                                                                                                   convert arg11,
                                                                                                                   convert arg12,
                                                                                                                   convert arg13,
                                                                                                                   convert arg14)
  | Tredecuple(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13) -> TREDECUPLE(convert arg1,
                                                                                                   convert arg2,
                                                                                                   convert arg3,
                                                                                                   convert arg4,
                                                                                                   convert arg5,
                                                                                                   convert arg6,
                                                                                                   convert arg7,
                                                                                                   convert arg8,
                                                                                                   convert arg9,
                                                                                                   convert arg10,
                                                                                                   convert arg11,
                                                                                                   convert arg12,
                                                                                                   convert arg13)
  | Duodecuple(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12) -> DUODECUPLE(convert arg1,
                                                                                             convert arg2,
                                                                                             convert arg3,
                                                                                             convert arg4,
                                                                                             convert arg5,
                                                                                             convert arg6,
                                                                                             convert arg7,
                                                                                             convert arg8,
                                                                                             convert arg9,
                                                                                             convert arg10,
                                                                                             convert arg11,
                                                                                             convert arg12)
  | Undecuple(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11) -> UNDECUPLE(convert arg1,
                                                                                     convert arg2,
                                                                                     convert arg3,
                                                                                     convert arg4,
                                                                                     convert arg5,
                                                                                     convert arg6,
                                                                                     convert arg7,
                                                                                     convert arg8,
                                                                                     convert arg9,
                                                                                     convert arg10,
                                                                                     convert arg11)
  | Decuple(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10) -> DECUPLE(convert arg1,
                                                                           convert arg2,
                                                                           convert arg3,
                                                                           convert arg4,
                                                                           convert arg5,
                                                                           convert arg6,
                                                                           convert arg7,
                                                                           convert arg8,
                                                                           convert arg9,
                                                                           convert arg10)
  | Nonuple(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) -> NONUPLE(convert arg1,
                                                                     convert arg2,
                                                                     convert arg3,
                                                                     convert arg4,
                                                                     convert arg5,
                                                                     convert arg6,
                                                                     convert arg7,
                                                                     convert arg8,
                                                                     convert arg9)
  | Octuple(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) -> OCTUPLE(convert arg1,
                                                                convert arg2,
                                                                convert arg3,
                                                                convert arg4,
                                                                convert arg5,
                                                                convert arg6,
                                                                convert arg7,
                                                                convert arg8)
  | Septuple(arg1,arg2,arg3,arg4,arg5,arg6,arg7) -> SEPTUPLE(convert arg1,
                                                             convert arg2,
                                                             convert arg3,
                                                             convert arg4,
                                                             convert arg5,
                                                             convert arg6,
                                                             convert arg7)
  | Sextuple(arg1,arg2,arg3,arg4,arg5,arg6) -> SEXTUPLE(convert arg1,
                                                        convert arg2,
                                                        convert arg3,
                                                        convert arg4,
                                                        convert arg5,
                                                        convert arg6)
  | Quintuple(arg1,arg2,arg3,arg4,arg5) -> QUINTUPLE(convert arg1,
                                                     convert arg2,
                                                     convert arg3,
                                                     convert arg4,
                                                     convert arg5)
  | Quadruple(arg1,arg2,arg3,arg4) -> QUADRUPLE(convert arg1,
                                                convert arg2,
                                                convert arg3,
                                                convert arg4)
  | Triple(arg1,arg2,arg3) -> TRIPLE(convert arg1, convert arg2, convert arg3)
  | Double(arg1,arg2) -> let exp = convert arg2 in if exp <> EMPTY then DOUBLE(convert arg1, exp) else convert arg1
  | List [lst] -> convert lst
  | List lst -> TLIST (List.map convert lst)
  | Num n -> INT (Big_int.int_of_big_int n)
  | Char '0' -> BINNUM "1'b0"
  | Char '1' -> BINNUM "1'b1"
  | Str s -> ID (enterid s)
  | others -> ASCNUM (Asctoken.asctoken others)

let logf = function
  | Setup.Open x -> x
  | _ -> stderr

let dbody = ref (Double(VhdUnknown,VhdUnknown))

let find c nam = try Hashtbl.find Vabstraction.vhdlhash (c,nam) with e -> failwith ((Asctoken.asctoken c)^" "^nam^" missing")

let convert' nam = if (Hashtbl.mem Vabstraction.vhdlhash (VhdEntityDeclaration,nam)) &
    (Hashtbl.mem Vabstraction.vhdlhash (VhdArchitectureBody,nam)) then (
  let decl = find VhdEntityDeclaration nam and arch = find VhdArchitectureBody nam in
  let (arch,body) = (match convert (match_combined arch) with 
      | QUADRUPLE(COVER, ID arch, TLIST lst0, TLIST lst1) -> (arch,lst0@lst1)
      | QUADRUPLE(COVER, ID arch, TLIST lst0, stmt) -> (arch,lst0@[stmt])
      | err -> (enterid "unknown",[err])) in
  (match convert (match_combined decl) with
      | TRIPLE(IOPORT, iolst, TLIST iodecl) -> (arch,QUINTUPLE(MODULE, ID (enterid nam), EMPTY, iolst, TLIST (iodecl@body)))
      | err -> (enterid "unknown",err)))
   else (Printf.fprintf stderr "%s does not have an EntityDeclaration and ArchitectureBody\n" nam; (enterid "unknown",EMPTY))

let rec vectors = function
  | EMPTY -> ()
  | TLIST vec -> List.iter vectors vec
  | TRIPLE (VECTORED, ID bus, RANGE rng) ->
      Hashtbl.replace pkgtab (String.uppercase bus.Idhash.id) (RANGE rng)
  | TRIPLE (VECTORED, ID bus, ID kind) ->
      Hashtbl.replace pkgtab (String.uppercase bus.Idhash.id) (ID (enterid (String.uppercase kind.Idhash.id)))
  | QUADRUPLE (VECTORED, ID bus, ID kind, eval) ->
      Hashtbl.replace pkgtab (String.uppercase bus.Idhash.id) (DOUBLE(ID (enterid (String.uppercase kind.Idhash.id)), eval))
  | err -> errlst := err :: !errlst

let convertpkg pkg = (match convert (match_combined pkg) with
  | TLIST lst -> List.iter vectors lst
  | err -> pkglst := err :: !pkglst)

let _ = Hashtbl.replace pkgtab "STD_LOGIC" EMPTY
