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
open List
open Read_library

type decision_data = {
  wirehash: (token, Prop.prop Formulas.formula) Hashtbl.t;
  decisionhash: (token, Prop.prop Formulas.formula) Hashtbl.t;
  mutable regdecllst: token list;
  asgnhash: (token, token) Hashtbl.t;
  mutable tused: (idhash, token) Hashtbl.t;
}

let asgnadd data conn = function
  | ID id -> Hashtbl.replace data.asgnhash (ID id) conn
  | err -> ()

let rec vstrip = function
  | DOUBLE(arg1, arg2) -> DOUBLE(arg1, vstrip arg2)
  | TRIPLE(arg1, arg2, arg3) -> TRIPLE(arg1, vstrip arg2, vstrip arg3)
  | other -> other

let rec genverilog arglst = function
  | _ when List.length arglst = 0 -> BINNUM "1'b0"
  | NOT when List.length arglst = 1 -> DOUBLE(NOT, vstrip (List.hd arglst))
  | AND when List.length arglst = 1 -> vstrip (List.hd arglst)
  | AND -> List.fold_left (fun arg1 arg2 -> TRIPLE(AND, vstrip arg1, vstrip arg2)) (List.hd arglst) (List.tl arglst)
  | OR when List.length arglst = 1 -> vstrip (List.hd arglst)
  | OR -> List.fold_left (fun arg1 arg2 -> TRIPLE(OR, vstrip arg1, vstrip arg2)) (List.hd arglst) (List.tl arglst)
  | NAND -> DOUBLE(NOT, genverilog arglst AND)
  | NOR -> DOUBLE(NOT, genverilog arglst OR)
  | XOR when List.length arglst = 2 -> TRIPLE(XOR, vstrip (List.nth arglst 0), vstrip (List.nth arglst 1))
  | XOR -> List.fold_left (fun arg1 arg2 -> genverilog [arg1;arg2] XOR) (List.hd arglst) (List.tl arglst)
  | XNOR -> DOUBLE(NOT, genverilog arglst XOR)
  | BUF when List.length arglst = 1 -> vstrip (List.hd arglst)
  | ID str -> vstrip (ID str)
  | other -> vstrip other

let rec reduce = function
  | ID id -> Formulas.Atom (Prop.P id.id)
  | BINNUM "1'b0" -> Formulas.False
  | BINNUM "1'b1" -> Formulas.True
  | DOUBLE(LPAREN, exp) -> reduce exp
  | DOUBLE(NOT, exp) -> Formulas.Not(reduce exp)
  | TRIPLE(BITSEL, ID id, INT idx) -> Formulas.Atom (Prop.P (sprintf "%s[%d]" id.id idx))
  | TRIPLE((OR|P_OROR), arg1, arg2) -> Formulas.Or(reduce arg1, reduce arg2)
  | TRIPLE((AND|P_ANDAND), arg1, arg2) -> Formulas.And(reduce arg1, reduce arg2)
  | TRIPLE(XOR, arg1, arg2) -> Formulas.Not(Formulas.Iff(reduce arg1, reduce arg2))
  | QUADRUPLE(QUERY, arg1, arg2, arg3) -> Formulas.Or(Formulas.And(reduce arg1, reduce arg2),
                                                      Formulas.And(Formulas.Not(reduce arg1), reduce arg3))
  | other -> unhandled stderr 68 other; Formulas.Atom(Prop.P "")

let bdd_of_formula fm =
  Bdd.mkbdd (Bdd.mk_bdd (<),Lib.undefined) fm

let rec bddtran = function
  | Pnot arg -> Formulas.Not (bddtran arg)
  | Ptrue -> Formulas.True
  | Pfalse -> Formulas.False
  | Pvar(str) -> reduce str
  | Por(arg1,arg2) -> Formulas.Or(bddtran arg1,bddtran arg2)
  | Pand(arg1,arg2) -> Formulas.And(bddtran arg1,bddtran arg2)
  | Piff(arg1,arg2) -> Formulas.Iff(bddtran arg1,bddtran arg2)
  | Pimp(arg1,arg2) -> Formulas.Imp(bddtran arg1,bddtran arg2)
  | Punknown -> Formulas.Atom (Prop.P "")

let rec subst hash = function
  | Formulas.True -> Formulas.True 
  | Formulas.False -> Formulas.False
  | Formulas.Iff(arg1,arg2) -> Formulas.Iff(subst hash arg1, subst hash arg2)
  | Formulas.Imp(arg1,arg2) -> Formulas.Imp(subst hash arg1, subst hash arg2)
  | Formulas.Or(arg1,arg2) -> Formulas.Or(subst hash arg1, subst hash arg2)
  | Formulas.And(arg1,arg2) -> Formulas.And(subst hash arg1, subst hash arg2)
  | Formulas.Not(arg) -> Formulas.Not(subst hash arg)
  | Formulas.Atom(Prop.P id) -> hash id
  | Formulas.Exists _ -> failwith "Exists"
  | Formulas.Forall _ -> failwith "Forall"

let rec unmap data syms = function
| TLIST lst -> List.iter (fun x -> unmap data syms x) lst
| QUADRUPLE((WIRE|TRI0|TRI1), arg0, TRIPLE(arg1, rng, arg3), TLIST arg4) -> ()
| QUADRUPLE(MODINST, ID kind, EMPTY, TLIST arg3) -> if Hashtbl.mem libhash kind.id then (
  let prop = Hashtbl.find libhash kind.id in
  if (prop.prop = Punknown) then failwith (sprintf "Boolean function property for %s is unknown\n" kind.id);
  List.iter (function
    | TRIPLE (ID id, SCALAR, TLIST arg4) ->
      let pinhash = Hashtbl.create 256 in
      List.iter (fun itm -> let (cellpin,conn) = (function
        | TRIPLE (CELLPIN, ID cellpin, conn) -> (cellpin, conn)
        | err -> unhandled stderr 100 err; (enterid "",EMPTY)) itm in
                            Hashtbl.add pinhash cellpin conn;
      ) arg4;
      let mysubst = subst (function
        | str' -> let str = enterid str' in reduce (if Hashtbl.mem pinhash str then Hashtbl.find pinhash str else ID str)
        | other -> failwith "") (bddtran prop.prop) in
      List.iter (fun itm -> if Hashtbl.mem pinhash itm.idpin then
          Hashtbl.add data.wirehash (Hashtbl.find pinhash itm.idpin) mysubst) prop.opinlst
    | err -> unhandled stderr 69 err) arg3)
  else
    (printf "modinst %s not found\n" kind.id)
| RANGE(INT arg1,INT arg2) -> ()
| QUINTUPLE((INPUT|OUTPUT), arg1, arg2, rng, TLIST arg4) ->
  List.iter (function
    | TRIPLE (ID id, EMPTY, EMPTY) -> ()
    | other -> unhandled stderr 112 other) arg4
| TRIPLE(ASSIGN, EMPTY, TLIST lst) -> List.iter (function
    | TRIPLE (ASSIGNMENT, conn, exp) ->
      let rslt = Prop.cnf (reduce exp) in
      Hashtbl.replace data.decisionhash conn rslt
    | oth -> unhandled stderr 119 oth) lst
| EMPTY -> ()
| other -> unhandled stderr 121 other

let is_used data = function
  | ID id -> Hashtbl.mem data.tused id
  | other -> unhandled stderr 125 other; false

let rec used data lst = List.iter (function
  | BINNUM lev -> ()
  | ID id -> if not (Hashtbl.mem data.tused id) then Hashtbl.add data.tused id EMPTY
  | DOUBLE((LPAREN|NOT), arg) -> used data [arg]
  | TRIPLE(BITSEL, ID id, INT n) -> if Hashtbl.mem data.tused id then
      (match Hashtbl.find data.tused id with
        | EMPTY -> Hashtbl.replace data.tused id (RANGE(INT n, INT n))
        | RANGE(INT hi, INT lo) -> Hashtbl.replace data.tused id (RANGE(INT (max hi n), INT (min lo n)))
        | oth -> unhandled stderr 133 oth)
    else
      Hashtbl.add data.tused id (RANGE(INT n, INT n))
  | TRIPLE((AND|OR), arg1, arg2) -> used data [arg1;arg2]
  | other -> unhandled stderr 137 other) lst

let rec to_verilog data = function
  | Formulas.True -> BINNUM "1'b1"
  | Formulas.False -> BINNUM "1'b0"
  | Formulas.Or(arg1,arg2) -> TRIPLE(OR,to_verilog data arg1,to_verilog data arg2)
  | Formulas.And(arg1,arg2) -> TRIPLE(AND,to_verilog data arg1,to_verilog data arg2)
  | Formulas.Iff(arg1,arg2) -> TRIPLE(XNOR,to_verilog data arg1,to_verilog data arg2)
  | Formulas.Not(arg) -> DOUBLE(NOT,to_verilog data arg)
  | Formulas.Atom(Prop.P id) -> Read_blif.blifGetName id
  | Formulas.Exists _ -> failwith "Exists"
  | Formulas.Forall _ -> failwith "Forall"
  | Formulas.Imp _ -> failwith "Implies"

let to_formula truth = 
        genfunc (List.map (fun lst ->
          genfunc (List.map (fun (decisionvar, logic) -> if logic then Pvar decisionvar else Pnot(Pvar decisionvar)) lst) AND
        ) truth) OR

let data_nul () = {wirehash=Hashtbl.create 256;
                   decisionhash=Hashtbl.create 256;
                   regdecllst=[];
                   asgnhash=Hashtbl.create 256;
                   tused=Hashtbl.create 256}

let dumpdata = ref (data_nul())
let dumpasgn = ref []
let unused = ref []

let signal' data signal = if Hashtbl.mem data.decisionhash signal then
    let truth = Hashtbl.find data.decisionhash signal in
    to_verilog data truth
  else signal

let subst' data signal = if Hashtbl.mem data.asgnhash signal then
    let rslt = Hashtbl.find data.asgnhash signal in
    Hashtbl.remove data.asgnhash signal;
    if Hashtbl.mem data.decisionhash rslt then
       Hashtbl.remove data.decisionhash rslt;
    (match signal with
      | ID id -> if Hashtbl.mem data.tused id then
          Hashtbl.remove data.tused id
      | oth -> unhandled stderr 163 oth);
    rslt
  else signal

let rec map' data syms = function
| TLIST lst -> (List.flatten (List.map (fun itm -> map' data syms itm) lst))
| QUADRUPLE((WIRE|TRI0|TRI1), arg0, TRIPLE(arg1, rng, arg3), TLIST arg4) -> []
| QUINTUPLE(OUTPUT, arg1, arg2, rng, TLIST arg4) -> List.iter (function
    | TRIPLE (ID id, EMPTY, EMPTY) -> used data [ID id]; (match rng with
        | RANGE(INT hi,INT lo) -> for i = lo to hi do
        let conn = TRIPLE(BITSEL, ID id, INT i) in
        if Hashtbl.mem data.decisionhash conn then
          asgnadd data conn (to_verilog data (Hashtbl.find data.decisionhash conn))
          done
        | EMPTY ->  if Hashtbl.mem data.decisionhash (ID id) then
            asgnadd data (ID id) (to_verilog data (Hashtbl.find data.decisionhash (ID id)))
        | other -> unhandled stderr 151 other) 
    | other -> unhandled stderr 145 other) arg4;
  [QUINTUPLE(OUTPUT, arg1, arg2, rng, TLIST arg4)]
| QUADRUPLE(MODINST, ID kind, EMPTY, TLIST arg3) -> if Hashtbl.mem libhash kind.id then (
  let prop = Hashtbl.find libhash kind.id and pinhash = Hashtbl.create 256 in
  List.flatten (List.map (fun inst -> (match inst with
    | TRIPLE (ID id, SCALAR, TLIST arg4) -> List.iter (fun itm ->
      let (cellpin,conn) = (function
        | TRIPLE (CELLPIN, ID cellpin, conn) -> (cellpin,conn)
        | err -> unhandled stderr 273 err; (enterid "",EMPTY)) itm in
      if Read_library.is_member cellpin prop.ipinlst then (
        if Hashtbl.mem data.decisionhash conn then 
          let truth = Hashtbl.find data.decisionhash conn in
          Hashtbl.add pinhash cellpin (to_verilog data truth)
        else Hashtbl.add pinhash cellpin conn)
      else if Read_library.is_member cellpin prop.opinlst then Hashtbl.add pinhash cellpin conn
      else Hashtbl.add pinhash cellpin EMPTY;
    ) arg4;
    (try match prop.seq with
      | POSEDGE -> let q = Hashtbl.find pinhash prop.qout in data.regdecllst <- q :: data.regdecllst;
                   let clk = Hashtbl.find pinhash prop.clk in
                   let dat = Hashtbl.find pinhash prop.dat in
                   used data [q;clk;dat];
                   [QUADRUPLE(MODINST, ID prop.nam, EMPTY,
                              TLIST
                                [TRIPLE
                                    (Minimap.instid [clk;dat;q] syms prop, SCALAR,
                                     TLIST
                                       [TRIPLE (CELLPIN, ID (prop.clk), signal' data clk);
                                        TRIPLE (CELLPIN, ID (prop.dat), signal' data dat);
                                        TRIPLE (CELLPIN, ID (prop.qout), subst' data q)])])]
        
      | DOUBLE(POSEDGE,IF) -> let q = Hashtbl.find pinhash prop.qout in data.regdecllst <- q :: data.regdecllst;
                              let clk = Hashtbl.find pinhash prop.clk in
                              let dat = Hashtbl.find pinhash prop.dat in
                              let en = Hashtbl.find pinhash prop.en in
                              let clr = Hashtbl.find pinhash prop.clr in
                              used data [q;clk;dat;en;clr];
    [QUADRUPLE
        (MODINST, ID prop.nam, EMPTY,
         TLIST
           [TRIPLE
               (Minimap.instid [en;clk;clr;dat;q] syms prop, SCALAR,
                TLIST
                  [TRIPLE (CELLPIN, ID (prop.qout), subst' data q);
                   TRIPLE (CELLPIN, ID (prop.dat), signal' data dat);
                   TRIPLE (CELLPIN, ID (prop.clk), signal' data clk);
                   TRIPLE (CELLPIN, ID (prop.clr), signal' data clr);
                   TRIPLE (CELLPIN, ID (prop.en), signal' data en)])])]
      | EMPTY -> []
      | err -> unhandled stderr 224 err; [] with err -> [])
    | _ -> [])) arg3)) else []
| TRIPLE(ASSIGN, _, TLIST _) -> []
| QUINTUPLE(INPUT, arg1, arg2, rng, TLIST arg4) as io -> [io]
| other -> unhandled stderr 227 other; []

let rec simple = function
  | Formulas.True -> 1
  | Formulas.False -> 1
  | Formulas.Atom(Prop.P id) -> 2
  | Formulas.Iff(arg1,arg2) -> 2+simple arg1+simple arg2
  | Formulas.Imp(arg1,arg2) -> 2+simple arg1+simple arg2
  | Formulas.Or(arg1,arg2) -> 2+simple arg1+simple arg2
  | Formulas.And(arg1,arg2) -> 2+simple arg1+simple arg2
  | Formulas.Not(arg) -> 1+simple arg
  | Formulas.Exists _ -> failwith "Exists"
  | Formulas.Forall _ -> failwith "Forall"

let generate_cnf_netlist k =
  let cnt = ref 0 in
  let changed = ref true in
  let data = data_nul() in
  (function
    | QUINTUPLE(MODULE, ID arg1, TLIST arg2, TLIST arg3, THASH thash) ->
      Hashtbl.iter (fun x _ -> unmap data k.symbols x) (fst thash);
      Hashtbl.iter (fun x _ -> unmap data k.symbols x) (snd thash)
    | _ -> failwith "CNF") k.tree;
  while !changed do changed := false;
    Hashtbl.iter (fun k itm ->
      let mysubst = subst (fun str' -> let str = ID (enterid str') in if (Hashtbl.mem data.wirehash str)
        then
          (let repl = Hashtbl.find data.wirehash str in
           if simple itm < 2000 then (incr cnt; changed := true; repl) else reduce str)
        else reduce str) itm in
      Hashtbl.replace data.wirehash k mysubst
    ) data.wirehash done;
  printf "Total of %d substitutions made\n" !cnt;
  Hashtbl.iter (fun k itm ->
    let rslt = Prop.cnf itm in
    Hashtbl.replace data.decisionhash k rslt
  ) data.wirehash;
  (function
    | QUINTUPLE(MODULE, ID arg1, TLIST arg2, TLIST arg3, THASH thash) ->
      let inst = ref [] in
      Hashtbl.iter (fun x _ -> inst := map' data k.symbols x @ !inst) (fst thash);
      Hashtbl.iter (fun x _ -> inst := map' data k.symbols x @ !inst) (snd thash);
        List.iter (function
          | QUADRUPLE(MODINST, ID kind, EMPTY, TLIST arg3) ->
            List.iter (function
              | TRIPLE (ID id, SCALAR, TLIST arg4) ->
                List.iter (function
                  | TRIPLE (CELLPIN, ID cellpin, conn) -> used data [conn]
                  | err -> unhandled stderr 323 err) arg4
              | oth -> unhandled stderr 324 oth) arg3
          | _ -> ()) !inst;

      let asgnlst = ref [] and card = ref 0 in
      while !card <> Hashtbl.length data.tused do card := Hashtbl.length data.tused;
        Hashtbl.iter (fun conn itm ->
          let exp = to_verilog data itm in
          if conn <> exp then let con = TRIPLE(ASSIGN, EMPTY, TLIST [TRIPLE (ASSIGNMENT, conn, exp)]) in (match conn with
            | ID id -> if is_used data conn then
                ( if not (List.mem con !asgnlst) then asgnlst := con :: !asgnlst; used data [conn;exp])
              else unused := id :: !unused
            | TRIPLE(BITSEL, ID id, INT idx) -> if is_used data (ID id) then
                ( if not (List.mem con !asgnlst) then asgnlst := con :: !asgnlst; used data [conn;exp])
              else unused := id :: !unused
            | err -> unhandled stderr 325 err)
        ) data.decisionhash done;

        Hashtbl.iter (fun id rng -> match rng with
        | EMPTY | RANGE _ ->
          inst := QUADRUPLE(WIRE,
                            EMPTY,
                            TRIPLE(EMPTY, rng, EMPTY),
                            TLIST [DOUBLE(ID id,EMPTY)]) :: !inst
        | err -> unhandled stderr 344 err) data.tused;

      dumpdata := data;
      dumpasgn := !asgnlst;

      let arg5 = Semantics.genthash (!inst @ !asgnlst) in

      QUINTUPLE(MODULE, ID arg1, TLIST arg2,
                TLIST (List.map (function
                  | ID id -> ID id
                  | x -> unhandled stderr 279 x; EMPTY) arg3),
                THASH arg5)
    | _ -> failwith "not a module") k.tree

let tree = ref EMPTY

let gen_cnf_arch arch nam =
  let lst = Hashtbl.find_all modprims nam in
  List.iter (fun arg -> if arg.arch = arch then
      begin
      tree := generate_cnf_netlist arg;
      Semantics.prescan stderr !Globals.archenv !tree "Generated by generate_cnf_arch"
      end) lst;;
