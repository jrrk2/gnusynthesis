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
open Globals
open Printf
open Idhash
open Vparser
open Dump
open Read_library

type thash = (token, unit) Hashtbl.t
type lgen = token -> token -> token list

let verbose = ref false

let pathid' lev id = lev^id

let pathid lev id = ID (enterid (pathid' lev id.id))

let find_dir_buf sym syms' first second = match syms' with
  | Vparser.EndShash -> []
  | Vparser.Shash {Vparser.nxt;syms} ->
    let rslt = ref None in Vparser.TokSet.iter (fun e -> match e with
      | OUTPUT -> rslt := Some (Minimap.insert_buf syms' second first)
      | INPUT -> rslt := Some (Minimap.insert_buf syms' first second)
      | INOUT -> rslt := Some (Minimap.insert_buf syms' first second)
      | _ -> ()) (Hashtbl.find syms sym).Vparser.symattr;
    match !rslt with Some x -> x | None -> []

let find_dir_inv sym syms' first second = match syms' with
  | Vparser.EndShash -> []
  | Vparser.Shash {Vparser.nxt;syms} ->
    let rslt = ref None in Vparser.TokSet.iter (fun e -> match e with
      | OUTPUT -> rslt := Some (Minimap.insert_inv syms' second first)
      | INPUT -> rslt := Some (Minimap.insert_inv syms' first second)
      | INOUT -> rslt := Some (Minimap.insert_inv syms' first second)
      | _ -> ()) (Hashtbl.find syms sym).Vparser.symattr;
    match !rslt with Some x -> x | None -> []

let buffer_bitsel_general (fn:lgen) (hash:thash) isyms dest syms bit idx1 path1 path2 =
  List.iter (fun arg -> Hashtbl.add hash arg ()) (fn (path1 dest) (TRIPLE(BITSEL, (path2 bit), INT idx1)))

let buffer_bus_general (fn:lgen) (hash:thash) isyms dest syms conn path1 path2 =
  if !verbose then printf "buffer_bus_general %s\n" dest.id;
  let (lft,rght,inc) = Minimap.find_width dest isyms in
  if lft <> rght then (
    let (lft2,rght2,inc2) = Minimap.find_width conn syms in
    let wid1' = (max lft rght) - (min lft rght)
    and wid2' = (max lft2 rght2) - (min lft2 rght2) in
    let idx = ref (if wid1' < wid2' then lft else rght2 - inc2*wid2')
    and idx2 = ref (if wid2' < wid1' then lft2 else rght - inc*wid1') in
    if (wid1' < wid2') && !verbose then
      printf "Bus truncation - ignoring one or more bits %d:%d in %s\n" lft2 (!idx2-inc2) conn.id;
    if (wid2' < wid1') && !verbose then
      printf "Bus truncation - ignoring one or more bits %d:%d in %s\n" lft (!idx-inc) dest.id;
    while !idx <> rght + inc do
      let buf = fn
        (TRIPLE(BITSEL, (path1 dest), INT !idx))
        (TRIPLE(BITSEL, (path2 conn), INT !idx2)) in List.iter (fun arg -> Hashtbl.add hash arg ()) buf;
      idx := !idx + inc;
      idx2 := !idx2 + inc2
    done)
  else
    (let (lft2,rght2,inc2) = Minimap.find_width conn syms in
     if lft2 <> rght2 then
       List.iter (fun arg -> Hashtbl.add hash arg ()) (fn (path1 dest) (TRIPLE(BITSEL, (path2 conn), INT rght2)))
     else
       List.iter (fun arg -> Hashtbl.add hash arg ()) (fn (path1 dest) (path2 conn)))

let buffer_num_general (fn:lgen) (hash:thash) isyms dest syms str path1 path2 =
  let (lft,rght,inc) = Minimap.find_width dest isyms in
  let wid = (max lft rght) - (min lft rght) + 1
  and len = String.length str in
  if !verbose then printf "buffer_num_general %s <= %s\n" dest.id str;
  let vec = if len < wid then
      String.make (wid-len) '0'^str
    else if len > wid then
      (printf "Constant %s does not fit in vector %s (w=%d)\n" str dest.id wid;
      String.sub str (len-wid) wid)
    else str in
  if lft <> rght then (
    let idx = ref lft and idx2 = ref 0 in
    while !idx <> rght + inc do
      let sel = "1'b"^String.sub vec !idx2 1 in

      if !verbose then printf "%s %d <= %s\n" dest.id !idx sel;

      let buf = fn
        (TRIPLE(BITSEL, (path1 dest), INT !idx))
        (BINNUM sel) in List.iter (fun arg -> Hashtbl.add hash arg ()) buf;
      idx := !idx + inc;
      incr idx2
    done)
  else
    List.iter (fun arg -> Hashtbl.add hash arg ()) (fn (path1 dest) (BINNUM ("1'b"^String.sub str (String.length str-1) 1)))

let rec buffer_concat_general' fn hash isyms dest syms path1 path2 idx rght inc = function
    | ID whole ->
      let (clft,crght,cinc) = Minimap.find_width whole syms in
      if !verbose then printf "%s[%d:%d:%d]\n" whole.id clft crght cinc;
      let cidx = ref clft in if clft <> crght then
          while !idx <> rght + inc && !cidx <> crght + cinc do
            let buf = fn
              (TRIPLE(BITSEL, (path1 dest), INT !idx))
              (TRIPLE(BITSEL, (path2 whole), INT !cidx)) in List.iter (fun arg -> Hashtbl.add hash arg ()) buf;
            idx := !idx + inc;
            cidx := !cidx + cinc
          done
        else
          begin
            let buf = fn
              (TRIPLE(BITSEL, (path1 dest), INT !idx))
              (path2 whole) in List.iter (fun arg -> Hashtbl.add hash arg ()) buf;
            idx := !idx + inc;
          end         
    | TRIPLE (BITSEL, ID bit, INT idx1) ->
      let buf = fn
        (TRIPLE(BITSEL, (path1 dest), INT !idx))
        (TRIPLE(BITSEL, (path2 bit), INT idx1)) in List.iter (fun arg -> Hashtbl.add hash arg ()) buf;
      idx := !idx + inc
    | QUADRUPLE (PARTSEL, ID part, INT clft, INT crght) ->
      let cinc = if clft < crght then 1 else -1 in
      if !verbose then printf "%s[%d:%d:%d]\n" part.id clft crght cinc;
      let cidx = ref clft in if clft <> crght then
          while !idx <> rght + inc && !cidx <> crght + cinc do
            let buf = fn
              (TRIPLE(BITSEL, (path1 dest), INT !idx))
              (TRIPLE(BITSEL, (path2 part), INT !cidx)) in List.iter (fun arg -> Hashtbl.add hash arg ()) buf;
            idx := !idx + inc;
            cidx := !cidx + cinc
          done
        else
          begin
            let buf = fn
              (TRIPLE(BITSEL, (path1 dest), INT !idx))
              (path2 part) in List.iter (fun arg -> Hashtbl.add hash arg ()) buf;
            idx := !idx + inc;
          end         
    | WIDTHNUM(radix,wid,num) ->
      let str = Minimap.bin num wid in
      let len = String.length str in
      let vec = String.sub str (len-wid) wid in
      for i = 0 to wid-1 do
        let buf = fn
          (TRIPLE(BITSEL, (path1 dest), INT !idx))
          (BINNUM ("1'b"^String.sub vec i 1)) in List.iter (fun arg -> Hashtbl.add hash arg ()) buf;
	idx := !idx + inc
      done
    | DOUBLE (CONCAT, TLIST innerlst) ->
        List.iter (buffer_concat_general' fn hash isyms dest syms path1 path2 idx rght inc) innerlst
    | z -> unhandled stderr 184 z

and buffer_concat_general fn hash isyms dest syms clst path1 path2 =
  let (lft,rght,inc) = Minimap.find_width dest isyms in
  let idx = ref lft in List.iter (buffer_concat_general' fn hash isyms dest syms path1 path2 idx rght inc) clst

and buffer_bus_partsel fn hash isyms dest syms (conn,lft2,rght2) path1 path2 =
  let (lft,rght,inc) = Minimap.find_width dest isyms in
  if !verbose then printf "buffer_bus_partsel (%d,%d,%d) %s (%s[%d:%d])\n"
    lft rght inc
    dest.id
    conn.id lft rght;
    let inc2 = if lft2 < rght2 then 1 else -1 in
    let idx = ref rght
    and idx2 = ref rght2 in
    while !idx2 <> lft2 - inc2 do
      let buf = fn
        (TRIPLE(BITSEL, (path1 dest), INT !idx))
        (TRIPLE(BITSEL, (path2 conn), INT !idx2)) in List.iter (fun arg -> Hashtbl.add hash arg ()) buf;
      idx := !idx - inc;
      idx2 := !idx2 - inc2
    done

let buffer_dest_general (fn:lgen) (hash:thash) isyms dest syms path1 path2 = function
  | ID conn ->
    if !verbose then printf "buffer_dest_general %s\n" conn.id;
    buffer_bus_general fn hash isyms dest syms conn path1 path2
  | (INT _ | HEXNUM _ | DECNUM _ | OCTNUM _ | BINNUM _ | WIDTHNUM _) as const ->
      let (sz,num) = Minimap.basenum const in buffer_num_general fn hash isyms dest syms (Minimap.bin num sz) path1 path2
  | TRIPLE (BITSEL, ID bit, INT idx1) -> buffer_bitsel_general fn hash isyms dest syms bit idx1 path1 path2
  | DOUBLE (CONCAT, TLIST clst) ->
    if !verbose then printf "buffer_concat_general %s %s\n" dest.id (Count.tokenstr (TLIST clst));
    buffer_concat_general fn hash isyms dest syms clst path1 path2
  | DOUBLE (NOT, ID conn) ->
    if !verbose then printf "buffer_dest_general ~%s\n" conn.id;
    buffer_bus_general fn hash isyms dest syms conn path1 path2
  | QUADRUPLE(PARTSEL, ID id, hi, lo) ->
      buffer_bus_partsel fn hash isyms dest syms (id,snd(Minimap.basenum hi),snd(Minimap.basenum lo)) path1 path2
  | oth -> unhandled stderr 276 oth

let buffer_cellpin hash isyms syms path1 path2 = function
  | TRIPLE (CELLPIN, ID cellpin, (DOUBLE (NOT, ID conn) as arg)) ->
    if !verbose then printf "buffer_cellpin %s %s\n" cellpin.id (Count.tokenstr arg);
    buffer_dest_general (find_dir_inv cellpin isyms) hash isyms cellpin syms path1 path2 arg
  | TRIPLE (CELLPIN, ID cellpin, arg) ->
    if !verbose then printf "buffer_cellpin %s %s\n" cellpin.id (Count.tokenstr arg);
    buffer_dest_general (find_dir_buf cellpin isyms) hash isyms cellpin syms path1 path2 arg
  | DOUBLE (CELLPIN, ID cellpin) -> ()
  | oth -> unhandled stderr 220 oth

let buffer_signal hash syms path = function
    | TRIPLE (ASSIGNMENT, ID id, exp) ->
      if !verbose then printf "buffer_signal %s %s\n" id.id (Count.tokenstr exp);
      buffer_dest_general (Minimap.insert_buf syms) hash syms id syms path path exp
    | TRIPLE (ASSIGNMENT, TRIPLE (BITSEL, ID dest, INT destidx), TRIPLE (BITSEL, ID src, INT srcidx)) ->
      let buf = Minimap.insert_buf syms (TRIPLE(BITSEL, (path dest), INT destidx)) (TRIPLE(BITSEL, (path src), INT srcidx)) in
      List.iter (fun arg -> Hashtbl.add hash arg ()) buf
    | TRIPLE (ASSIGNMENT, TRIPLE (BITSEL, ID dest, INT destidx), ID src) ->
      let buf = Minimap.insert_buf syms (TRIPLE(BITSEL, (path dest), INT destidx)) (path src) in
      List.iter (fun arg -> Hashtbl.add hash arg ()) buf
    | TRIPLE (ASSIGNMENT, TRIPLE (BITSEL, ID dest, INT destidx), WIDTHNUM (base, sz, num)) ->
      let buf = Minimap.insert_buf syms (TRIPLE(BITSEL, (path dest), INT destidx)) (BINNUM (if num mod 2 = 1 then "1'b1" else "1'b0")) in
      List.iter (fun arg -> Hashtbl.add hash arg ()) buf
    | TRIPLE (ASSIGNMENT, DOUBLE (CONCAT, TLIST lst'), ID src) ->
	let (lft,rght,inc) = Minimap.find_width src syms in
	let lst = Minimap.concat_flatten' syms lst' in
	let nxt = ref (rght - (List.length lst)*inc) in
	List.iter (function
	  | ID id ->
	      nxt := !nxt + inc;
	      assert (Minimap.find_width id syms=(0,0,0));
	      let buf = Minimap.insert_buf syms (path id) (TRIPLE(BITSEL, (path src), INT !nxt)) in
	      List.iter (fun arg -> Hashtbl.add hash arg ()) buf
	  | TRIPLE(BITSEL, ID dest, INT destidx) ->
	      nxt := !nxt + inc;
	      let buf = Minimap.insert_buf syms (TRIPLE(BITSEL, (path dest), INT destidx)) (TRIPLE(BITSEL, (path src), INT !nxt)) in
	      List.iter (fun arg -> Hashtbl.add hash arg ()) buf
	  | oth -> unhandled stderr 249 oth) lst
    | TRIPLE (ASSIGNMENT, DOUBLE (CONCAT, TLIST lst), QUADRUPLE(PARTSEL, ID src, INT lft, INT rght)) ->
	let nxt = ref (rght + (List.length lst)) in
	List.iter (function
	  | ID id ->
	      decr nxt;
	      assert (Minimap.find_width id syms=(0,0,0));
	      let buf = Minimap.insert_buf syms (path id) (TRIPLE(BITSEL, (path src), INT !nxt)) in
	      List.iter (fun arg -> Hashtbl.add hash arg ()) buf
	  | TRIPLE(BITSEL, ID dest, INT destidx) ->
	      decr nxt;
	      let buf = Minimap.insert_buf syms (TRIPLE(BITSEL, (path dest), INT destidx)) (TRIPLE(BITSEL, (path src), INT !nxt)) in
	      List.iter (fun arg -> Hashtbl.add hash arg ()) buf
	  | oth -> unhandled stderr 257 oth) lst
    | TRIPLE (ASSIGNMENT, DOUBLE (CONCAT, TLIST lst), TRIPLE(BITSEL, ID src, INT sel)) ->
	List.iter (function
	  | ID id ->
	      assert (Minimap.find_width id syms=(0,0,0));
	      let buf = Minimap.insert_buf syms (path id) (TRIPLE(BITSEL, (path src), INT sel)) in
	      List.iter (fun arg -> Hashtbl.add hash arg ()) buf
	  | TRIPLE(BITSEL, ID dest, INT destidx) ->
	      let buf = Minimap.insert_buf syms (TRIPLE(BITSEL, (path dest), INT destidx)) (TRIPLE(BITSEL, (path src), INT sel)) in
	      List.iter (fun arg -> Hashtbl.add hash arg ()) buf
	  | oth -> unhandled stderr 263 oth) lst
    | TRIPLE (ASSIGNMENT, DOUBLE (CONCAT, TLIST lst), (INT num|WIDTHNUM(_,_,num))) ->
	let numlst = Minimap.num_flatten (List.length lst) num [] in
	assert ((List.length lst) = (List.length numlst));
	let nxt = ref 0 in
	List.iter (function
	  | ID id ->
	      assert (Minimap.find_width id syms=(0,0,0));
	      let buf = Minimap.insert_buf syms (path id) (List.nth numlst !nxt) in
	      incr nxt;
	      List.iter (fun arg -> Hashtbl.add hash arg ()) buf
	  | TRIPLE(BITSEL, ID dest, INT destidx) ->
	      let buf = Minimap.insert_buf syms (TRIPLE(BITSEL, (path dest), INT destidx)) (List.nth numlst !nxt) in
	      incr nxt;
	      List.iter (fun arg -> Hashtbl.add hash arg ()) buf
	  | oth -> unhandled stderr 263 oth) lst
    | x -> unhandled stderr 258 x

let const_shift num sel = BINNUM (if (num land (1 lsl sel)) > 0 then "1'b1" else "1'b0")

let flt_subcell syms thash2 lev kind params arg3 =
  begin Hashtbl.add (snd thash2)
          (QUADRUPLE(MODINST, ID kind, params, TLIST (List.map (fun x -> (match x with
            | TRIPLE (ID id, SCALAR, TLIST arg4) ->
              TRIPLE (pathid ("flt$"^kind.id^"$"^lev) id, SCALAR, 
                      TLIST (List.map (fun y -> (match y with
                        | TRIPLE (CELLPIN, ID cellpin, ((WIDTHNUM _ | INT _ | DECNUM _ | HEXNUM _ | BINNUM _) as const)) ->
			  let (sz,num) = Minimap.basenum const in TRIPLE (CELLPIN, ID cellpin, WIDTHNUM(2, sz, num))
                        | TRIPLE (CELLPIN, ID cellpin, ID conn) ->
                            let (hi,lo,inc) = Minimap.find_width conn syms in
                            if hi <> lo then
                              TRIPLE (CELLPIN, ID cellpin, QUADRUPLE(PARTSEL, pathid lev conn, INT hi, INT lo))
                            else if inc <> 0 then
                              TRIPLE (CELLPIN, ID cellpin, TRIPLE(BITSEL, pathid lev conn, INT hi))
                            else TRIPLE (CELLPIN, ID cellpin, pathid lev conn)
                        | TRIPLE (CELLPIN, ID cellpin, TRIPLE (BITSEL, ID bit, INT idx1)) ->
                          TRIPLE (CELLPIN, ID cellpin, TRIPLE (BITSEL, pathid lev bit, INT idx1))
                        | TRIPLE (CELLPIN, ID cellpin, TRIPLE (BITSEL, ID bit, WIDTHNUM(radix, wid, num))) ->
                          TRIPLE (CELLPIN, ID cellpin, TRIPLE (BITSEL, pathid lev bit, INT num))
                        | TRIPLE (CELLPIN, ID cellpin, QUADRUPLE(PARTSEL, ID conn, lft, rght)) ->
                          let (selhi,sello,dir) = Const.iwidth stderr syms (RANGE(lft,rght)) in
                          TRIPLE (CELLPIN, ID cellpin, QUADRUPLE(PARTSEL, pathid lev conn, INT selhi, INT sello))
                        | TRIPLE (CELLPIN, ID cellpin, DOUBLE(CONCAT, TLIST clst)) ->
                          TRIPLE (CELLPIN, ID cellpin,
                                  DOUBLE(CONCAT,
	                                 TLIST (List.map (fun z -> (match z with
	                                   | TRIPLE (BITSEL, ID bit, INT idx1) -> TRIPLE (BITSEL, pathid lev bit, INT idx1)
                                           | QUADRUPLE(PARTSEL, ID bits, INT hi, INT lo) -> QUADRUPLE(PARTSEL, pathid lev bits, INT hi, INT lo)
	                                   | ID id -> pathid lev id
	                                   | (WIDTHNUM _ | INT _ | DECNUM _ | HEXNUM _ | BINNUM _) as const ->
					     let (sz,num) = Minimap.basenum const in WIDTHNUM(2, sz, num)
	                                   | _ -> unhandled stderr 143 z; EMPTY)) clst)))
                        | DOUBLE(CELLPIN, ID cellpin) -> DOUBLE(CELLPIN, ID cellpin) (* unconnected pin *)
                        | TRIPLE (CELLPIN, ID cellpin, DOUBLE (NOT, ID conn)) ->
                          let inv = Minimap.insert_inv syms (pathid lev cellpin) (pathid lev conn) in
                          List.iter (fun arg -> Hashtbl.add (snd thash2) arg ()) inv;
                          TRIPLE (CELLPIN, ID cellpin, pathid lev cellpin)
                        | ID net -> fprintf stderr "Modinst %s of kind %s: connect by name (%s) not supported in this context\n"
                          id.id kind.id net.id;
                          UNKNOWN
                        | TRIPLE (CELLPIN, ID cellpin, TRIPLE(BITSEL, INT num, INT sel)) ->
                            TRIPLE (CELLPIN, ID cellpin, const_shift num sel)
                        | TRIPLE (CELLPIN, ID cellpin, EMPTY) ->
			  DOUBLE (CELLPIN, ID cellpin)
                        | _ -> unhandled stderr 274 y; EMPTY)) arg4))
            | _ -> unhandled stderr 275 x; EMPTY)) arg3))) ();
      end

let rec flatten_sub arch syms thash2 lev kind params arg3 nam top =
  if top.is_netlist then
    begin
      if !verbose then printf "Flatten netlist sub-cell arch %s name %s kind %s\n" arch nam kind.id;
      let isyms = top.symbols in
      List.iter (function
        | TRIPLE (ID id, SCALAR, TLIST arg4) ->
            let path = if lev <> "" then pathid' (lev^"_") (id.id^"_") else pathid' "" (id.id^"_") in
            (match top.tree with
              | QUINTUPLE(MODULE, ID id, EMPTY, TLIST ports, THASH(decls,insts)) ->
                  flt' arch thash2 isyms decls insts path
              | err -> unhandled stderr 365 err);
            List.iter (fun itm -> buffer_cellpin (snd thash2) isyms syms (pathid path) (pathid lev) itm) arg4
        | oth -> unhandled stderr 367 oth) arg3;
      if !verbose then printf "End netlist sub-cell arch %s name %s\n" arch nam;
    end
  else
    flt_subcell syms thash2 lev kind params arg3

and flt arch osymtab thash2 syms lev = function
| (TRIPLE(ASSIGNMENT, arg1, arg2)) as exp ->
  if !verbose then printf "flt %s %s %s\n" lev (Count.tokenstr arg1) (Count.tokenstr arg2);
  buffer_signal (snd thash2) syms (pathid lev) exp
| (TRIPLE(ASSIGN, arg1, TLIST arg2)) as exp ->
    (match arg1 with 
      | DOUBLE (HASH, TLIST [INT n]) -> fprintf stderr "/* #%d Delay ignored */\n" n
      | EMPTY -> ()
      | oth -> unhandled stderr 379 oth);
    let ilst = ref [] and oinsts = Hashtbl.create 256 in
    Minimap.minimap_body''' (pathid lev) syms osymtab ilst oinsts exp;
    Minimap.minimap_body'' (pathid lev) (snd thash2) syms osymtab ilst oinsts;
    let extlst = ref [] in
    Hashtbl.iter (fun k (kind,extra) -> if extra then extlst := DOUBLE(ID (enterid k), EMPTY) :: !extlst) osymtab;
    Hashtbl.add (fst thash2) (QUADRUPLE(WIRE, EMPTY, TRIPLE(EMPTY, EMPTY, EMPTY), TLIST !extlst)) ()
| QUADRUPLE((WIRE|TRI0|TRI1) as tok, arg0, TRIPLE(arg1, srng, arg3), TLIST arg4) ->
  let intrng = Minimap.flatten_width srng syms in
  Hashtbl.replace (fst thash2) (QUADRUPLE(tok, arg0, TRIPLE(arg1, intrng, arg3),
            TLIST (List.map (fun x -> (match x with
              | DOUBLE (ID id, EMPTY) -> DOUBLE (pathid lev id, EMPTY)
              | _ -> unhandled stderr 151 x; EMPTY)) arg4))) ()
| QUADRUPLE(MODINST, ID kind, params, TLIST arg3) ->
  if Hashtbl.mem modprims kind.id then
    begin
      Minimap.select_sub false (flatten_sub arch syms thash2 lev kind params arg3) arch kind.id;
    end
  else if Hashtbl.mem libhash kind.id then
    begin
      flt_subcell syms thash2 lev kind params arg3
    end
  else
    failwith ("sub-module "^kind.id^" not found")
| QUINTUPLE((INPUT|OUTPUT|INOUT) as tok, arg1, arg2, srng, TLIST arg4) ->
  let intrng = Minimap.flatten_width srng syms in
  let lst = TLIST (List.map (fun x -> (match x with
    | TRIPLE (ID id, EMPTY, EMPTY) ->
      if lev <> "" then
        DOUBLE (pathid lev id, EMPTY)
      else
        TRIPLE(pathid lev id, EMPTY, EMPTY)
    | _ -> unhandled stderr 227 x; EMPTY)) arg4) in
                  if lev <> "" then Hashtbl.replace (fst thash2) (QUADRUPLE(WIRE, arg1, TRIPLE(arg2, intrng, EMPTY), lst)) ()
                  else Hashtbl.replace (fst thash2) (QUINTUPLE(tok, arg1, arg2, intrng, lst)) ()
| DOUBLE(DEFPARAM, TLIST lst) ->
  List.iter (function
    | TRIPLE(ID id1, ID id2, INT num) -> fprintf stderr "Ignoring defparam %s.%s = %d\n" id1.id id2.id num
    | oth -> unhandled stderr 696 oth) lst
| ASCNUM str -> fprintf stderr "Ignoring pragmatic string %s\n" str
| DOUBLE(ALWAYS, _) -> fprintf stderr "Trying to flatten an always block\n"
| exp -> unhandled stderr 356 exp
    
and flt' arch thash2 syms decls insts lev =
        let osymtab = Minimap.decl_to_hls (hfilter (function _ -> true) decls) syms in
        Hashtbl.iter (fun x _ -> flt arch osymtab thash2 syms lev x) decls;
        Hashtbl.iter (fun x _ -> flt arch osymtab thash2 syms lev x) insts;
        let extlst = ref [] in
        Hashtbl.iter (fun k (kind,extra) ->
          let combined = DOUBLE(ID (enterid k), EMPTY) in
          if extra (* && not (List.mem combined !oldlst) *) then extlst := combined :: !extlst) osymtab;
        Hashtbl.add (fst thash2) (QUADRUPLE(WIRE, EMPTY, TRIPLE(EMPTY, EMPTY, EMPTY), TLIST !extlst)) ()

let generate_flat_netlist k =
  let thash2 = (Hashtbl.create 256,Hashtbl.create 256) in
  match k.tree with
    | QUINTUPLE(MODULE, ID arg1, arg2, TLIST arg3, THASH (decls,insts)) ->
        flt' k.arch thash2 k.symbols decls insts "";
        let oldwires = Hashtbl.create 256 and obsolete = ref [] in
        Hashtbl.iter (fun k _ -> match k with
          | QUADRUPLE(WIRE, EMPTY, TRIPLE(EMPTY, EMPTY, EMPTY), TLIST wires) ->
            List.iter (fun k -> Hashtbl.replace oldwires k ()) wires;
            obsolete := k :: !obsolete
          | _ -> ()) (fst thash2);
        List.iter (Hashtbl.remove (fst thash2)) !obsolete;
        let extlst = ref [] in
        Hashtbl.iter (fun k _ -> extlst := k :: !extlst) oldwires;
        Hashtbl.add (fst thash2) (QUADRUPLE(WIRE, EMPTY, TRIPLE(EMPTY, EMPTY, EMPTY), TLIST !extlst)) ();
        QUINTUPLE(MODULE, ID (enterid (arg1.id^ !Globals.modsuffix)), arg2,
                TLIST (List.map (fun x -> (match x with
                  | ID id -> ID (id)
                  | _ -> unhandled stderr 532 x; EMPTY)) arg3),
                THASH thash2)
    | _ -> failwith (Dump.dumpstr k.tree^" is not a module netlist")

let gen_flat_arch arch nam =
  let lst = List.filter (fun arg -> arg.arch = arch) (Hashtbl.find_all modprims nam) in
  match lst with
    | [] -> failwith ("No modules matched arch "^arch^" name "^nam)
    | _ -> List.iter (fun arg -> Semantics.prescan stderr !Globals.archenv (generate_flat_netlist arg) "Generated by gen_flat_arch"; Printf.printf "Module report %s\n" (Semantics.endscan())) lst
