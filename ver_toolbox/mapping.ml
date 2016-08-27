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

type uptr = UPTR of (out_channel -> int -> Vparser.token -> unit) | UNIL;;

let rec monadic_buffer (tbl:tbl) syms path wid (dst,src) =
  let out = Minimap.netid "buf" tbl.gentab tbl.namtab in
  Minimap.declare tbl.namtab syms out (wid-1) 0;  
  assert (wid>0);
  monadic_signal (tbl:tbl) syms path (Minimap.insert_buf syms) wid (dst,src)

and monadic_signal (tbl:tbl) syms path fn wid (dst,src) =
  let dst' = Minimap.list_shorten wid (Minimap.concat_flatten' syms [dst]) in
  let src' = Minimap.list_shorten wid (Minimap.list_pad wid (Minimap.concat_flatten' syms [src])) in
  let rec iter2 f l1 l2 =
  match (l1, l2) with
    ([], []) -> ()
  | (a1::l1, a2::l2) -> iter2 f l1 l2; f a1 a2
  | (oth1,oth2) -> Dump.unhandled stderr 31 (DOUBLE(TLIST oth1, TLIST oth2)) in
  let manip = function
    | TRIPLE(BITSEL, ID dest, INT idx0) -> TRIPLE(BITSEL, (path dest), INT idx0)
    | BINNUM _ as num -> num
    | ID id -> path id
    | oth -> Dump.unhandled stderr 647 oth; UNKNOWN in
  iter2 (fun arg1 arg2 -> List.iter (fun itm -> Hashtbl.add tbl.snd itm ()) (fn (manip arg1) (manip arg2))) dst' src'

let dyadic_signal (tbl:tbl) syms path fn wid (dst,lft,rght) =
  if !verbose then printf "dyadic ops %s\n%s\n%s\n" (Verilogout.dump_logic (dst)) (Verilogout.dump_logic (lft)) (Verilogout.dump_logic (rght));
  let dst' = Minimap.list_shorten wid (Minimap.concat_flatten' syms [dst]) in
  let lft' = Minimap.list_shorten wid (Minimap.list_pad wid (Minimap.concat_flatten' syms [lft])) in
  let rght' = Minimap.list_shorten wid (Minimap.list_pad wid (Minimap.concat_flatten' syms [rght])) in
  if !verbose then printf "dyadic ops' %s\n%s\n%s\n" (Verilogout.dump_logic (TLIST dst')) (Verilogout.dump_logic (TLIST lft')) (Verilogout.dump_logic (TLIST rght'));
  let rec iter3 f l1 l2 l3 =
  match (l1, l2, l3) with
    ([], [], []) -> ()
  | (a1::l1, a2::l2, a3::l3) -> iter3 f l1 l2 l3; f a1 a2 a3
  | (oth1,oth2,oth3) -> Dump.unhandled stderr 47 (TRIPLE(TLIST oth1, TLIST oth2, TLIST oth3)) in
  let manip = function
    | TRIPLE(BITSEL, ID dest, INT idx0) -> TRIPLE(BITSEL, (path dest), INT idx0)
    | BINNUM _ as num -> num
    | ID id -> path id
    | oth -> Dump.unhandled stderr 647 oth; UNKNOWN in
  iter3 (fun arg1 arg2 arg3 ->
    if !verbose then printf "dyadic_signal %s %s %s\n" (Count.tokenstr arg1) (Count.tokenstr arg2) (Count.tokenstr arg3);
    List.iter (fun itm -> Hashtbl.add tbl.snd itm ()) (fn (manip arg1) (manip arg2) (manip arg3))) dst' lft' rght'

let redor exp (tbl:tbl) syms (src,wid) =
  let w = ref wid and cmp' = ref src in
  while !w > 1 do
    if !w mod 2 = 1 then incr w;
    let cmp'' = Minimap.netid "ror" tbl.gentab tbl.namtab in
    Minimap.declare tbl.namtab syms cmp'' (!w/2-1) 0;
    if !verbose then printf "insert_or %d %s %s %s\n"
      (!w/2)
      cmp''.id
      (Count.tokenstr (QUADRUPLE (PARTSEL, !cmp', INT (!w-1), INT (!w/2))))
      (Count.tokenstr (QUADRUPLE (PARTSEL, !cmp', INT (!w/2-1), INT 0)));
    dyadic_signal (tbl:tbl) syms (Minimap.validid tbl.namtab) (Minimap.insert_or syms) (!w/2)
      (ID cmp'',
       QUADRUPLE (PARTSEL, !cmp', INT (!w-1), INT (!w/2)),
       QUADRUPLE (PARTSEL, !cmp', INT (!w/2-1), INT 0));
    cmp' := ID cmp'';
    w := !w / 2
  done;
  (!cmp',!w)

let tilde exp (tbl:tbl) syms (src,wid) =
  let out = Minimap.netid "til" tbl.gentab tbl.namtab in
  Minimap.declare tbl.namtab syms out (wid-1) 0;
  monadic_signal (tbl:tbl) syms (Minimap.validid tbl.namtab) (Minimap.insert_inv syms) wid (ID out,src);
  (ID out,wid)

let pling' exp (tbl:tbl) syms (src,wid) =
  let collapse = redor exp (tbl:tbl) syms (src,wid) in
  tilde exp (tbl:tbl) syms collapse

let unaryplus exp (tbl:tbl) syms (src,wid) =
  let out = Minimap.netid "idn" tbl.gentab tbl.namtab in
  Minimap.declare tbl.namtab syms out (wid-1) 0;
  monadic_signal (tbl:tbl) syms (Minimap.validid tbl.namtab) (Minimap.insert_buf syms) wid (ID out,src);
  (ID out,wid)
      
let rec log2 = function
  | 0 -> 0
  | 1 -> 0
  | n -> 1 + log2(n/2)

let pling exp (tbl:tbl) syms (src,wid) =
  let ewid = 1 lsl (log2 wid) in
  if ewid = wid then
    pling' exp (tbl:tbl) syms (src,wid)
  else
    begin
    let wider = Minimap.lfill (src,wid) (ewid+ewid) in
    let buffered = unaryplus exp tbl syms wider in
    pling' exp (tbl:tbl) syms buffered
    end

and insert_plus syms cin gen prop prop_ carry bitcnt out lft rght =
  let carryin = if !bitcnt = 0 then cin else TRIPLE(BITSEL, ID carry, INT !bitcnt) in
  let gen1 = TRIPLE(BITSEL, ID gen, INT !bitcnt) in
  let prop1 = TRIPLE(BITSEL, ID prop, INT !bitcnt) in
  let prop2 = TRIPLE(BITSEL, ID prop_, INT !bitcnt) in
  let carryout = TRIPLE(BITSEL, ID carry, INT (!bitcnt+1)) in
  let gen' = Minimap.insert_and syms gen1 lft rght in
  let prop' = Minimap.insert_xor syms prop1 lft rght in
  let carry' = Minimap.insert_and syms prop2 prop1 carryin in
  let carry'' = Minimap.insert_or syms carryout gen1 prop2 in
  let sum' = Minimap.insert_xor syms out prop1 carryin in
  incr bitcnt;
  gen' @ prop' @ carry' @ carry'' @ sum'

let plus' exp (tbl:tbl) syms cin (lft,lwid) (rght,rwid) =
  let wid = (max lwid rwid)+1 in
  let gen = Minimap.netid "gen" tbl.gentab tbl.namtab in
  let prop = Minimap.netid "prop" tbl.gentab tbl.namtab in
  let prop_ = Minimap.netid "prop_" tbl.gentab tbl.namtab  in
  let carry = Minimap.netid "carry" tbl.gentab tbl.namtab in
  let out = Minimap.netid "sum" tbl.gentab tbl.namtab  in
  Minimap.declare tbl.namtab syms gen (wid-2) 0;
  Minimap.declare tbl.namtab syms prop (wid-2) 0;
  Minimap.declare tbl.namtab syms prop_ (wid-2) 0;
  Minimap.declare tbl.namtab syms carry (wid-1) 0;
  Minimap.declare tbl.namtab syms out (wid-1) 0;
  if !verbose then printf "Plus %s %d + %s %d => %s %d\n" (Verilogout.dump_logic lft) lwid (Verilogout.dump_logic rght) rwid out.id wid;
  dyadic_signal (tbl:tbl) syms (Minimap.validid tbl.namtab) (insert_plus syms cin gen prop prop_ carry (ref 0)) (wid-1) (ID out,lft,rght);
  monadic_signal (tbl:tbl) syms (Minimap.validid tbl.namtab) (Minimap.insert_buf syms) 1 
    (TRIPLE(BITSEL,ID out,INT (wid-1)),TRIPLE(BITSEL, ID carry, INT (wid-1)));
  (ID out,wid)

let equal exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let cmp = Minimap.netid "equ" tbl.gentab tbl.namtab in
  Minimap.declare tbl.namtab syms cmp (wid-1) 0;
  dyadic_signal (tbl:tbl) syms (Minimap.validid tbl.namtab) (Minimap.insert_xor syms) wid (ID cmp,lft,rght);
  pling exp (tbl:tbl) syms (ID cmp,wid)

and noteq exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let cmp = Minimap.netid "neq" tbl.gentab tbl.namtab in
  Minimap.declare tbl.namtab syms cmp (wid-1) 0;
  dyadic_signal (tbl:tbl) syms (Minimap.validid tbl.namtab) (Minimap.insert_xor syms) wid (ID cmp,lft,rght);
  let eq = pling exp (tbl:tbl) syms (ID cmp,wid) in
  tilde exp tbl syms eq

and gte exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let out = Minimap.netid "gte" tbl.gentab tbl.namtab in
  Minimap.declare tbl.namtab syms out 0 0;
  let invert = tilde exp tbl syms (Minimap.lfill (rght,rwid) wid) in
  let (diff,dwid) = plus' (TRIPLE(MINUS,lft,rght)) tbl syms (WIDTHNUM(2,1,1)) (Minimap.lfill (lft,lwid) wid) invert in
  (TRIPLE(BITSEL,diff, INT (dwid-1)),1)

and lte exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let out = Minimap.netid "lte" tbl.gentab tbl.namtab in
  Minimap.declare tbl.namtab syms out 0 0;
  let invert = tilde exp tbl syms (Minimap.lfill (rght,rwid) wid) in
  let (diff,dwid) = plus' (TRIPLE(MINUS,lft,rght)) tbl syms (WIDTHNUM(2,1,0)) (Minimap.lfill (lft,lwid) wid) invert in
  tilde exp tbl syms (TRIPLE(BITSEL,diff, INT (dwid-1)),1)

and less exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let out = Minimap.netid "les" tbl.gentab tbl.namtab in
  Minimap.declare tbl.namtab syms out 0 0;
  let invert = tilde exp tbl syms (Minimap.lfill (rght,rwid) wid) in
  let (diff,dwid) = plus' (TRIPLE(MINUS,lft,rght)) tbl syms (WIDTHNUM(2,1,1)) (Minimap.lfill (lft,lwid) wid) invert in
  tilde exp tbl syms (TRIPLE(BITSEL,diff, INT (dwid-1)),1)

and greater exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let out = Minimap.netid "grt" tbl.gentab tbl.namtab in
  Minimap.declare tbl.namtab syms out 0 0;
  let invert = tilde exp tbl syms (Minimap.lfill (rght,rwid) wid) in
  let (diff,dwid) = plus' (TRIPLE(MINUS,lft,rght)) tbl syms (WIDTHNUM(2,1,0)) (Minimap.lfill (lft,lwid) wid) invert in
   (TRIPLE(BITSEL,diff, INT (dwid-1)),1)

and logand exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let out = Minimap.netid "land" tbl.gentab tbl.namtab in
  Minimap.declare tbl.namtab syms out (wid-1) 0;
  if !verbose then printf "logand %s %s %s\n" out.id (Dump.dumpstr lft) (Dump.dumpstr rght);
  dyadic_signal (tbl:tbl) syms (Minimap.validid tbl.namtab) (Minimap.insert_and syms) wid (ID out,lft,rght);
  (ID out,wid)

and logor exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let out = Minimap.netid "lor" tbl.gentab tbl.namtab in
  Minimap.declare tbl.namtab syms out (wid-1) 0;
  dyadic_signal (tbl:tbl) syms (Minimap.validid tbl.namtab) (Minimap.insert_or syms) wid (ID out,lft,rght);
  (ID out,wid)

and log_pand exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let arg1 = Minimap.netid "pand1" tbl.gentab tbl.namtab in
  let arg2 = Minimap.netid "pand2" tbl.gentab tbl.namtab in
  let out = Minimap.netid "pand" tbl.gentab tbl.namtab in
  Minimap.declare tbl.namtab syms arg1 (lwid-1) 0;
  Minimap.declare tbl.namtab syms arg2 (rwid-1) 0;
  Minimap.declare tbl.namtab syms out 0 0;
  let (pling1,_) = pling exp (tbl:tbl) syms (lft,lwid) in
  let (pling2,_) = pling exp (tbl:tbl) syms (rght,rwid) in
  dyadic_signal (tbl:tbl) syms (Minimap.validid tbl.namtab) (Minimap.insert_or syms) 1 (ID out,pling1,pling2);
  tilde exp tbl syms (ID out,1)

and log_por exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let arg1 = Minimap.netid "por1" tbl.gentab tbl.namtab in
  let arg2 = Minimap.netid "por2" tbl.gentab tbl.namtab in
  let out = Minimap.netid "por" tbl.gentab tbl.namtab in
  Minimap.declare tbl.namtab syms arg1 (lwid-1) 0;
  Minimap.declare tbl.namtab syms arg2 (rwid-1) 0;
  Minimap.declare tbl.namtab syms out 0 0;
  let (pling1,_) = pling exp (tbl:tbl) syms (lft,lwid) in
  let (pling2,_) = pling exp (tbl:tbl) syms (rght,rwid) in
  dyadic_signal (tbl:tbl) syms (Minimap.validid tbl.namtab) (Minimap.insert_and syms) 1 (ID out,pling1,pling2);
  tilde exp tbl syms (ID out,1)

and logxor exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let out = Minimap.netid "lxor" tbl.gentab tbl.namtab in
  Minimap.declare tbl.namtab syms out (wid-1) 0;
  dyadic_signal (tbl:tbl) syms (Minimap.validid tbl.namtab) (Minimap.insert_xor syms) wid (ID out,lft,rght);
  (ID out,1)

and plus exp (tbl:tbl) syms lft' rght' = plus' exp tbl syms (WIDTHNUM(2,1,0)) lft' rght'

and plusc exp (tbl:tbl) syms lft' rght' = plus' exp tbl syms (WIDTHNUM(2,1,1)) lft' rght'
    
let minus exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let invert = tilde exp tbl syms (Minimap.lfill (rght,rwid) wid) in
  plusc exp tbl syms (Minimap.lfill (lft,lwid) wid) invert

let dyadic_bus_wild (tbl:tbl) isyms (dest,dwid) syms conn1 lft2 rght2 str path1 path2 =
  let (sz,_,bin2) = Const.widthnum' stderr 2 str in
  let str_index idx = while bin2.[!idx] = '_' do incr idx; done;
    bin2.[!idx] in
  let (lft,rght,inc) = Minimap.find_width dest isyms in
  let idx = ref lft
  and idx2 = ref lft2
  and inc2 = if lft2 < rght2 then 1 else -1
  and idx3 = ref 0 in
  while !idx <> rght+inc do 
    if !verbose then printf "dyadic bus wild bit %d: %s[%d] := %s[%d] <-> %c\n"
      !idx3
      dest.id !idx
      conn1.id !idx2
      (str_index idx3);
    List.iter (fun arg -> Hashtbl.add tbl.snd arg ()) (match str_index idx3 with
    | '0' -> Minimap.insert_buf syms (TRIPLE(BITSEL, (path1 dest), INT !idx)) (TRIPLE(BITSEL, (path2 conn1), INT !idx2))
    | '1' -> Minimap.insert_inv syms (TRIPLE(BITSEL, (path1 dest), INT !idx)) (TRIPLE(BITSEL, (path2 conn1), INT !idx2))
    | '?' -> Minimap.insert_gnd syms (TRIPLE(BITSEL, (path1 dest), INT !idx))
    | _ -> failwith bin2);
    idx := !idx + inc;
    idx2 := !idx2 + inc2;
    incr idx3
  done

let wildequal exp (tbl:tbl) syms (lft,lwid) (rght,rwid) =
  let wid = max lwid rwid in
  let cmp = Minimap.netid "weq" tbl.gentab tbl.namtab in
  Minimap.declare tbl.namtab syms cmp (wid-1) 0;
  if !verbose then printf "wildequal %s %d : " (Dump.dumpstr lft) lwid;
  if !verbose then printf " %s %d\n" (Dump.dumpstr rght) rwid;
  (match (Minimap.lfill' tbl.namtab syms wid lft,Minimap.lfill' tbl.namtab syms wid rght) with
  | (ID lft, BINNUM str) ->
    let (lft2,rght2,inc2) = Minimap.find_width lft syms in
    dyadic_bus_wild (tbl:tbl) syms (cmp,wid) syms lft lft2 rght2 str (Minimap.validid tbl.namtab) (Minimap.validid tbl.namtab)
  | (QUADRUPLE (PARTSEL, ID lft, INT lft2, INT rght2), BINNUM str) ->
    dyadic_bus_wild (tbl:tbl) syms (cmp,wid) syms lft lft2 rght2 str (Minimap.validid tbl.namtab) (Minimap.validid tbl.namtab)
  | (a,BINNUM str) -> myfailwith (Printf.sprintf "Wildcard case expression %s,%s not supported (yet)\n" (Dump.dumpstr a) str)
  | (a,b) -> dyadic_signal (tbl:tbl) syms  (Minimap.validid tbl.namtab) (Minimap.insert_xor syms) wid (ID cmp,a,b)
  );
  pling exp (tbl:tbl) syms (ID cmp,wid)

and mux2 exp (tbl:tbl) syms (sel,_) (hi,hwid) (lo,lwid) =
  let wid = max hwid lwid in
  let out = Minimap.netid "mux" tbl.gentab tbl.namtab in
  Minimap.declare tbl.namtab syms out (wid-1) 0;
  if !verbose then printf "mux2 signal %s ? %s : %s\n" (Count.tokenstr sel) (Count.tokenstr hi) (Count.tokenstr lo);
  dyadic_signal (tbl:tbl) syms (Minimap.validid tbl.namtab) (Minimap.insert_mux syms sel) wid (ID out,hi,lo);
  (ID out,wid)

and dyadic_bitsel_general fn (tbl:tbl) isyms dest syms bit idx1 path1 path2 =
  Hashtbl.add tbl.snd (fn (path1 dest) (TRIPLE(BITSEL, (path2 bit), INT idx1))) ()
    
and insert_flipflop syms edglst output input =
  Minimap.assert_unit_width syms [input;output];
  let dumpedge edglst :string =
    let buf = Buffer.create 64 in
    List.iter (function
      | DOUBLE(edg,net) -> bprintf buf "%s:%s " (Ord.getstr edg) (Count.tokenstr net)
      | TRIPLE(edg,net,clrval) -> bprintf buf "%s:%s:%s " (Ord.getstr edg) (Count.tokenstr net) (Count.tokenstr clrval) 
      | x -> Dump.unhandled stderr 506 x; failwith "invalid (tbl:tbl) edgelist") edglst;
    Buffer.contents buf in
  match edglst with
    | [DOUBLE(POSEDGE,clk)] ->
        [QUADRUPLE(MODINST, ID mybuf.ff.nam, EMPTY,
                   TLIST
                     [TRIPLE
                         (Minimap.instid [clk;input;output] syms mybuf.ff, SCALAR,
                          TLIST
                            [TRIPLE (CELLPIN, ID (mybuf.ff.clk), clk);
                             TRIPLE (CELLPIN, ID (mybuf.ff.dat), input);
                             TRIPLE (CELLPIN, ID (mybuf.ff.qout), output)])])]
    | [DOUBLE(POSEDGE,clk);TRIPLE(POSEDGE,clr, clr_val)] ->
        [QUADRUPLE
            (MODINST, ID mybuf.ffc.nam, EMPTY,
             TLIST
               [TRIPLE
                   (Minimap.instid [clk;clr;input;output] syms mybuf.ffc, SCALAR,
                    TLIST
                      [TRIPLE (CELLPIN, ID (mybuf.ffc.qout), output);
                       TRIPLE (CELLPIN, ID (mybuf.ffc.dat), input);
                       TRIPLE (CELLPIN, ID (mybuf.ffc.clk), clk);
                       TRIPLE (CELLPIN, ID (mybuf.ffc.clr), clr)])])]
    | [DOUBLE(POSEDGE,clk);DOUBLE(POSEDGE,clr);DOUBLE(POSEDGE,pre)] -> failwith "flip-flop with clr and preset not implemented"
    | [DOUBLE (POSEDGE, clk); TRIPLE (POSEDGE, clr, clr_val); DOUBLE (dly, clken)] ->
        [QUADRUPLE
            (MODINST, ID mybuf.ffce.nam, EMPTY,
             TLIST
               [TRIPLE
                   (Minimap.instid [clken;clk;clr;input;output] syms mybuf.ffce, SCALAR,
                    TLIST
                      [TRIPLE (CELLPIN, ID (mybuf.ffce.qout), output);
                       TRIPLE (CELLPIN, ID (mybuf.ffce.dat), input);
                       TRIPLE (CELLPIN, ID (mybuf.ffce.clk), clk);
                       TRIPLE (CELLPIN, ID (mybuf.ffce.clr), clr);
                       TRIPLE (CELLPIN, ID (mybuf.ffce.en), clken)])])]
    | [DOUBLE(NEGEDGE,clk)] ->
        [QUADRUPLE(MODINST, ID mybuf.ff.nam, EMPTY,
                   TLIST
                     [TRIPLE
                         (Minimap.instid [DOUBLE(NOT,clk);input;output] syms mybuf.ff, SCALAR,
                          TLIST
                            [TRIPLE (CELLPIN, ID (mybuf.ff.clk), DOUBLE(NOT, clk));
                             TRIPLE (CELLPIN, ID (mybuf.ff.dat), input);
                             TRIPLE (CELLPIN, ID (mybuf.ff.qout), output)])])]
    | [DOUBLE(NEGEDGE,clk);TRIPLE(NEGEDGE,clr, clr_val)] ->
        [QUADRUPLE
            (MODINST, ID mybuf.ffc.nam, EMPTY,
             TLIST
               [TRIPLE
                   (Minimap.instid [DOUBLE(NOT,clk);clr;input;output] syms mybuf.ffc, SCALAR,
                    TLIST
                      [TRIPLE (CELLPIN, ID (mybuf.ffc.qout), output);
                       TRIPLE (CELLPIN, ID (mybuf.ffc.dat), input);
                       TRIPLE (CELLPIN, ID (mybuf.ffc.clk), DOUBLE(NOT, clk));
                       TRIPLE (CELLPIN, ID (mybuf.ffc.clr), clr)])])]
    | [DOUBLE(NEGEDGE,clk);DOUBLE(NEGEDGE,clr);DOUBLE(NEGEDGE,pre)] -> failwith "flip-flop with clr and preset not implemented"
    | [DOUBLE (NEGEDGE, clk); TRIPLE (NEGEDGE, clr, clr_val); DOUBLE (dly, clken)] ->
        [QUADRUPLE
            (MODINST, ID mybuf.ffce.nam, EMPTY,
             TLIST
               [TRIPLE
                   (Minimap.instid [clken;DOUBLE(NOT, clk);clr;input;output] syms mybuf.ffce, SCALAR,
                    TLIST
                      [TRIPLE (CELLPIN, ID (mybuf.ffce.qout), output);
                       TRIPLE (CELLPIN, ID (mybuf.ffce.dat), input);
                       TRIPLE (CELLPIN, ID (mybuf.ffce.clk), DOUBLE(NOT, clk));
                       TRIPLE (CELLPIN, ID (mybuf.ffce.clr), clr);
                       TRIPLE (CELLPIN, ID (mybuf.ffce.en), clken)])])]
    | _ ->
        Dump.unhandled stderr 495 (TLIST edglst);
        failwith ("ff with control logic "^(dumpedge edglst)^" not implemented")

let bus_flipflop (tbl:tbl) syms edglst (dst,dwid) (src,swid) =
  monadic_signal (tbl:tbl) syms (Minimap.validid tbl.namtab) (insert_flipflop syms edglst) dwid (dst,src);
  if !verbose then printf "assign_next %s w=%d\n" (Count.tokenstr dst) dwid

let redand exp (tbl:tbl) syms (src,wid) =
  let w = ref wid and cmp' = ref src in
  while !w > 1 do
    if !w mod 2 = 1 then incr w;
    let cmp'' = Minimap.netid "rand" tbl.gentab tbl.namtab in
    Minimap.declare tbl.namtab syms cmp'' (!w/2-1) 0;
    if !verbose then printf "insert_and %d %s %s %s\n"
      (!w/2)
      cmp''.id
      (Count.tokenstr (QUADRUPLE (PARTSEL, !cmp', INT (!w-1), INT (!w/2))))
      (Count.tokenstr (QUADRUPLE (PARTSEL, !cmp', INT (!w/2-1), INT 0)));
    dyadic_signal (tbl:tbl) syms (Minimap.validid tbl.namtab) (Minimap.insert_and syms) (!w/2)
      (ID cmp'',
       QUADRUPLE (PARTSEL, !cmp', INT (!w-1), INT (!w/2)),
       QUADRUPLE (PARTSEL, !cmp', INT (!w/2-1), INT 0));
    cmp' := ID cmp'';
    w := !w / 2
  done;
  (!cmp',!w)

let map_select = {
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
(*
 map_times = times ;
*)
  map_mux2 = mux2 ;
(*
 map_sleft = sleft ;
 map_sright = sright ;
*)
  map_buffer = monadic_buffer ;
 map_flipflop = bus_flipflop ;
 map_redand = redand ;
 }

