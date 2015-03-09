(***********************************************************************)
(*                                                                     *)
(*                     Objective Caml Extension                        *)
(*                                                                     *)
(*                 Jonathan Richard Robert Kimmitt                     *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Ctypes
open Printf

(* let unhand_list = ref [] *)
let newconslst = ref []

let newcons nam = let newcon = "Vhd"^nam in if not (List.mem newcon !newconslst) then newconslst := newcon :: !newconslst; newcon

let seqcnt = ref 0
 
let dumplen lst = (match List.length lst with
  | 1 -> ""
  | 2 -> "Double"
  | 3 -> "Triple"
  | 4 -> "Quadruple"
  | 5 -> "Quintuple"
  | 6 -> "Sextuple"
  | 7 -> "Septuple"
  | 8 -> "Octuple"
  | 9 -> "Nonuple"
  | 10 -> "Decuple"
  | 11 -> "Undecuple"
  | 12 -> "Duodecuple"
  | 13 -> "Tredecuple"
  | 14 -> "Quattuordecuple"
  | 15 -> "Quindecuple"
  | 16 -> "Sexdecuple"
  | 17 -> "Septendecuple"
  | 18 -> "Octodecuple"
  | 19 -> "Novemdecuple"
  | 20 -> "Vigenuple"
  | 21 -> "Unvigenuple"
  | 22 -> "Duovigenuple"
  | 23 -> "Trevigenuple"
  | 24 -> "Quattuorvigenuple"
  | 25 -> "Quinvigenuple"
  | err -> "Tuple")
 
let guardlst id = if String.length id > 5 then String.sub id (String.length id - 5) 5 = "_list" else false

let atypevar n = string_of_int n

(* let unhandled ix exp = let itm = (ix, exp) in if not (List.mem itm !unhand_list) then unhand_list := itm :: !unhand_list; "\n" *)

let shorten id = if (String.length id > 5) & (String.sub id 0 5 = "vhdl_")
    then String.sub id 5 (String.length id - 5) else "vhd_"^id

let buflst = ref []

let rec rewrite exp :string = match exp with
  | CSTRINGXTYPDECL (VSTRING vhdl_id,
     PTYPEDECL
      ([], [], PTYPEABSTRACT, CPublic,
       CSOME
        (CTYPTUPLE
          [CTYPCONSTR (CID "string", []); CTYPCONSTR (CID "int", [])]))) ->
            sprintf "  and dump_%s (str,_) = Str str\n" (shorten vhdl_id)
  | CSTRINGXTYPDECL
 (VSTRING vhdl_id,
  PTYPEDECL
   ([], [], PTYPEABSTRACT, CPublic,
    CSOME
     (CTYPTUPLE
       [CTYPCONSTR (CIDLIST ["big_int"; "Big_int"], []);
        CTYPCONSTR (CID "int", [])]))) ->
            sprintf "  and dump_%s (n,_) = Num n\n" (shorten vhdl_id)
  | CSTRINGXTYPDECL
 (VSTRING vhdl_id,
  PTYPEDECL
   ([], [], PTYPEABSTRACT, CPublic,
    CSOME
     (CTYPTUPLE [CTYPCONSTR (CID "float", []); CTYPCONSTR (CID "int", [])]))) ->
            sprintf "  and dump_%s (f,_) = Real f\n" (shorten vhdl_id)
  | CSTRINGXTYPDECL
 (VSTRING vhdl_id,
  PTYPEDECL
   ([], [], PTYPEABSTRACT, CPublic,
    CSOME
     (CTYPTUPLE [CTYPCONSTR (CID "char", []); CTYPCONSTR (CID "int", [])]))) ->
            sprintf "  and dump_%s (ch,_) = Char ch\n" (shorten vhdl_id)
  | CSTRINGXTYPDECL (VSTRING vhdl_id,
     PTYPEDECL
      ([], [], PTYPEABSTRACT, CPublic,
       CSOME
        (CTYPTUPLE
          [CTYPCONSTR (CID id1, []); CTYPCONSTR (CID id2, [])]))) ->
            sprintf "  and dump_%s (%s,%s) = Double(dump_%s %s,\n\tdump_%s %s)\n"
            (shorten vhdl_id)
            (shorten id1)
            (shorten id2)
            (shorten id1)
            (shorten id1)
            (shorten id2)
            (shorten id2)
  | CSTRINGXTYPDECL
 (VSTRING (vhdl_lst),
  PTYPEDECL
   ([], [], PTYPEABSTRACT, CPublic,
    CSOME (CTYPCONSTR (CID "list", [CTYPCONSTR (CID vhdl_id, [])])))) (*when guardlst vhdl_lst*) ->
            sprintf "  and dump_%s x = List (List.map dump_%s x)\n"
             (shorten vhdl_lst)
             (shorten vhdl_id)
  | CSTRINGXTYPDECL
 (VSTRING vhdl_id,
  PTYPEDECL
   ([], [], PTYPEABSTRACT, CPublic,
    CSOME (CTYPCONSTR (CID vhdl_alias, [])))) ->
            sprintf "  and dump_%s x = dump_%s x\n"
            (shorten vhdl_id)
            (shorten vhdl_alias)
  | CSTRINGXTYPDECL
 (VSTRING vhdl_id,
  PTYPEDECL
   ([], [],
    PTYPEVARIANT tlst,
    CPublic, CNONE)) -> (let buf = Buffer.create 256 in
    bprintf buf "  and dump_%s = function\n" (shorten vhdl_id);
   List.iter (function
  | CSTRINGTYPLOC(VSTRING nam, [CTYPCONSTR (CID "list", [CTYPCONSTR (CID vhdl_subtype_id, [])])]) -> let buf' = Buffer.create 256 in
    bprintf buf "  | %s %s_lst -> List (List.map dump_%s %s_lst)\n" nam
            (shorten vhdl_subtype_id)
            (shorten vhdl_subtype_id)
            (shorten vhdl_subtype_id);
    bprintf buf' "  | %s %s_lst -> List (List.map dump_%s %s_lst)\n" nam
            (shorten vhdl_subtype_id)
            (shorten vhdl_subtype_id)
            (shorten vhdl_subtype_id);
    buflst := (Buffer.contents buf') :: !buflst
  | CSTRINGTYPLOC(VSTRING (("AtomExpression"|
                            "AtomLogicalExpression"|
                            "AtomRelation"|
                            "AtomShiftExpression"|
                            "AtomSimpleExpression"|
                            "AtomTerm"|
                            "AtomFactor"|
                            "AtomDotted") as nam), [CTYPCONSTR (CID vhdl_subtype_id, [])]) -> let buf' = Buffer.create 256 in
    bprintf buf "  | %s %s -> dump_%s %s\n" nam
            (shorten vhdl_subtype_id)
            (shorten vhdl_subtype_id)
            (shorten vhdl_subtype_id);
    bprintf buf' "  | %s -> convert %s\n"
            (shorten vhdl_subtype_id)
            (shorten vhdl_subtype_id);
    buflst := (Buffer.contents buf') :: !buflst
  | CSTRINGTYPLOC(VSTRING nam, [CTYPCONSTR (CID vhdl_subtype_id, [])]) -> let cons = newcons nam and buf' = Buffer.create 256 in
    bprintf buf "  | %s %s -> Double(%s,\n\tdump_%s %s)\n" nam
            (shorten vhdl_subtype_id)
            cons
            (shorten vhdl_subtype_id)
            (shorten vhdl_subtype_id);
    bprintf buf' "  | Double(%s, %s) -> convert %s\n"
            cons
            (shorten vhdl_subtype_id)
            (shorten vhdl_subtype_id);
    buflst := (Buffer.contents buf') :: !buflst
  | CSTRINGTYPLOC(VSTRING nam, []) -> let cons = newcons nam and buf' = Buffer.create 256 in
    bprintf buf "  | %s -> %s\n" nam cons;
    bprintf buf' "  | %s -> EMPTY\n" cons;
    buflst := (Buffer.contents buf') :: !buflst
  | CSTRINGTYPLOC(VSTRING nam, [CTYPTUPLE tuplelst])
  | CSTRINGTYPLOC(VSTRING nam, tuplelst) -> let cons = newcons nam and dlen = dumplen ((CTYPTUPLE [])::tuplelst) and buf' = Buffer.create 256 in
    let delim = ref '(' and idx = ref 0 in bprintf buf "  | %s " nam; List.iter (function
      | CTYPCONSTR (CID vhdl_subtype_id, []) -> bprintf buf "%c%s%d" !delim (shorten vhdl_subtype_id) !idx; delim := ','; incr idx
      | _ -> bprintf buf "?") tuplelst;
    bprintf buf ") -> %s(%s" dlen cons; idx := 0; List.iter (function
      | CTYPCONSTR (CID vhdl_subtype_id, []) -> bprintf buf ", dump_%s %s%d\n\t"
         (shorten vhdl_subtype_id) (shorten vhdl_subtype_id) !idx; incr idx
      | _ -> bprintf buf "?") tuplelst;
    bprintf buf ")\n";
    bprintf buf' "  | %s(%s" dlen cons; idx := 0; List.iter (function
      | CTYPCONSTR (CID vhdl_subtype_id, []) -> bprintf buf' ", %s%d"
         (shorten vhdl_subtype_id) !idx; incr idx
      | _ -> bprintf buf' "?") tuplelst;
    bprintf buf' ") -> %s(convert %s" (String.uppercase dlen) cons;
    idx := 0; List.iter (function
      | CTYPCONSTR (CID vhdl_subtype_id, []) -> bprintf buf' ", convert %s%d"
         (shorten vhdl_subtype_id) !idx; incr idx
      | _ -> bprintf buf' "?") tuplelst;
    bprintf buf' ")\n";
    buflst := (Buffer.contents buf') :: !buflst;
  | _ -> bprintf buf "?\n") tlst;
    Buffer.contents buf)
  | CSTRINGXTYPDECL
 (VSTRING vhdl_id,
  PTYPEDECL
   ([], [],
    PTYPERECORD tlst,
    CPublic, CNONE)) -> (let buf = Buffer.create 256 and buf' = Buffer.create 256 in
    bprintf buf "  and dump_%s {\n" (shorten vhdl_id);
   let cnt = ref 0 in List.iter (function
  | CSTRINGXMUT(VSTRING nam, CImmutable, CTYPPOLY ([], CTYPCONSTR (CID ("list"|"option"), [CTYPCONSTR (CID vhdl_subtype_id, [])]))) ->
    bprintf buf "  %s=%s%d;\n" nam (shorten vhdl_subtype_id) !cnt; incr cnt
  | CSTRINGXMUT(VSTRING nam, CImmutable, CTYPPOLY ([], CTYPCONSTR (CID vhdl_subtype_id, []))) ->
    bprintf buf "  %s=%s%d;\n" nam (shorten vhdl_subtype_id) !cnt; incr cnt
  | _ -> bprintf buf "?\n") tlst; bprintf buf "} = %s(%s" (dumplen ((CTYPTUPLE [])::tlst)) (newcons (shorten vhdl_id));
   let delim = ref ',' in cnt := 0; List.iter (function
  | CSTRINGXMUT(VSTRING nam, CImmutable, CTYPPOLY ([], CTYPCONSTR (CID "list", [CTYPCONSTR (CID vhdl_subtype_id, [])]))) ->
    bprintf buf "%c\n\tList (List.map dump_%s %s%d)" !delim
            (shorten vhdl_subtype_id)
            (shorten vhdl_subtype_id)
            !cnt; delim := ','; incr cnt
  | CSTRINGXMUT(VSTRING nam, CImmutable, CTYPPOLY ([], CTYPCONSTR (CID "option", [CTYPCONSTR (CID vhdl_subtype_id, [])]))) ->
    bprintf buf "%c\n\t(match %s%d with Some %s -> dump_%s %s | None -> VhdNone)" !delim
            (shorten vhdl_subtype_id)
            !cnt
            (shorten vhdl_subtype_id)
            (shorten vhdl_subtype_id)
            (shorten vhdl_subtype_id);
             delim := ','; incr cnt
  | CSTRINGXMUT(VSTRING nam, CImmutable, CTYPPOLY ([], CTYPCONSTR (CID vhdl_subtype_id, []))) ->
    bprintf buf "%c\n\tdump_%s %s%d" !delim
            (shorten vhdl_subtype_id)
            (shorten vhdl_subtype_id)
            !cnt; delim := ','; incr cnt
  | _ -> bprintf buf "?\n") tlst; bprintf buf ")\n";
    buflst := (Buffer.contents buf') :: !buflst;
    Buffer.contents buf)
  | _ -> failwith "unhandled expression"

and clist arg = List.map rewrite arg

let rec f (exp:ctypes) = rewrite exp
