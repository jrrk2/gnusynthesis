(**************************************************************************)
(*                                                                        *)
(* OCaml template Copyright (C) 2004-2010                                 *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(* Adapted to boolean logic by Jonathan Kimmitt                           *)
(*  Copyright 2016 University of Cambridge                                *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open File_types
open File

let parse_output_ast_from_chan ch =
  let lb = Lexing.from_channel ch in
  let output = try
      File.ml_start File_lex.token lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "File.parse: parse error at character %d" n);
(*
    | _ ->
      failwith (Printf.sprintf "Parser error at line %d" !Scope.lincnt)
*)
  in
  output

let parse arg =
  let ch = open_in arg in
  let rslt = parse_output_ast_from_chan ch in
  close_in ch;
  rslt

(*
let unhand = ref None
*)

let rec rw = function
| TUPLE2 (arg, (EOF_TOKEN|SEMI)) -> rw arg
| TUPLE2 ((PLUS|MINUS) as op, arg) -> TUPLE2(op, rw arg)
| TUPLE2 (lst, arg) -> (match rw lst with TLIST lst -> TLIST (rw arg :: lst) | oth -> TLIST (rw arg :: oth :: []))
| TUPLE3 (lst, COMMA, arg) -> (match rw lst with TLIST lst -> TLIST (rw arg :: lst) | oth -> TLIST (rw arg :: oth :: []))
| TLIST lst -> TLIST (List.map rw lst)
| ELIST lst -> ELIST (List.map rw lst)
| TUPLE3(arg1,arg2,arg3) -> TUPLE3 (rw arg1, rw arg2, rw arg3)
| TUPLE4(arg1,arg2,arg3,arg4) -> TUPLE4 (rw arg1, rw arg2, rw arg3, rw arg4)
| TUPLE5(arg1,arg2,arg3,arg4,arg5) -> TUPLE5 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5)
| TUPLE6(arg1,arg2,arg3,arg4,arg5,arg6) -> TUPLE6 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6)
| TUPLE7(arg1,arg2,arg3,arg4,arg5,arg6,arg7) -> TUPLE7 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7)
| TUPLE8(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) ->
   TUPLE8(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8)
| TUPLE9(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) ->
   TUPLE9(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8, rw arg9)
| TUPLE10(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10) ->
   TUPLE10(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8, rw arg9, rw arg10)
| STRING _ as x -> x
| SLIST _ as x -> x
| NUM _ as x -> x
| IDENT _ as x -> x
| CONS4 _ as x -> x
| CONS3 _ as x -> x
| CONS2 _ as x -> x
| CONS1 _ as x -> x
| VBAR -> VBAR
| UNDERSCORE -> UNDERSCORE
| UNARY -> UNARY
| TILDE -> TILDE
| SEMI -> SEMI
| RPAR -> RPAR
| RCURLY -> RCURLY
| RBRACK -> RBRACK
| RBRACE -> RBRACE
| QUOTE -> QUOTE
| QUERY -> QUERY
| PLUS -> PLUS
| PLING -> PLING
| PERCENT -> PERCENT
| MULT -> MULT
| MINUS -> MINUS
| LPAR -> LPAR
| LINEFEED -> LINEFEED
| LESS -> LESS
| LCURLY -> LCURLY
| LBRACK -> LBRACK
| LBRACE -> LBRACE
| KW_TRUE -> KW_TRUE
| KW_FALSE -> KW_FALSE
| KW_DEFINE_GROUP -> KW_DEFINE_GROUP
| KW_DEFINE -> KW_DEFINE
| HASH -> HASH
| GREATER -> GREATER
| ERROR_TOKEN -> ERROR_TOKEN
| ERROR -> ERROR
| EQ -> EQ
| EOF_TOKEN -> EOF_TOKEN
| END -> END
| EMPTY_TOKEN -> EMPTY_TOKEN
| DOUBLEQUOTE -> DOUBLEQUOTE
| DOT -> DOT
| DOLLAR -> DOLLAR
| DIV -> DIV
| DEFAULT -> DEFAULT
| COMMA -> COMMA
| COLON -> COLON
| CARET -> CARET
| BACKSLASH -> BACKSLASH
| BACKQUOTE -> BACKQUOTE
| AT -> AT
| AMPERSAND -> AMPERSAND
| ACCEPT -> ACCEPT

type liberty =
| Library of string * liberty list
| LibCell of string * liberty list
| CellPin of string * liberty list
| Timing of liberty list
| IPower of liberty list
| LPower of liberty list
| Power of string * string * liberty list
| CellValues of liberty list
| CellIndex1 of string
| CellIndex2 of string
| Other of string * string * liberty list
| String of string
| Sense of string
| Related of string * string
| SDF_cond of string
| Function of string
| Direction of string
| When of string
| Parameter of string *float
| Transition of string * string * liberty
| FlipFlop of liberty list * liberty list
| Latch of liberty list * liberty list
| StateTable of liberty list * string
| WireLoad of string * liberty list
| FanoutLength of float list
| Define of string list
| VoltageMap of liberty list
| CapLoadUnit of liberty list
| LibFeatures of liberty list

let unhand = ref None

let rec rw' = function
| TUPLE4 (TUPLE4 (IDENT "library", LPAR, IDENT nam, RPAR), LCURLY, TLIST lst, RCURLY) -> Library (nam, List.rev_map rw' lst)
| TUPLE4 (TUPLE4 (IDENT "cell", LPAR, IDENT nam, RPAR), LCURLY, TLIST lst, RCURLY) -> LibCell (nam, List.rev_map rw' lst)
| TUPLE4 (TUPLE4 (IDENT "pin", LPAR, IDENT nam, RPAR), LCURLY, TLIST lst, RCURLY) -> CellPin (nam, List.rev_map rw' lst)
| TUPLE4 (TUPLE3 (IDENT "timing", LPAR, RPAR), LCURLY, TLIST lst, RCURLY) -> Timing (List.rev_map rw' lst)
| TUPLE4 (TUPLE3 (IDENT "internal_power", LPAR, RPAR), LCURLY, TLIST lst, RCURLY) -> IPower (List.rev_map rw' lst)
| TUPLE4 (TUPLE4 (IDENT ("rise_power"|"fall_power" as p), LPAR, IDENT nam, RPAR), LCURLY, lst, RCURLY) ->
    Power (p, nam, match lst with TLIST lst -> List.rev_map rw' lst | oth -> rw' oth :: [])
| TUPLE4 (TUPLE4 (IDENT ("rise_transition"|"fall_transition" as x), LPAR, IDENT ("scalar" as s), RPAR), LCURLY, values, RCURLY) ->
Transition(x, s, rw' values)
| TUPLE4 (TUPLE4 (IDENT "ff", LPAR, TLIST oplst, RPAR), LCURLY, TLIST lst, RCURLY) -> FlipFlop(List.rev_map rw' oplst, List.rev_map rw' lst)
| TUPLE4 (TUPLE4 (IDENT "latch", LPAR, TLIST oplst, RPAR), LCURLY, TLIST lst, RCURLY) -> Latch(List.rev_map rw' oplst, List.rev_map rw' lst)
| TUPLE4 (TUPLE4 (IDENT "statetable", LPAR, TLIST pinlst, RPAR), LCURLY, TUPLE4 (IDENT "table", COLON, STRING s, SEMI), RCURLY) ->
StateTable(List.rev_map rw' pinlst, s)
| TUPLE4 (TUPLE4 (IDENT "wire_load", LPAR, STRING nam, RPAR), LCURLY, TLIST wlst, RCURLY) -> WireLoad(nam, List.rev_map rw' wlst)
| TUPLE4 (TUPLE3 (IDENT "leakage_power", LPAR, RPAR), LCURLY, TLIST lst, RCURLY) -> LPower (List.rev_map rw' lst)
| TUPLE4 (IDENT "fanout_length", LPAR, TLIST lst, RPAR) -> FanoutLength (List.rev_map (function NUM x -> float_of_string x | oth -> unhand := Some oth; failwith "fanout") lst)
| TUPLE4 (IDENT "voltage_map", LPAR, TLIST lst, RPAR) -> VoltageMap (List.rev_map rw' lst)
| TUPLE4 (IDENT "capacitive_load_unit", LPAR, TLIST lst, RPAR) -> CapLoadUnit (List.rev_map rw' lst)
| TUPLE4 (IDENT "library_features", LPAR, TLIST lst, RPAR) -> LibFeatures (List.rev_map rw' lst)

| TUPLE4 (IDENT "values", LPAR, TLIST lst, RPAR) -> CellValues (List.rev_map rw' lst)
| TUPLE4 (IDENT "values", LPAR, (STRING _ as x), RPAR) -> CellValues (rw' x :: [])
| TUPLE4 (IDENT "index_1", LPAR, STRING s, RPAR) -> CellIndex1 s
| TUPLE4 (IDENT "index_2", LPAR, STRING s, RPAR) -> CellIndex2 s
| TUPLE4 (IDENT ("technology" as p), LPAR, IDENT s, RPAR) -> Related(p,s)
| TUPLE4 (TUPLE4 (IDENT  oth, LPAR, IDENT nam, RPAR), LCURLY, TLIST lst, RCURLY) -> Other (oth, nam, List.rev_map rw' lst)
| TUPLE4 (IDENT "when", COLON, STRING s, SEMI) -> When s
| TUPLE4 (IDENT "timing_sense", COLON, IDENT s, SEMI) -> Sense s
| TUPLE4 (IDENT ("related_pin"|"related_ground_pin"|"related_power_pin"|"three_state"|"enable"|"data_in"|"clocked_on"|"next_state"|"preset"|"clear"|"state_function"|"default_wire_load"|"process_corner"|"pulling_resistance_unit"|"current_unit"|"voltage_unit"|"leakage_power_unit"|"time_unit"|"comment"|"revision"|"date" as p), COLON, STRING s, SEMI) -> Related (p, s)
| TUPLE4 (IDENT "sdf_cond", COLON, STRING s, SEMI) -> SDF_cond s
| TUPLE4 (IDENT "function", COLON, STRING s, SEMI) -> Function s
| TUPLE4 (IDENT "direction", COLON, IDENT s, SEMI) -> Direction s
| TUPLE4 (IDENT ("pg_type"|"voltage_name"|"timing_type"|"clock"|"nextstate_type"|"clear_preset_var2"|"clear_preset_var1"|"dont_use"|"dont_touch"|"clock_gate_out_pin"|"clock_gate_enable_pin"|"clock_gate_clock_pin"|"internal_node"|"clock_gating_integrated_cell"|"clock_gate_test_pin"|"variable_1"|"variable_2"|"default_operating_conditions"|"tree_type"|"in_place_swap_mode"|"delay_model" as p), COLON, IDENT s, SEMI) -> Related (p, s)
| TUPLE4 (IDENT ("drive_strength"|"area"|"cell_leakage_power"|"capacitance"|"max_capacitance"|"rise_capacitance"|"fall_capacitance"|"value"|"slope"|"resistance"|"default_max_transition"|"default_fanout_load"|"default_output_pin_cap"|"default_input_pin_cap"|"default_inout_pin_cap"|"default_cell_leakage_power"|"default_leakage_power_density"|"output_threshold_pct_rise"|"output_threshold_pct_fall"|"input_threshold_pct_rise"|"input_threshold_pct_fall"|"slew_derate_from_library"|"slew_upper_threshold_pct_rise"|"slew_upper_threshold_pct_fall"|"slew_lower_threshold_pct_rise"|"slew_lower_threshold_pct_fall"|"temperature"|"voltage"|"process"|"nom_voltage"|"nom_temperature"|"nom_process" as p), COLON, NUM s, SEMI) ->
    Parameter (p, float_of_string s)
| TUPLE4 (IDENT "define", LPAR, TLIST lst, RPAR) -> Define(List.rev_map (function IDENT id -> id | oth -> unhand := Some oth; failwith "define") lst)
| STRING s -> String s
| IDENT s -> String s
| NUM n ->  Parameter ("", float_of_string n)
| oth -> unhand := Some oth; failwith "rw'"

type attr = {cellhash: (string,((string * string) list * (string * string) list))Hashtbl.t}

let rec scan attr = function
| Library (_, lst) -> List.iter (scan attr) lst
| Related (p,q) -> ()
| LibFeatures _ -> ()
| CapLoadUnit _ -> ()
| Parameter (p,f) -> ()
| VoltageMap _ -> ()
| Define _ -> ()
| Other (_, _ , _) -> ()
| WireLoad _ -> ()
| LibCell (nam, lst) -> let pins = ref [] and fn = ref [] in List.iter (function
    | Other ("pg_pin", pin, lst) -> List.iter (function Related ("pg_type", pwr) -> pins := (pin,pwr) :: !pins | _ -> ()) lst
    | CellPin (pin, lst) -> List.iter (function Direction dir -> pins := (pin,dir) :: !pins | Function f -> fn := (pin,f) :: !fn | _ -> ()) lst
    | _ -> ()) lst; Hashtbl.add attr.cellhash nam (!pins,!fn)
| CellPin (_, _) -> ()
| Timing _ -> ()
| IPower _ -> ()
| LPower _ -> ()
| Power (_, _, _) -> ()
| CellValues _ -> ()
| CellIndex1 _ -> ()
| CellIndex2 _ -> ()
| String _ -> ()
| Sense _ -> ()
| SDF_cond _ -> ()
| Function _ -> ()
| Direction _ -> ()
| When _ -> ()
| Transition (_, _, _) -> ()
| FlipFlop (_, _) -> ()
| Latch (_, _) -> ()
| StateTable (_, _) -> ()
| FanoutLength _ -> ()

let rec strip = function
| TUPLE3(STRING _, arg1, arg2) -> TUPLE2(strip arg1, strip arg2)
| TUPLE4(STRING _, arg1, arg2, arg3) -> TUPLE3(strip arg1, strip arg2, strip arg3)
| TUPLE5(STRING _, arg1, arg2, arg3, arg4) -> TUPLE4(strip arg1, strip arg2, strip arg3, strip arg4)
| TUPLE6(STRING _, arg1, arg2, arg3, arg4, arg5) -> TUPLE5(strip arg1, strip arg2, strip arg3, strip arg4, strip arg5)
| TUPLE7(STRING _, arg1, arg2, arg3, arg4, arg5, arg6) -> TUPLE6(strip arg1, strip arg2, strip arg3, strip arg4, strip arg5, strip arg6)
| TUPLE8(STRING _, arg1, arg2, arg3, arg4, arg5, arg6, arg7) -> TUPLE7(strip arg1, strip arg2, strip arg3, strip arg4, strip arg5, strip arg6, strip arg7)
| TUPLE9(STRING _, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) -> TUPLE8(strip arg1, strip arg2, strip arg3, strip arg4, strip arg5, strip arg6, strip arg7, strip arg8)
| TUPLE10(STRING _, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) -> TUPLE9(strip arg1, strip arg2, strip arg3, strip arg4, strip arg5, strip arg6, strip arg7, strip arg8, strip arg9)
| (COMMA|LCURLY|RCURLY|EOF_TOKEN|SEMI|COLON|LPAR|RPAR|IDENT _|STRING _|NUM _) as x -> x
| oth -> unhand := Some oth; failwith "strip"

let rewrite arg =
  let p = parse arg in
  let p' = rw (strip p) in
  let rw = rw' p' in
  let cellhash = Hashtbl.create 255 in
  let _ = scan {cellhash} rw in
  rw,cellhash

let _ = if Array.length Sys.argv > 1 then ignore(parse Sys.argv.(1))
