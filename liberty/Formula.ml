
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | VBAR
    | UNDERSCORE
    | UNARY
    | TUPLE9 of (
# 71 "Formula.mly"
       (token*token*token*token*token*token*token*token*token)
# 18 "Formula.ml"
  )
    | TUPLE8 of (
# 70 "Formula.mly"
       (token*token*token*token*token*token*token*token)
# 23 "Formula.ml"
  )
    | TUPLE7 of (
# 69 "Formula.mly"
       (token*token*token*token*token*token*token)
# 28 "Formula.ml"
  )
    | TUPLE6 of (
# 68 "Formula.mly"
       (token*token*token*token*token*token)
# 33 "Formula.ml"
  )
    | TUPLE5 of (
# 67 "Formula.mly"
       (token*token*token*token*token)
# 38 "Formula.ml"
  )
    | TUPLE4 of (
# 66 "Formula.mly"
       (token*token*token*token)
# 43 "Formula.ml"
  )
    | TUPLE3 of (
# 65 "Formula.mly"
       (token*token*token)
# 48 "Formula.ml"
  )
    | TUPLE2 of (
# 64 "Formula.mly"
       (token*token)
# 53 "Formula.ml"
  )
    | TUPLE10 of (
# 63 "Formula.mly"
       (token*token*token*token*token*token*token*token*token*token)
# 58 "Formula.ml"
  )
    | TLIST of (
# 62 "Formula.mly"
       (token list)
# 63 "Formula.ml"
  )
    | TILDE
    | STRING of (
# 60 "Formula.mly"
       (string)
# 69 "Formula.ml"
  )
    | SLIST of (
# 59 "Formula.mly"
       (string list)
# 74 "Formula.ml"
  )
    | SEMI
    | RPAR
    | RCURLY
    | RBRACK
    | RBRACE
    | QUOTE
    | QUERY
    | PLUS
    | PLING
    | PERCENT
    | NUM of (
# 48 "Formula.mly"
       (string)
# 89 "Formula.ml"
  )
    | MULT
    | MINUS
    | LPAR
    | LINEFEED
    | LESS
    | LCURLY
    | LBRACK
    | LBRACE
    | KW_TRUE
    | KW_FALSE
    | KW_DEFINE_GROUP
    | KW_DEFINE
    | IDENT of (
# 35 "Formula.mly"
       (string)
# 106 "Formula.ml"
  )
    | HASH
    | GREATER
    | ERROR_TOKEN
    | ERROR
    | EQ
    | EOF_TOKEN
    | END
    | EMPTY_TOKEN
    | ELIST of (
# 26 "Formula.mly"
       (token list)
# 119 "Formula.ml"
  )
    | DOUBLEQUOTE
    | DOT
    | DOLLAR
    | DIV
    | DEFAULT
    | CONS4 of (
# 20 "Formula.mly"
       (token*token*token*token)
# 129 "Formula.ml"
  )
    | CONS3 of (
# 19 "Formula.mly"
       (token*token*token)
# 134 "Formula.ml"
  )
    | CONS2 of (
# 18 "Formula.mly"
       (token*token)
# 139 "Formula.ml"
  )
    | CONS1 of (
# 17 "Formula.mly"
       (token)
# 144 "Formula.ml"
  )
    | COMMA
    | COLON
    | CARET
    | BACKSLASH
    | BACKQUOTE
    | AT
    | AMPERSAND
    | ACCEPT
  
end

include MenhirBasics

# 1 "Formula.mly"
  
  open Parsing
  open Formula_types
  let declst = ref []
  let packhash_add id_t = Hashtbl.add packhash id_t ()
  let typehash_add id_t = Hashtbl.add typehash id_t ()

# 167 "Formula.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_ml_start) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: ml_start. *)

  | MenhirState01 : (('s, _menhir_box_ml_start) _menhir_cell1_PLING, _menhir_box_ml_start) _menhir_state
    (** State 01.
        Stack shape : PLING.
        Start symbol: ml_start. *)

  | MenhirState03 : (('s, _menhir_box_ml_start) _menhir_cell1_LPAR, _menhir_box_ml_start) _menhir_state
    (** State 03.
        Stack shape : LPAR.
        Start symbol: ml_start. *)

  | MenhirState07 : (('s, _menhir_box_ml_start) _menhir_cell1_fact, _menhir_box_ml_start) _menhir_state
    (** State 07.
        Stack shape : fact.
        Start symbol: ml_start. *)

  | MenhirState12 : (('s, _menhir_box_ml_start) _menhir_cell1_caretfact, _menhir_box_ml_start) _menhir_state
    (** State 12.
        Stack shape : caretfact.
        Start symbol: ml_start. *)

  | MenhirState15 : (('s, _menhir_box_ml_start) _menhir_cell1_andfact, _menhir_box_ml_start) _menhir_state
    (** State 15.
        Stack shape : andfact.
        Start symbol: ml_start. *)


and ('s, 'r) _menhir_cell1_andfact = 
  | MenhirCell1_andfact of 's * ('s, 'r) _menhir_state * (token)

and ('s, 'r) _menhir_cell1_caretfact = 
  | MenhirCell1_caretfact of 's * ('s, 'r) _menhir_state * (token)

and ('s, 'r) _menhir_cell1_fact = 
  | MenhirCell1_fact of 's * ('s, 'r) _menhir_state * (token)

and ('s, 'r) _menhir_cell1_LPAR = 
  | MenhirCell1_LPAR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_PLING = 
  | MenhirCell1_PLING of 's * ('s, 'r) _menhir_state

and _menhir_box_ml_start = 
  | MenhirBox_ml_start of (token) [@@unboxed]

let _menhir_action_01 =
  fun _1 _3 ->
    (
# 86 "Formula.mly"
                                        ( TUPLE3(_1, AMPERSAND, _3) )
# 224 "Formula.ml"
     : (token))

let _menhir_action_02 =
  fun _1 ->
    (
# 87 "Formula.mly"
                    ( _1 )
# 232 "Formula.ml"
     : (token))

let _menhir_action_03 =
  fun _1 _3 ->
    (
# 89 "Formula.mly"
                           ( TUPLE3(_1, CARET, _3) )
# 240 "Formula.ml"
     : (token))

let _menhir_action_04 =
  fun _1 ->
    (
# 90 "Formula.mly"
               ( _1 )
# 248 "Formula.ml"
     : (token))

let _menhir_action_05 =
  fun _1 ->
    (
# 81 "Formula.mly"
                 ( _1 )
# 256 "Formula.ml"
     : (token))

let _menhir_action_06 =
  fun _1 ->
    (
# 92 "Formula.mly"
                ( IDENT _1 )
# 264 "Formula.ml"
     : (token))

let _menhir_action_07 =
  fun _1 ->
    (
# 93 "Formula.mly"
              ( NUM _1 )
# 272 "Formula.ml"
     : (token))

let _menhir_action_08 =
  fun _2 ->
    (
# 94 "Formula.mly"
                         ( TUPLE3 (LPAR, _2, RPAR) )
# 280 "Formula.ml"
     : (token))

let _menhir_action_09 =
  fun _2 ->
    (
# 95 "Formula.mly"
              ( TUPLE2(PLING, _2) )
# 288 "Formula.ml"
     : (token))

let _menhir_action_10 =
  fun _1 ->
    (
# 79 "Formula.mly"
                           ( _1 )
# 296 "Formula.ml"
     : (token))

let _menhir_action_11 =
  fun _1 _3 ->
    (
# 83 "Formula.mly"
                               ( TUPLE3(_1, VBAR, _3) )
# 304 "Formula.ml"
     : (token))

let _menhir_action_12 =
  fun _1 ->
    (
# 84 "Formula.mly"
                  ( _1 )
# 312 "Formula.ml"
     : (token))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ACCEPT ->
        "ACCEPT"
    | AMPERSAND ->
        "AMPERSAND"
    | AT ->
        "AT"
    | BACKQUOTE ->
        "BACKQUOTE"
    | BACKSLASH ->
        "BACKSLASH"
    | CARET ->
        "CARET"
    | COLON ->
        "COLON"
    | COMMA ->
        "COMMA"
    | CONS1 _ ->
        "CONS1"
    | CONS2 _ ->
        "CONS2"
    | CONS3 _ ->
        "CONS3"
    | CONS4 _ ->
        "CONS4"
    | DEFAULT ->
        "DEFAULT"
    | DIV ->
        "DIV"
    | DOLLAR ->
        "DOLLAR"
    | DOT ->
        "DOT"
    | DOUBLEQUOTE ->
        "DOUBLEQUOTE"
    | ELIST _ ->
        "ELIST"
    | EMPTY_TOKEN ->
        "EMPTY_TOKEN"
    | END ->
        "END"
    | EOF_TOKEN ->
        "EOF_TOKEN"
    | EQ ->
        "EQ"
    | ERROR ->
        "ERROR"
    | ERROR_TOKEN ->
        "ERROR_TOKEN"
    | GREATER ->
        "GREATER"
    | HASH ->
        "HASH"
    | IDENT _ ->
        "IDENT"
    | KW_DEFINE ->
        "KW_DEFINE"
    | KW_DEFINE_GROUP ->
        "KW_DEFINE_GROUP"
    | KW_FALSE ->
        "KW_FALSE"
    | KW_TRUE ->
        "KW_TRUE"
    | LBRACE ->
        "LBRACE"
    | LBRACK ->
        "LBRACK"
    | LCURLY ->
        "LCURLY"
    | LESS ->
        "LESS"
    | LINEFEED ->
        "LINEFEED"
    | LPAR ->
        "LPAR"
    | MINUS ->
        "MINUS"
    | MULT ->
        "MULT"
    | NUM _ ->
        "NUM"
    | PERCENT ->
        "PERCENT"
    | PLING ->
        "PLING"
    | PLUS ->
        "PLUS"
    | QUERY ->
        "QUERY"
    | QUOTE ->
        "QUOTE"
    | RBRACE ->
        "RBRACE"
    | RBRACK ->
        "RBRACK"
    | RCURLY ->
        "RCURLY"
    | RPAR ->
        "RPAR"
    | SEMI ->
        "SEMI"
    | SLIST _ ->
        "SLIST"
    | STRING _ ->
        "STRING"
    | TILDE ->
        "TILDE"
    | TLIST _ ->
        "TLIST"
    | TUPLE10 _ ->
        "TUPLE10"
    | TUPLE2 _ ->
        "TUPLE2"
    | TUPLE3 _ ->
        "TUPLE3"
    | TUPLE4 _ ->
        "TUPLE4"
    | TUPLE5 _ ->
        "TUPLE5"
    | TUPLE6 _ ->
        "TUPLE6"
    | TUPLE7 _ ->
        "TUPLE7"
    | TUPLE8 _ ->
        "TUPLE8"
    | TUPLE9 _ ->
        "TUPLE9"
    | UNARY ->
        "UNARY"
    | UNDERSCORE ->
        "UNDERSCORE"
    | VBAR ->
        "VBAR"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_18 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF_TOKEN ->
          let _1 = _v in
          let _v = _menhir_action_10 _1 in
          MenhirBox_ml_start _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PLING (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState01 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PLING ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUM _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAR ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_07 _1 in
      _menhir_goto_fact _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_fact : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState07 ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState00 ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState01 ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState15 ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState12 ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_08 : type  ttv_stack. (ttv_stack, _menhir_box_ml_start) _menhir_cell1_fact -> _ -> _ -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_fact (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_03 _1 _3 in
      _menhir_goto_caretfact _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_caretfact : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState12 ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState00 ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState01 ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState15 ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_13 : type  ttv_stack. (ttv_stack, _menhir_box_ml_start) _menhir_cell1_caretfact -> _ -> _ -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_caretfact (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_01 _1 _3 in
      _menhir_goto_andfact _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_andfact : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState15 ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState00 ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState01 ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_16 : type  ttv_stack. (ttv_stack, _menhir_box_ml_start) _menhir_cell1_andfact -> _ -> _ -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_andfact (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_11 _1 _3 in
      _menhir_goto_orfact _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_orfact : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_18 _menhir_stack _v _tok
      | MenhirState01 ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_05 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_05 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState01 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState03 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_17 : type  ttv_stack. (ttv_stack, _menhir_box_ml_start) _menhir_cell1_PLING -> _ -> _ -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_PLING (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_09 _2 in
      _menhir_goto_fact _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_09 : type  ttv_stack. (ttv_stack, _menhir_box_ml_start) _menhir_cell1_LPAR -> _ -> _ -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAR (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_08 _2 in
          _menhir_goto_fact _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_14 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VBAR ->
          let _menhir_stack = MenhirCell1_andfact (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState15 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | PLING ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUM _v ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAR ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | AMPERSAND | CARET | EOF_TOKEN | RPAR ->
          let _1 = _v in
          let _v = _menhir_action_12 _1 in
          _menhir_goto_orfact _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState03 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PLING ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUM _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAR ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_04 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_06 _1 in
      _menhir_goto_fact _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_11 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | AMPERSAND ->
          let _menhir_stack = MenhirCell1_caretfact (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState12 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | PLING ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUM _v ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAR ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | CARET | EOF_TOKEN | RPAR | VBAR ->
          let _1 = _v in
          let _v = _menhir_action_02 _1 in
          _menhir_goto_andfact _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_06 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | CARET ->
          let _menhir_stack = MenhirCell1_fact (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState07 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | PLING ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUM _v ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAR ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | AMPERSAND | EOF_TOKEN | RPAR | VBAR ->
          let _1 = _v in
          let _v = _menhir_action_04 _1 in
          _menhir_goto_caretfact _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState00 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PLING ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUM _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAR ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
end

let ml_start =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_ml_start v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
