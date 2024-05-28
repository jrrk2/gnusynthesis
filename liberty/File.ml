
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
# 71 "File.mly"
       (token*token*token*token*token*token*token*token*token)
# 18 "File.ml"
  )
    | TUPLE8 of (
# 70 "File.mly"
       (token*token*token*token*token*token*token*token)
# 23 "File.ml"
  )
    | TUPLE7 of (
# 69 "File.mly"
       (token*token*token*token*token*token*token)
# 28 "File.ml"
  )
    | TUPLE6 of (
# 68 "File.mly"
       (token*token*token*token*token*token)
# 33 "File.ml"
  )
    | TUPLE5 of (
# 67 "File.mly"
       (token*token*token*token*token)
# 38 "File.ml"
  )
    | TUPLE4 of (
# 66 "File.mly"
       (token*token*token*token)
# 43 "File.ml"
  )
    | TUPLE3 of (
# 65 "File.mly"
       (token*token*token)
# 48 "File.ml"
  )
    | TUPLE2 of (
# 64 "File.mly"
       (token*token)
# 53 "File.ml"
  )
    | TUPLE10 of (
# 63 "File.mly"
       (token*token*token*token*token*token*token*token*token*token)
# 58 "File.ml"
  )
    | TLIST of (
# 62 "File.mly"
       (token list)
# 63 "File.ml"
  )
    | TILDE
    | STRING of (
# 60 "File.mly"
       (string)
# 69 "File.ml"
  )
    | SLIST of (
# 59 "File.mly"
       (string list)
# 74 "File.ml"
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
# 48 "File.mly"
       (string)
# 89 "File.ml"
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
# 35 "File.mly"
       (string)
# 106 "File.ml"
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
# 26 "File.mly"
       (token list)
# 119 "File.ml"
  )
    | DOUBLEQUOTE
    | DOT
    | DOLLAR
    | DIV
    | DEFAULT
    | CONS4 of (
# 20 "File.mly"
       (token*token*token*token)
# 129 "File.ml"
  )
    | CONS3 of (
# 19 "File.mly"
       (token*token*token)
# 134 "File.ml"
  )
    | CONS2 of (
# 18 "File.mly"
       (token*token)
# 139 "File.ml"
  )
    | CONS1 of (
# 17 "File.mly"
       (token)
# 144 "File.ml"
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

# 1 "File.mly"
  
  open Parsing
  open File_types
  let declst = ref []
  let packhash_add id_t = Hashtbl.add packhash id_t ()
  let typehash_add id_t = Hashtbl.add typehash id_t ()

# 167 "File.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_ml_start) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: ml_start. *)

  | MenhirState02 : (('s, _menhir_box_ml_start) _menhir_cell1_IDENT, _menhir_box_ml_start) _menhir_state
    (** State 02.
        Stack shape : IDENT.
        Start symbol: ml_start. *)

  | MenhirState10 : (('s, _menhir_box_ml_start) _menhir_cell1_s_or_i, _menhir_box_ml_start) _menhir_state
    (** State 10.
        Stack shape : s_or_i.
        Start symbol: ml_start. *)

  | MenhirState12 : ((('s, _menhir_box_ml_start) _menhir_cell1_IDENT, _menhir_box_ml_start) _menhir_cell1_param_list, _menhir_box_ml_start) _menhir_state
    (** State 12.
        Stack shape : IDENT param_list.
        Start symbol: ml_start. *)

  | MenhirState14 : (((('s, _menhir_box_ml_start) _menhir_cell1_IDENT, _menhir_box_ml_start) _menhir_cell1_param_list, _menhir_box_ml_start) _menhir_cell1_COMMA, _menhir_box_ml_start) _menhir_state
    (** State 14.
        Stack shape : IDENT param_list COMMA.
        Start symbol: ml_start. *)

  | MenhirState20 : (('s, _menhir_box_ml_start) _menhir_cell1_head, _menhir_box_ml_start) _menhir_state
    (** State 20.
        Stack shape : head.
        Start symbol: ml_start. *)

  | MenhirState23 : (('s, _menhir_box_ml_start) _menhir_cell1_KW_DEFINE_GROUP, _menhir_box_ml_start) _menhir_state
    (** State 23.
        Stack shape : KW_DEFINE_GROUP.
        Start symbol: ml_start. *)

  | MenhirState25 : ((('s, _menhir_box_ml_start) _menhir_cell1_KW_DEFINE_GROUP, _menhir_box_ml_start) _menhir_cell1_s_or_i, _menhir_box_ml_start) _menhir_state
    (** State 25.
        Stack shape : KW_DEFINE_GROUP s_or_i.
        Start symbol: ml_start. *)

  | MenhirState30 : (('s, _menhir_box_ml_start) _menhir_cell1_KW_DEFINE, _menhir_box_ml_start) _menhir_state
    (** State 30.
        Stack shape : KW_DEFINE.
        Start symbol: ml_start. *)

  | MenhirState32 : ((('s, _menhir_box_ml_start) _menhir_cell1_KW_DEFINE, _menhir_box_ml_start) _menhir_cell1_s_or_i, _menhir_box_ml_start) _menhir_state
    (** State 32.
        Stack shape : KW_DEFINE s_or_i.
        Start symbol: ml_start. *)

  | MenhirState34 : (((('s, _menhir_box_ml_start) _menhir_cell1_KW_DEFINE, _menhir_box_ml_start) _menhir_cell1_s_or_i, _menhir_box_ml_start) _menhir_cell1_s_or_i, _menhir_box_ml_start) _menhir_state
    (** State 34.
        Stack shape : KW_DEFINE s_or_i s_or_i.
        Start symbol: ml_start. *)

  | MenhirState39 : (('s, _menhir_box_ml_start) _menhir_cell1_IDENT, _menhir_box_ml_start) _menhir_state
    (** State 39.
        Stack shape : IDENT.
        Start symbol: ml_start. *)

  | MenhirState41 : (('s, _menhir_box_ml_start) _menhir_cell1_PLUS, _menhir_box_ml_start) _menhir_state
    (** State 41.
        Stack shape : PLUS.
        Start symbol: ml_start. *)

  | MenhirState43 : (('s, _menhir_box_ml_start) _menhir_cell1_MINUS, _menhir_box_ml_start) _menhir_state
    (** State 43.
        Stack shape : MINUS.
        Start symbol: ml_start. *)

  | MenhirState44 : (('s, _menhir_box_ml_start) _menhir_cell1_LPAR, _menhir_box_ml_start) _menhir_state
    (** State 44.
        Stack shape : LPAR.
        Start symbol: ml_start. *)

  | MenhirState48 : (('s, _menhir_box_ml_start) _menhir_cell1_expr, _menhir_box_ml_start) _menhir_state
    (** State 48.
        Stack shape : expr.
        Start symbol: ml_start. *)

  | MenhirState50 : (('s, _menhir_box_ml_start) _menhir_cell1_expr, _menhir_box_ml_start) _menhir_state
    (** State 50.
        Stack shape : expr.
        Start symbol: ml_start. *)

  | MenhirState52 : (('s, _menhir_box_ml_start) _menhir_cell1_expr, _menhir_box_ml_start) _menhir_state
    (** State 52.
        Stack shape : expr.
        Start symbol: ml_start. *)

  | MenhirState54 : (('s, _menhir_box_ml_start) _menhir_cell1_expr, _menhir_box_ml_start) _menhir_state
    (** State 54.
        Stack shape : expr.
        Start symbol: ml_start. *)

  | MenhirState63 : (('s, _menhir_box_ml_start) _menhir_cell1_IDENT, _menhir_box_ml_start) _menhir_state
    (** State 63.
        Stack shape : IDENT.
        Start symbol: ml_start. *)

  | MenhirState66 : ((('s, _menhir_box_ml_start) _menhir_cell1_head, _menhir_box_ml_start) _menhir_cell1_statements, _menhir_box_ml_start) _menhir_state
    (** State 66.
        Stack shape : head statements.
        Start symbol: ml_start. *)


and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (token)

and ('s, 'r) _menhir_cell1_head = 
  | MenhirCell1_head of 's * ('s, 'r) _menhir_state * (token)

and ('s, 'r) _menhir_cell1_param_list = 
  | MenhirCell1_param_list of 's * ('s, 'r) _menhir_state * (token)

and ('s, 'r) _menhir_cell1_s_or_i = 
  | MenhirCell1_s_or_i of 's * ('s, 'r) _menhir_state * (token)

and ('s, 'r) _menhir_cell1_statements = 
  | MenhirCell1_statements of 's * ('s, 'r) _menhir_state * (token)

and ('s, 'r) _menhir_cell1_COMMA = 
  | MenhirCell1_COMMA of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_IDENT = 
  | MenhirCell1_IDENT of 's * ('s, 'r) _menhir_state * (
# 35 "File.mly"
       (string)
# 298 "File.ml"
)

and ('s, 'r) _menhir_cell1_KW_DEFINE = 
  | MenhirCell1_KW_DEFINE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_KW_DEFINE_GROUP = 
  | MenhirCell1_KW_DEFINE_GROUP of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAR = 
  | MenhirCell1_LPAR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_MINUS = 
  | MenhirCell1_MINUS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_PLUS = 
  | MenhirCell1_PLUS of 's * ('s, 'r) _menhir_state

and _menhir_box_ml_start = 
  | MenhirBox_ml_start of (token) [@@unboxed]

let _menhir_action_01 =
  fun _1 ->
    (
# 117 "File.mly"
              ( (NUM _1) )
# 324 "File.ml"
     : (token))

let _menhir_action_02 =
  fun _1 ->
    (
# 118 "File.mly"
          ( (_1) )
# 332 "File.ml"
     : (token))

let _menhir_action_03 =
  fun _1 _3 ->
    (
# 119 "File.mly"
                       ( TUPLE4(STRING("attr_val33"),_1,COLON,_3) )
# 340 "File.ml"
     : (token))

let _menhir_action_04 =
  fun () ->
    (
# 120 "File.mly"
           ( (KW_TRUE) )
# 348 "File.ml"
     : (token))

let _menhir_action_05 =
  fun () ->
    (
# 121 "File.mly"
            ( (KW_FALSE) )
# 356 "File.ml"
     : (token))

let _menhir_action_06 =
  fun _1 ->
    (
# 123 "File.mly"
                      ( (STRING _1) )
# 364 "File.ml"
     : (token))

let _menhir_action_07 =
  fun () ->
    (
# 124 "File.mly"
           ( (KW_TRUE) )
# 372 "File.ml"
     : (token))

let _menhir_action_08 =
  fun () ->
    (
# 125 "File.mly"
            ( (KW_FALSE) )
# 380 "File.ml"
     : (token))

let _menhir_action_09 =
  fun _1 ->
    (
# 126 "File.mly"
        ( (_1) )
# 388 "File.ml"
     : (token))

let _menhir_action_10 =
  fun _1 ->
    (
# 100 "File.mly"
                        ( TUPLE3(STRING("complex_attr19"),_1,SEMI) )
# 396 "File.ml"
     : (token))

let _menhir_action_11 =
  fun _1 ->
    (
# 101 "File.mly"
        ( (_1) )
# 404 "File.ml"
     : (token))

let _menhir_action_12 =
  fun _3 _5 _7 ->
    (
# 110 "File.mly"
                                                                  ( TUPLE10(STRING("define27"),KW_DEFINE,LPAR,_3,COMMA,_5,COMMA,_7,RPAR,SEMI) )
# 412 "File.ml"
     : (token))

let _menhir_action_13 =
  fun _3 _5 ->
    (
# 112 "File.mly"
                                                                 ( TUPLE8(STRING("define_group28"),KW_DEFINE_GROUP,LPAR,_3,COMMA,_5,RPAR,SEMI) )
# 420 "File.ml"
     : (token))

let _menhir_action_14 =
  fun _1 _3 ->
    (
# 128 "File.mly"
                     ( TUPLE4(STRING("expr40"),_1,PLUS,_3) )
# 428 "File.ml"
     : (token))

let _menhir_action_15 =
  fun _1 _3 ->
    (
# 129 "File.mly"
                   ( TUPLE4(STRING("expr41"),_1,MINUS,_3) )
# 436 "File.ml"
     : (token))

let _menhir_action_16 =
  fun _1 _3 ->
    (
# 130 "File.mly"
                  ( TUPLE4(STRING("expr42"),_1,MULT,_3) )
# 444 "File.ml"
     : (token))

let _menhir_action_17 =
  fun _1 _3 ->
    (
# 131 "File.mly"
                 ( TUPLE4(STRING("expr43"),_1,DIV,_3) )
# 452 "File.ml"
     : (token))

let _menhir_action_18 =
  fun _2 ->
    (
# 132 "File.mly"
                  ( TUPLE4(STRING("expr44"),LPAR,_2,RPAR) )
# 460 "File.ml"
     : (token))

let _menhir_action_19 =
  fun _2 ->
    (
# 133 "File.mly"
              ( TUPLE3(STRING("expr45"),MINUS,_2) )
# 468 "File.ml"
     : (token))

let _menhir_action_20 =
  fun _2 ->
    (
# 134 "File.mly"
             ( TUPLE3(STRING("expr46"),PLUS,_2) )
# 476 "File.ml"
     : (token))

let _menhir_action_21 =
  fun _1 ->
    (
# 135 "File.mly"
       ( (NUM _1) )
# 484 "File.ml"
     : (token))

let _menhir_action_22 =
  fun _1 ->
    (
# 136 "File.mly"
         ( (IDENT _1) )
# 492 "File.ml"
     : (token))

let _menhir_action_23 =
  fun _1 ->
    (
# 82 "File.mly"
                    ( (_1) )
# 500 "File.ml"
     : (token))

let _menhir_action_24 =
  fun _1 _3 ->
    (
# 84 "File.mly"
                                             ( TUPLE5(STRING("group4"),_1,LCURLY,_3,RCURLY) )
# 508 "File.ml"
     : (token))

let _menhir_action_25 =
  fun _1 ->
    (
# 85 "File.mly"
                              ( TUPLE4(STRING("group6"),_1,LCURLY,RCURLY) )
# 516 "File.ml"
     : (token))

let _menhir_action_26 =
  fun _1 _3 ->
    (
# 103 "File.mly"
                                         ( TUPLE5(STRING("head22"),IDENT _1,LPAR,_3,RPAR) )
# 524 "File.ml"
     : (token))

let _menhir_action_27 =
  fun _1 ->
    (
# 104 "File.mly"
                   ( TUPLE4(STRING("head23"),IDENT _1,LPAR,RPAR) )
# 532 "File.ml"
     : (token))

let _menhir_action_28 =
  fun _1 ->
    (
# 80 "File.mly"
                         ( TUPLE3(STRING("ml_start0"),_1,EOF_TOKEN) )
# 540 "File.ml"
     : (token))

let _menhir_action_29 =
  fun _1 ->
    (
# 106 "File.mly"
                     ( (_1) )
# 548 "File.ml"
     : (token))

let _menhir_action_30 =
  fun _1 _3 ->
    (
# 107 "File.mly"
                             ( TUPLE4(STRING("param_list25"),_1,COMMA,_3) )
# 556 "File.ml"
     : (token))

let _menhir_action_31 =
  fun _1 _2 ->
    (
# 108 "File.mly"
                       ( TUPLE3(STRING("param_list26"),_1,_2) )
# 564 "File.ml"
     : (token))

let _menhir_action_32 =
  fun _1 ->
    (
# 114 "File.mly"
               ( (STRING _1) )
# 572 "File.ml"
     : (token))

let _menhir_action_33 =
  fun _1 ->
    (
# 115 "File.mly"
         ( (IDENT _1) )
# 580 "File.ml"
     : (token))

let _menhir_action_34 =
  fun _1 _3 ->
    (
# 96 "File.mly"
                                                    ( TUPLE5(STRING("simple_attr15"),IDENT _1,COLON,_3,SEMI) )
# 588 "File.ml"
     : (token))

let _menhir_action_35 =
  fun _1 _3 ->
    (
# 97 "File.mly"
                             ( TUPLE4(STRING("simple_attr16"),IDENT _1,COLON,_3) )
# 596 "File.ml"
     : (token))

let _menhir_action_36 =
  fun _1 _3 ->
    (
# 98 "File.mly"
                                       ( TUPLE5(STRING("simple_attr18"),IDENT _1,EQ,_3,SEMI) )
# 604 "File.ml"
     : (token))

let _menhir_action_37 =
  fun _1 ->
    (
# 90 "File.mly"
                       ( (_1) )
# 612 "File.ml"
     : (token))

let _menhir_action_38 =
  fun _1 ->
    (
# 91 "File.mly"
                ( (_1) )
# 620 "File.ml"
     : (token))

let _menhir_action_39 =
  fun _1 ->
    (
# 92 "File.mly"
          ( (_1) )
# 628 "File.ml"
     : (token))

let _menhir_action_40 =
  fun _1 ->
    (
# 93 "File.mly"
                ( (_1) )
# 636 "File.ml"
     : (token))

let _menhir_action_41 =
  fun _1 ->
    (
# 94 "File.mly"
         ( (_1) )
# 644 "File.ml"
     : (token))

let _menhir_action_42 =
  fun _1 ->
    (
# 87 "File.mly"
                      ( (_1) )
# 652 "File.ml"
     : (token))

let _menhir_action_43 =
  fun _1 _2 ->
    (
# 88 "File.mly"
                        ( TUPLE3(STRING("statements8"),_1,_2) )
# 660 "File.ml"
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
  
  let _menhir_run_77 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_23 _1 in
      match (_tok : MenhirBasics.token) with
      | EOF_TOKEN ->
          let _1 = _v in
          let _v = _menhir_action_28 _1 in
          MenhirBox_ml_start _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_02 : type  ttv_stack. (ttv_stack, _menhir_box_ml_start) _menhir_cell1_IDENT -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STRING _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState02
      | RPAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _v = _menhir_action_27 _1 in
          _menhir_goto_head _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | NUM _v ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState02
      | KW_TRUE ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | KW_FALSE ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | IDENT _v ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState02
      | _ ->
          _eRR ()
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_32 _1 in
      _menhir_goto_s_or_i _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_s_or_i : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState34 ->
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState32 ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState30 ->
          _menhir_run_31 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState25 ->
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState23 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState10 ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState12 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState14 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState02 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_35 : type  ttv_stack. (((ttv_stack, _menhir_box_ml_start) _menhir_cell1_KW_DEFINE, _menhir_box_ml_start) _menhir_cell1_s_or_i, _menhir_box_ml_start) _menhir_cell1_s_or_i -> _ -> _ -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SEMI ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let MenhirCell1_s_or_i (_menhir_stack, _, _5) = _menhir_stack in
              let MenhirCell1_s_or_i (_menhir_stack, _, _3) = _menhir_stack in
              let MenhirCell1_KW_DEFINE (_menhir_stack, _menhir_s) = _menhir_stack in
              let _7 = _v in
              let _v = _menhir_action_12 _3 _5 _7 in
              let _1 = _v in
              let _v = _menhir_action_39 _1 in
              _menhir_goto_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_statement : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState20 ->
          _menhir_run_76 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState66 ->
          _menhir_run_68 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_76 : type  ttv_stack. ((ttv_stack, _menhir_box_ml_start) _menhir_cell1_head as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_42 _1 in
      _menhir_goto_statements _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_statements : type  ttv_stack. ((ttv_stack, _menhir_box_ml_start) _menhir_cell1_head as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RCURLY ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_head (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_24 _1 _3 in
          _menhir_goto_group _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | KW_DEFINE_GROUP ->
          let _menhir_stack = MenhirCell1_statements (_menhir_stack, _menhir_s, _v) in
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState66
      | KW_DEFINE ->
          let _menhir_stack = MenhirCell1_statements (_menhir_stack, _menhir_s, _v) in
          _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState66
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_statements (_menhir_stack, _menhir_s, _v) in
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState66
      | _ ->
          _eRR ()
  
  and _menhir_goto_group : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_77 _menhir_stack _v _tok
      | MenhirState20 ->
          _menhir_run_72 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState66 ->
          _menhir_run_72 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_72 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_41 _1 in
      _menhir_goto_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_22 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_KW_DEFINE_GROUP (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          let _menhir_s = MenhirState23 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STRING _v ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_08 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_33 _1 in
      _menhir_goto_s_or_i _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_29 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_KW_DEFINE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          let _menhir_s = MenhirState30 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STRING _v ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_38 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_s = MenhirState39 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STRING _v ->
              _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | PLUS ->
              _menhir_run_41 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUM _v ->
              _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAR ->
              _menhir_run_44 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | KW_TRUE ->
              _menhir_run_58 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | KW_FALSE ->
              _menhir_run_59 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_45 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | COLON ->
          let _menhir_s = MenhirState63 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STRING _v ->
              _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | PLUS ->
              _menhir_run_41 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUM _v ->
              _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAR ->
              _menhir_run_44 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | KW_TRUE ->
              _menhir_run_58 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | KW_FALSE ->
              _menhir_run_59 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_45 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_40 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_06 _1 in
      _menhir_goto_attr_val_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_attr_val_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState63 ->
          _menhir_run_64 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState39 ->
          _menhir_run_61 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_64 : type  ttv_stack. (ttv_stack, _menhir_box_ml_start) _menhir_cell1_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_34 _1 _3 in
          _menhir_goto_simple_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | IDENT _ | KW_DEFINE | KW_DEFINE_GROUP | RCURLY ->
          let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_35 _1 _3 in
          _menhir_goto_simple_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_simple_attr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_37 _1 in
      _menhir_goto_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_61 : type  ttv_stack. (ttv_stack, _menhir_box_ml_start) _menhir_cell1_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_36 _1 _3 in
          _menhir_goto_simple_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_41 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PLUS (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState41 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          _menhir_run_41 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUM _v ->
          _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_44 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_45 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_42 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_21 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState63 ->
          _menhir_run_60 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState39 ->
          _menhir_run_60 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState41 ->
          _menhir_run_57 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState43 ->
          _menhir_run_56 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState54 ->
          _menhir_run_55 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState52 ->
          _menhir_run_53 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState50 ->
          _menhir_run_51 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState48 ->
          _menhir_run_49 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState44 ->
          _menhir_run_46 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_60 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_48 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_50 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_52 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_54 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IDENT _ | KW_DEFINE | KW_DEFINE_GROUP | RCURLY | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_09 _1 in
          _menhir_goto_attr_val_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_48 : type  ttv_stack. (ttv_stack, _menhir_box_ml_start) _menhir_cell1_expr -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState48 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          _menhir_run_41 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUM _v ->
          _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_44 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_45 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_43 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState43 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          _menhir_run_41 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUM _v ->
          _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_44 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_45 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_44 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState44 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          _menhir_run_41 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUM _v ->
          _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_44 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_45 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_45 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_22 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_50 : type  ttv_stack. (ttv_stack, _menhir_box_ml_start) _menhir_cell1_expr -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState50 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          _menhir_run_41 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUM _v ->
          _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_44 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_45 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_52 : type  ttv_stack. (ttv_stack, _menhir_box_ml_start) _menhir_cell1_expr -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState52 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          _menhir_run_41 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUM _v ->
          _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_44 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_45 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_54 : type  ttv_stack. (ttv_stack, _menhir_box_ml_start) _menhir_cell1_expr -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState54 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          _menhir_run_41 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUM _v ->
          _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_44 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_45 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_57 : type  ttv_stack. ((ttv_stack, _menhir_box_ml_start) _menhir_cell1_PLUS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_48 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_50 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_52 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_54 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IDENT _ | KW_DEFINE | KW_DEFINE_GROUP | RCURLY | RPAR | SEMI ->
          let MenhirCell1_PLUS (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_20 _2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_56 : type  ttv_stack. ((ttv_stack, _menhir_box_ml_start) _menhir_cell1_MINUS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_48 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_50 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_52 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_54 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IDENT _ | KW_DEFINE | KW_DEFINE_GROUP | RCURLY | RPAR | SEMI ->
          let MenhirCell1_MINUS (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_19 _2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_55 : type  ttv_stack. ((ttv_stack, _menhir_box_ml_start) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_48 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_50 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_52 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_54 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IDENT _ | KW_DEFINE | KW_DEFINE_GROUP | RCURLY | RPAR | SEMI ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_17 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_53 : type  ttv_stack. ((ttv_stack, _menhir_box_ml_start) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_48 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_50 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_52 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_54 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IDENT _ | KW_DEFINE | KW_DEFINE_GROUP | RCURLY | RPAR | SEMI ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_15 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_51 : type  ttv_stack. ((ttv_stack, _menhir_box_ml_start) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_48 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_50 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_52 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_54 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IDENT _ | KW_DEFINE | KW_DEFINE_GROUP | RCURLY | RPAR | SEMI ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_16 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_49 : type  ttv_stack. ((ttv_stack, _menhir_box_ml_start) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_48 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_50 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_52 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_54 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IDENT _ | KW_DEFINE | KW_DEFINE_GROUP | RCURLY | RPAR | SEMI ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_14 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_46 : type  ttv_stack. ((ttv_stack, _menhir_box_ml_start) _menhir_cell1_LPAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAR (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_18 _2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_48 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MULT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_50 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_52 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_54 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_58 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_07 () in
      _menhir_goto_attr_val_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_59 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_08 () in
      _menhir_goto_attr_val_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_68 : type  ttv_stack. ((ttv_stack, _menhir_box_ml_start) _menhir_cell1_head, _menhir_box_ml_start) _menhir_cell1_statements -> _ -> _ -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_statements (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_43 _1 _2 in
      _menhir_goto_statements _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_33 : type  ttv_stack. (((ttv_stack, _menhir_box_ml_start) _menhir_cell1_KW_DEFINE, _menhir_box_ml_start) _menhir_cell1_s_or_i as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_s_or_i (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_s = MenhirState34 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STRING _v ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_31 : type  ttv_stack. ((ttv_stack, _menhir_box_ml_start) _menhir_cell1_KW_DEFINE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_s_or_i (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_s = MenhirState32 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STRING _v ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_26 : type  ttv_stack. ((ttv_stack, _menhir_box_ml_start) _menhir_cell1_KW_DEFINE_GROUP, _menhir_box_ml_start) _menhir_cell1_s_or_i -> _ -> _ -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SEMI ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let MenhirCell1_s_or_i (_menhir_stack, _, _3) = _menhir_stack in
              let MenhirCell1_KW_DEFINE_GROUP (_menhir_stack, _menhir_s) = _menhir_stack in
              let _5 = _v in
              let _v = _menhir_action_13 _3 _5 in
              let _1 = _v in
              let _v = _menhir_action_40 _1 in
              _menhir_goto_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_24 : type  ttv_stack. ((ttv_stack, _menhir_box_ml_start) _menhir_cell1_KW_DEFINE_GROUP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_s_or_i (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_s = MenhirState25 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STRING _v ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_11 : type  ttv_stack. (ttv_stack, _menhir_box_ml_start) _menhir_cell1_s_or_i -> _ -> _ -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_s_or_i (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_03 _1 _3 in
      _menhir_goto_attr_val _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_attr_val : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState02 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState12 ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState14 ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_17 : type  ttv_stack. ((ttv_stack, _menhir_box_ml_start) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_29 _1 in
      _menhir_goto_param_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_param_list : type  ttv_stack. ((ttv_stack, _menhir_box_ml_start) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STRING _v_0 ->
          let _menhir_stack = MenhirCell1_param_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState12
      | RPAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_26 _1 _3 in
          _menhir_goto_head _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | NUM _v_1 ->
          let _menhir_stack = MenhirCell1_param_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState12
      | KW_TRUE ->
          let _menhir_stack = MenhirCell1_param_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
      | KW_FALSE ->
          let _menhir_stack = MenhirCell1_param_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
      | IDENT _v_2 ->
          let _menhir_stack = MenhirCell1_param_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState12
      | COMMA ->
          let _menhir_stack = MenhirCell1_param_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, MenhirState12) in
          let _menhir_s = MenhirState14 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STRING _v ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NUM _v ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | KW_TRUE ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | KW_FALSE ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_head : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState20 ->
          _menhir_run_70 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState66 ->
          _menhir_run_70 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState00 ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_70 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_10 _1 in
          _menhir_goto_complex_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | LCURLY ->
          let _menhir_stack = MenhirCell1_head (_menhir_stack, _menhir_s, _v) in
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IDENT _ | KW_DEFINE | KW_DEFINE_GROUP | RCURLY ->
          let _1 = _v in
          let _v = _menhir_action_11 _1 in
          _menhir_goto_complex_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_complex_attr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_38 _1 in
      _menhir_goto_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_20 : type  ttv_stack. (ttv_stack, _menhir_box_ml_start) _menhir_cell1_head -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RCURLY ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_head (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _v = _menhir_action_25 _1 in
          _menhir_goto_group _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | KW_DEFINE_GROUP ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState20
      | KW_DEFINE ->
          _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState20
      | IDENT _v ->
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState20
      | _ ->
          _eRR ()
  
  and _menhir_run_19 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_head (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LCURLY ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_05 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_01 _1 in
      _menhir_goto_attr_val _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_06 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_04 () in
      _menhir_goto_attr_val _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_07 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_05 () in
      _menhir_goto_attr_val _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_16 : type  ttv_stack. ((ttv_stack, _menhir_box_ml_start) _menhir_cell1_IDENT, _menhir_box_ml_start) _menhir_cell1_param_list -> _ -> _ -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_param_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_31 _1 _2 in
      _menhir_goto_param_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_15 : type  ttv_stack. (((ttv_stack, _menhir_box_ml_start) _menhir_cell1_IDENT, _menhir_box_ml_start) _menhir_cell1_param_list, _menhir_box_ml_start) _menhir_cell1_COMMA -> _ -> _ -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_COMMA (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_param_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_30 _1 _3 in
      _menhir_goto_param_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_09 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_ml_start) _menhir_state -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _menhir_stack = MenhirCell1_s_or_i (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState10 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STRING _v ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | COMMA | IDENT _ | KW_FALSE | KW_TRUE | NUM _ | RPAR | STRING _ ->
          let _1 = _v in
          let _v = _menhir_action_02 _1 in
          _menhir_goto_attr_val _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_ml_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState00 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAR ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
end

let ml_start =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_ml_start v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
