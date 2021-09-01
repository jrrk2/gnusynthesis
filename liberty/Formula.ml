
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | VBAR
    | UNDERSCORE
    | UNARY
    | TUPLE9 of (
# 71 "Formula.mly"
       (token*token*token*token*token*token*token*token*token)
# 14 "Formula.ml"
  )
    | TUPLE8 of (
# 70 "Formula.mly"
       (token*token*token*token*token*token*token*token)
# 19 "Formula.ml"
  )
    | TUPLE7 of (
# 69 "Formula.mly"
       (token*token*token*token*token*token*token)
# 24 "Formula.ml"
  )
    | TUPLE6 of (
# 68 "Formula.mly"
       (token*token*token*token*token*token)
# 29 "Formula.ml"
  )
    | TUPLE5 of (
# 67 "Formula.mly"
       (token*token*token*token*token)
# 34 "Formula.ml"
  )
    | TUPLE4 of (
# 66 "Formula.mly"
       (token*token*token*token)
# 39 "Formula.ml"
  )
    | TUPLE3 of (
# 65 "Formula.mly"
       (token*token*token)
# 44 "Formula.ml"
  )
    | TUPLE2 of (
# 64 "Formula.mly"
       (token*token)
# 49 "Formula.ml"
  )
    | TUPLE10 of (
# 63 "Formula.mly"
       (token*token*token*token*token*token*token*token*token*token)
# 54 "Formula.ml"
  )
    | TLIST of (
# 62 "Formula.mly"
       (token list)
# 59 "Formula.ml"
  )
    | TILDE
    | STRING of (
# 60 "Formula.mly"
       (string)
# 65 "Formula.ml"
  )
    | SLIST of (
# 59 "Formula.mly"
       (string list)
# 70 "Formula.ml"
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
# 85 "Formula.ml"
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
# 102 "Formula.ml"
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
# 115 "Formula.ml"
  )
    | DOUBLEQUOTE
    | DOT
    | DOLLAR
    | DIV
    | DEFAULT
    | CONS4 of (
# 20 "Formula.mly"
       (token*token*token*token)
# 125 "Formula.ml"
  )
    | CONS3 of (
# 19 "Formula.mly"
       (token*token*token)
# 130 "Formula.ml"
  )
    | CONS2 of (
# 18 "Formula.mly"
       (token*token)
# 135 "Formula.ml"
  )
    | CONS1 of (
# 17 "Formula.mly"
       (token)
# 140 "Formula.ml"
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

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState15
  | MenhirState12
  | MenhirState7
  | MenhirState3
  | MenhirState1
  | MenhirState0

# 1 "Formula.mly"
  
  open Parsing
  open Formula_types
  let declst = ref []
  let packhash_add id_t = Hashtbl.add packhash id_t ()
  let typehash_add id_t = Hashtbl.add typehash id_t ()

# 181 "Formula.ml"

let rec _menhir_goto_orfact : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_orfact -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState1 | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71 * _menhir_state * 'tv_orfact) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69 * _menhir_state * 'tv_orfact) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_orfact)) = _menhir_stack in
        let _v : 'tv_expr = 
# 81 "Formula.mly"
                 ( _1 )
# 196 "Formula.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv67) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_expr) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState3 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv61 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv57 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv55 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
                let _v : 'tv_fact = 
# 94 "Formula.mly"
                         ( TUPLE3 (LPAR, _2, RPAR) )
# 220 "Formula.ml"
                 in
                _menhir_goto_fact _menhir_env _menhir_stack _menhir_s _v) : 'freshtv56)) : 'freshtv58)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv59 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)) : 'freshtv62)
        | MenhirState1 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv65 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv63 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_fact = 
# 95 "Formula.mly"
              ( TUPLE2(PLING, _2) )
# 239 "Formula.ml"
             in
            _menhir_goto_fact _menhir_env _menhir_stack _menhir_s _v) : 'freshtv64)) : 'freshtv66)
        | _ ->
            _menhir_fail ()) : 'freshtv68)) : 'freshtv70)) : 'freshtv72)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state * 'tv_orfact) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF_TOKEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv81 * _menhir_state * 'tv_orfact) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv79 * _menhir_state * 'tv_orfact) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_orfact)) = _menhir_stack in
            let _v : (
# 75 "Formula.mly"
      (token)
# 259 "Formula.ml"
            ) = 
# 79 "Formula.mly"
                           ( _1 )
# 263 "Formula.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv77) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 75 "Formula.mly"
      (token)
# 271 "Formula.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv75) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 75 "Formula.mly"
      (token)
# 279 "Formula.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv73) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 75 "Formula.mly"
      (token)
# 287 "Formula.ml"
            )) : (
# 75 "Formula.mly"
      (token)
# 291 "Formula.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv74)) : 'freshtv76)) : 'freshtv78)) : 'freshtv80)) : 'freshtv82)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv83 * _menhir_state * 'tv_orfact) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)) : 'freshtv86)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_andfact : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_andfact -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState1 | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * _menhir_state * 'tv_andfact) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | VBAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv43 * _menhir_state * 'tv_andfact) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
            | LPAR ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | NUM _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
            | PLING ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15) : 'freshtv44)
        | AMPERSAND | CARET | EOF_TOKEN | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv45 * _menhir_state * 'tv_andfact) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_andfact)) = _menhir_stack in
            let _v : 'tv_orfact = 
# 84 "Formula.mly"
                  ( _1 )
# 344 "Formula.ml"
             in
            _menhir_goto_orfact _menhir_env _menhir_stack _menhir_s _v) : 'freshtv46)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv47 * _menhir_state * 'tv_andfact) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)) : 'freshtv50)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv53 * _menhir_state * 'tv_andfact)) * _menhir_state * 'tv_andfact) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv51 * _menhir_state * 'tv_andfact)) * _menhir_state * 'tv_andfact) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_andfact)), _, (_3 : 'tv_andfact)) = _menhir_stack in
        let _v : 'tv_orfact = 
# 83 "Formula.mly"
                               ( TUPLE3(_1, VBAR, _3) )
# 363 "Formula.ml"
         in
        _menhir_goto_orfact _menhir_env _menhir_stack _menhir_s _v) : 'freshtv52)) : 'freshtv54)
    | _ ->
        _menhir_fail ()

and _menhir_goto_caretfact : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_caretfact -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState1 | MenhirState15 | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37 * _menhir_state * 'tv_caretfact) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMPERSAND ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv31 * _menhir_state * 'tv_caretfact) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
            | LPAR ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | NUM _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
            | PLING ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12) : 'freshtv32)
        | CARET | EOF_TOKEN | RPAR | VBAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv33 * _menhir_state * 'tv_caretfact) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_caretfact)) = _menhir_stack in
            let _v : 'tv_andfact = 
# 87 "Formula.mly"
                    ( _1 )
# 404 "Formula.ml"
             in
            _menhir_goto_andfact _menhir_env _menhir_stack _menhir_s _v) : 'freshtv34)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv35 * _menhir_state * 'tv_caretfact) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)) : 'freshtv38)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv41 * _menhir_state * 'tv_caretfact)) * _menhir_state * 'tv_caretfact) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv39 * _menhir_state * 'tv_caretfact)) * _menhir_state * 'tv_caretfact) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_caretfact)), _, (_3 : 'tv_caretfact)) = _menhir_stack in
        let _v : 'tv_andfact = 
# 86 "Formula.mly"
                                        ( TUPLE3(_1, AMPERSAND, _3) )
# 423 "Formula.ml"
         in
        _menhir_goto_andfact _menhir_env _menhir_stack _menhir_s _v) : 'freshtv40)) : 'freshtv42)
    | _ ->
        _menhir_fail ()

and _menhir_goto_fact : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_fact -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState1 | MenhirState15 | MenhirState12 | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state * 'tv_fact) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CARET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv19 * _menhir_state * 'tv_fact) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
            | LPAR ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | NUM _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
            | PLING ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv20)
        | AMPERSAND | EOF_TOKEN | RPAR | VBAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv21 * _menhir_state * 'tv_fact) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_fact)) = _menhir_stack in
            let _v : 'tv_caretfact = 
# 90 "Formula.mly"
               ( _1 )
# 464 "Formula.ml"
             in
            _menhir_goto_caretfact _menhir_env _menhir_stack _menhir_s _v) : 'freshtv22)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv23 * _menhir_state * 'tv_fact) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)) : 'freshtv26)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv29 * _menhir_state * 'tv_fact)) * _menhir_state * 'tv_fact) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv27 * _menhir_state * 'tv_fact)) * _menhir_state * 'tv_fact) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_fact)), _, (_3 : 'tv_fact)) = _menhir_stack in
        let _v : 'tv_caretfact = 
# 89 "Formula.mly"
                           ( TUPLE3(_1, CARET, _3) )
# 483 "Formula.ml"
         in
        _menhir_goto_caretfact _menhir_env _menhir_stack _menhir_s _v) : 'freshtv28)) : 'freshtv30)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv7 * _menhir_state * 'tv_andfact)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv9 * _menhir_state * 'tv_caretfact)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv11 * _menhir_state * 'tv_fact)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv18)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | LPAR ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | NUM _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | PLING ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 48 "Formula.mly"
       (string)
# 542 "Formula.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 48 "Formula.mly"
       (string)
# 552 "Formula.ml"
    )) : (
# 48 "Formula.mly"
       (string)
# 556 "Formula.ml"
    )) = _v in
    ((let _v : 'tv_fact = 
# 93 "Formula.mly"
              ( NUM _1 )
# 561 "Formula.ml"
     in
    _menhir_goto_fact _menhir_env _menhir_stack _menhir_s _v) : 'freshtv6)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | LPAR ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | NUM _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | PLING ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 35 "Formula.mly"
       (string)
# 587 "Formula.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 35 "Formula.mly"
       (string)
# 597 "Formula.ml"
    )) : (
# 35 "Formula.mly"
       (string)
# 601 "Formula.ml"
    )) = _v in
    ((let _v : 'tv_fact = 
# 92 "Formula.mly"
                ( IDENT _1 )
# 606 "Formula.ml"
     in
    _menhir_goto_fact _menhir_env _menhir_stack _menhir_s _v) : 'freshtv4)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and ml_start : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 75 "Formula.mly"
      (token)
# 625 "Formula.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LPAR ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NUM _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | PLING ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 269 "<standard.mly>"
  

# 655 "Formula.ml"
