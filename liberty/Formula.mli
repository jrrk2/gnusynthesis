
(* The type of tokens. *)

type token = 
  | VBAR
  | UNDERSCORE
  | UNARY
  | TUPLE9 of (token*token*token*token*token*token*token*token*token)
  | TUPLE8 of (token*token*token*token*token*token*token*token)
  | TUPLE7 of (token*token*token*token*token*token*token)
  | TUPLE6 of (token*token*token*token*token*token)
  | TUPLE5 of (token*token*token*token*token)
  | TUPLE4 of (token*token*token*token)
  | TUPLE3 of (token*token*token)
  | TUPLE2 of (token*token)
  | TUPLE10 of (token*token*token*token*token*token*token*token*token*token)
  | TLIST of (token list)
  | TILDE
  | STRING of (string)
  | SLIST of (string list)
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
  | NUM of (string)
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
  | IDENT of (string)
  | HASH
  | GREATER
  | ERROR_TOKEN
  | ERROR
  | EQ
  | EOF_TOKEN
  | END
  | EMPTY_TOKEN
  | ELIST of (token list)
  | DOUBLEQUOTE
  | DOT
  | DOLLAR
  | DIV
  | DEFAULT
  | CONS4 of (token*token*token*token)
  | CONS3 of (token*token*token)
  | CONS2 of (token*token)
  | CONS1 of (token)
  | COMMA
  | COLON
  | CARET
  | BACKSLASH
  | BACKQUOTE
  | AT
  | AMPERSAND
  | ACCEPT

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val ml_start: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (token)
