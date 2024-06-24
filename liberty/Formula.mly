%{
  open Parsing
  open Formula_types
  let declst = ref []
  let packhash_add id_t = Hashtbl.add packhash id_t ()
  let typehash_add id_t = Hashtbl.add typehash id_t ()
%}

%token  ACCEPT
%token  AMPERSAND
%token  AT
%token  BACKQUOTE
%token  BACKSLASH
%token  CARET
%token  COLON
%token  COMMA
%token <token> CONS1
%token <token*token> CONS2
%token <token*token*token> CONS3
%token <token*token*token*token> CONS4
%token  DEFAULT
%token  DIV
%token  DOLLAR
%token  DOT
%token  DOUBLEQUOTE
%token <token list> ELIST
%token  EMPTY_TOKEN
%token  END
%token  EOF_TOKEN
%token  EQ
%token  ERROR
%token  ERROR_TOKEN
%token  GREATER
%token  HASH
%token <string> IDENT
%token  KW_DEFINE
%token  KW_DEFINE_GROUP
%token  KW_FALSE
%token  KW_TRUE
%token  LBRACE
%token  LBRACK
%token  LCURLY
%token  LESS
%token  LINEFEED
%token  LPAR
%token  MINUS
%token  MULT
%token <string> NUM
%token  PERCENT
%token  PLING
%token  PLUS
%token  QUERY
%token  QUOTE
%token  RBRACE
%token  RBRACK
%token  RCURLY
%token  RPAR
%token  SEMI
%token <string list> SLIST
%token <string> STRING
%token  TILDE
%token <token list> TLIST
%token <token*token*token*token*token*token*token*token*token*token> TUPLE10
%token <token*token> TUPLE2
%token <token*token*token> TUPLE3
%token <token*token*token*token> TUPLE4
%token <token*token*token*token*token> TUPLE5
%token <token*token*token*token*token*token> TUPLE6
%token <token*token*token*token*token*token*token> TUPLE7
%token <token*token*token*token*token*token*token*token> TUPLE8
%token <token*token*token*token*token*token*token*token*token> TUPLE9
%token  UNARY
%token  UNDERSCORE
%token  VBAR
%type <token> ml_start
%start ml_start
%%

ml_start: orfact EOF_TOKEN { $1 }

expr:     orfact { $1 }

orfact:   andfact VBAR orfact { TUPLE3($1, VBAR, $3) }
        | andfact { $1 }

andfact:  caretfact AMPERSAND andfact { TUPLE3($1, AMPERSAND, $3) }
        | caretfact { $1 }

caretfact: fact CARET caretfact { TUPLE3($1, CARET, $3) }
        | fact { $1 }

fact:     IDENT { IDENT $1 }
        | NUM { NUM $1 }
        | LPAR expr RPAR { TUPLE3 (LPAR, $2, RPAR) }
	| PLING expr { TUPLE2(PLING, $2) }
	