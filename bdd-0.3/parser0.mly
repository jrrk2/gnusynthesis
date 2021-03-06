/**************************************************************************/
/*                                                                        */
/*  Copyright (C) Jean-Christophe Filliatre                               */
/*                                                                        */
/*  This software is free software; you can redistribute it and/or        */
/*  modify it under the terms of the GNU Library General Public           */
/*  License version 2, with the special exception on linking              */
/*  described in file LICENSE.                                            */
/*                                                                        */
/*  This software is distributed in the hope that it will be useful,      */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  */
/*                                                                        */
/**************************************************************************/

%{
  open Globals
  let loc () = symbol_start_pos (), symbol_end_pos ()

%}

%token <string> IDENT
%token AND OR
%token IMP EQUIV
%token NOT
%token LPAR RPAR
%token EOF 

%right IMP EQUIV
%left AND OR
%right NOT

%start file
%type <Globals.prop_t list> file

%%

file:
| formulas EOF { List.rev $1}
;
 
formulas:
| { [] }
| formulas formula { $2 :: $1 }

formula:
| IDENT { Pvar (Vparser.IDSTR $1) }
| formula IMP formula   { Pimp($1,$3) }
| formula EQUIV formula { Piff($1,$3) }
| formula AND formula   { Pand($1,$3) }
| formula OR formula    { Por($1,$3) }
| NOT formula           { Pnot($2) }
| LPAR formula RPAR     { $2 }
;
