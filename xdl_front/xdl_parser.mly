// Torc - Copyright 2011-2013 University of Southern California.  All Rights Reserved.
// $HeadURL: https://svn.code.sf.net/p/torc-isi/code/trunk/src/torc/physical/xdl/parser.yy $
 // $Id: parser.yy 16 2013-11-12 22:50:42Z nsteiner $

 // This program is free software: you can redistribute it and/or modify it under the terms of the 
 // GNU General Public License as published by the Free Software Foundation, either version 3 of the 
 // License, or (at your option) any later version.
 // 
 // This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 // without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See 
 // the GNU General Public License for more details.
 // 
 // You should have received a copy of the GNU General Public License along with this program.  If 
 // not, see <http://www.gnu.org/licenses/>.


     // Torc - Copyright 2011-2013 University of Southern California.  All Rights Reserved.
     // $HeadURL: https://svn.code.sf.net/p/torc-isi/code/trunk/src/torc/physical/xdl/parser.yy $
      // $Id: parser.yy 16 2013-11-12 22:50:42Z nsteiner $

      // This program is free software: you can redistribute it and/or modify it under the terms of the 
      // GNU General Public License as published by the Free Software Foundation, either version 3 of the 
      // License, or (at your option) any later version.
      // 
      // This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
      // without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See 
      // the GNU General Public License for more details.
      // 
      // You should have received a copy of the GNU General Public License along with this program.  If 
      // not, see <http://www.gnu.org/licenses/>.

%{
open Classify
%}

      //  \brief Grammar start symbol is named "start".
%start start
%type <token list> start
%type <token list> module_elements

      // ------------------------------------------------------------------------------------------------
      // ----------------------------------- Begin XDL grammar tokens -----------------------------------
      // ------------------------------------------------------------------------------------------------

%token NORMAL
%token UNKNOWN
%token CONFIG
%token NEWLINE
%token<int> PATTERN
%token<char> ILLEGAL
%token<token list> TLIST
%token<string list> SLIST
// Generic void
%token EMPTY

%token<string> ID

%token DESIGN_PROP
%token BUS_INFO
%token PIN_INFO
%token DUPLEQUOTE
%token ESCDQUOTE
%token SEMICOLON
%token DUPLECOLON
%token COLON
%token COMMA
%token SPACE
%token DOT
%token END
%token EOL

%token DESIGN
%token MODULE
%token ENDMODULE
%token PORT
%token INST
%token NET
%token CFG
%token PLACED
%token UNPLACED
%token BONDED
%token UNBONDED
%token POWER
%token GROUND
%token INPIN
%token OUTPIN
%token PIP
%token ROUTETHROUGH
%token<string> STRING
%token CFGSETTING
%token CFGNAME
%token CFGVALUE
%token<string> IDENTIFIER
%token<string> XDLVERSION
%token<string> INUM
%token<string> FLOATNUM
%token BIDIRUNBUF
%token BIDIRUNIBUF
%token BIDIRBIBUF
%token UNIDIRBUF

%token WORD
%token CHAR
%token QUOTE
%token OBRACE
%token EBRACE
%token OPAREN
%token EPAREN
%token OBRACK
%token EBRACK
%token OBUS
%token EBUS
%token HASH
%token HASHFF
%token HASHON
%token HASHOFF
%token HASHLUT
%token HASHRAM
%token HASHAND2L
%token EQUALS
%token SLASH
%token BACKSLASH
%token TILDE
%token AT
%token PLUS
%token DASH
%token STAR
%token BUSDELIM
%token ENDOFFILE

      // ------------------------------------------------------------------------------------------------
      // ------------------------------------ End XDL grammar tokens ------------------------------------
      // ------------------------------------------------------------------------------------------------

%%

nl		                	: /* empty */ { [] }
					| NEWLINE nl { NEWLINE :: $2 }
					;

      // ------------------------------------------------------------------------------------------------
      // ------------------------------------ Begin XDL grammar rules -----------------------------------
      // ------------------------------------------------------------------------------------------------

start: nl design_statement statements { $2 @ $3 }
					;

      // ---------------------------------------------- top ---------------------------------------------

statements			        : ENDOFFILE { [] }
					| statement statements  { if $1 <> NEWLINE then $1 :: $2 else $2 }
					;

statement                               : module_block { $1 }
					| instance_statement { $1 }
					| net_statement { $1 }
					| NEWLINE { NEWLINE }
					;

      // -------------------------------------------- design --------------------------------------------

      //# design <design_name> <part> <ncd version>;             
      //# design <design_name> <device> <package> <speed> <ncd_version>

design_statement: DESIGN design_name device_info version optional_config { [DESIGN;$2;$3;$4;$5]; }
					;

design_name				: double_quoted_string { $1 }
					;

device_info				: part { $1 }
					| device package speed { TLIST[$1;$2;$3]; }
					;

part					: IDENTIFIER speed { TLIST[ID $1;$2] }
					;

device				        : IDENTIFIER { ID $1; }
					;

package				        : IDENTIFIER { ID $1; }
					;

speed				        : DASH INUM { ID $2; }
					;

version
					: /* empty */ { EMPTY }
					| XDLVERSION { ID $1; }
					;


      // -------------------------------------------- modules -------------------------------------------

      //# module <name> <inst_name> ;
      //#   port <name> <inst_name> <inst_pin> ;
      //#   ...
      //#   ...
      //#   instance ... ;
      //#   ...
      //#   ...
      //#   net ... ;
      //#   ...
      //#   ...
      //# endmodule <name> ;

module_block	                        : module_statement module_elements endmodule_statement { TLIST[$1; TLIST $2; $3] }
					;

module_elements
					: /* empty */ { [] }
					| module_element module_elements { $1 :: $2 }
					;

module_element				: port_statement { $1 }
					| instance_statement { $1 }
					| net_statement { $1 }
					;

module_statement			: MODULE double_quoted_string double_quoted_string optional_config { TLIST[MODULE; $2; $3; $4]; }
					;

endmodule_statement			: ENDMODULE double_quoted_string SEMICOLON { TLIST[ENDMODULE; $2]; }
					;

// --------------------------------------------- ports --------------------------------------------

port_statement				: PORT double_quoted_string double_quoted_string double_quoted_string SEMICOLON { TLIST[PORT; $2; $3; $4]; }
					;

// ------------------------------------------- instances ------------------------------------------

//# instance <name> <sitedef>, placed <tile> <site>, cfg <string> ;
//# instance <name> <sitedef>, placed <tile> <site>, module "instantiation_name" "module_name" 
//		"instance_name", cfg <string> ;
//# instance <name> <sitedef>, unplaced, cfg <string> ;

instance_statement			: INST double_quoted_string double_quoted_string COMMA instance_placement reference_and_config { TLIST[INST;$2;$3;$5;$6]; }
					;

instance_placement			: UNPLACED instance_bonding { TLIST[UNPLACED;$2] }
					| PLACED IDENTIFIER string instance_bonding {  TLIST[PLACED;ID $2;ID $3;$4] }
					;

instance_bonding			: /* empty */ { UNKNOWN; }
					| BONDED { BONDED; }
					| UNBONDED { UNBONDED; }
					;

reference_and_config			: SEMICOLON { EMPTY }
					| COMMA nl config { $3 }
					| COMMA module_reference SEMICOLON { $2 }
					| COMMA module_reference COMMA nl config { TLIST[$2;$5] }
					;

module_reference			: MODULE double_quoted_string double_quoted_string double_quoted_string { TLIST[MODULE; $2; $3; $4] }
					;

// --------------------------------------------- nets ---------------------------------------------

//# net <name> <type>,
//#	  outpin <inst_name> <inst_pin>,
//#	  ...
//#	  ...
//#	  inpin <inst_name> <inst_pin>,
//#	  ...
//#	  ...
//#	  pip <tile> <wire0> <dir> <wire1> , # [<rt>]
//#	  ...
//#	  ...
//#	  ;

net_statement				: NET double_quoted_string net_power net_pins_or_pips_or_cfg { TLIST[NET;$2;$3;TLIST $4] }
					;

net_power				: COMMA { NORMAL }
					| NEWLINE COMMA { NORMAL }
					| POWER COMMA { POWER }
					| GROUND COMMA { GROUND }
					;

net_pins_or_pips_or_cfg			: SEMICOLON { [] }
					| net_pin_or_pip_or_cfg net_pins_or_pips_or_cfg { if $1 = NEWLINE then $2 else $1 :: $2 }
					;

net_pin_or_pip_or_cfg			: net_pin COMMA { $1 }
					| net_pip COMMA { $1 }
					| routethrough COMMA { $1 }
					| net_cfg { $1 }
					| NEWLINE { NEWLINE }
					;

net_cfg					: config { $1 }
					;

// ---------------------------------------------- pins --------------------------------------------

net_pin					: net_pin_direction double_quoted_string IDENTIFIER { TLIST[$1;$2;ID $3] }
					;

net_pin_direction			: INPIN { INPIN; }
					| OUTPIN { OUTPIN; }
					;

// ---------------------------------------------- pips --------------------------------------------

net_pip					: PIP IDENTIFIER IDENTIFIER net_pip_direction IDENTIFIER { TLIST[PIP;ID $2;ID $3;$4;ID $5]; }
					;

net_pip_direction			: BIDIRUNBUF // ==   bidirectional, unbuffered
						{ BIDIRUNBUF }
					| BIDIRUNIBUF // =>   bidirectional, buffered in one direction
						{ BIDIRUNIBUF }
					| BIDIRBIBUF // =-   bidirectional, buffered in both directions
						{ BIDIRBIBUF }
					| UNIDIRBUF // ->   directional, buffered
						{ UNIDIRBUF }
					;

routethrough				: /* empty */ { EMPTY }
					| ROUTETHROUGH COLON IDENTIFIER COLON IDENTIFIER double_quoted_string IDENTIFIER net_pip_direction IDENTIFIER
					    { TLIST[ROUTETHROUGH;ID $3;ID $5;$6;ID $7;$8;ID $9] }
					;

// -------------------------------------------- configs -------------------------------------------

optional_config				: /* empty */ { EMPTY }
					| COMMA nl config { $3 }
					;

config					: CFG DUPLEQUOTE config_tuples { TLIST (CFG::$3) }
					;

config_tuples				: SEMICOLON { [] }
					| COMMA { [] }
					| config_tuple_lst config_tuples { if $1 <> [] then TLIST $1 :: $2 else $2 }
					;

config_tuple_lst			: NEWLINE { [] }
					| DUPLEQUOTE { [] }
					| config_tuple config_tuple_lst { $1 :: $2 }
					;

config_tuple				: DESIGN_PROP { DESIGN_PROP }
					| BUS_INFO { BUS_INFO }
					| PIN_INFO { PIN_INFO }
					| DUPLECOLON { DUPLECOLON }
					| COLON { COLON }
					| COMMA { COMMA }
					| SPACE { SPACE }
					| ESCDQUOTE { ESCDQUOTE }
					| OBUS { OBUS }
					| EBUS { EBUS }
					| OPAREN { OPAREN }
					| EPAREN { EPAREN }
					| OBRACK { OBRACK }
					| EBRACK { EBRACK }
					| HASHFF { HASHFF }
					| HASHON { HASHON }
					| HASHOFF { HASHOFF }
					| HASHLUT { HASHLUT }
					| HASHRAM { HASHRAM }
					| HASHAND2L { HASHAND2L }
					| TILDE { TILDE }
					| AT { AT }
					| FLOATNUM { FLOATNUM $1 }
					| PLUS { PLUS }
					| STAR { STAR }
					| DASH { DASH }
					| SLASH { SLASH }
					| EQUALS { EQUALS }
					| BUSDELIM { BUSDELIM }
					| config_path { match $1 with [] -> EMPTY | hd::[] -> ID hd | oth -> SLIST oth }
					;

config_path                             : IDENTIFIER { [ $1 ] }
					| INUM { [ $1 ] }
                                        | IDENTIFIER DOT config_path { $1 :: "." :: $3 }
					;
      
// -------------------------------------------- strings -------------------------------------------

double_quoted_string			: DUPLEQUOTE string DUPLEQUOTE { ID $2 }
					;

string					: STRING { $1 }
					| IDENTIFIER { $1 }
					| IDENTIFIER IDENTIFIER string { $1^" "^$2^$3 }
					| IDENTIFIER DASH IDENTIFIER { $1^"-"^$3 }
					| OBRACK INUM EBRACK string { "["^$2^"]"^$4 }
					| OBRACK IDENTIFIER INST EBRACK { "["^$2^" instance]" }
					| IDENTIFIER OBUS INUM EBUS { $1^"<"^$3^">" }
					| IDENTIFIER OBUS INUM EBUS SLASH string { $1^"<"^$3^">/"^$6 }
					| IDENTIFIER OBUS INUM EBUS INUM { $1^"<"^$3^">"^$5 }
					| IDENTIFIER OBUS INUM EBUS INUM string { $1^"<"^$3^">"^$5^$6 }
					| IDENTIFIER OBUS INUM EBUS INUM SLASH string{ $1^"<"^$3^">"^$5^"/"^$7 }
					| IDENTIFIER OBUS INUM EBUS string { $1^"<"^$3^">"^$5 }
					| IDENTIFIER OBRACK INUM EBRACK string { $1^"["^$3^"]"^$5 }
					| IDENTIFIER DOT string { $1^"."^$3 }
					| IDENTIFIER DOT INUM { $1^"."^$3 }
					| IDENTIFIER DOT INUM string { $1^"."^$3^$4 }
					| IDENTIFIER SLASH string { $1^"/"^$3 }
					| IDENTIFIER BACKSLASH string { $1^"\\"^$3 }
					| IDENTIFIER COLON string { $1^":"^$3 }
					| IDENTIFIER COLON BACKSLASH string { $1^":\\"^$4 }
					;

unreachable				: CFGNAME { $1 }
					| CFGNAME { $1 }
					| CFGSETTING { $1 }
					| CFGVALUE { $1 }
					| CHAR { $1 }
					| EBRACE { $1 }
					| EMPTY { $1 }
					| END { $1 }
					| EOL { $1 }
					| HASH { $1 }
					| ID { $1 }
					| ILLEGAL { $1 }
					| NORMAL { $1 }
					| OBRACE { $1 }
					| QUOTE { $1 }
					| SLIST { $1 }
					| TLIST { $1 }
					| UNKNOWN { $1 }
					| WORD { $1 }
					| CONFIG { $1 }
					| PATTERN { $1 }

// ------------------------------------------------------------------------------------------------
// ------------------------------------- End XDL grammar rules ------------------------------------
// ------------------------------------------------------------------------------------------------
