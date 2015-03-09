#    <vscr - Verilog converter to abc format.>
#    Copyright (C) <2011,2012>  <Jonathan Richard Robert Kimmitt>
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

PARSER=menhir --explain # --trace
PARSERINC:=`ocamlc -where`

CMO = ord.cmo xdl_parser.cmo setup.cmo xdl_lexer.cmo dump.cmo xdl_parse.cmo latin.cmo classify.cmo
CMX = ord.cmx xdl_parser.cmx setup.cmx xdl_lexer.cmx dump.cmx xdl_parse.cmx latin.cmx classify.cmx
CML = str.cma unix.cma nums.cma
CMLX = str.cmxa unix.cmxa nums.cmxa
INC = -I $(PARSERINC)

all: xdl_front.cma xdl_front.cmxa xdl_top tester

xdl_top: $(CMO)
	ocamlmktop -g -o $@ $(CML) -custom $(PARSERLIB) $(CMO)

tester: $(CMX) tester.cmx
	ocamlopt.opt -g -o $@ $(CMLX) $(PARSERLIB) $(CMX) tester.cmx

gentuple: latin.cmo gentuple.cmo
	ocamlc.opt -o $@ latin.cmo gentuple.cmo

tuple.mli: gentuple
	./gentuple > $@

xdl_front.cma: $(CMO)
	ocamlc.opt -a -g -o $@ $(CMO)

xdl_front.cmxa: $(CMX)
	ocamlopt.opt -a -g -p -o $@ $(CMX)

xdl_parser.mli xdl_parser.ml: xdl_parser.mly
	rm -f xdl_parser.mli xdl_parser.conflicts
	$(PARSER) $(PARSEROPT) xdl_parser.mly
#	ocamlc.opt -i $(INC) -g -c xdl_parser.ml >xdl_parser.tmp
#	mv xdl_parser.tmp xdl_parser.mli

xdl_parser.cmo: xdl_parser.ml
	ocamlc.opt $(INC) -g -c $<

xdl_parser.cmx: xdl_parser.ml
	ocamlopt.opt $(INC) -g -p -c $<

xdl_lexer.ml: xdl_parser.cmi xdl_lexer.mll
	ocamllex.opt xdl_lexer.mll

depend: xdl_parser.ml xdl_lexer.ml
	ocamldep *.ml *.mli *.mll *.mly > .depend

clean:
	rm -rf *.cmi *.cmo *.cmx *.o *.a *.cma *.cmxa vchk xdl_top tester
	rm -rf xdl_parser.ml xdl_parser.mli xdl_lexer.ml xdl_lexer.mli xdl_parser.mli xdl_parser.ml ord.ml

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi .cmx

.ml.cmo:
	ocamlc.opt $(INC) -g -c $<

.mli.cmi:
	ocamlc.opt $(INC) -g -c $<

.mll.ml:
	ocamllex.opt $(LEXOPTS) $<

.ml.cmx:
	ocamlopt.opt $(INC) -g -p -c $<

ord.ml: ord.sh xdl_parser.cmi
	sh ord.sh

xdl_parser.cmi: xdl_parser.mli
	ocamlc.opt $(INC) -g -c xdl_parser.mli

ocamlyacc:

include .depend
