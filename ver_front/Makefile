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

default: ocamlyacc

menhir:
	make all \
	PARSER=/usr/bin/menhir \
	PARSEROPT="--table -b vparser --explain" \
	PARSERINC=/usr/lib/ocaml/menhirLib \
	PARSERLIB=/usr/lib/ocaml/menhirLib/menhirLib.cmo \
	PARSERLIBX=/usr/lib/ocaml/menhirLib/menhirLib.cmx \

ocamlyacc:
	make all \
	PARSER=ocamlyacc \
	PARSEROPT="-b vparser" \
	PARSERINC:=`ocamlc -where` \

#LEXOPTS = -ml

CMO = ord.cmo globals.cmo vparser.cmo setup.cmo vlexer.cmo dump.cmo const.cmo check.cmo semantics.cmo vparse.cmo
CMX = ord.cmx globals.cmx vparser.cmx setup.cmx vlexer.cmx dump.cmx const.cmx check.cmx semantics.cmx vparse.cmx
CML = str.cma unix.cma nums.cma
CMLX = str.cmxa unix.cmxa nums.cmxa
INC = -I $(PARSERINC)

all: ver_front.cma ver_front.cmxa vtop

vtop: $(CMO)
	ocamlmktop -g -o $@ $(CML) -custom $(PARSERLIB) $(CMO)

ver_front.cma: $(CMO)
	ocamlc.opt -a -g -o $@ $(CMO)

ver_front.cmxa: $(CMX)
	ocamlopt.opt -a -g -p -o $@ $(CMX)

vparser.mli vparser.ml: idhash.cmi grammar.mly
	rm -f vparser.mli
	$(PARSER) $(PARSEROPT) grammar.mly
	ocamlc.opt -i $(INC) -g -c vparser.ml >vparser.tmp
	mv vparser.tmp vparser.mli

vparser.cmo: vparser.ml
	ocamlc.opt $(INC) -g -c $<

vparser.cmx: vparser.ml
	ocamlopt.opt $(INC) -g -p -c $<

depend: vparser.ml vlexer.ml
	ocamldep *.ml *.mli *.mll *.mly > .depend

clean_menhir: clean
	git clean -f ../menhir-20110201

clean:
	rm -rf *.cmi *.cmo *.cmx *.o *.a *.cma *.cmxa vchk vtop vparser
	rm -rf vparser.ml vparser.mli vlexer.ml vlexer.mli vparser.mli vparser.ml ord.ml

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi .cmx

.ml.cmo:
	ocamlc.opt $(INC) -g -c $<

.mli.cmi:
	ocamlc.opt $(INC) -g -c $<

.mll.ml:
	ocamllex.opt $(LEXOPTS) $<

.ml.cmx:
	ocamlopt.opt $(INC) -g -p -c $<

ord.ml: ord.sh vparser.cmi
	sh ord.sh

vparser.cmi: vparser.mli
	ocamlc.opt $(INC) -g -c vparser.mli

ocamlyacc:

include .depend
