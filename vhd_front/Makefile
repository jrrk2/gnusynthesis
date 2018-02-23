#
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

# default: ocamlyacc

export PARSER=`ocamlc -where`/../../bin/menhir --table --base VhdlParser # --trace
export PARSERINC=`ocamlc -where`/../menhirLib
export PARSERLIB=`ocamlc -where`/../menhirLib/menhirLib.cmo
export PARSERLIBX=`ocamlc -where`/../menhirLib/menhirLib.cmx

INCLUDE = -I $(PARSERINC) -I ../ver_front
COMBINED = # combined.cmo vconvert.cmo 
COMBINEDX = # combined.cmx vconvert.cmx
VHDO = VhdlTypes.cmo asctoken.cmo VhdlTree.cmo vabstraction.cmo $(COMBINED) VhdlParser.cmo VhdlLexer.cmo VhdlSettings.cmo VhdlMain.cmo rewrite.cmo
CMX = ../ver_front/ver_front.cmxa
VHDX = VhdlTypes.cmx asctoken.cmx VhdlTree.cmx vabstraction.cmx $(COMBINEDX) VhdlParser.cmx VhdlLexer.cmx VhdlSettings.cmx VhdlMain.cmx
CML = str.cma unix.cma nums.cma ../ver_front/ver_front.cma
CMLX = str.cmxa unix.cmxa nums.cmxa
MAING = v2abcgen.cmo

all: vgen vhdltop vhd_front.cma vhd_front.cmxa

vgen: $(VHDO) $(MAING)
	ocamlc.opt -g -o $@ $(PARSERLIB) $(CML) $(VHDO) $(MAING)

vhdltop: $(VHDO)
	ocamlmktop -g -o $@ `ocamlc -where`/{nums,str,unix}.cma $(PARSERLIB) ../ver_front/ver_front.cma $(VHDO)

vhd_front.cma: $(VHDO)
	ocamlc.opt -g -a -o $@ $(VHDO)

vhd_front.cmxa: $(VHDX)
	ocamlopt.opt -g -a -o $@ $(VHDX)

depend:
	ocamldep *.ml *.mli > .depend

clean:
	rm -rf *.cmi *.cmo *.cmx *.o vhdltop vgen vhd_front.a vhd_front.cma vhd_front.cmxa
	rm -rf vparser.ml vparser.mli vlexer.ml vlexer.mli VhdlParser.mli VhdlParser.ml ord.ml

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi .cmx

.ml.cmo:
	ocamlc.opt $(INCLUDE) -g -c $<

.mli.cmi:
	ocamlc.opt $(INCLUDE) -g -c $<

.mll.ml:
	ocamllex.opt $(LEXOPTS) $<

.ml.cmx:
	ocamlopt.opt $(INCLUDE) -g -c $<

VhdlParser.mli VhdlParser.ml: VhdlParser.mly
	$(PARSER) VhdlParser.mly

VhdlParser.cmo: VhdlParser.cmi
VhdlTypes.cmo: ../ver_front/ver_front.cma
VhdlTypes.cmx: ../ver_front/ver_front.cmxa

../ver_front/ver_front.cma ../ver_front/ver_front.cmxa:
	make -C ../ver_front

include .depend
