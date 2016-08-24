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
#
OCAMLC:=ocamlc.opt
OCAMLOPT:=ocamlopt.opt
TOPLOOP:=mytoploop
PARSERINC:=`ocamlc -where`

default: ocamlyacc

menhir:
	make all \
	PARSER=/usr/bin/menhir \
	PARSEROPT="--table --base VhdlParser" \
	PARSERINC=/usr/lib/ocaml/menhirLib \

ocamlyacc:
	make all \
	PARSER=ocamlyacc \
	PARSEROPT="-v -b VhdlParser" \
	PARSERINC:=`ocamlc -where` \

.PHONY: libs

OS:=$(shell uname -s)
BASE:=$(shell ocamlc -where)

CAMLIDL = $(BASE)/../camlidl
PARSERLIB= $(BASE)/../menhirLib/menhirLib.cmo
PARSERLIBX= $(BASE)/../menhirLib/menhirLib.cmx
INCLUDES = -I $(PARSERINC) -I ../ver_front -I ../vhd_front -I ../bdd-0.3 -I ../edif_parser -I ../theorem_proving -I $(BASE)/../zip -I $(BASE)/caml -I $(BASE)/threads -I $(BASE)/../xmlm
CMO = ../ver_front/ver_front.cma ../bdd-0.3/decision.cma ../vhd_front/vhd_front.cma $(BASE)/../xmlm/xmlm.cma $(BASE)/../zip/zip.cma ../edif_parser/edif2.cma ../theorem_proving/theorem_proving.cma
CMX = ../ver_front/ver_front.cmxa ../bdd-0.3/decision.cmxa ../vhd_front/vhd_front.cmxa $(BASE)/../xmlm/xmlm.cmxa $(BASE)/../zip/zip.cmxa ../edif_parser/edif2.cmxa ../theorem_proving/theorem_proving.cmxa
SRC = shortlist.ml xmlio.ml read_library.ml count.ml erc.ml minimap.ml aigerin.ml aigerout.ml flatten.ml verilogout.ml write_blif.ml milefout.ml extractregs.ml rtl2rtl.ml miter.ml tolerate.ml sop.ml mapping.ml hls.ml elaborate.ml read_blif.ml edifout.ml license.ml gencnf.ml smvout.ml # xmlout.ml
FLATTENO = $(subst .ml,.cmo,$(SRC))
FLATTENX = $(subst .ml,.cmx,$(SRC))
FLATTENA = $(subst .ml,.o,$(SRC))
SCRIPTO = $(FLATTENO) main.cmo
SCRIPTX = $(FLATTENX) main.cmx
CML =  str.cma unix.cma threads.cma nums.cma #toplevellib.cma
CMLX = str.cmxa unix.cmxa threads.cmxa nums.cmxa # wrapper.cmxa
LDFLAGO = -cclib -lstdc++ -cclib -lreadline -cclib -L$(BASE)/../zip/ -cclib -lcamlidl -cclib -lstdc++ -cclib -L$(CAMLIDL)
LDFLAGX = -lstdc++ -lreadline -L$(BASE)/../zip/ -lcamlzip -lm -lz -L$(CAMLIDL) -lcamlidl -lstdc++

all: libs vhier vscr.opt vtop vscr vflatten.cmxa vflatten

libs: ../ver_front/ver_front.cma ../ver_front/ver_front.cmxa ../bdd-0.3/decision.cma ../bdd-0.3/decision.cmxa
	make -C ../edif_parser

vflatten: $(CMX) $(FLATTENX) vflatten.cmx
	$(OCAMLOPT) -o $@ $(INCLUDES) unix.cmxa nums.cmxa $(PARSERLIBX) $(CMX) $(FLATTENX) vflatten.cmx $(LDFLAGO)

vhier: ../ver_front/ver_front.cmxa ../vhd_front/vhd_front.cmxa shortlist.cmx xmlio.cmx read_library.cmx count.cmx vhier.cmx
	$(OCAMLOPT) -o $@ $(INCLUDES) unix.cmxa nums.cmxa $(PARSERLIBX) ../ver_front/ver_front.cmxa ../vhd_front/vhd_front.cmxa $(BASE)/../xmlm/xmlm.cmxa shortlist.cmx xmlio.cmx read_library.cmx count.cmx vhier.cmx

vtop: $(CMO) $(SCRIPTO) $(TOPLOOP).cmi $(TOPLOOP).cmo
	ocamlmktop -g -custom -o $@ $(INCLUDES) $(PARSERLIB) $(CML) $(CMO) $(TOPLOOP).cmo $(SCRIPTO) $(LDFLAGO)

vscr: $(CMO) $(SCRIPTO)
	$(OCAMLC) -g -custom -o $@ $(INCLUDES) $(PARSERLIB) $(CML) $(CMO) $(SCRIPTO) $(LDFLAGO)

vscr.opt: ../ver_front/ver_front.cmxa ../vhd_front/vhd_front.cmxa $(PARSERLIBX) $(FLATTENX) main.cmx
	$(OCAMLOPT) -o $@ $(INCLUDES) $(PARSERLIBX) $(CMLX) $(CMX) $(FLATTENX) $(LDFLAGO) main.cmx

vflatten.cmxa: $(CMX) $(FLATTENX)
	$(OCAMLOPT) -a -g -p -o $@ $(FLATTENX)

depend:
	ocamldep *.ml *.mli > .depend

clean:
	rm -rf *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa vscr vscr.opt vtop vhier
	rm -rf vparser.ml vparser.mli vlexer.ml vlexer.mli vparser.mli vparser.ml

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(INCLUDES) -g -c $<

.mli.cmi:
	$(OCAMLC) $(INCLUDES) -g -c $<

.mll.ml:
	ocamllex.opt $(LEXOPTS) $<

.ml.cmx:
	$(OCAMLOPT) $(INCLUDES) -c -g $<

../theorem_proving/theorem_proving.cma ../theorem_proving/theorem_proving.cmxa:
	make -C ../theorem_proving

../ver_front/ver_front.cma ../ver_front/ver_front.cmxa:
	make -C ../ver_front

../vhd_front/vhd_front.cma ../vhd_front/vhd_front.cmxa: ../ver_front/globals.cmi
	make -C ../vhd_front

../bdd-0.3/decision.cma ../bdd-0.3/decision.cmxa: ../ver_front/globals.cmi
	(cd ../bdd-0.3; autoconf; ./configure; make)

../edif_parser/edif2.cma ../edif_parser/edif2.cmxa: ../ver_front/globals.cmi
	make -C ../edif_parser

INC := $(BASE)/../../build/ocaml
I := -I $(BASE)/compiler-libs/utils -I $(BASE)/compiler-libs/parsing -I $(BASE)/compiler-libs/typing -I $(BASE)/compiler-libs/utils -I $(BASE)/compiler-libs/bytecomp -I $(BASE)/compiler-libs/toplevel -I $(BASE)/compiler-libs/driver -I $(BASE)/compiler-libs $(INCLUDES)

$(TOPLOOP).cmo: $(TOPLOOP).ml
	$(OCAMLC) -g -c $I $<

$(TOPLOOP).cmi: $(TOPLOOP).mli
	$(OCAMLC) -g -c $I $<

read_library.cmo:  ../ver_front/ver_front.cmxa
vhier.cmx main.cmx flatten.cmx read_library.cmx: ../ver_front/ver_front.cmxa
gencnf.cmo: ../theorem_proving/theorem_proving.cma
gencnf.cmx: ../theorem_proving/theorem_proving.cmxa
hls.cmi: ../ver_front/setup.cmi

include .depend
