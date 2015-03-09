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

all: edif2 edif2.opt

edif2: edif2.cma
	ocamlmktop -g -o $@ edif2.cma

edif2.cma: edif2.ml elexer.ml eord.ml eparse.ml ../ver_front/globals.cmo
	ocamlc.opt -I ../ver_front -g -a -o $@ edif2.mli edif2.ml eord.ml elexer.ml eparse.ml

edif2.opt: edif2.cmxa emain.ml
	ocamlopt.opt -g -o $@ edif2.cmxa emain.ml

edif2.cmxa: edif2.ml elexer.ml eord.ml eparse.ml ../ver_front/globals.cmx
	ocamlopt.opt -I ../ver_front -g -a -o $@ edif2.mli edif2.ml eord.ml elexer.ml eparse.ml

edif2.ml: edif2.mly
	ocamlyacc edif2.mly

elexer.ml: elexer.mll
	ocamllex elexer.mll

eord.ml: eord.sh edif2.ml
	sh eord.sh

clean:
	rm -f \
	edif2 \
	edif2.cma \
	edif2.cmxa \
	edif2.cmi \
	edif2.cmo \
	edif2.cmx \
	edif2.ml \
	edif2.mli \
	edif2.a \
	edif2.o \
	edif2.opt \
	elexer.cmi \
	elexer.cmo \
	elexer.cmx \
	elexer.ml \
	elexer.o \
	eord.cmi \
	eord.cmo \
	eord.cmx \
	eord.ml \
	eord.o \
	eparse.cmi \
	eparse.cmo \
	eparse.cmx \
	eparse.o \
