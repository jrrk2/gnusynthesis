.SUFFIXES:
.SUFFIXES: .cmi .cmo

SRC = Quotexpander.ml initialization.ml lib.ml intro.ml formulas.ml prop.ml propexamples.ml defcnf.ml dp.ml stal.ml bdd.ml fol.ml skolem.ml herbrand.ml unif.ml tableaux.ml resolution.ml prolog.ml meson.ml skolems.ml equal.ml cong.ml trewrite.ml order.ml completion.ml eqelim.ml paramodulation.ml decidable.ml qelim.ml cooper.ml complexelim.ml real.ml grobner.ml geom.ml interpolation.ml combining.ml lcf.ml lcfprop.ml folderived.ml lcffol.ml tactics.ml limitations.ml

BASE:=$(shell ocamlc -where)
INT = $(subst .ml,.mli,$(SRC))
CINT = $(subst .ml,.cmi,$(SRC))
OBJ = $(subst .ml,.cmo,$(SRC))
NAT = $(subst .ml,.cmx,$(SRC))
P5 = -I +camlp5 -I $(BASE)/../camlp5

.PRECIOUS: $(INT) $(CINT)

dummy: $(OBJ)  theorem_proving.cma theorem_proving.cmxa
	true

all: target native

target: $(OBJ) $(INT)
	ocamlmktop -o $@ $(P5) camlp5o.cma nums.cma $(OBJ)

native: $(NAT) $(INT)
	ocamlopt.opt -o $@ $(P5) camlp5.cmxa nums.cmxa $(NAT)

theorem_proving.cma: $(OBJ) $(INT)
	ocamlc.opt -o $@ -a $(OBJ)

theorem_proving.cmxa: $(NAT) $(INT)
	ocamlopt.opt -o $@ -a -I $(NAT)

%.mli: %.ml
	ocamlc -i $(P5) $< >$*.tmp
	mv $*.tmp $@

%.cmi: %.mli
	ocamlc.opt -c  $<

%.cmo: %.cmi %.ml
	ocamlc.opt -c $(P5) $*.ml

%.cmx: %.cmi %.ml
	ocamlopt.opt -c $(P5) $*.ml

clean:
	rm -f $(OBJ) $(INT) $(CINT) target

depend:
	ocamldep $(SRC) $(INT) > .depend

include .depend
