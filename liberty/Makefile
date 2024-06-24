#**************************************************************************)
#*                                                                        *)
#* OCaml template Copyright (C) 2004-2010                                 *)
#*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
#* Adapted to boolean logic by Jonathan Kimmitt                           *)
#*  Copyright 2016 University of Cambridge                                *)
#*                                                                        *)
#*  This software is free software; you can redistribute it and/or        *)
#*  modify it under the terms of the GNU Library General Public           *)
#*  License version 2.1, with the special exception on linking            *)
#*  described in file LICENSE.                                            *)
#*                                                                        *)
#*  This software is distributed in the hope that it will be useful,      *)
#*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
#*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
#*                                                                        *)
#**************************************************************************)

.PHONY: everything
PARSER=ocamlyacc
MENHIRFLAGS= --infer # --trace
MENHIRFLAGSF= --infer
PARSER=menhir $(MENHIRFLAGS)

libs: File.cma File.cmxa Formula.cma Formula.cmxa

demo: Formula.cma File.cma
	ocamlmktop -o $@ Formula.cma File.cma

File.cma: File.cmo File_types.cmo File_ord.ml File_lex.ml File_rewrite_types.mli File_rewrite.ml
	ocamlc -g -a -o $@ File_types.cmo File.cmo File_ord.ml File_lex.ml File_rewrite_types.mli File_rewrite.ml

File.cmxa: File.cmx File_types.cmx File_ord.ml File_lex.ml File_rewrite_types.mli File_rewrite.ml
	ocamlopt.opt -g -a -o $@ File_types.cmx File.cmx File_ord.ml File_lex.ml File_rewrite_types.mli File_rewrite.ml

File_types.cmo: File_types.ml File.mli
	ocamlc.opt -g -c File.mli File_types.ml

File.cmo: File_types.ml File.ml File.mli
	ocamlc.opt -g -c File.mli File_types.ml File.ml

File.cmx: File_types.ml File.ml File.mli
	ocamlopt.opt -g -c File.mli File_types.ml File.ml

File_lex.ml: File_lex.mll
	ocamllex $<

File.ml File.mli: File.mly
	menhir $(MENHIRFLAGSF) $<

File_ord.ml: ord_file.sh File.mli
	sh ord_file.sh

Formula.cma: Formula.cmo Formula_types.cmo Formula_ord.ml Formula_lex.ml Formula_rewrite_types.mli Formula_rewrite.ml
	ocamlc -g -a -o $@ -I ../ver_front Formula_types.cmo Formula.cmo Formula_ord.ml Formula_lex.ml Formula_rewrite_types.mli Formula_rewrite.ml

Formula.cmxa: Formula.cmx Formula_types.cmx Formula_ord.ml Formula_lex.ml Formula_rewrite_types.mli Formula_rewrite.ml
	ocamlopt.opt -g -a -o $@ -I ../ver_front Formula_types.cmx Formula.cmx Formula_ord.ml Formula_lex.ml Formula_rewrite_types.mli Formula_rewrite.ml

Formula.cmo: Formula_types.ml Formula.ml Formula.mli
	ocamlc.opt -g -c Formula.mli Formula_types.ml Formula.ml

Formula.cmx: Formula_types.ml Formula.ml Formula.mli
	ocamlopt.opt -g -c Formula.mli Formula_types.ml Formula.ml

Formula_lex.ml: Formula_lex.mll
	ocamllex $<

Formula.ml Formula.mli: Formula.mly
	menhir $(MENHIRFLAGS) $<
