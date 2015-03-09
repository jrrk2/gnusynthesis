########################################################################
#                                                                      #
#                      Objective Caml Extension                        #
#                                                                      #
#                  Jonathan Richard Robert Kimmitt                     #
#                                                                      #
#   Copyright 1996 Institut National de Recherche en Informatique et   #
#   en Automatique.  All rights reserved.  This file is distributed    #
#   under the terms of the Q Public License version 1.0.               #
#                                                                      #
########################################################################

.SUFFIXES: .ml .mli .cmi .cmo

O = terminfo.cmo misc.cmo linenum.cmo warnings.cmo location.cmo longident.cmo config.cmo clflags.cmo syntaxerr.cmo parser0.cmo lexer.cmo conversion.cmo rewrite.cmo main.cmo

generate: testprog debug
	./debug ../VhdlTypes.ml ../VhdlTree.ml outfile.ml ../asctoken.ml

testprog: $I $O $M
	ocamlmktop -g -o $@ $M $O

debug: $I $O $M
	ocamlc -g -o $@ $M $O

clean:
	rm -f $M $O *.cmi debug testprog vhdltop lexer.ml parser0.ml parser0.mli

.mli.cmi:
	ocamlc -c -g -I . -I .. $<

.ml.cmo:
	ocamlc -c -g -I . -I .. $<

parser0.ml: parsetree0.cmi parser0.mly
	ocamlyacc parser0.mly
	ocamlc -c parser0.mli

lexer.ml: lexer.mll
	ocamllex lexer.mll

clflags.cmo: config.cmo
clflags.cmx: config.cmx
config.cmo:
config.cmx:
conversion.cmo: parsetree0.cmi longident.cmo location.cmo ctypes.cmi \
    asttypes0.cmi
conversion.cmx: parsetree0.cmi longident.cmx location.cmx ctypes.cmi \
    asttypes0.cmi
linenum.cmo: misc.cmo
linenum.cmx: misc.cmx
location.cmo: warnings.cmo terminfo.cmo linenum.cmo
location.cmx: warnings.cmx terminfo.cmx linenum.cmx
longident.cmo: misc.cmo
longident.cmx: misc.cmx
main.cmo: rewrite.cmo ctypes.cmi conversion.cmo
main.cmx: rewrite.cmx ctypes.cmi conversion.cmx
misc.cmo:
misc.cmx:
rewrite.cmo: ctypes.cmi
rewrite.cmx: ctypes.cmi
syntaxerr.cmo: location.cmo
syntaxerr.cmx: location.cmx
terminfo.cmo:
terminfo.cmx:
warnings.cmo:
warnings.cmx:
asttypes0.cmi:
ctypes.cmi:
parsetree0.cmi: longident.cmo location.cmo asttypes0.cmi
