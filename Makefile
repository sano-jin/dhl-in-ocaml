#
# Makefile
#

SRC= util.ml syntax.ml parser.mly lexer.mll preprocess.ml compile.ml vm.ml findatom.ml pushatom.ml eval.ml main.ml 
COMPONENT= util.ml syntax.ml parser.mli parser.ml lexer.ml preprocess.ml compile.ml vm.ml findatom.ml pushatom.ml eval.ml main.ml 
TARGET= dhl

all:	$(TARGET)

$(TARGET): 	$(COMPONENT) 
	ocamlmktop $(COMPONENT) -w -31 -o $(TARGET)

parser.mli:	parser.mly
	opam exec ocamlyacc -- parser.mly

parser.ml:	parser.mly
	opam exec ocamlyacc -- parser.mly

lexer.ml:	lexer.mll
	opam exec ocamllex -- lexer.mll

backup:
	/bin/cp -f Makefile $(SRC) back

clean:
	/bin/rm -f parser.ml parser.mli lexer.ml $(TARGET) *.cmi *.cmo *.mli
