OCAMLFIND=ocamlfind
OCAMLOPT=ocamlopt
OCAMLINCLUDE=WagonC.ml MTypeAccessor.ml
WCODE=WagonProgram.ml
OCAMLSUFFIX=main.ml
CC=gcc
OUT_GEN=wagongen.exe
OUT_CCODE=wagonprogram.c
OUT_CEXE=wagonprogram.exe
FILE_EXISTS := $(or $(and $(wildcard $(PATH_TO_FILE)),1),0)

gen:
	$(OCAMLFIND) $(OCAMLOPT) -o $(OUT_GEN) -linkpkg -package str $(OCAMLINCLUDE) $(WCODE) $(OCAMLSUFFIX)
