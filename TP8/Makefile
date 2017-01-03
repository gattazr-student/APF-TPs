
EXECUTABLES=main

PLIBS=ordre tri
PCMAS=$(foreach lib, $(PLIBS), $(addprefix $(lib),.cma))

# ENVIRONMENT
OCAMLC=ocamlc

# BUILD RULES
all: $(EXECUTABLES)

%: $(PCMAS) %.ml
	$(OCAMLC) -o $@ $+

ordre.cma: ordre.ml ordre.cmi
	$(OCAMLC) -o $@ -a $<
ordre.cmi: ordre.mli
	$(OCAMLC) -c $<
ordre.cmo: ordre.ml
	$(OCAMLC) -c $<

tri.cma: tri.ml tri.cmi ordre.cma
	$(OCAMLC) -o $@ -a $<
tri.cmi: tri.mli ordre.cmi
	$(OCAMLC) -c $<
tri.cmo: tri.ml
	$(OCAMLC) -c $<

# CLEANING RULES
clean:
	rm -vf $(foreach demo, $(EXECUTABLES), $(addprefix $(demo),.cmi .cmo))
	rm -vf $(foreach lib, $(PLIBS), $(addprefix $(lib),.cma .cmi .cmo))

mrproper: clean
	rm -vf $(EXECUTABLES)

.PHONY: all clean mrproper
