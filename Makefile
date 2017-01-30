#
# Pure OCaml, package from Opam, two directories
#

# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot

OCB_FLAGS = -use-ocamlfind -I src -I tests
OCB = ocamlbuild $(OCB_FLAGS)

TESTS = $(shell find tests/ -name '*.ml')

all: native byte lib # profile debug

clean:
			$(OCB) -clean

lib:
			$(OCB) apronext.cma
			$(OCB) apronext.cmxa
			$(OCB) apronext.cmxs

native:  	sanity
			for i in $(TESTS:.ml=.native); do $(OCB) $$i; done

byte: 		sanity
			for i in $(TESTS:.ml=.byte); do $(OCB) $$i; done

profile: 	sanity
			$(OCB) -tag profile main.native

debug: 		sanity
			$(OCB) -tag debug main.byte

sanity:
			ocamlfind query apron

test: native
			for i in $(wildcard *.native); do echo $$i; ./$$i; echo '**************'; done

.PHONY: 	all clean byte native profile debug lib sanity test
