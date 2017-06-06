OCB_FLAGS = -use-ocamlfind -I tests -pkg graphics
OCB = ocamlbuild $(OCB_FLAGS) -I src

TESTS = $(shell find tests/ -name '*.ml')

all: native byte lib # profile debug

clean:
			$(OCB) -clean

lib:
			$(OCB) apronext.cma
			$(OCB) apronext.cmxa
			$(OCB) apronext.cmxs

native:  	sanity
			$(OCB) main.native

byte: 		sanity
			$(OCB) main.byte

doc:
			ocamlbuild -use-ocamlfind -pkg apron apronext.docdir/index.html

profile: 	sanity
			$(OCB) -tag profile main.native

debug:
			$(OCB) -tag debug main.byte

sanity:
			ocamlfind query apron

test: native
			./main.native

alltest: native
			for i in $(TESTS:.ml=.native); do $(OCB) $$i; done
			for i in $(wildcard *.native); do echo $$i; ./$$i; done

.PHONY: 	all clean byte native profile debug lib sanity test
