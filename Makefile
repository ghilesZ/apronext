default: build install

build:
	dune build

test:
	@dune runtest -f

clean:
	dune clean

install:
	dune install apronext

uninstall:
	dune uninstall apronext

.PHONY: build test clean
