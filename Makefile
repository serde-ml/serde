
.PHONY: all
all:
	dune build @all --watch

.PHONY: test
test:
	dune test

.PHONY: clean
clean:
	dune clean

.PHONY: fmt
fmt:
	dune build @fmt --auto-promote

.PHONY: setup
setup:
	opam install dune ppxlib ocamlformat ocaml-lsp-server ppx_inline_test -y
	opam install sexplib yojson tyxml -y
