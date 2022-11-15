
all:
	dune build @all --watch

fmt:
	dune build @fmt --auto-promote

setup:
	opam install dune ppxlib ocamlformat ocaml-lsp-server -y
