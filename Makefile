
all:
	dune build @all @fmt --auto-promote --watch

setup:
	opam install dune ppxlib ocamlformat ocaml-lsp-server -y
