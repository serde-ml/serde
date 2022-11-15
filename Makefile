
all:
	dune build @all --watch

clean:
	dune clean

fmt:
	dune build @fmt --auto-promote

setup:
	opam install dune ppxlib ocamlformat ocaml-lsp-server -y
	opam install sexplib yojson tyxml -y
