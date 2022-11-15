(lang dune 3.5)

(name serde)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name serde)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 )

(package
 (name serde_derive)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune ppxlib serde)
 )

(package
 (name serde_sexpr)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune serde sexplib)
 )

(package
 (name serde_json)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune serde yojson)
 )
