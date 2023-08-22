(lang dune 3.5)

(name serde)

(generate_opam_files true)

(source
 (github leostera/serde.ml))

(authors "Leandro Ostera <leandro@abstractmachines.dev>")

(maintainers "Leandro Ostera <leandro@abstractmachines.dev>")

(license "MIT")

(package
 (name serde)
 (synopsis "A serialization framework for OCaml")
 (description "Serde is a serialization framework for OCaml that runs on the principle of maximum efficiency and user ergonomics while maintaining format flexibility. Internally Serde uses an intermediate language that can be cheaply mapped to and from different target formats like JSON, XML, or S-expressions, which makes Serde good for transcoding data as well.")
 (depends
   (ocaml (>= "4.12.0"))
   (ocamlformat (>= "0.17.0"))
   (ppx_inline_test (>= "0.16.0"))
   ))

(package
 (name serde_derive)
 (synopsis "Derive-macros for the Serde serialization framework")
 (description "These macros help derive serializers and deserializers for your existing types and provide all the functionality you expect to plug in different data-formats.")
 (depends
   (ocaml (>= "4.12.0"))
   (ppxlib (>= "0.28.0"))
   (ppx_deriving (>= "5.2.1"))
   (serde (= :version))
   ))

(package
 (name serde_sexpr)
 (synopsis "S-expression format support for Serde")
 (depends
   (ocaml (>= "4.12.0"))
   (serde (= :version))
   (serde_derive (= :version))
   (sexplib (>= "0.16.0"))
   ))

(package
 (name serde_json)
 (synopsis "JSON format support for Serde")
 (depends
   (ocaml (>= "4.12.0"))
   (serde (= :version))
   (serde_derive (= :version))
   (yojson (>= "2.1.0"))
   ))

(package
 (name serde_xml)
 (synopsis "XML format support for Serde")
 (depends
   (ocaml (>= "4.12.0"))
   (serde (= :version))
   (serde_derive (= :version))
   (tyxml (>= "4.5.0"))
   ))

(package
 (name serde_debug)
 (synopsis "A human-friendly format for Serde that helps you debug any data during development")
 (depends
   (ocaml (>= "4.12.0"))
   (serde (= :version))
   (serde_derive (= :version))
   ))
