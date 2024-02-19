(lang dune 3.13)
(cram enable)

(name serde)

(generate_opam_files true)

(source
 (github serde-ml/serde))

(authors "Leandro Ostera <leandro@abstractmachines.dev>")

(maintainers "Leandro Ostera <leandro@abstractmachines.dev>")

(license "MIT")

(package
 (name serde)
 (synopsis "A serialization framework for OCaml")
 (description "Serde is a serialization framework for OCaml that runs on the principle of maximum efficiency and user ergonomics while maintaining format flexibility.")
 (depends
   (ocaml (>= "5.1.1"))
   (qcheck :with-test)
   (rio (>= "0.0.8"))
   (spices :with-test)
   dune))
   

(package
 (name serde_derive)
 (synopsis "Derive-macros for the Serde serialization framework")
 (description "These macros help derive serializers and deserializers for your existing types and provide all the functionality you expect to plug in different data-formats.")
 (depends
   (ocaml (>= "5.1.1"))
   (ppx_deriving (>= "5.2.1"))
   (ppxlib (>= "0.28.0"))
   (serde (= :version))
   dune))
   

(package
 (name serde_json)
 (synopsis "JSON format support for Serde")
 (depends
   (ocaml (>= "5.1.1"))
   (ppx_inline_test (>= "v0.16.0"))
   (qcheck :with-test)
   (rio (>= "0.0.8"))
   (serde (= :version))
   (serde_derive (= :version))
   (spices :with-test)
   (yojson (>= "2.1.0"))
   dune))
   
