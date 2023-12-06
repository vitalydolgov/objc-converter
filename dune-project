(lang dune 3.11)
(using menhir 2.0)

(name objc-converter)

(generate_opam_files)

(package
 (name objc-converter)
 (depends
  (menhir (>= 2.0))
  ppx_deriving))