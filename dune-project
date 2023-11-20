(lang dune 3.11)
(using menhir 2.0)

(generate_opam_files)

(package
 (name objc-converter)
 (depends
  (menhir (>= 2.0))))