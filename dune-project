(lang dune 2.8)
(name dhl)
(version 0.0.0)
(generate_opam_files true)

(source (github username/myproject))
(license MIT)
(authors "author<email@email.com>" "author2<email2@email.com>")
(maintainers "author<email@email.com>")

(package
 (name dhl)
 (synopsis "Short description")
 (description "Long description")
 (depends
  (ounit :with-test)
  (minher (> 20210419))
  (dune (> 1.5))))
(using menhir 2.1)
