(lang dune 2.5)
(name imandra-goals)
(implicit_transitive_deps false)

(generate_opam_files true)
(license proprietary)
(maintainers "The Imandra team <tech@imandra.ai>")
(bug_reports "The Imandra team <tech@imandra.ai>")
(authors "The Imandra team <tech@imandra.ai>")
(homepage "https://imandra.ai")

(package
 (name imandra-goals)
 (synopsis "Create reports from Imandra verification goals")
 (depends
  (dune (>= 2.5))
  containers
  imandra-document
  imandra-base
  imandra-surface
  imandra-util))

(package
 (name imandra-goals-alcotest)
 (synopsis "Run Imandra verification goals as unit tests with Alcotest")
 (depends
  (dune (>= 2.5))
   alcotest
   containers
   imandra-goals))
