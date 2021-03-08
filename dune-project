(lang dune 2.0)
(name tjr_monad)

;; for opam
(authors              "Tom Ridge <tom.j.ridge@googlemail.com>")
(maintainers          "Tom Ridge <tom.j.ridge@googlemail.com>")
(source               (github tomjridge/tjr_monad))
(homepage             https://github.com/tomjridge/tjr_monad)
(documentation        https://tomjridge.github.io/ocamldocs/)
(generate_opam_files  true)


(package 
 (name tjr_monad)
 (synopsis "A generic monad library (part of ImpFS)")
 (depends 
(lwt (>= 5.2.0))  ;; prefer Lwt_unix.pread,pwrite for atomic file update
tjr_lib_core
))
