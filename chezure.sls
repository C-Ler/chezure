;;;; chezure.sls
(library (chezure)
  (export chezure-flag chezure-flags make-chezure-options chezure-options?
          chezure-match? chezure-match-start chezure-match-end chezure-match-str chezure-match->alist
          chezure? chezure-set? chezure-set-len
          chezure-captures? chezure-captures-names chezure-captures-ref chezure-captures-string-ref
          chezure-compile chezure-compile-set chezure-escape
          chezure-has-match?  chezure-set-has-match? chezure-set-matches chezure-shortest-match
          chezure-find chezure-find-captures chezure-split chezure-replace)
  (import (chezure low-level)
          (chezure api)
          (chezscheme))   
  )

(define librure
  (case (machine-type)
    ((a6nt ta6nt) "rure.dll")
    ((a6le i3le ta6le ti3le) "librure.so")
    ((a6osx i3osx ta6osx ti3osx) "librure.so")
    (else (void))))

;;; FIXME: need to find a way to load the shared library
;; (load-shared-object (string-append "./" librure)
