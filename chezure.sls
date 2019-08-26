;;;; chezure.sls
(library (chezure)
  (export chezure-flag chezure-flags make-chezure-options chezure-options?
          chezure-match? chezure-match-start chezure-match-end chezure-match-str chezure-match->alist
          chezure? chezure-set? chezure-set-len
          captures? captures-names captures-ref captures-string-ref
          chezure-compile chezure-compile-set chezure-escape
          chezure-has-match?  chezure-set-has-match? chezure-set-matches chezure-shortest-match
          chezure-find chezure-find-captures chezure-split chezure-replace
          load-librure)
  (import (chezure low-level)
          (chezure api)
          (chezscheme))

  ;;; better way to load a shared library?
  (define librure
    (case (machine-type)
      ((a6nt ta6nt) "rure.dll")
      ((a6le i3le ta6le ti3le) "librure.so")
      ((a6osx i3osx ta6osx ti3osx) "librure.dylib")
      (else "librure.so")))
  
  (define load-librure
    (case-lambda
      [() (load-shared-object librure)]
      [(so) (load-shared-object so)]))
  )
