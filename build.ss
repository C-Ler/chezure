;;;; build.ss
(import (chezscheme))

;;; update submodules
(system "git submodule update --init")
;; checkout latest version of regex-capi?

;;; compile librure and copy the shared library
(system "cargo build --release --manifest-path regex/regex-capi/Cargo.toml --target-dir librure")

(define librure
  (case (machine-type)
    ((a6nt ta6nt) "rure.dll")
    ((a6le i3le ta6le ti3le) "librure.so")
    ((a6osx i3osx ta6osx ti3osx) "librure.dylib")
    (else "librure.so")))

(let ([cp (case (machine-type)
            ((a6nt ta6nt) "copy")
            (else "cp"))])
  (system (format "~a ./librure/release/~a ./" cp librure)))

;;; compile whole library for distribution
(parameterize ([optimize-level 2]
               [compile-imported-libraries #t]
               [generate-wpo-files #t]
               [generate-inspector-information #f])
  (compile-file "chezure.sls")
  (compile-whole-library "chezure.wpo" "chezure.so"))
