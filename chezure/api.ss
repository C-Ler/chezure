;;;; api.ss
(library (chezure api)
  (export chezure-flag chezure-flags make-chezure-options chezure-options?
          chezure-match? chezure-match-start chezure-match-end chezure-match-str chezure-match->alist
          chezure? chezure-set? chezure-set-len
          chezure-captures? chezure-captures-names chezure-captures-ref chezure-captures-string-ref
          chezure-compile chezure-compile-set chezure-escape
          chezure-has-match?  chezure-set-has-match? chezure-set-matches chezure-shortest-match
          chezure-find chezure-find-captures chezure-split chezure-replace)
  (import (chezscheme)
          (chezure low-level))

  (define-syntax unwind-protect
    (syntax-rules ()
      [(_ body cleanup ...)
       (dynamic-wind
         (lambda () #f)
         (lambda () body)
         (lambda () cleanup ...))]))

  (define (string-not-empty? s)
    (not (or (fxzero? (string-length s))
             (for-all char-whitespace? (string->list s)))))

  (define (substring-bv8 bv start end)
    (let* ([len (fx- end start)]
           [new-bv (make-bytevector len)])
      (bytevector-copy! bv start new-bv 0 len)
      (utf8->string new-bv)))

  (include "definitions.ss")

  ;;; FIXME: better approaches to handle GC?
  (module ()
    (collect-request-handler
     (lambda ()
       (collect)
       ;; collect chezure
       (do ([re (chezure-guardian) (chezure-guardian)])
           ((not re))
         (rure_free (chezure-ptr re))
         ;; (display (format "~a dropped~%" re)))
         )
       ;; collect chezure-set
       (do ([re-set (chezure-set-guardian) (chezure-set-guardian)])
           ((not re-set))
         (rure_set_free (chezure-set-ptr re-set))
         ;; (display (format "~a dropped~%" re-set)))
         )
       )))

  ;;; Compile
  (define (%chezure-compile pattern flags options)
    (unless (string? pattern)
      (assertion-violationf 'chezure-compile "~a is not a string" pattern))
    (unless (enum-set? flags)
      (assertion-violationf 'chezure-compile "Illegal flags: ~a" flags))
    (unless (chezure-options? options)
      (assertion-violationf 'chezure-compile "Illegal options: ~a" options))
    (let* ([bv (string->utf8 pattern)]
           [options* (chezure-options-ptr options)]
           [error* (rure_error_new)]
           [re* (rure_compile bv
                              (bytevector-length bv)
                              (chezure-flags->n flags)
                              options*
                              error*)])
      (unwind-protect
       (if (fxzero? re*)
           (error 'chezure-compile (rure_error_message error*))
           (make-chezure re*))
       ;; cleanup
       (rure_options_free options*)
       (rure_error_free error*))))

  (define chezure-compile
    (case-lambda
      [(pattern)
       (%chezure-compile pattern (chezure-flags) (make-chezure-options))]
      [(pattern flags)
       (%chezure-compile pattern flags (make-chezure-options))]
      [(pattern flags options)
       (%chezure-compile pattern flags options)]))

  (define (%chezure-compile-set patterns flags options)
    
    (define (bv->pointer bv len)
      (do ([ptr (foreign-alloc (fx* len (foreign-sizeof 'unsigned-8)))]
           [i 0 (fx1+ i)]
           [offset 0 (fx+ (foreign-sizeof 'unsigned-8) offset)])
          ((fx=? i len) ptr)
        (foreign-set! 'unsigned-8 ptr offset
                      (bytevector-u8-ref bv i))))
    
    (define (patterns->pointer bvs lens count)
      (do ([ptr (foreign-alloc (fx* count (foreign-sizeof 'void*)))]
           [offset 0 (fx+ (foreign-sizeof 'void*) offset)]
           [bvs bvs (cdr bvs)]
           [lens lens (cdr lens)])
          ((null? bvs) ptr)
        (foreign-set! 'void* ptr offset
                      (bv->pointer (car bvs) (car lens)))))

    (define (lens->pointer lens count)
      (do ([ptr (foreign-alloc (fx* count (foreign-sizeof 'size_t)))]
           [offset 0 (fx+ (foreign-sizeof 'size_t) offset)]
           [lens lens (cdr lens)])
          ((null? lens) ptr)
        (foreign-set! 'size_t ptr offset (car lens))))

    (unless (and (list? patterns)
                 (for-all string? patterns))
      (assertion-violationf 'chezure-compile-set "~a is not a list of strings" patterns))
    (unless (enum-set? flags)
      (assertion-violationf 'chezure-compile-set "Illegal flags: ~a" flags))
    (unless (chezure-options? options)
      (assertion-violationf 'chezure-compile-set "Illegal options: ~a" options))
    
    (let* ([bvs (map string->utf8 patterns)]
           [lens (map bytevector-length bvs)]
           [count (length patterns)]
           [patterns* (patterns->pointer bvs lens count)]
           [lens* (lens->pointer lens count)]
           [options* (chezure-options-ptr options)]
           [error* (rure_error_new)]
           [set* (rure_compile_set patterns* lens* count
                                   (chezure-flags->n flags)
                                   options*
                                   error*)])
      (unwind-protect
       (if (fxzero? set*)
           (error 'chezure-compile-set (rure_error_message error*))
           (make-chezure-set set* count))
       ;; cleanup
       (foreign-free patterns*)
       (foreign-free lens*)
       (rure_options_free options*)
       (rure_error_free error*))))

  (define chezure-compile-set
    (case-lambda
      [(patterns)
       (%chezure-compile-set patterns (chezure-flags) (make-chezure-options))]
      [(patterns flags)
       (%chezure-compile-set patterns flags (make-chezure-options))]
      [(patterns flags options)
       (%chezure-compile-set patterns flags options)]))

  ;;; Escape
  (define chezure-escape
    (lambda (pattern)
      (rure_escape_must pattern)))

  ;;; Matched?
  (define (%chezure-has-match? chezure str start)
    (let ([bv (string->utf8 str)])
      (rure_is_match (chezure-ptr chezure)
                     bv (bytevector-length bv) start)))

  (define chezure-has-match?
    (case-lambda
      [(chezure str)
       (unless (chezure? chezure)
         (assertion-violationf 'chezure-has-match? "~a is not a chezure object" chezure))
       (unless (string? str)
         (assertion-violationf 'chezure-has-match? "~a is not a string" str))     
       (%chezure-has-match? chezure  str 0)]
      [(chezure str start)
       (unless (and (fixnum? start)
                    (fx<? -1 start (string-length str)))
         (assertion-violationf 'chezure-has-match? "Illegal start index: ~a" start))
       (%chezure-has-match? chezure str start)]))

;;; Set Matched?
  (define (%chezure-set-has-match? chezure-set str start)
    (let ([bv (string->utf8 str)])
      (rure_set_is_match (chezure-set-ptr chezure-set)
                         bv (bytevector-length bv) start)))

  (define chezure-set-has-match?
    (case-lambda
      [(chezure-set str)
       (unless (chezure-set? chezure-set)
         (assertion-violationf 'chezure-set-has-match? "~a is not a chezure-set object" chezure-set))
       (unless (string? str)
         (assertion-violationf 'chezure-set-has-match? "~a is not a string" str))
       (%chezure-set-has-match? chezure-set str 0)]
      [(chezure-set str start)
       (unless (chezure-set? chezure-set)
         (assertion-violationf 'chezure-set-has-match? "~a is not a chezure-set object" chezure-set))
       (unless (string? str)
         (assertion-violationf 'chezure-set-has-match? "~a is not a string" str))
       (unless (and (fixnum? start)
                    (fx<? -1 start (string-length str)))
         (assertion-violationf 'chezure-set-has-match? "Illegal start index: ~a" start))
       (%chezure-set-has-match? chezure-set str start)]))

  ;;; Set Matches
  (define (%chezure-set-matches chezure-set str start)
    (let* ([len (chezure-set-len chezure-set)]
           [m* (foreign-alloc len)]
           [res (let ([bv (string->utf8 str)])
                  (if (rure_set_matches (chezure-set-ptr chezure-set)
                                        bv (bytevector-length bv) start m*)
                      ;; then collect bits
                      (let loop ([res (list)]
                                 [i 0])
                        (if (fx=? i len)
                            (reverse! res)
                            (loop (cons (if (fxzero? (foreign-ref 'unsigned-8 m* i))
                                            #f #t)
                                        res)
                                  (fx1+ i))))
                      ;; otherwise return False
                      #f))])
      (foreign-free m*)
      res))

  (define chezure-set-matches
    (case-lambda
      [(chezure-set str)
       (unless (chezure-set? chezure-set)
         (assertion-violationf 'chezure-set-matches "~a is not a chezure-set object" chezure-set))
       (unless (string? str)
         (assertion-violationf 'chezure-set-matches "~a is not a string" str))
       (%chezure-set-matches chezure-set str 0)]
      [(chezure-set str start)
       (unless (chezure-set? chezure-set)
         (assertion-violationf 'chezure-set-matches "~a is not a chezure-set object" chezure-set))
       (unless (string? str)
         (assertion-violationf 'chezure-set-matches "~a is not a string" str))
       (unless (and (fixnum? start)
                    (fx<? -1 start (string-length str)))
         (assertion-violationf 'chezure-set-matches "Illegal start index: ~a" start))
       (%chezure-set-matches chezure-set str start)]))

  ;;; Shortest Match
  (define (%chezure-shortest-match chezure str start)
    (let* ([bv (string->utf8 str)]
           [end* (foreign-alloc (foreign-sizeof 'size_t))]
           [matched? (rure_shortest_match (chezure-ptr chezure)
                                          bv (bytevector-length bv)
                                          start end*)]
           [res (if matched?
                    (let ([end (foreign-ref 'size_t end* 0)])
                      (mk-chezure-match start end
                                        (substring str start end)))
                    #f)])
      (foreign-free end*)
      res))

  (define chezure-shortest-match
    (case-lambda
      [(chezure str)
       (unless (chezure? chezure)
         (assertion-violationf 'chezure-shortest-match "~a is not a chezure object" chezure))
       (unless (string? str)
         (assertion-violationf 'chezure-shortest-match "~a is not a string" str))     
       (%chezure-shortest-match chezure str 0)]
      [(chezure str start)
       (unless (chezure? chezure)
         (assertion-violationf 'chezure-shortest-match "~a is not a chezure object" chezure))
       (unless (string? str)
         (assertion-violationf 'chezure-shortest-match "~a is not a string" str))
       (unless (and (fixnum? start)
                    (fx<? -1 start (string-length str)))
         (assertion-violationf 'chezure-shortest-match "Illegal start index: ~a" start))
       (%chezure-shortest-match chezure str start)]))

  
  ;;; Find
  (define (%chezure-find chezure str limit)
    (let* ([bv (string->utf8 str)]
           [len (bytevector-length bv)]
           [re* (chezure-ptr chezure)]
           [iter* (rure_iter_new re*)]
           [m* (rure_match_new)])
      (let loop ([i 0]
                 [res (list)])
        (if (and (or (fxzero? limit)
                     (fx<? i limit))
                 (rure_iter_next iter* bv len m*))
            (loop (fx1+ i)
                  (cons (make-chezure-match str m*) res))
            (begin ;; cleanup
              (rure_iter_free iter*)
              (rure_match_free m*)
              (reverse! res))))))

  (define chezure-find
    (case-lambda
      [(chezure str)
       (unless (chezure? chezure)
         (assertion-violationf 'chezure-find "~a is not a chezure object" chezure))
       (unless (string? str)
         (assertion-violationf 'chezure-find "~a is not a string" str))
       (%chezure-find chezure str 0)]
      [(chezure str limit)
       (unless (chezure? chezure)
         (assertion-violationf 'chezure-find "~a is not a chezure object" chezure))
       (unless (string? str)
         (assertion-violationf 'chezure-find "~a is not a string" str))
       (unless (and (fixnum? limit)
                    (fx<=? 0 limit))
         (assertion-violationf 'chezure-find "~a is not a non-negative fixnum" limit))
       (%chezure-find chezure str limit)]))

  ;;; Find Captures
  (define (%chezure-find-captures chezure str limit)
    (let* ([re* (chezure-ptr chezure)]
           [bv (string->utf8 str)]
           [len (bytevector-length bv)]
           [caps* (rure_captures_new re*)]
           [iter* (rure_iter_new re*)])
      (let loop ([i 0]
                 [res (list)])
        (if (and (or (fxzero? limit)
                     (fx<? i limit))
                 (rure_iter_next_captures iter* bv len caps*))
            (loop (fx1+ i)
                  (cons (make-chezure-captures str re* caps*) res))
            (begin (rure_captures_free caps*)
                   (rure_iter_free iter*)
                   (reverse! res))))))

  (define chezure-find-captures
    (case-lambda
      [(chezure str)
       (unless (chezure? chezure)
         (assertion-violationf 'chezure-find-captures "~a is not a chezure object" chezure))
       (unless (string? str)
         (assertion-violationf 'chezure-find-captures "~a is not a string" str))
       (%chezure-find-captures chezure str 0)]
      [(chezure str limit)
       (unless (chezure? chezure)
         (assertion-violationf 'chezure-find-captures "~a is not a chezure object" chezure))
       (unless (string? str)
         (assertion-violationf 'chezure-find-captures "~a is not a string" str))
       (unless (and (fixnum? limit)
                    (fx<=? 0 limit))
         (assertion-violationf 'chezure-find-captures "~a is not a non-negative fixnum" limit))
       (%chezure-find-captures chezure str limit)]))

  ;;; Split
  (define (%chezure-split chezure str limit preserve?)
    
    (define (iter matches offset stack)
      (if (null? matches)
          (reverse! (cons (substring str offset (string-length str))
                          stack))
          (let* ([m (car matches)]
                 [start (chezure-match-start m)]
                 [end (chezure-match-end m)]
                 [matched (chezure-match-str m)]
                 [slice (substring str offset start)])
            (iter (cdr matches) end
                  (if preserve?
                      (cons matched (cons slice stack))
                      (cons slice stack))))))
    
    (let ([matches (%chezure-find chezure str limit)])
      (if (null? matches)
          (list)
          (iter matches 0 (list)))))

  (define (string-not-empty? s)
    (not (or (fxzero? (string-length s))
             (for-all char-whitespace? (string->list s)))))

  (define chezure-split
    (case-lambda
      [(chezure str)
       (unless (chezure? chezure)
         (assertion-violationf 'chezure-split "~a is not a chezure object" chezure))
       (unless (string? str)
         (assertion-violationf 'chezure-split "~a is not a string" str))
       (%chezure-split chezure str 0 #f)]
      [(chezure str limit)
       (unless (chezure? chezure)
         (assertion-violationf 'chezure-split "~a is not a chezure object" chezure))
       (unless (string? str)
         (assertion-violationf 'chezure-split "~a is not a string" str))
       (unless (and (fixnum? limit)
                    (fx<=? 0 limit))
         (assertion-violationf 'chezure-split "~a is not a non-negative fixnum" limit))
       (%chezure-split chezure str limit #f)]
      [(chezure str limit preserve?)
       (unless (chezure? chezure)
         (assertion-violationf 'chezure-split "~a is not a chezure object" chezure))
       (unless (string? str)
         (assertion-violationf 'chezure-split "~a is not a string" str))
       (unless (and (fixnum? limit)
                    (fx<=? 0 limit))
         (assertion-violationf 'chezure-split "~a is not a non-negative fixnum" limit))
       (%chezure-split chezure str limit preserve?)]
      [(chezure str limit preserve? remove-empty?)
       (unless (chezure? chezure)
         (assertion-violationf 'chezure-split "~a is not a chezure object" chezure))
       (unless (string? str)
         (assertion-violationf 'chezure-split "~a is not a string" str))
       (unless (and (fixnum? limit)
                    (fx<=? 0 limit))
         (assertion-violationf 'chezure-split "~a is not a non-negative fixnum" limit))
       (if remove-empty?
           (filter string-not-empty? (%chezure-split chezure str limit preserve?))
           (%chezure-split chezure str limit preserve?))]))

  ;;; Replace
  (define (%chezure-replace chezure str repl limit)
    
    (define (iter all offset stack)
      (if (null? all)
          (reverse! (cons (substring str offset (string-length str))
                          stack))
          (let* ([caps (car all)]
                 [m (vector-ref (chezure-captures-matches caps) 0)]
                 [start (chezure-match-start m)]
                 [end (chezure-match-end m)]
                 [matched (chezure-match-str m)]
                 [slice (substring str offset start)]
                 [replacement (if (string? repl)
                                  repl
                                  (repl caps))])
            (unless (string? replacement)
              (errorf 'chezure-replace "the repl procedure doesn't return a string"))            
            (iter (cdr all) end
                  (cons replacement (cons slice stack))))))
    
    (let ([all (chezure-find-captures chezure str limit)])
      (if (null? all)
          (list)
          (fold-left string-append ""
                     (iter all 0 (list))))))

  (define chezure-replace
    (case-lambda
      [(chezure str repl)
       (unless (chezure? chezure)
         (assertion-violationf 'chezure-replace "~a is not a chezure object" chezure))
       (unless (string? str)
         (assertion-violationf 'chezure-replace "~a is not a string" str))
       (unless (or (string? repl)
                   (and (procedure? repl)
                        (logbit? 1 (procedure-arity-mask repl))))
         (assertion-violationf 'chezure-replace
                               "~a is not a string or a unary procedure" repl))
       (%chezure-replace chezure str repl 0)]
      [(chezure str repl limit)
       (unless (chezure? chezure)
         (assertion-violationf 'chezure-replace "~a is not a chezure object" chezure))
       (unless (string? str)
         (assertion-violationf 'chezure-replace "~a is not a string" str))
       (unless (or (string? repl)
                   (and (procedure? repl)
                        (logbit? 1 (procedure-arity-mask repl))))
         (assertion-violationf 'chezure-replace
                               "~a is not a string or a unary procedure" repl))
       (unless (and (fixnum? limit) (nonnegative? limit))
         (assertion-violationf 'chezure-replace
                               "~a is not a non-negative fixnum" limit))
       (%chezure-replace chezure str repl limit)]))
  
  ) ;; end of library
