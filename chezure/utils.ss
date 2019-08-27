;;;; utils.ss
(library (chezure utils)
  (export unwind-protect string-not-empty?
          make-index-map utf8-index->byte-index byte-index->utf8-index)
  (import (chezscheme))
  
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

  ;;; get the corresponding byte index from a given UTF-8 string
  ;;; FIXME: maybe there's a better way to do it?
  (define (make-index-map str)
    (define len (string-length str))
    (define indices (make-fxvector len))
    (let loop ([sum 0]
               [i 0])
      (if (fx=? i len)
          indices
          (let ([c (char->integer (string-ref str i))])
            (fxvector-set! indices i sum)
            (when (fx>? c #x10000)
              (set! i (fx1+ i))
              (fxvector-set! indices i sum))
            (loop (cond [(fx<=? c #x7F) (fx1+ sum)]
                        [(fx<=? c #x7FF) (fx+ 2 sum)]
                        [(fx<=? c #xFFFF) (fx+ 3 sum)]
                        [(fx<=? c #x1FFFFF) (fx+ 4 sum)]
                        [else (errorf 'utf8-index->byte-index "Cannot get byte index for ~s" str)])
                  (fx1+ i))))))

  (define (utf8-index->byte-index index-map index)
    (fxvector-ref index-map index))    

  (define (byte-index->utf8-index index-map index)
    (define len (fxvector-length index-map))
    (call/1cc
     (lambda (return)
       (do ([i 0 (fx1+ i)])
           ((fx=? i len) (return len))
         (when (fx=? (fxvector-ref index-map i) index)
           (return i))))))
  )
