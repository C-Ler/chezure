;;; definitions.ss

;;; Flags
(define-enumeration chezure-flag
  (ignorecase multiline dotnl swap-greed space unicode)
  chezure-flags)

(define (chezure-flags->n flags)
  (define (mapper flag)
    (case flag
      [(ignorecase) RURE_FLAG_CASEI]
      [(multiline) RURE_FLAG_MULTI]
      [(dotnl) RURE_FLAG_DOTNL]
      [(swap-greed) RURE_FLAG_SWAP_GREED]
      [(space) RURE_FLAG_SPACE]
      [(unicode) RURE_FLAG_UNICODE]
      [else (error 'chezure-flags "Unknown chezure flag: ~a" flag)]))

  (let* ([lst (enum-set->list flags)]
         [len (length lst)])
    (cond [(fxzero? len)
           ;; use RURE_DEFAULT_FLAGS when no args
           RURE_DEFAULT_FLAGS]
          [(fx=? len 1) (mapper (car lst))]
          [else (fold-left logior 0 (map mapper lst))])))

;;; Options
;;; https://github.com/rust-lang/regex/blob/master/regex-capi/src/rure.rs#L71
(define default-size-limit (fx* 10 (fxsll 1 20)))
(define default-dfa-size-limit (fx* 2 (fxsll 1 20)))

(define-record-type
    (chezure-options mk-chezure-options chezure-options?)
  (fields (immutable ptr)))

(define make-chezure-options
  (case-lambda
    [() (mk-chezure-options 0)]
    [(size-limit dfa-size-limit)
     (let ([ptr (rure_options_new)])
       (rure_options_size_limit ptr size-limit)
       (rure_options_dfa_size_limit ptr dfa-size-limit)
       (mk-chezure-options ptr))]))

;;; Match
(define-record-type (chezure-match mk-chezure-match chezure-match?)
  (fields (immutable start)
          (immutable end)
          (immutable str)))

(module ()
  (record-writer
   (type-descriptor chezure-match)
   (lambda (r p wr)
     (display (format "#<chezure-match start=~d, end=~d, string=~s>"
                      (chezure-match-start r)
                      (chezure-match-end r)
                      (chezure-match-str r))
              p))))

;;; FIXME: instead of allocating a u8 vector everytime, it should take a bytevector as arg
;;; start and end could be index as in the bytevector, not string
(define (make-chezure-match str m*)
  (let ([start (ftype-ref rure_match (start) m*)]
        [end (ftype-ref rure_match (end) m*)])
    (mk-chezure-match
     start end (substring-bv8 (string->utf8 str) start end))))

(define (chezure-match->alist m)
  (list (cons 'start (chezure-match-start m))
        (cons 'end (chezure-match-end m))
        (cons 'match (chezure-match-str m))))

;;; Chezure
(define-record-type (chezure mk-chezure chezure?)
  (fields (immutable ptr)))

(module ()
  (record-writer
   (type-descriptor chezure)
   (lambda (r p wr)
     (display
      (format "#<chezure @~x>" (chezure-ptr r))
      p))))
  
;; (define make-chezure
;;   (let ([g (make-guardian)])
;;     (lambda (ptr)
;;       (do ([i 0 (fx1+ i)]
;;            [re (g) (g)])
;;           ((or (not re)
;;                (fx=? i 8)) ;; free 8 objects at most
;;            ;; return
;;            (let ([re (mk-chezure ptr)])
;;              (g re)
;;              re))
;;         (rure_free (chezure-ptr re))
;;         ;; (display (format "~a dropped.~%" re))
;;         ))))

(define chezure-guardian (make-guardian))

(define (make-chezure ptr)
  (let ([re (mk-chezure ptr)])
    (chezure-guardian re)
    re))

;;; Compile Set
(define-record-type (chezure-set mk-chezure-set chezure-set?)
  (fields (immutable ptr) (immutable len)))

(module ()
  (record-writer
   (type-descriptor chezure-set)
   (lambda (r p wr)
     (display
      (format "#<chezure-set @~x, ~d patterns>"
              (chezure-set-ptr r) (chezure-set-len r))
    p))))

;; (define make-chezure-set
;;   (let ([g (make-guardian)])
;;     (lambda (ptr len)
;;       (do ([i 0 (fx1+ i)]
;;            [re-set (g) (g)])
;;           ((or (not re-set)
;;                (fx=? i 8)) ;; free 8 objects at most
;;            ;; return
;;            (let ([re-set (mk-chezure-set ptr len)])
;;              (g re-set)
;;              re-set))
;;         (rure_set_free (chezure-set-ptr re-set))
;;         ;; (display (format "~a dropped~%"))
;;         ))))

(define chezure-set-guardian (make-guardian))

(define make-chezure-set
  (lambda (ptr len)
    (let ([re-set (mk-chezure-set ptr len)])
      (chezure-set-guardian re-set)
      re-set)))

;;; Captures
(define-record-type
    (chezure-captures mk-chezure-captures chezure-captures?)
  (fields (immutable names %chezure-captures-names)
          (immutable names-index-map)
          (immutable matches)))

(module ()
  (record-writer
   (type-descriptor chezure-captures)
   (lambda (r p wr)
     (display (format "#<chezure-captures, ~d groups>"
                      (vector-length (chezure-captures-matches r)))
              p))))

(define (get-all-captures-names re*)
  (define (char*->string ptr)
    (let loop ([i 0]
               [lst (list)])
      (let ([c (foreign-ref 'unsigned-8 ptr i)])
        (if (fxzero? c)
            (utf8->string (u8-list->bytevector (reverse! lst)))
            (loop (fx1+ i) (cons c lst))))))
  (let ([it_names* (rure_iter_capture_names_new re*)]
        [name* (foreign-alloc (foreign-sizeof 'void*))])
    (let loop ([res (list)])
      (if (rure_iter_capture_names_next it_names* name*)
          (loop (cons (char*->string (foreign-ref 'void* name* 0)) res))
          (begin (rure_iter_capture_names_free it_names*)
                 (foreign-free name*)
                 (filter (lambda (s)
                           (not (fxzero? (string-length s))))
                         (reverse! res)))))))

(define (chezure-captures-names x)
  (cond [(chezure? x)
         (get-all-captures-names (chezure-ptr x))]
        [(chezure-captures? x)
         (%chezure-captures-names x)]
        [else (errorf "~a is not a chezure or chezure-captures object" x)]))

(define (make-chezure-captures str re* caps*)
  (let* ([len (rure_captures_len caps*)]
         [matches (make-vector len)]
         [m* (rure_match_new)]
         [names (get-all-captures-names re*)]
         [indices (map (lambda (nm)
                         (rure_capture_name_index re* nm))
                       names)])
    (do ([i 0 (fx1+ i)])
        ((fx=? i len)
         (begin (rure_match_free m*)
                ;; the caller is responsible for releasing caps*
                ;; (rure_captures_free caps*)
                ;; so this API won't be exposed from the library
                (mk-chezure-captures
                 names
                 (map (lambda (n i) (cons n i)) names indices)
                 matches)))                       
      (rure_captures_at caps* i m*)
      (vector-set! matches i (make-chezure-match str m*)))))

(define (chezure-captures-index-valid? captures index)
  (cond [(string? index)
         (and (not (fxzero? (string-length index)))
              (member index (%chezure-captures-names captures)) #t)]
        [(and (fixnum? index)
              (fx<=? 0 index))
         (fx<? index (vector-length (chezure-captures-matches captures)))]
        [else #f]))

(define chezure-captures-ref/1
  (lambda (captures)
    (lambda (index)
      (unless (chezure-captures-index-valid? captures index)
        (assertion-violationf 'chezure-captures-ref "Invalid index: ~a" index))  
      (if (string? index)
          (vector-ref (chezure-captures-matches captures)
                      (cdr (assoc index (chezure-captures-names-index-map captures))))
          (vector-ref (chezure-captures-matches captures) index)))))

(define (chezure-captures-ref captures index)
  (unless (chezure-captures? captures)
    (assertion-violationf 'chezure-captures-ref "~a is not a chezurecaptures object" captures))
  (let ([mapper (chezure-captures-ref/1 captures)])
    (cond [(list? index) (map mapper index)]
          [(vector? index) (vector-map mapper index)]
          [else (mapper index)])))

(define (chezure-captures-string-ref captures index)
  (unless (chezure-captures? captures)
    (assertion-violationf 'chezure-captures-string-ref "~a is not a chezurecaptures object" captures))
  (let ([m (chezure-captures-ref captures index)])
    (cond [(list? index) (map chezure-match-str m)]
          [(vector? index) (vector-map chezure-match-str m)]
          [else (chezure-match-str m)])))
