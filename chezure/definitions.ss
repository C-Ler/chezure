;;; definitions.ss
(library (chezure definitions)
  (export chezure-flag chezure-flags chezure-flags->n
          make-chezure-options chezure-options? chezure-options-ptr
          mk-chezure-match make-chezure-match chezure-match?
          chezure-match-start chezure-match-end chezure-match-str chezure-match->alist
          make-chezure chezure? chezure-ptr
          make-chezure-set chezure-set-ptr chezure-set? chezure-set-len
          make-captures captures? captures-names captures-matches captures-ref captures-string-ref)
  (import (chezscheme)
          (chezure utils)
          (chezure low-level)
          (finalize))
  
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

  ;;; FIXME: is there a better way to specify a print for record types?
  (module ()
    (record-writer
     (type-descriptor chezure-match)
     (lambda (r p wr)
       (display (format "#<chezure-match start=~d, end=~d, str=~s>"
                        (chezure-match-start r)
                        (chezure-match-end r)
                        (chezure-match-str r))
                p))))

  (define (make-chezure-match str index-map m*)
    (let* ([byte-start (ftype-ref rure_match (start) m*)]
           [byte-end (ftype-ref rure_match (end) m*)]
           [start (byte-index->utf8-index index-map byte-start)]
           [end (byte-index->utf8-index index-map byte-end)])
      (mk-chezure-match start end (substring str start end))))

  (define (chezure-match->alist m)
    (list (cons 'start (chezure-match-start m))
          (cons 'end (chezure-match-end m))
          (cons 'str (chezure-match-str m))))

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

  (define (make-chezure ptr)    
    (finalize (mk-chezure ptr)
              (lambda (re)
                (rure_free (chezure-ptr re)))))

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

  (define (make-chezure-set ptr len)
    (finalize (mk-chezure-set ptr len)
              (lambda (re-set)
                (rure_set_free (chezure-set-ptr re-set)))))

  ;;; Captures
  (define-record-type
      (captures mk-captures captures?)
    (fields (immutable names %captures-names)
            (immutable names-index-map)
            (immutable matches)))

  (module ()
    (record-writer
     (type-descriptor captures)
     (lambda (r p wr)
       (display (format "#<captures, ~d groups>"
                        (vector-length (captures-matches r)))
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

  (define (captures-names x)
    (cond [(chezure? x)
           (get-all-captures-names (chezure-ptr x))]
          [(captures? x)
           (%captures-names x)]
          [else (errorf "~a is not a chezure or captures object" x)]))

  (define (make-captures str re* caps*)
    (let* ([len (rure_captures_len caps*)]
           [matches (make-vector len)]
           [m* (rure_match_new)]
           [names (get-all-captures-names re*)]
           [indices (map (lambda (nm)
                           (rure_capture_name_index re* nm))
                         names)])
      (do ([index-map (make-index-map str)]
           [i 0 (fx1+ i)])
          ((fx=? i len)
           (begin (rure_match_free m*)
                  ;; the caller is responsible for releasing caps*
                  ;; (rure_captures_free caps*)
                  ;; so this API won't be exposed from the library
                  (mk-captures
                   names
                   (map (lambda (n i) (cons n i)) names indices)
                   matches)))                       
        (rure_captures_at caps* i m*)
        (vector-set! matches i (make-chezure-match str index-map m*)))))

  (define (captures-index-valid? captures index)
    (cond [(string? index)
           (and (not (fxzero? (string-length index)))
                (member index (%captures-names captures)) #t)]
          [(and (fixnum? index)
                (fx<=? 0 index))
           (fx<? index (vector-length (captures-matches captures)))]
          [else #f]))

  (define captures-ref/1
    (lambda (captures)
      (lambda (index)
        (unless (captures-index-valid? captures index)
          (assertion-violationf 'captures-ref "Invalid index: ~a" index))  
        (if (string? index)
            (vector-ref (captures-matches captures)
                        (cdr (assoc index (captures-names-index-map captures))))
            (vector-ref (captures-matches captures) index)))))

  (define (captures-ref captures index)
    (unless (captures? captures)
      (assertion-violationf 'captures-ref "~a is not a chezurecaptures object" captures))
    (let ([mapper (captures-ref/1 captures)])
      (cond [(list? index) (map mapper index)]
            [(vector? index) (vector-map mapper index)]
            [else (mapper index)])))

  (define (captures-string-ref captures index)
    (unless (captures? captures)
      (assertion-violationf 'captures-string-ref "~a is not a chezurecaptures object" captures))
    (let ([m (captures-ref captures index)])
      (cond [(list? index) (map chezure-match-str m)]
            [(vector? index) (vector-map chezure-match-str m)]
            [else (chezure-match-str m)])))
  ) ;; end of library
