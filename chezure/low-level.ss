;;; low-level.ss
;;; all low-level definitions from librure
(library (chezure low-level)
  (export
   RURE_FLAG_CASEI RURE_FLAG_MULTI RURE_FLAG_DOTNL
   RURE_FLAG_SWAP_GREED RURE_FLAG_SPACE RURE_FLAG_UNICODE RURE_DEFAULT_FLAGS
   rure_match rure_match_new rure_match_free
   rure_compile_must rure_compile rure_free rure_is_match rure_find rure_find_captures
   rure_shortest_match rure_capture_name_index
   rure_iter_capture_names_new rure_iter_capture_names_free rure_iter_capture_names_next
   rure_iter_new rure_iter_free rure_iter_next rure_iter_next_captures
   rure_captures_new rure_captures_free rure_captures_at rure_captures_len
   rure_options_new rure_options_free rure_options_size_limit rure_options_dfa_size_limit
   rure_compile_set rure_set_free rure_set_is_match rure_set_matches rure_set_len
   rure_error_new rure_error_free rure_error_message
   rure_escape_must rure_cstring_free)
  (import (chezscheme))

  ;;; The case insensitive (i) flag.
  (define RURE_FLAG_CASEI (fxsll 1 0))
  ;;; /* The multi-line matching (m) flag. (^ and $ match new line boundaries.)
  (define RURE_FLAG_MULTI (fxsll 1 1))
  ;;; The any character (s) flag. (. matches new line.)
  (define RURE_FLAG_DOTNL (fxsll 1 2))
  ;;; The greedy swap (U) flag. (e.g., + is ungreedy and +? is greedy.) 
  (define RURE_FLAG_SWAP_GREED (fxsll 1 3))
  ;;; The ignore whitespace (x) flag.
  (define RURE_FLAG_SPACE (fxsll 1 4))
  ;;; The Unicode (u) flag.
  (define RURE_FLAG_UNICODE (fxsll 1 5))
  ;;; The default set of flags enabled when no flags are set.
  (define RURE_DEFAULT_FLAGS RURE_FLAG_UNICODE)

  (define-ftype rure_match
    (struct [start size_t]
            [end size_t]))

  (define rure_match_new
    (lambda ()
      (make-ftype-pointer
       rure_match
       (foreign-alloc (ftype-sizeof rure_match)))))

  (define rure_match_free
    (lambda (m*)
      (foreign-free (ftype-pointer-address m*))))

  (define rure_compile_must
    (foreign-procedure "rure_compile_must" (string) void*))

  (define rure_compile
    (foreign-procedure "rure_compile" (u8* size_t unsigned-32 void* void*) void*))

  (define rure_free
    (foreign-procedure "rure_free" (void*) void))

  (define rure_is_match
    (foreign-procedure "rure_is_match" (void* u8* size_t size_t) boolean))

  (define rure_find
    (foreign-procedure "rure_find" (void* u8* size_t size_t (* rure_match)) boolean))

  (define rure_find_captures
    (foreign-procedure "rure_find_captures" (void* u8* size_t size_t void*) boolean))

  (define rure_shortest_match
    (foreign-procedure "rure_shortest_match" (void* u8* size_t size_t void*) boolean))

  (define rure_capture_name_index
    (foreign-procedure "rure_capture_name_index" (void* string) integer-32))

  (define rure_iter_capture_names_new
    (foreign-procedure "rure_iter_capture_names_new" (void*) void*))

  (define rure_iter_capture_names_free
    (foreign-procedure "rure_iter_capture_names_free" (void*) void))

  (define rure_iter_capture_names_next
    (foreign-procedure "rure_iter_capture_names_next" (void* void*) boolean))

  (define rure_iter_new
    (foreign-procedure "rure_iter_new" (void*) void*))

  (define rure_iter_free
    (foreign-procedure "rure_iter_free" (void*) void))

  (define rure_iter_next
    (foreign-procedure "rure_iter_next" (void* u8* size_t (* rure_match)) boolean))

  (define rure_iter_next_captures
    (foreign-procedure "rure_iter_next_captures" (void* u8* size_t void*) boolean))

  (define rure_captures_new
    (foreign-procedure "rure_captures_new" (void*) void*))

  (define rure_captures_free
    (foreign-procedure "rure_captures_free" (void*) void))

  (define rure_captures_at
    (foreign-procedure "rure_captures_at" (void* size_t (* rure_match)) boolean))

  (define rure_captures_len
    (foreign-procedure "rure_captures_len" (void*) size_t))

  (define rure_options_new
    (foreign-procedure "rure_options_new" () void*))

  (define rure_options_free
    (foreign-procedure "rure_options_free" (void*) void))

  (define rure_options_size_limit
    (foreign-procedure "rure_options_size_limit" (void* size_t) void))

  (define rure_options_dfa_size_limit
    (foreign-procedure "rure_options_dfa_size_limit" (void* size_t) void))

  (define rure_compile_set
    (foreign-procedure "rure_compile_set"
                       (void* void* size_t unsigned-32 void* void*)
                       void*))

  (define rure_set_free
    (foreign-procedure "rure_set_free" (void*) void))

  (define rure_set_is_match
    (foreign-procedure "rure_set_is_match" (void* u8* size_t size_t) boolean))

  (define rure_set_matches
    (foreign-procedure "rure_set_matches" (void* u8* size_t size_t void*) boolean))

  (define rure_set_len
    (foreign-procedure "rure_set_len" (void*) size_t))

  (define rure_error_new
    (foreign-procedure "rure_error_new" () void*))

  (define rure_error_free
    (foreign-procedure "rure_error_free" (void*) void))

  (define rure_error_message
    (foreign-procedure "rure_error_message" (void*) string))

  (define rure_escape_must
    (foreign-procedure "rure_escape_must" (string) string))

  (define rure_cstring_free
    (foreign-procedure "rure_cstring_free" (string) void))
  
  ) ;; end of library
