;;;; chezure.sls
(library (chezure)
  (export chezure-flag chezure-flags make-chezure-options chezure-options?
          chezure-match? chezure-match-start chezure-match-end chezure-match-str chezure-match->alist
          chezure? chezure-set? chezure-set-len
          captures? captures-names captures-ref captures-string-ref
          chezure-compile chezure-compile-set chezure-escape
          chezure-has-match? chezure-set-has-match? chezure-set-matches chezure-shortest-match
          chezure-find chezure-find-captures chezure-split chezure-replace
          ;; Low-level APIs
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
  (import (chezure low-level)
          (chezure definitions)
          (chezure api)
          (chezscheme))
  )
