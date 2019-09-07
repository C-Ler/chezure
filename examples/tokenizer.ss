;;;; tokenizer.ss
;;; similar to https://docs.python.org/3/library/re.html#writing-a-tokenizer

(import (chezscheme) (chezure))

(define-record-type token
  (fields type value line column))

(define keywords
  (map symbol->string '(IF THEN ENDIF FOR NEXT GOSUB RETURN)))

(define token-specification
  '(("NUMBER" .  "\\d+(\\.\\d*)?")  ;; Integer or decimal number
    ("ASSIGN" .  ":=")           ;; Assignment operator
    ("END" .     ";")            ;; Statement terminator
    ("ID" .      "[A-Za-z]+")    ;; Identifiers
    ("OP" .      "[+\\-*/]")      ;; Arithmetic operators
    ("NEWLINE" . "\\n")           ;; Line endings
    ("SKIP" .    "[ \\t]+")       ;; Skip over spaces and tabs
    ("MISMATCH" .".")))            ;; Any other character

(define tok-regex
  (chezure-compile 
   (format "~{~a~^|~}"
           (map (lambda (s)
                  (format "(?P<~a>~a)" (car s) (cdr s)))
                token-specification))))

(define statements "IF quantity THEN
        total := total + price * quantity;
        tax := price * 0.05;
    ENDIF;")

(define (tokenizer caps)
  (let* ([m (captures-ref caps 1)]
         [type (chezure-match-name m)]
         [value (chezure-match-str m)])
    (cond [(string=? type "NUMBER")
           (set! value (string->number value))]
          [(and (string=? type "ID")
                (member value keywords))
           (set! type "KEYWORD")]
          [(string=? type "MISMATCH")
           (errorf 'tokenize "~a unexpected on line ~d" value line-num)])
    (list (cons 'type type)
          (cons 'value value))))
  
(define (tokenize code)
  (filter (lambda (x)
            (not (member (cdr (assq 'type x)) '("NEWLINE" "SKIP"))))
          (map tokenizer (chezure-find-captures tok-regex code))))
