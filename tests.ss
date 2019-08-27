;;;; tests.ss
(import (chezscheme) (chezure) (srfi s78 lightweight-testing))

(check-set-mode! 'report)

(define re (chezure-compile "[a-z]+"))
(define re-i (chezure-compile "[a-z]+" (chezure-flags ignorecase unicode)))
(define re-unicode (chezure-compile "中国|香港"))
(define re-named (chezure-compile "(?P<year>\\d{4})-(?P<month>\\d{2})-(?P<day>\\d{2})"))
(define re-named-unicode (chezure-compile "(?P<country>中国|瑞典)\\w+(?P<continent>亚洲|欧洲)\\w+"))
(define re-list (list re re-i re-unicode re-named re-named-unicode))
(define re-set (chezure-compile-set (list "foo" "barfoo" "\\w+" "\\d+" "foobar" "bar")))

;;; chezure-compile
(for-each (lambda (re) (check (chezure? re) => #t))
          re-list)

;;; chezure-compile-set
(check (chezure-set? re-set) => #t)
(check (chezure-set-len re-set) => 6)
(check (chezure-set-has-match? re-set "foobar") => #t)
(check (chezure-set-matches re-set "foobar")
       => '(#t #f #t #f #t #t))

;;; chezure-escape
(check (chezure-escape "^[a-z]+.*$") => "\\^\\[a\\-z\\]\\+\\.\\*\\$")

;;; chezure-has-match?
(check (chezure-has-match? re "abc123") => #t)
(check (chezure-has-match? re "abc123" 3) => #f)
(check (chezure-has-match? re "123ABC") => #f)
(check (chezure-has-match? re-i "123ABC") => #t)
(check (chezure-has-match? re-unicode "你好，世界！") => #f)
(check (chezure-has-match? re-unicode "中国与香港的关系走向何方？") => #t)

;;; chezure-shortest-match
(let* ([re (chezure-compile "a+")]
       [str "aaaaab"]
       [res1 (chezure-shortest-match re str)]
       [res2 (chezure-shortest-match re str 5)])
  (check res1 => 1)
  (check res2 => #f))

(let* ([str "中国与香港的关系走向何方？"]
       [r1 (chezure-shortest-match re-unicode str)]
       [r2 (chezure-shortest-match re-unicode str 2)]
       [r3 (chezure-shortest-match re-unicode str 5)])
  (check r1 => 2)
  (check r2 => 5)
  (check r3 => #f))

;;; chezure-find
(let ([matches (chezure-find re "abc123def456")])
  (check (length matches) => 2)
  (check (chezure-match-str (car matches)) => "abc")
  (check (chezure-match-str (cadr matches)) => "def"))

(let ([matches (chezure-find re "abc123def456ghi789" 2)])
  (check (length matches) => 2)
  (check (chezure-match-str (car matches)) => "abc")
  (check (chezure-match-str (cadr matches)) => "def"))

(let ([matches (chezure-find re-i "ABC123DEF456GHI789")])
  (check (length matches) => 3)
  (check (chezure-match-str (car matches)) => "ABC")
  (check (chezure-match-str (cadr matches)) => "DEF")
  (check (chezure-match-str (caddr matches)) => "GHI"))

(let ([matches (chezure-find re-unicode "中国与香港的关系走向何方？")])
  (check (length matches) => 2)
  (check (chezure-match-str (car matches)) => "中国")
  (check (chezure-match-str (cadr matches)) => "香港"))

;;; chezure-find-captures
(check (captures-names re-named) => '("year" "month" "day"))
(let ([caps (car (chezure-find-captures re-named "1992-01-03, 1993-10-01, 1992-04-10" 1))])
  (check (captures-names caps) => '("year" "month" "day")))

(let ([caps (car (chezure-find-captures re-named "1992-01-03, 1993-10-01, 1992-04-10" 1))])
  (check (captures-string-ref caps 0) => "1992-01-03")
  (check (captures-string-ref caps 1) => "1992")
  (check (captures-string-ref caps 2) => "01")
  (check (captures-string-ref caps 3) => "03")
  (check (captures-string-ref caps "year") => "1992")
  (check (captures-string-ref caps "month") => "01")
  (check (captures-string-ref caps "day") => "03"))

(let ([caps (cadr (chezure-find-captures re-named "1992-01-03, 1993-10-01, 1992-04-10" 2))])
  (check (captures-string-ref caps 0) => "1993-10-01")
  (check (captures-string-ref caps 1) => "1993")
  (check (captures-string-ref caps 2) => "10")
  (check (captures-string-ref caps 3) => "01")
  (check (captures-string-ref caps "year") => "1993")
  (check (captures-string-ref caps "month") => "10")
  (check (captures-string-ref caps "day") => "01"))

(let ([caps (caddr (chezure-find-captures re-named "1992-01-03, 1993-10-01, 1992-04-10" 10))])
  (check (captures-string-ref caps 0) => "1992-04-10")
  (check (captures-string-ref caps 1) => "1992")
  (check (captures-string-ref caps 2) => "04")
  (check (captures-string-ref caps 3) => "10")
  (check (captures-string-ref caps "year") => "1992")
  (check (captures-string-ref caps "month") => "04")
  (check (captures-string-ref caps "day") => "10"))

(let ([caps (car (chezure-find-captures re-named "1992-01-03, 1993-10-01, 1992-04-10" 1))])
  (check (captures-string-ref caps '(1 2)) => '("1992" "01"))
  (check (captures-string-ref caps (vector 1 3)) => (vector "1992" "03"))
  (check (captures-string-ref caps (vector "day" "year")) => (vector "03" "1992")))

(check (captures-names re-named-unicode) => '("country" "continent"))
(let ([caps (car (chezure-find-captures re-named-unicode "中国是亚洲国家，瑞典是欧洲国家"))])
  (check (captures-string-ref caps 0) => "中国是亚洲国家")
  (check (captures-string-ref caps 1) => "中国")
  (check (captures-string-ref caps 2) => "亚洲")
  (check (captures-string-ref caps "country") => "中国")
  (check (captures-string-ref caps "continent") => "亚洲"))

(let ([caps (cadr (chezure-find-captures re-named-unicode "中国是亚洲国家，瑞典是欧洲国家"))])
  (check (captures-string-ref caps 0) => "瑞典是欧洲国家")
  (check (captures-string-ref caps 1) => "瑞典")
  (check (captures-string-ref caps 2) => "欧洲")
  (check (captures-string-ref caps "country") => "瑞典")
  (check (captures-string-ref caps "continent") => "欧洲"))

;;; chezure-split
(check (chezure-split re "abc123def456ghi789")
       => '("" "123" "456" "789"))
(check (chezure-split re "abc123def456ghi789" 2)
       => '("" "123" "456ghi789"))
(check (chezure-split re "abc123def456ghi789" 2 #t)
       => '("" "abc" "123" "def" "456ghi789"))
(check (chezure-split re "abc123def456ghi789" 0 #t #t)
       => '("abc" "123" "def" "456" "ghi" "789"))
(check (chezure-split re-i (string-upcase "abc123def456ghi789") 0 #t #t)
       => '("ABC" "123" "DEF" "456" "GHI" "789"))
(check (chezure-split re-unicode "中国与香港的关系走向何方？" 0 #t #t)
       => '("中国" "与" "香港" "的关系走向何方？"))

;;; chezure-replace
(check (chezure-replace re "abc123def456ghi789" " hello ")
       => " hello 123 hello 456 hello 789")
(check (chezure-replace re "abc123def456ghi789" " hello " 2)
       => " hello 123 hello 456ghi789")
(check (chezure-replace re-named "1992-01-03, 1993-10-01, 1992-04-10" "<DATE>")
       => "<DATE>, <DATE>, <DATE>")
(let ([repl (lambda (caps)
              (if (string=? (captures-string-ref caps "year")
                            "1992")
                  "the year of 1992"
                  (captures-string-ref caps 0)))])
  (check (chezure-replace re-named "1992-01-03, 1993-10-01, 1992-04-10" repl)
         => "the year of 1992, 1993-10-01, the year of 1992"))
(check (chezure-replace re-unicode "中国与香港的关系走向何方？" "<COUNTRY>")
       => "<COUNTRY>与<COUNTRY>的关系走向何方？")
(check (chezure-replace re-unicode "中国与香港的关系走向何方？" "<COUNTRY>" 1)
       => "<COUNTRY>与香港的关系走向何方？")
(let ([repl (lambda (caps)
              (if (and (string=? (captures-string-ref caps "country") "中国")
                       (string=? (captures-string-ref caps "continent") "亚洲"))
                  "日本是亚洲国家"
                  (captures-string-ref caps 0)))])
  (check (chezure-replace re-named-unicode "中国是亚洲国家，瑞典是欧洲国家" repl)
         => "日本是亚洲国家，瑞典是欧洲国家"))

(check-report)
