# Chezure

[![Build Status](https://travis-ci.org/macdavid313/Chezure.svg?branch=master)](https://travis-ci.org/macdavid313/Chezure)

Chez Scheme bindings for Rust's [regular expression API](https://crates.io/crates/regex).

Documentation is *still* under construction, and the APIs may be changed.

## Installation

You can either download the pre-compiled binaries from [release]() or build it by running `build.ss`. Don't forget letting Chez know where is `chezure` and `chez-finalize` libraries, for example:

```shell
> scheme --libdirs '$PROJECT:$PROJECT/chez-finalize' --script build.ss
```

`$PROJECT` is the path of the source code.

## Usage

Essentially, every regular expression must be compiled by `chezure-compile`:

```scheme
(import (chezure)) ;; again, don't forget to setup the library path for Chez Scheme
(define re (chezure-compile "[0-9]+")) ;; => a compiled regular expression object
```

now you can use the compiled pattern to search for matches:

```scheme
(define matches (chezure-find re "abc123def456")) ;; a list of chezure-match object
(chezure-match->alist (car matches))
;; => ((start . 3) (end . 6) (match . "123"))
(chezure-match->alist (cadr matches))
;; => ((start . 9) (end . 12) (match . "456"))
```

A `chezure-match` object records the span information of the matched substring. **Chezure** is also supports unicode strings:

```scheme
(define re (chezure-compile "中国"))
(define matches (chezure-find re "中国是亚洲国家，盐城是中国的一个城市")) ;; a list of chezure-match object
(chezure-match->alist (car matches))
;; => ((start . 0) (end . 2) (match . "中国"))
(chezure-match->alist (cadr matches))
;; => ((start . 11) (end . 13) (match . "中国"))
```

**Chezure** also implements capturing groups, but it's considered to be slower than ordinary patterns -- as stated by Rust's [API](https://github.com/rust-lang/regex/blob/master/regex-capi/include/rure.h#L71):

> Computing the capture groups of a match can carry a significant performance penalty, so their use in the API is optional.

```scheme
(define re (chezure-compile "(?P<year>\\d{4})-(?P<month>\\d{2})-(?P<day>\\d{2})"))
(captures-names re) ;; => ("year" "month" "day")
(define all-captures (chezure-find-captures re "2019-08-17, 1884-10-01")) ;; => a list of captures
(define caps (car all-captures)) ;; select the first captures
(captures-ref caps 0) ;; => #<chezure-match start=0, end=10, string="2019-08-17">
(captures-ref caps 1) ;; => #<chezure-match start=0, end=4, string="2019">
(captures-ref caps "year") ;; => #<chezure-match start=0, end=4, string="2019">
;; reference by either a list or vector of names or indices
(captures-ref caps '(0 1 "month" "day")) 
;; => (#<chezure-match start=0, end=10, string="2019-08-17">
;;     #<chezure-match start=0, end=4, string="2019">
;;     #<chezure-match start=5, end=7, string="08">
;;     #<chezure-match start=8, end=10, string="17">)
(captures-ref caps (vector "year" "month" "day"))
;; => #(#<chezure-match start=0, end=4, string="2019">
;;      #<chezure-match start=5, end=7, string="08">
;;      #<chezure-match start=8, end=10, string="17">)
```

If you just want to access the captured `string`, use `captures-string-ref`:

```scheme
(captures-string-ref caps 0) ;; => "2019-08-17"
(captures-string-ref caps "year") ;; => "2019"
(captures-string-ref caps '("year" "month" "day")) ;; => ("2019" "08" "17")
```

Finally, `split` and `replace` are implementted:

```scheme
(define re (chezure-compile "[0-9]+"))

;;; split
(chezure-split re "abc123def") ;; => ("abc" "def")
(chezure-split re "abc123def456" 1) ;; split only once (maximum), 0 means no limit
;; => ("abc" "def456")
(chezure-split re "abc123def456" 0 #t) ;; preverse the matched substring
;; => ("abc" "123" "def" "456" "")
(chezure-split re "abc123def456" 0 #t #t) ;; remove empty strings
;; => ("abc" "123" "def" "456")

;;; replace
(chezure-replace re "abc123def" "<NUMBER>") ;; => "abc<NUMBER>def"
(chezure-replace re "abc123def456" "<NUMBER>" 1) ;; replace only once (maximum), 0 means no limit
;; => "abc<NUMBER>def456"

;; the third aargument (replacement) can also be a procedure that takes only one argument
;; when it's a procedure, it will be applied to the current captures object and expect a string returned to become the actual replacement
;; since it needs to manipulate captures, `chezure-find-captures` will be used and thus will cause performace issues
(define re (chezure-compile "(?P<year>\\d{4})-(?P<month>\\d{2})-(?P<day>\\d{2})"))
(define (repl caps)
  (if (string=? (captures-string-ref caps "year") "2019")
      "<NOW>"
      "<PAST>"))
(chezure-replace re "2019-10-03, 1900-10-20" repl) ;; => "<NOW>, <PAST>"
```