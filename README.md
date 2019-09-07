# Chezure

[![Build Status](https://travis-ci.org/macdavid313/Chezure.svg?branch=master)](https://travis-ci.org/macdavid313/Chezure)

Chez Scheme bindings for Rust's [regular expression API](https://crates.io/crates/regex).

[Documentation](https://macdavid313.github.io/chezure) is **still** under construction, and the APIs may be changed.

## Installation

You can either download the pre-compiled binaries from [release](https://github.com/macdavid313/chezure/releases) or build it by running `build.ss`. Don't forget letting Chez know where is `chezure` and `chez-finalize` libraries, for example:

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
;; => ((start . 3) (end . 6) (str . "123"))
(chezure-match->alist (cadr matches))
;; => ((start . 9) (end . 12) (str . "456"))
```

A `chezure-match` object records the span information of the matched substring. **Chezure** also supports unicode strings:

```scheme
(define re (chezure-compile "中国"))
(define matches (chezure-find re "中国是亚洲国家，盐城是中国的一个城市")) ;; a list of chezure-match object
(chezure-match->alist (car matches))
;; => ((start . 0) (end . 2) (str . "中国"))
(chezure-match->alist (cadr matches))
;; => ((start . 11) (end . 13) (str . "中国"))
```

**Chezure** also implements capturing groups, but it's considered to be slower than ordinary patterns -- as stated by Rust's [API](https://github.com/rust-lang/regex/blob/master/regex-capi/include/rure.h#L71):

> Computing the capture groups of a match can carry a significant performance penalty, so their use in the API is optional.

```scheme
(define re (chezure-compile "(?P<year>\\d{4})-(?P<month>\\d{2})-(?P<day>\\d{2})"))
(captures-names re) ;; => ("year" "month" "day")
(define all-captures (chezure-find-captures re "2019-08-17, 1884-10-01")) ;; => a list of captures
(define caps (car all-captures)) ;; select the first captures
(captures-ref caps 0) ;; => #<chezure-match start=0, end=10, str="2019-08-17">
(captures-ref caps 1) ;; => #<chezure-match start=0, end=4, str="2019">
(captures-ref caps "year") ;; => #<chezure-match start=0, end=4, str="2019">
;; reference by either a list or vector of names or indices
(captures-ref caps '(0 1 "month" "day")) 
;; => (#<chezure-match start=0, end=10, str="2019-08-17">
;;     #<chezure-match start=0, end=4, str="2019">
;;     #<chezure-match start=5, end=7, str="08">
;;     #<chezure-match start=8, end=10, str="17">)
(captures-ref caps (vector "year" "month" "day"))
;; => #(#<chezure-match start=0, end=4, str="2019">
;;      #<chezure-match start=5, end=7, str="08">
;;      #<chezure-match start=8, end=10, str="17">)
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

## APIs

### chezure-compile

```
procedure: (chezure-compile pattern)
procedure: (chezure-compile pattern flags)
procedure: (chezure-compile pattern flags options)
returns: a `chezure` object holding the compiled regular expression
library: (chezure)
```

`pattern` must be a string, `flags` must be a list of valid flags:

* ignorecase, as the case insensitive (i) flag.
* multiline, as the multi-line matching (m) flag. (^ and $ match new line boundaries.)
* dotnl, as the any character (s) flag. (. matches new line.)
* swap-greed, as the greedy swap (U) flag. (e.g., + is ungreedy and +? is greedy.)
* space, as the ignore whitespace (x) flag.
* unicode, as the Unicode (u) flag.

and `options` must be a list of arguments (`size-limit` and `dfa-size-limit` respectively) to setup the regular expression compiler's options. See Rust's documentation for [`size-limit`](https://github.com/rust-lang/regex/blob/master/regex-capi/include/rure.h#L428) and [`dfa-size-limit`](https://github.com/rust-lang/regex/blob/master/regex-capi/include/rure.h#L442)

If there was a problem compiling the pattern, an error with debug information will be raised.

### chezure?

As the predicate of `chezure` type.

### chezure-match

```
procedure: (chezure-match? x)
procedure: (chezure-match-name x)
procedure: (chezure-match-start x)
procedure: (chezure-match-end x)
procedure: (chezure-match-str x)
procedure: (chezure-match->alist x)
library: (chezure)
```

`chezure-match` is an object recording the information of the matched substring. `chezure` exports APIs to access its fields.

### chezure-has-match?

```
procedure: (chezure-has-match? chezure str)
procedure: (chezure-has-match? chezure str start)
returns: a boolean indicating if there exists a match
library: (chezure)
```

`chezure-has-match?` returns `#t` if and only if `chezure` matches anywhere in in the given string `str`. `start` is the position at which to start searching, hence it must be a non-negative fixnum. If `start` is not given, `0` will be applied.

### chezure-shortest-match

```
procedure: (chezure-shortest-match chezure str)
procedure: (chezure-shortest-match chezure str start)
returns: a non-negative fixnum or a boolean
library: (chezure)
```

`chezure-shortest-match` returns the `#f` if and only if `chezure` matches nowhere in the given string `str`. Otherwise, if a match is found, then return the `end` location of the given `str`. The end location is the place at which the regex engine determined that a match exists, but may occur before the end of the proper leftmost-first match.

`start` is the position at which to start searching, hence it must be a non-negative fixnum. If `start` is not given, `0` will be applied.

### chezure-find

```
procedure: (chezure-find chezure str)
procedure: (chezure-find chezure str limit)
returns: a list of `chezure-match`, if any
library: (chezure)
```

`chezure-find` returns a list of `chezure-match` objects, if `chezure` matches anywhere in the given string `str`. `limit` sets the maximum number of collected `chezure-match`; `0` means no limit at all.

### captures

```
procedure: (captures? x)
library: (chezure)
```

A `captures` object represents capturing groups in `chezure`. Internally, it records all group names, captured matches, and how to access those matches by either index or name.

### captures-names

```
procedure: (captures-names x)
returns: a list of group names
library: (chezure)
```

`x` must be either a `chezure` or a `captures` object. `captures-names` returns a list of group names:

```scheme
(define re (chezure-compile "(?P<first_name\\w+) (?P<last_name>\\w+)"))
(captures-names re) ;; => ("first_name" "last_name")
```

### chezure-find-captures

```
procedure: (chezure-find-captures chezure str)
procedure: (chezure-find-captures chezure str limit)
returns: a list of found `captures`
library: (chezure)
```

`chezure-find-captures` returns a list of `chezure-captures` objects, if `chezure` find capturing groups anywhere in the given string `str`. `limit` sets the maximum number of collected `chezure-match`; `0` means no limit at all.

### captures-ref

```
procedure: (captures-ref caps indices)
returns: the referenced `chezure-match` object(s)
library: (chezure)
```

`caps` must be a `captures` object, indices must be one of these types:

* a non-negative fixnum
* a string
* a list of non-negative fixnum or string
* a vector of non-negative fixnum or string

When a non-negative fixnum or a string is given, `captures-ref` returns the corresponding `chezure-match` object. If a list or a vector is given, all contained indices will be mapped (by either `map` or `vector-map`) to the corresponding `chezure-match` object.

If any index is invalid, an error will be raised.

### captures-string-ref

```
procedure: (captures-string-ref caps indices)
returns: the `str` field(s) of the corresponding `chezure-match` object(s)
library: (chezure)
```

Like `captures-ref`, but returns the `str` field (s) of the corresponding `chezure-match` object(s).

### chezure-compile-set

```
procedure: (chezure-compile-set patterns)
procedure: (chezure-compile-set patterns flags)
procedure: (chezure-compile-set patterns flags options)
returns: a `chezure-set` object
library: (chezure)
```

Rust's `regex` provides an [API](https://github.com/rust-lang/regex/blob/master/regex-capi/include/rure.h#L24) to compile a set of patterns (a list of strings). `flags` and `options` here are handled the same as in `chezure-compile`. It returns a `chezure-set` object.

### chezure-set?

As the predicate of `chezure-set` type.

### chezure-set-has-match?

```
procedure: (chezure-set-has-match? chezure-set str)
procedure: (chezure-set-has-match? chezure-set str start)
returns: a boolean indicating if there exists a match
library: (chezure)
```
`chezure-set-has-match?` returns `#t` if and only if `chezure-set` matches anywhere in in the given string `str`. `start` is the position at which to start searching, hence it must be a non-negative fixnum. If `start` is not given, `0` will be applied.

### chezure-set-matches

```
procedure: (chezure-set-matches chezure-set str)
procedure: (chezure-set-matches chezure-set str start)
returns: a list booleans
library: (chezure)
```

`chezure-set-matches` compares each regex in the patterns set against the given string `str` and returns a list of booleans indicating the match result of each pattern. 

Booleans are ordered in the same way as the `chezure-set` was compiled. For example, index 0 of matches corresponds to the first pattern passed to `chezure-compile-set`.

`start` is the position at which to start searching, hence it must be a non-negative fixnum. If `start` is not given, `0` will be applied.

### chezure-split

```
procedure: (chezure-split chezure str)
procedure: (chezure-split chezure str limit)
procedure: (chezure-split chezure str preserve?)
procedure: (chezure-split chezure str preserve? remove-empty?)
returns: the splited string
library: (chezure)
```

`chezure-split` splits string `str` by using `chezure`. `limit` sets the maximum number of splited ocurrances; `0` means no limit at all. If `preserve?` is set to `#t`, matched substring will be preserved. If `remove-empty?` is set to `#t`, all empty strings (including strings that only contain whitespace characters) will be filtered out.

`limit`, `preserve` and `remove-empty?` are set to `0`, `#f` and `#f` by default.

### chezure-replace

```
procedure: (chezure-replace chezure str repl)
procedure: (chezure-replace chezure str repl limit)
returns: the replaced string
library: (chezure)
```

`chezure-replace` replaces given string `str` by `repl`, which is either a string or a procedure. when `repl` is a procedure, it will be applied to the current `captures` object and expect a string returned to become the actual replacement. `limit` sets the maximum number of splited ocurrances; `0` means no limit at all.