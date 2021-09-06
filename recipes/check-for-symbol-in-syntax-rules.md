# Check for symbol in syntax-rules

## Problem

You are writing a `syntax-rules` macro, and want to check whether or
not the value of a pattern variable is a symbol.

## Solution

`syntax-rules` doesn't let the programmer directly check data types,
but there's a famous trick to do it using nested `syntax-rules`:

```Scheme
(define-syntax symbol??
  (syntax-rules ()
    ((symbol?? (x . y) kt kf) kf)       ; It's a pair, not a symbol
    ((symbol?? #(x ...) kt kf) kf)      ; It's a vector, not a symbol
    ((symbol?? maybe-symbol kt kf)
      (let-syntax
        ((test
           (syntax-rules ()
             ((test maybe-symbol t f) t)
             ((test x t f) f))))
        (test abracadabra kt kf)))))
```

The expression `(symbol?? expr kt kf)` expands to `kt` if `expr` is a
symbol. Else it expands to `kf`.

Credit: [Oleg Kiselyov](http://okmij.org/ftp/)
(ref: [okmij.org](http://okmij.org/ftp/Scheme/macros.html#macro-symbol-p))

## Usage

```Scheme
(define-syntax study
  (syntax-rules ()
    ((study obj)
     (begin (write 'obj)
            (display (symbol?? obj
                               " is a symbol"
                               " is not a symbol"))
            (newline)))))

(study 78)
;; 78 is not a symbol
(study (kekkonen))
;; (kekkonen) is not a symbol
(study "kekkonen")
;; "kekkonen" is not a symbol
(study 'kekkonen)
;; (quote kekkonen) is not a symbol
(study kekkonen)
;; kekkonen is a symbol
```
