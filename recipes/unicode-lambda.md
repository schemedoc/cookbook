# Unicode lambda

## Problem

Instead of

`(lambda (a b c) (* a (+ b c))`

you'd like to type

`(λ (a b c) (* a (+ b c)))`

i.e. use the Unicode greek letter lambda in your Scheme code.

## Solution

### Using import renaming

R6RS:

```Scheme
(import (rename (only (rnrs base) lambda) (lambda λ)))
```

R7RS:

```Scheme
(import (rename (only (scheme base) lambda) (lambda λ)))
```

### Using a macro

```Scheme
(define-syntax λ (syntax-rules () ((λ . rest) (lambda . rest))))
```

or the non-standard:

```Scheme
(define-macro (λ . rest) `(lambda ,@rest))
```

### Spelling out `lambda` in source files but displaying `λ` on screen

See [Display Unicode symbols in Emacs](https://cookbook.scheme.org/display-unicode-symbols-in-emacs/).

### Safely storing `λ` in text files

`λ` is one Unicode codepoint, so changing the Unicode normalization
form does not change the way `λ` characters are encoded. In R7RS `λ`
can also be escaped using vertical bar syntax as `|\x3bb;|`.

## See also

[Unicode lambda at Scheme Surveys](https://doc.scheme.org/surveys/unicode-lambda/)
