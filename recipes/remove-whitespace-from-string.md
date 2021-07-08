# Remove whitespace from string

## Problem

Remove extraneous whitespace (i.e. space, tab, newline) characters
from a string.

## Solution

### Using SRFI 13

These can remove arbitrary characters, but the default is to remove
whitespace:

* `(string-trim s)`
* `(string-trim-right s)`
* `(string-trim-both s)`

### Using regular expressions

```
(define string-trim-both
  (let ((r (regexp "^[ \t\r]*(.*?)[ \t\r]*$")))
    (lambda (s)
      (cadr (regexp-match r s)))))
```

The regular expression picks out the middle of the string. It is
compiled only once, namely when `string-trim-both` is defined.

The `regexp` and `regexp-match` procedures are from Racket. Equivalent
procedures exist for some other implementations, and in
[SRFI 115](https://srfi.schemers.org/srfi-115/srfi-115.html).

Credit: [Jens Axel Søgaard](http://scheme.dk/)

## Usage

```
> (string-trim-both " foo ")
"foo"
```

```
> (string-trim-both " foo bar ")
"foo bar"
```

```
> (string-trim-both " foo bar")
"foo bar"
```

```
> (string-trim-both "foo bar")
"foo bar"
```

```
> (string-trim-both "")
""
```

```
> (string-trim-both "   ")
""
```
