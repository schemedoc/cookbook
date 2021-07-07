# Join list of strings with delimiter

## Problem

You have a list of strings and string delimiter and you want to create
a single string from the list with a delimiter in between

## Solution

### SRFI

A `string-join` procedure is provided
[by SRFI 13](https://srfi.schemers.org/srfi-13/srfi-13.html#string-join) and
[by SRFI 130](https://srfi.schemers.org/srfi-130/srfi-130.html#string-join).

### Using a loop

```
(define (string-join lst delimiter)
  (if (null? lst) ""
      (let loop ((result (car lst)) (lst (cdr lst)))
        (if (null? lst)
            result
            (loop (string-append result delimiter (car lst))
                  (cdr lst))))))
```

Credit: [Jakub T. Jankiewicz](https://jcubic.pl/me)

### Using fold

```
(define (string-join lst delimiter)
  (if (null? lst) ""
      (fold (lambda (item result) (string-append result delimiter item))
            (car lst)
            (cdr lst))))
```

Credit: [Lassi Kortela](https://github.com/lassik)

### Using string ports

```
(define (string-join lst delimiter)
  (if (null? lst) ""
      (call-with-port (open-output-string)
                      (lambda (out)
                        (write-string (car lst) out)
                        (for-each (lambda (item)
                                    (write-string delimiter out)
                                    (write-string item out))
                                  (cdr lst))
                        (get-output-string out)))))
```

Credit: [Lassi Kortela](https://github.com/lassik)

## Usage

```
(string-join '("foo" "bar" "baz") ":")
;; ==> "foo:bar:baz"
```
