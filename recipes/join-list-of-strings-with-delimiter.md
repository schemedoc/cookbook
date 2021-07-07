# Join list of strings with delimiter

## Problem

You have a list of strings and string delimiter and you want to create a single string from the list with a delimiter in between

## Solution

### SRFI

SRFI 13 provides the [`string-join` procedure](https://srfi.schemers.org/srfi-13/srfi-13.html#string-join).

### Using a loop

```
(define (string-join list delimiter)
  (let iter ((list list) (result '()))
    (if (null? list)
        (apply string-append (reverse result))
        (let ((item (car list))
              (rest (cdr list)))
          (if (null? result)
              (iter rest (cons item result))
              (iter rest (cons item (cons delimiter result))))))))
```

Credit [Jakub T. Jankiewicz](https://jcubic.pl/me)

### Using fold

```
(define (string-join list delimiter)
  (if (null? list) ""
      (fold (lambda (item result) (string-append result delimiter item))
            (car list)
            (cdr list))))
```

Credit [Lassi Kortela](https://github.com/lassik)

### Using string ports

```
(define (string-join list delimiter)
  (if (null? list) ""
      (call-with-port (open-output-string)
                      (lambda (out)
                        (write-string (car list) out)
                        (for-each (lambda (item)
                                    (write-string delimiter out)
                                    (write-string item out))
                                  (cdr list))
                        (get-output-string out)))))
```

Credit [Lassi Kortela](https://github.com/lassik)

## Usage

```
(string-join '("foo" "bar" "baz") ":")
;; ==> "foo:bar:baz"
```
