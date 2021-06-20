## Problem
You have a list of strings and string delimiter and you want to create a single string from the list with a delimiter in between

## Solution
```scheme
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

**Alternative** you can use SRFI-13 that provides `string-join` function.

## Usage
```scheme
(string-join '("foo" "bar" "baz") ":")
;; ==> "foo:bar:baz"
```
