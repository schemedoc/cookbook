# Get type of object

## Problem

You need a procedure that will return the type of the object as a string.

## Solution

```Scheme
(cond-expand
 (r7rs
  (define type-of-alist-extra (list (cons bytevector? 'bytevector))))
 (else
  (define type-of-alist-extra '())))

(define type-of-alist
  (append type-of-alist-extra
          (list (cons boolean?    'boolean)
                (cons char?       'character)
                (cons eof-object? 'eof-object)
                (cons null?       'null)
                (cons number?     'number)
                (cons pair?       'pair)
                (cons port?       'port)
                (cons procedure?  'procedure)
                (cons string?     'string)
                (cons symbol?     'symbol)
                (cons vector?     'vector))))

(define (type-of obj)
  (let loop ((alist type-of-alist))
    (and (not (null? alist))
         (if ((caar alist) obj) (cdar alist) (loop (cdr alist))))))
```

Credit: [Lassi Kortela](https://github.com/lassik)

## Usage

```Scheme
(type-of "foo")
;; ==> string

(type-of #\x)
;; ==> character

(type-of (current-input-port))
;; ==> input-port
```
