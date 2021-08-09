# Index a list

## Problem

Compute an index of the list `elements`.  Use the procedure passed as
the `choose-key` argument to extract the values that will be used as
the keys.  Use the procedure passed as the `choose-data` argument to
extract lists of the values in the resulting index.  Store the result
in the hash table provided as the `table` argument.

The hash table procedures are from [SRFI 69](https://srfi.schemers.org/srfi-69/).

## Solution

```Scheme
(define (index-list elements table choose-data choose-key)
  (for-each
   (lambda (e)
     (let ((key (choose-key e)))
       (hash-table-set! table
                        key
                        (append (choose-data e)
                                (hash-table-ref/default table key '())))))
   elements)
  table)
```

Credit: [Arthur A. Gleckler](https://speechcode.com/)

## Usage

```Scheme
(hash-table->alist
 (index-list '((foo . bar) (bat . bar) (quux . baz))
             (make-eq-hash-table)
             (lambda (a) (list (car a)))
             cdr))

;; ==> ((bar bat foo) (baz quux))
```
