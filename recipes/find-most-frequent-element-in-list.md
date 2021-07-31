# Find most frequent element in list

## Problem

You have a list and you need to find the element with duplicates and
find the most frequent element. If the list has no duplicates is
should return `#f`.

## Solution

### Using an association list

```Scheme
(define (most-frequent xs)
  (let count-all-elements ((xs xs) (counters '()))
    (if (null? xs)
        (let find-max-count ((counters counters) (best #f))
          (if (null? counters)
              (and best (car best))
              (find-max-count
               (cdr counters)
               (let* ((counter (car counters))
                      (count (cdr counter)))
                 (if (and (> count 1) (or (not best) (> count (cdr best))))
                     counter
                     best)))))
        (count-all-elements
         (cdr xs)
         (let* ((x (car xs))
                (counter (assoc x counters)))
           (if (not counter)
               (cons (cons x 1) counters)
               (begin (set-cdr! counter (+ (cdr counter) 1))
                      counters)))))))
```

Credit [Lassi Kortela](https://github.com/lassik)

### Using a hash table (SRFI 125)

```Scheme
(define (most-frequent xs)
  (define (inc n) (+ n 1))
  (let ((counts (make-hash-table equal?)))
    (let loop ((xs xs) (best-x #f))
      (if (null? xs)
          best-x
          (loop (cdr xs)
                (let ((x (car xs)))
                  (hash-table-update!/default counts x inc 0)
                  (let ((count (hash-table-ref counts x)))
                    (if (and (> count 1)
                             (or (not best-x)
                                 (> count (hash-table-ref counts best-x))))
                        x
                        best-x))))))))
```

Credit [Lassi Kortela](https://github.com/lassik)

## Usage

```Scheme
(most-frequent '(1 2 3 4 2 3 4 2 2 2))
;; ==> 2

(most-frequent '(1 2 3 4 5 6 7))
;; ==> #f
```
