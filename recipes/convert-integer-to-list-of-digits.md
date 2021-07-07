# Convert integer to list of digits

## Problem

You have an integer, and you want a list of all its base-10 digits as
integers.

If the integer is negative, the list should start with a minus sign.

## Solution

### Using number->string

```
(define (integer->list integer)
  (let ((chars (string->list (number->string integer))))
    (map (lambda (char)
           (string->number (string char)))
         chars)))
```

Credit: [Jakub T. Jankiewicz](https://jcubic.pl/me)

### Using quotient and remainder

The `truncate/` procedure (from R7RS) performs an integer division,
and returns both the quotient and the remainder. In some other
programming languages, this operation is called `divmod`.

```
(define (integer->list integer)
  (let ((neg? (negative? integer)))
    (let loop ((integer (abs integer)) (digits '()))
      (let-values (((integer digit) (truncate/ integer 10)))
        (let ((digits (cons digit digits)))
          (if (> integer 0) (loop integer digits)
              (if neg? (cons '- digits) digits)))))))
```

Credit: [Lassi Kortela](https://github.com/lassik)

## Usage

```
(integer->list 123450)
;; ==> (1 2 3 4 5 0)
```

```
(integer->list -123450)
;; ==> (- 1 2 3 4 5 0)
```

```
(integer->list 0)
;; ==> (0)
```
