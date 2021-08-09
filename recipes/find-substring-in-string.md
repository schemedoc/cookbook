# Find substring in string

## Problem

You want to search and find the index of a string inside a bigger string.

## Solution

```scheme
(define (string-find haystack needle . rest)
  (let ((start (if (null? rest) 0 (car rest))))
    (let* ((haystack-len (string-length haystack))
           (needle-len (string-length needle))
           (start 0))
      (let loop ((h-index start)
                 (n-index 0))
        (let ((h-char (string-ref haystack h-index))
              (n-char (string-ref needle n-index)))
          (if (char=? h-char n-char)
              (if (= (+ n-index 1) needle-len)
                  (+ (- h-index needle-len) 1)
                  (loop (+ h-index 1) (+ n-index 1)))
              (if (= (+ h-index 1) haystack-len)
                  #f
                  (loop (+ h-index 1) 0))))))))
```

Credit: [Jakub T. Jankiewicz](https://jcubic.pl/me)

### SRFI

The same functionality is provided by SRFI-13 function `string-contains` and `string-contains-ci`

## Usage

```scheme
(let* ((input "This is hello world")
       (search "hello")
       (found (string-find input search)))
  (if found
      (begin
        (display (substring input found))
        (newline))))
;; ==> "hello world"
```
