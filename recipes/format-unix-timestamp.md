# Format Unix timestamp

## Problem

Given an integer saying the number of seconds since the Unix epoch
(`1970-01-01 00:00:00`) format it as a human-readable date and time
string.

## Solution

### Using SRFI 19

```Scheme
(define (time-unix->time-utc seconds)
  (add-duration (date->time-utc (make-date 0 0 0 0 1 1 1970 0))
                (make-time time-duration 0 seconds)))

(define (time-unix->string seconds . maybe-format)
  (apply date->string (time-utc->date (time-unix->time-utc seconds))
         maybe-format))
```

Credit: [Göran Weinholt](https://weinholt.se/)

## Usage

```Scheme
(time-unix->string 946684800)
;; => "Sat Jan 01 00:00:00Z 2000"      ; Loko Scheme
;; => "Sat Jan 01 02:00:00+0200 2000"  ; Chez Scheme
;; => "Sat Jan 01 01:00:00+0100 2000"  ; Guile
```
