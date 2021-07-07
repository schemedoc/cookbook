## Problem
Given an integer saying the number of seconds since the Unix epoch (1970-01-01 00:00:00) format it as a human-readable date and time string.

## Solution

### SRFI
Using SRFI-19
```scheme
(define (time-unix->time-utc seconds)
  (add-duration (date->time-utc (make-date 0 0 0 0 1 1 1970 0))
                (make-time time-duration 0 seconds)))

(define (time-unix->string seconds . maybe-format)
  (apply date->string (time-utc->date (time-unix->time-utc seconds))
         maybe-format))
```

Credit [Göran Weinholt](https://weinholt.se/)
## Usage
```scheme
; Loko
> (time-unix->string 946684800)
"Sat Jan 01 00:00:00Z 2000"
; Chez
> (time-unix->string 946684800)
"Sat Jan 01 02:00:00+0200 2000"
; Guile
> (time-unix->string 946684800)
$1 = "Sat Jan 01 01:00:00+0100 2000"
```
