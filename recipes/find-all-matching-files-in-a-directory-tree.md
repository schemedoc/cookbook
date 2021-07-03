## Problem
You have a directory containing an unknown set of subdirectories and files. For example:

```
a/a.png
a/a.html
b/b.html
b/c/c.html
c/d.png
d/
```

Return a list with the relative pathname (e.g. `"b/c/c.html"`) of each `.html` file in this tree.

* Do not include file names starting with a dot ("dotfiles").
* Do not visit subdirectories whose names start with a dot.
* The list does not have to be sorted.

No portable solution exists at the moment. SRFI 170 or implementation-specific libraries can be used.

## Solution

### SRFI

Using SRFI 170 (and string-suffix-ci? comes from SRFI 13)

```scheme
(define (directory-tree-fold merge state root)
  (define (path-append a b)
    (if (and a b) (string-append a "/" b) (or a b)))
  (let recurse ((state state) (relative #f))
    (let* ((path (path-append root relative))
           (state (merge path state))
           (directory? (file-info-directory? (file-info path #t))))
      (if (not directory?) state
          (let loop ((state state) (names (directory-files path)))
            (if (null? names) state
                (loop (recurse state (path-append relative (car names)))
                      (cdr names))))))))

(directory-tree-fold
 (lambda (name names)
   (if (string-suffix-ci? ".html" name) (cons name names) names))
 '() ".")
```

Credit [Lassi Kortela](https://github.com/lassik)