# Find matching files in directory tree

## Problem

You have a directory containing an unknown set of subdirectories and
files. For example:

```
a/a.png
a/a.html
b/b.html
b/c/c.html
c/d.png
d/
```

Return a list with the relative pathname (e.g. `"b/c/c.html"`) of each
`.html` file in this tree.

* Do not include file names starting with a dot ("dotfiles").
* Do not visit subdirectories whose names start with a dot.
* The list does not have to be sorted.

## Solution

### Using SRFI 170

```Scheme
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
```

The following procedures come from
[SRFI 170](https://srfi.schemers.org/srfi-170/srfi-170.html):

* `directory-files`
* `file-info`
* `file-info-directory?`

Equivalent implementation-specific procedures could be used just as
well.

Credit: [Lassi Kortela](https://github.com/lassik)

## Usage

The `string-suffix-ci?` procedure comes from SRFI 13.

```Scheme
(directory-tree-fold
 (lambda (name names)
   (if (string-suffix-ci? ".html" name) (cons name names) names))
 '() ".")
```
