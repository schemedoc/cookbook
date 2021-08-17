# Display Unicode symbols in Emacs

## Problem

In Emacs, display some Scheme keywords, like `lambda` and `>=`, using
special Unicode characters like `λ` and `≥`.

## Solution

The following works in modern versions of GNU Emacs:

```Emacs Lisp
(defvar pretty-scheme-keywords
  (mapcar (lambda (pair)
            (cons (concat "\\(" (regexp-quote (car pair)) "\\)")
                  (cdr pair)))
          '(("->"  . #x2192)
            ("<="  . #x2264)
            ("<==" . #x21D0)
            (">="  . #x2265)
            ("==>" . #x21D2)))
  "Alist from regexps to Unicode code points.")

(defun prettify-scheme ()
  (add-to-list 'prettify-symbols-alist '("lambda" . #x3BB))
  (font-lock-add-keywords
   nil
   (mapcar (lambda (keyword)
             `(,(car keyword)
               (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                         ,(cdr keyword)
                                         'decompose-region)
                         nil))))
           pretty-scheme-keywords))
  (prettify-quotes)
  (turn-on-font-lock))

(add-hook 'scheme-mode-hook 'prettify-scheme)
```

Credit: [Arthur A. Gleckler](https://speechcode.com/)
