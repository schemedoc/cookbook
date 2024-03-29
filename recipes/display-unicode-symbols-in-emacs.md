# Display Unicode symbols in Emacs

## Problem

In Emacs, display some Scheme keywords, like `lambda` and `>=`, using
special Unicode characters like `λ` and `≥`.

## Solution

The following solutions, which are written in the Emacs Lisp
programming language, work in current versions of GNU Emacs.

### Arrows and lambda

```Emacs-Lisp
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
  (turn-on-font-lock))

(add-hook 'scheme-mode-hook 'prettify-scheme)
```

Credit: [Arthur A. Gleckler](https://speechcode.com/)

### APL-like characters

```Emacs-Lisp
(add-hook
 'scheme-mode-hook
 (lambda ()
   (setq prettify-symbols-alist
         (seq-concatenate
          'list
          '(("<="       . ?≤)
            (">="       . ?≥)
            ("define"   . ?≝)
            ("set!"     . ?≐)
            ("set-car!" . ?≔)
            ("set-cdr!" . ?≕)
            ("#t"       . ?✓)
            ("#f"       . ?✗)
            ("'()"      . ?∅)
            ("if"       . ?⁇)
            ("or"       . ?∨)
            ("and"      . ?∧))
          prettify-symbols-alist))))
```

Credit: [Vladimir Nikishkin](https://lockywolf.net/)

### Greek letters

A [`prettify-greek`](https://melpa.org/#/prettify-greek) package can
be installed from MELPA.
