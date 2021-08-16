# Display some symbols in Scheme buffers using special characters

## Problem

In Emacs, one can display some keywords, like `lambda` and `>=`, using
special characters like `λ` and `≥`.

## Solution

```Emacs-Lisp
(defvar pretty-scheme-keywords
  '(("\\(->\\)" . #x2192)
    ("\\(<=\\)" . #x2264)
    ("\\(<==\\)" . #x21d0)
    ("\\(>=\\)" . #x2265)
    ("\\(==>\\)" . #x21d2))
  "alist from regexps to Unicode code points")

(defun prettify-scheme ()
  (add-to-list 'prettify-symbols-alist '("lambda" . ?λ))
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