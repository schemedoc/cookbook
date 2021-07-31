;; Write HTML files into the `www` subdirectory.
;;
;; You need Chicken 5 and
;; `chicken-install lowdown r7rs srfi-1 srfi-13 srfi-132 ssax`

(import (scheme base)
        (scheme file)
        (scheme read)
        (scheme write)
        (srfi 1)
        (srfi 13)
        (srfi 132)
        (only (chicken file) create-directory)
        (sxml-transforms)
        (lowdown)  ; Markdown->SXML parser.
        (www-lowdown-colorize))

(enable-www-lowdown-colorize!)

(define (disp . xs) (for-each display xs) (newline))

(define (edisp . xs)
  (parameterize ((current-output-port (current-error-port)))
    (apply disp xs)
    (flush-output-port)))

(define (write-html-file html-filename title description body)
  (edisp "Writing " html-filename)
  (with-output-to-file html-filename
    (lambda ()
      (write-string "<!DOCTYPE html>")
      (SXML->HTML
       `(html (@ (lang "en"))
              (head
               (meta (@ charset "UTF-8"))
               (title ,title)
               (link (@ (rel "stylesheet") (href "/style.css")))
               (link (@ (rel "stylesheet") (href "/colorize.css")))
               (meta (@ (name "viewport")
                        (content "width=device-width, initial-scale=1")))
               (meta (@ (name "description")
                        (content ,description))))
              (body ,@body))))))

(define (page-title-from-sxml tags)
  (let rec ((tags tags))
    (cond ((not (and (pair? tags) (pair? (car tags))))
           ;; (error "Markdown page has no title")
           #f)
          ((eqv? 'h1 (car (car tags)))
           (apply string-append (cadr (car tags))))
          (else (rec (cdr tags))))))

(define-record-type recipe
  (make-recipe stem title sxml)
  recipe?
  (stem recipe-stem)
  (title recipe-title)
  (sxml recipe-sxml))

(define (recipe<? a b) (string-ci<? (recipe-title a) (recipe-title b)))

(define (read-recipe-with-stem stem)
  (let ((md-filename (string-append "recipes" "/" stem ".md")))
    ;;(edisp "Reading " md-filename)
    (let* ((sxml (call-with-port (open-input-file md-filename)
                                 markdown->sxml))
           (title (or (page-title-from-sxml sxml) stem)))
      (make-recipe stem title sxml))))

(define groups-template (with-input-from-file "www-index.scm" read))

(define group-title car)
(define group-recipes cdr)

(define groups
  (map (lambda (group)
         (cons (group-title group)
               (list-sort recipe<?
                          (map read-recipe-with-stem
                               (group-recipes group)))))
       groups-template))

(define (write-front-page html-filename)
  (write-html-file
   html-filename
   "The Scheme Cookbook"
   (string-append "Scheme is a minimalist dialect of the Lisp family "
                  "of programming languages.")
   `((h1 (@ (id "logo")) "Scheme Cookbook")
     (h2 "Recipes")
     ,@(map (lambda (group)
              `(section
                (h3 ,(group-title group))
                (ul ,@(map (lambda (recipe)
                             (let ((href (string-append (recipe-stem recipe)
                                                        "/")))
                               `(li (a (@ (href ,href))
                                       ,(recipe-title recipe)))))
                           (group-recipes group)))))
            groups)
     (hr)
     (p "Source code " (a (@ (href "https://github.com/schemedoc/cookbook"))
                          "at GitHub"))
     (p (a (@ (href "https://www.scheme.org/"))
           "Back to Scheme.org")))))

(define (write-recipe-page recipe)
  (let ((recipe-dir (string-append "www" "/" (recipe-stem recipe))))
    (create-directory recipe-dir)
    (write-html-file
     (string-append recipe-dir "/" "index.html")
     (recipe-title recipe)
     "A recipe in the Scheme Cookbook."
     `(,@(recipe-sxml recipe)
       (hr)
       (p (a (@ (href "/")) "Back to the Scheme Cookbook"))))))

(define (main)
  (create-directory "www")
  (write-front-page  "www/index.html")
  (for-each write-recipe-page (append-map group-recipes groups))
  0)

(main)
