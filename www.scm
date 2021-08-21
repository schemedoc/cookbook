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
        (only (chicken file) copy-file create-directory)
        (sxml-transforms)
        (lowdown)  ; Markdown->SXML parser.
        (www-lowdown-colorize))

(enable-www-lowdown-colorize!)

(define licenses
  '("CC0-1.0"
    "ISC"
    "MIT"
    "BSD-3-Clause"
    "LGPL-2.1-or-later"))

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
               (link (@ (rel "stylesheet") (href "/schemeorg.css")))
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

(define (about-section subdomain user/repo)
  `(section
    (@ (id "schemeorg-contributing"))
    (h2 "About " ,subdomain ".scheme.org")
    (div (@ (class "round-box gray-box"))
         (p (kbd ,subdomain ".scheme.org")
            " is a community subdomain of "
            (kbd "scheme.org"))
         (ul
          (li "Source code: "
              (a (@ (href ,(string-append
                            "https://github.com/"
                            user/repo)))
                 (kbd (@ (class "github-repo"))
                      ,user/repo))
              " repository on GitHub.")
          (li "Discussion: "
              (code (@ (class "mailing-list"))
                    "schemeorg")
              " mailing list "
              "(" (a (@ (href
                         "https://srfi-email.schemers.org/schemeorg/"))
                     "archives")
              ", " (a (@ (href
                          ,(string-append
                            "https://srfi.schemers.org/"
                            "srfi-list-subscribe.html#schemeorg")))
                      "subscribe")
              ").")))))

(define (write-front-page html-filename)
  (write-html-file
   html-filename
   "The Scheme Cookbook"
   (string-append "Scheme is a minimalist dialect of the Lisp family "
                  "of programming languages.")
   `((h1 (@ (id "logo")) "Scheme Cookbook")
     (h2 "License")
     (p "The code in the cookbook is released under several common"
        " licenses simultaneously. The user is free to pick any one"
        " of them. The aim is to make it easy"
        " to copy code into existing projects without having"
        " to add a new license notice to cover the cookbook material.")
     (p "The licenses are: "
        ,@(cdr (append-map (lambda (license)
                             `(", "
                               (a (@ (href ,(string-append
                                             "licenses/" license ".txt")))
                                  (kbd ,license))))
                           licenses)))
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
     ,(about-section "cookbook" "schemedoc/cookbook")
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
  (create-directory "www/licenses")
  (for-each (lambda (license)
              (copy-file (string-append "LICENSES/" license ".txt")
                         (string-append "www/licenses/" license ".txt")
                         #t))
            licenses)
  (write-front-page  "www/index.html")
  (for-each write-recipe-page (append-map group-recipes groups))
  0)

(main)
