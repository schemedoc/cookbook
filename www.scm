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
        (lowdown)  ; Markdown->SXML parser.
        (sxml-transforms))

(define (disp . xs) (for-each display xs) (newline))

(define (string-remove-prefix-ci fix str)
  (if (string-prefix-ci? fix str) (string-drop str (string-length fix))
      str))

(define (string-remove-suffix-ci fix str)
  (if (string-suffix-ci? fix str) (string-drop-right str (string-length fix))
      str))

(define (code->pre sxml)
  (cond ((not (pair? sxml)) sxml)
        ((and (= 2 (length sxml))
              (eq? 'code (car sxml))
              (string? (cadr sxml))
              (string-contains (cadr sxml) "\n"))
         (list 'pre (string-remove-prefix-ci "scheme\n" (cadr sxml))))
        (else
         (map code->pre sxml))))

(define (write-html-file html-filename title description body)
  (disp "Writing " html-filename)
  (with-output-to-file html-filename
    (lambda ()
      (write-string "<!DOCTYPE html>")
      (SXML->HTML
       `(html (@ (lang "en"))
              (head
               (meta (@ charset "UTF-8"))
               (title ,title)
               (link (@ (rel "stylesheet") (href "/style.css")))
               (meta (@ (name "viewport")
                        (content "width=device-width, initial-scale=1")))
               (meta (@ (name "description")
                        (content ,description))))
              (body ,@body))))))

(define (page-title-from-sxml tags)
  (let rec ((tags tags))
    (cond ((not (pair? tags))
           ;; (error "Markdown page has no title")
           #f)
          ((eqv? 'h1 (car (car tags)))
           (apply string-append (cadr (car tags))))
          (else (rec (cdr tags))))))

(define-record-type page
  (make-page stem title sxml)
  page?
  (stem page-stem)
  (title page-title)
  (sxml page-sxml))

(define (page<? a b) (string-ci<? (page-title a) (page-title b)))

(define (read-page-with-stem stem)
  (let* ((md-filename (string-append "recipes" "/" stem ".md"))
         (sxml (call-with-port (open-input-file md-filename) markdown->sxml))
         (title (or (page-title-from-sxml sxml) stem)))
    (make-page stem title sxml)))

(define page-groups-template
  `(("Lists"
     "split-list-into-groups-that-are-equal-judging-by-a-procedure")))

(define page-group-title car)
(define page-group-pages cdr)

(define page-groups
  (map (lambda (group)
         (cons (page-group-title group)
               (list-sort page<?
                          (map read-page-with-stem
                               (page-group-pages group)))))
       page-groups-template))

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
                (h3 ,(page-group-title group))
                (ul ,@(map (lambda (page)
                             (let ((href (string-append (page-stem page) "/")))
                               `(li (a (@ (href ,href)) ,(page-title page)))))
                           (page-group-pages group)))))
            page-groups)
     (hr)
     (p "Source code " (a (@ (href "https://github.com/schemedoc/cookbook"))
                          "at GitHub")))))

(define (main)
  (create-directory "www")
  (write-front-page  "www/index.html")
  (for-each (lambda (page)
              (let ((stem (page-stem page)))
                (create-directory (string-append "www/" stem))
                (write-html-file (string-append "www/" stem "/index.html")
                                 (page-title page)
                                 "A recipe in the Scheme Cookbook."
                                 (code->pre (page-sxml page)))))
            (append-map page-group-pages page-groups))
  0)

(main)
