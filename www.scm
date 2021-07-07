;; Write HTML files into the `www` subdirectory.
;;
;; You need Chicken 5 and `chicken-install lowdown r7rs srfi-13 ssax`.

(import (scheme base)
        (scheme file)
        (scheme read)
        (scheme write)
        (srfi 1)
        (srfi 13)
        (only (chicken file) create-directory directory)
        (sxml-transforms)
        (lowdown))      ; Markdown-to-SXML parser.

(define (disp . xs) (for-each display xs) (newline))

(define (string-remove-prefix-ci fix str)
  (if (string-prefix-ci? fix str) (string-drop str (string-length fix)) str))

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
           "")
          ((eqv? 'h1 (car (car tags)))
           (apply string-append (cadr (car tags))))
          (else (rec (cdr tags))))))

(define (markdown-file->sxml md-filename)
  (call-with-port (open-input-file md-filename) markdown->sxml))

(define (write-simple-page html-filename md-filename description)
  (let ((sxml (markdown-file->sxml md-filename)))
    (write-html-file html-filename
                     (page-title-from-sxml sxml)
                     description
                     (code->pre sxml))))

(define pages
  (filter-map (lambda (name)
                (and (string-suffix? ".md" name)
                     (string-drop-right name (string-length ".md"))))
              (directory "recipes")))

(define (write-front-page html-filename)
  (write-html-file
   html-filename
   "The Scheme Cookbook"
   (string-append "Scheme is a minimalist dialect of the Lisp family "
                  "of programming languages.")
   `((h1 (@ (id "logo")) "Scheme Cookbook")
     (ul
      ,@(map (lambda (page)
               `(li (a (@ (href ,(string-append page "/")))
                       ,page)))
             pages)))))

(define (main)
  (create-directory "www")
  (write-front-page  "www/index.html")
  (for-each (lambda (page)
              (create-directory (string-append "www/" page))
              (write-simple-page (string-append "www/" page "/index.html")
                                 (string-append "recipes/" page ".md")
                                 "A recipe in the Scheme Cookbook."))
            pages)
  0)

(main)
