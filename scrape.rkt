#! /usr/bin/env racket

#lang errortrace racket

(require file/glob)

(require html-parsing)
(require sxml)
(require txexpr)

;;

(define dir-path (build-path (current-directory) "schemecookbook.org"))
(define dir-glob "schemecookbook.org/**/index.html")

(define wiki-dir (build-path (current-directory) "wiki"))

(define (whitespace-cleanup str)
  (regexp-replace* #px"(?m:\\s+$)" str ""))

(define (newline-join strings)
  (string-append (string-join strings "\n") "\n"))

(define (add-final-newline s)
  (if (= 0 (string-length s)) s (string-append s "\n")))

(define (list-html-files)
  (glob dir-glob))

(define (file->xexp html-file)
  (with-input-from-file html-file (compose html->xexp port->string)))

(define (scrape-contentbox html-file)
  (let* ((document (file->xexp html-file))
         (contentboxes ((sxpath "//div[@id='contentbox']") document)))
    (and (not (null? contentboxes))
         (string-append
          (whitespace-cleanup
           (string-join (map srl:sxml->html contentboxes) "\n"))
          "\n"))))

(define (output-file-name html-file)
  (let ((p (path->string
            (path-only (find-relative-path dir-path html-file)))))
    (set! p (regexp-replace* #px"[^A-Za-z0-9-]" p "_"))
    (set! p (regexp-replace* #px"__+" p "_"))
    (set! p (regexp-replace* #px"^_" p ""))
    (set! p (regexp-replace* #px"_$" p ""))
    (string-append p ".html")))

(define (convert-file html-file)
  (let ((output-file (output-file-name html-file)))
    (when (or (string-prefix? output-file "Cookbook_")
              (string-prefix? output-file "Scm_"))
      (let ((html-string (scrape-contentbox html-file)))
        (when html-string
          (call-with-atomic-output-file
           (build-path wiki-dir output-file)
           (λ (out . _) (display html-string out))))))))

(make-directory* wiki-dir)
(for-each convert-file (list-html-files))
