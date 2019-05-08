#! /usr/bin/env racket

#lang errortrace racket

(require file/glob)

(require html-parsing)
(require sxml)
(require txexpr)

;;

(define orig-dir (build-path (current-directory) "schemecookbook.org"))
(define orig-glob "schemecookbook.org/**/index.html")

(define wiki-dir (build-path (current-directory) "wiki"))

(define (whitespace-cleanup str)
  (regexp-replace* #px"(?m:\\s+$)" str ""))

(define (add-final-newline s)
  (if (= 0 (string-length s)) s (string-append s "\n")))

(define (list-orig-files)
  (glob orig-glob))

(define (file->xexp html-file)
  (with-input-from-file html-file (compose html->xexp port->string)))

(define (scrape-contentbox orig-file)
  (let* ((document (file->xexp orig-file))
         (contentboxes ((sxpath "//div[@id='contentbox']") document)))
    (and (not (null? contentboxes))
         (string-append
          (whitespace-cleanup
           (string-join (map srl:sxml->html contentboxes) "\n"))
          "\n"))))

(define (output-file-name orig-file)
  (let ((p (path->string
            (path-only (find-relative-path orig-dir orig-file)))))
    (set! p (regexp-replace* #px"[^A-Za-z0-9-]" p "_"))
    (set! p (regexp-replace* #px"__+" p "_"))
    (set! p (regexp-replace* #px"^_" p ""))
    (set! p (regexp-replace* #px"_$" p ""))
    (string-append p ".html")))

(define (convert-file orig-file)
  (let ((output-file (output-file-name orig-file)))
    (when (or (string-prefix? output-file "Cookbook_")
              (string-prefix? output-file "Scm_"))
      (let ((html-string (scrape-contentbox orig-file)))
        (when html-string
          (call-with-atomic-output-file
           (build-path wiki-dir output-file)
           (λ (out . _) (display html-string out))))))))

(make-directory* wiki-dir)
(for-each convert-file (list-orig-files))
