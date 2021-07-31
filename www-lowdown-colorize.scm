;; This code is ported from Vasilij Schneidermann's gist:
;; https://gist.github.com/wasamasa/e49e66e050255a8973270e0a52d68818

;; This is an extension to the Chicken 5 `lowdown` egg to recognize
;; GitHub Flavored Markdown code blocks. These code blocks may contain
;; blank lines, and the line with the opening ``` can say the name of
;; the language being used in the block. We also add syntax coloring.

;; Example:

;; ```Scheme
;; (display "Hello world")
;; (newline)
;; ```

;; TODO:

;; - (html-colorize lang*) returns a HTML string, which is parsed back
;;   into SXML by (html->sxml), then turned into HTML again by the
;;   code that calls this module. Send a patch to the `colorize` egg
;;   so it can return SXML directly and this module does not have to
;;   depend on html-parser.

;; - Send a patch to add the features from this module into the
;; `lowdown` egg. The (lowdown extra) module would be a good place.

(define (fenced-code-block-end fence)
  (any-of end-of-input
          (skip non-indent-space
                (char-seq fence)
                (zero-or-more (is (string-ref fence 0)))
                space*
                line-end)))

(define (fenced-code-block-lines indent code-block-end)
  (zero-or-more
   (preceded-by (none-of code-block-end)
                (repeated (is #\space) max: (length indent))
                line)))

(define fenced-code-block-info-string
  (as-string (zero-or-more (none-of* (is #\`) normal-line-end item))))

(define fenced-code-block
  (sequence* ((indent non-indent-space)
              (fence (as-string (repeated (in #\` #\~) min: 3)))
              (_ space*)
              (info fenced-code-block-info-string)
              (_ normal-line-end))
             (let ((code-block-end (fenced-code-block-end fence)))
               (sequence* ((code-lines (fenced-code-block-lines
                                        indent code-block-end))
                           (_ code-block-end))
                          (result `(verbatim (info ,(string-trim-both info))
                                             (code ,@code-lines)))))))

(define fenced-code-block-conversion-rules*
  `((verbatim
     . ,(lambda (_ contents)
          (or (and-let* (((pair? contents))
                         ((pair? (car contents)))
                         (info (alist-ref 'info contents))
                         (code (alist-ref 'code contents))
                         (code* (string-intersperse code ""))
                         (lang (car info))
                         (lang* (string->symbol lang)))
                (if (coloring-type-exists? lang*)
                    `(pre (code (@ (class ,(string-append "colorize "
                                                          "language-" lang)))
                                ,@(->> code*
                                       (html-colorize lang*)
                                       (html->sxml)
                                       (cdr))))
                    `(pre (code ,code*))))
              `(pre (code ,@contents)))))))

(define (enable-www-lowdown-colorize!)
  (block-hook (cons fenced-code-block (block-hook)))
  (markdown-html-conversion-rules*
   (append fenced-code-block-conversion-rules*
           (markdown-html-conversion-rules*)))
  (void))
