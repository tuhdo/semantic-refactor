(defcustom srefactor-clojure-symbol-stand-alone '("progn"
                                                  "while"
                                                  "save-excursion"
                                                  "save-window-excursion"
                                                  "unwind-protect")
  "A list of first symbols in a form that must stand alone on its
  own line without any other symbol.")

(defun srefactor-clojure-one-line ()
  (interactive)
  (when (looking-at "(")
    (srefactor-one-or-multi-lines-clojure 'one-line)))

(defun srefactor-clojure-multi-line ()
  (interactive)
  (when (looking-at "(")
    (srefactor-one-or-multi-lines-clojure 'multi-line)))

(defun srefactor-one-or-multi-lines-clojure (format-type)
  (save-excursion
    (let* ((tag-start (point))
           (tag-end (save-excursion
                      (forward-sexp)
                      (point)))
           (lexemes (wisent-clojure-lexer tag-start tag-end 1))
           (first-token (car lexemes))
           (first-token-name  (buffer-substring-no-properties
                               (semantic-lex-token-start first-token)
                               (semantic-lex-token-end first-token)))
           (tmp-buf (generate-new-buffer "let-buf"))
           start end
           tag-str token)
      (setq start (point))
      (while lexemes
        (setq token (pop lexemes))
        (setq tag-str (if token
                          (buffer-substring-no-properties
                           (semantic-lex-token-start token)
                           (semantic-lex-token-end token))
                        ""))
        (let* ((token-type (car token))
               (next-token (car lexemes))
               (next-token-type (car next-token)))
          (with-current-buffer tmp-buf
            (insert tag-str)
            (cond
             ((and (eq format-type 'one-line)
                   (not (or (eq token-type 'RPAREN)
                            (eq token-type 'LPAREN)
                            (eq token-type 'BRACK_BLOCK)
                            (eq token-type 'BRACE_BLOCK))))
              (insert " "))
             ((eq format-type 'multi-line)
              (if (and (eq token-type 'symbol)
                       (string-match ":.*" tag-str))
                  (insert " ")
                (insert "\n")))
             (t))
            )))
      (setq end (point))
      (kill-region start tag-end)
      (goto-char start)
      (insert (with-current-buffer tmp-buf
                (buffer-substring-no-properties (point-min)
                                                (point-max))))
      (when (and (eq format-type 'multi-line)
                 (not (member first-token-name srefactor-clojure-symbol-stand-alone)))
        (kill-whole-line)
        (forward-line -1)
        (delete-indentation)
        (save-excursion
          (goto-char start)
          (delete-indentation 1)
          (goto-char (line-end-position))
          (delete-indentation 1)))
      (indent-region tag-start (point)))))

(provide 'srefactor-clojure)
