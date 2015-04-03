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
           (first-symbol (cadr lexemes))
           (first-symbol-name  (buffer-substring-no-properties
                                (semantic-lex-token-start first-symbol)
                                (semantic-lex-token-end first-symbol)))
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
             ((or (eq token-type 'RPAREN)
                  (eq token-type 'LPAREN)
                  (eq token-type 'RBRACK)
                  (eq token-type 'LBRACK)
                  (eq token-type 'RBRACE)
                  (eq token-type 'LBRACE)
                  (eq next-token-type 'RPAREN)
                  (eq next-token-type 'RBRACK)
                  (eq next-token-type 'RBRACE))
              "")
             ((eq format-type 'one-line)
              (insert " "))
             ((eq format-type 'multi-line)
              (if (and (eq token-type 'symbol)
                       (string-match ":.*" tag-str))
                  (insert " ")
                (insert "\n")))))))
      (setq end (point))
      (kill-region start tag-end)
      (goto-char start)
      (insert (with-current-buffer tmp-buf
                (buffer-substring-no-properties (point-min)
                                                (point-max))))
      (when (eq format-type 'multi-line)
        (unless (member first-symbol-name srefactor-symbol-stand-alone)
          (save-excursion
            (goto-char (semantic-lex-token-end first-symbol))
            (delete-indentation 1))))
      (indent-region tag-start (point)))))

(provide 'srefactor-clojure)
