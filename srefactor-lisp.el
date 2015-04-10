;;; Srefactor --- A refactoring tool based on Semantic parser framework
;;
;; Filename: srefactor-lisp.el
;; Description: A refactoring tool based on Semantic parser framework
;; Author: Tu, Do Hoang <tuhdo1710@gmail.com>
;; URL      : https://github.com/tuhdo/semantic-refactor
;; Maintainer: Tu, Do Hoang
;; Created: Wed Feb 11 21:25:51 2015 (+0700)
;; Version: 0.3
;; Package-Requires: ((emacs "24.4"))
;; Last-Updated: Wed Feb 11 21:25:51 2015 (+0700)
;;           By: Tu, Do Hoang
;;     Update #: 1
;; URL:
;; Doc URL:
;; Keywords: emacs-lisp, languages, tools
;; Compatibility: GNU Emacs: 24.3+
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Semantic is a package that provides a framework for writing
;; parsers. Parsing is a process of analyzing source code based on
;; programming language syntax. This package relies on Semantic for
;; analyzing source code and uses its results to perform smart code
;; refactoring that based on code structure of the analyzed language,
;; instead of plain text structure.
;;
;; This package provides the following features for Emacs Lisp:
;;
;; - `srefactor-elisp-format-buffer': Format whole buffer.
;; - `srefactor-elisp-format-defun': Format the current defun point is in.
;; - `srefactor-elisp-one-line': Transform all sub-sexpressions current sexpression at
;; point into one line separated each one by a space.
;;
;; - `srefactor-elisp-multi-line': Transform all sub-sexpressions current sexpression
;; - at point into multiple lines separated. If the head symbol belongs to the
;; - list `srefactor-lisp-symbol-to-skip', then the first N next symbol/sexpressions
;; - (where N is the nummber associated with the head symbol as stated in the
;; - list) are skipped before a newline is inserted.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(require 'semantic/bovine/el)

;; (defcustom srefactor-newline-threshold 30
;;   "After a token is inserted, if the length of current
;;   S-EXPRESSION is greater than this value, start inserting a newline.")

(defcustom srefactor-lisp-symbol-to-skip '(("progn" . 0)
                                           ("cond" . 0)
                                           ("save-excursion" . 0)
                                           ("unwind-protect" . 0)
                                           ("with-temp-buffer" . 0)
                                           ("condition-case" . 1)
                                           ("with-current-buffer" . 1)
                                           ("let" . 1)
                                           ("let*" . 1)
                                           ("if" . 1)
                                           ("while" . 1)
                                           ("dolist" . 1)
                                           ("do" . 1)
                                           ("when" . 1)
                                           ("buffer-substring-no-properties" . 1)
                                           ("unless" . 1)
                                           ("not" . 1)
                                           ("null" . 1)
                                           ("null?" . 1)
                                           ("or" . 1)
                                           ("and" . 1)
                                           ("catch" . 1)
                                           ("mapcar" . 1)
                                           ("mapcan" . 1)
                                           ("mapc" . 1)
                                           ("+" . 1)
                                           ("-" . 1)
                                           ("*" . 1)
                                           ("/" . 1)
                                           ("error" . 1)
                                           ("goto-char" . 1)
                                           ("insert" . 1)
                                           ("car" . 1)
                                           ("cdr" . 1)
                                           ("lambda" . 1)
                                           ("1+" . 1)
                                           ("1-" . 1)
                                           ("cons" . 2)
                                           ("kill-region" . 2)
                                           ("equal" . 2)
                                           ("concat" . 2)
                                           ("member" . 2)
                                           ("eq?" . 2)
                                           ("eq" . 2)
                                           ("get" . 2)
                                           ("assoc" . 2)
                                           ("defun" . 2)
                                           ("defclass" . 2)
                                           ("defstruct" . 2)
                                           ("defmacro" . 2)
                                           ("defsubst" . 2)
                                           ("defface" . 2)
                                           ("defalias" . 2)
                                           ("defcustom" . 2)
                                           ("declare" . 2)
                                           ("defvar" . 2)
                                           ("string-match" . 2)
                                           ("defcustom" . 2)
                                           ("setq" . 2)
                                           ("setq-default" . 2)
                                           ("member" . 2)
                                           ("setf" . 2)
                                           (">" . 2)
                                           ("<" . 2)
                                           ("<=" . 2)
                                           (">=" . 2)
                                           ("/=" . 2)
                                           ("=" . 2)
                                           ("some" . 2))
  "A list of pairs of a symbol and a number that denotes how many
  sexp to skip before inserting the first newline. "
  :group 'srefactor)

(defcustom srefactor-clojure-symbol-to-skip '(
                                              ("fn" . 1)
                                              ("ns" . 1)
                                              (":require" . 1)
                                              (":import" . 1)
                                              ("struct-map" . 1)
                                              ("defmacro" . 1)
                                              ("binding" . 1)
                                              ("with-bindings" . 1)
                                              ("doseq" . 1)
                                              ("catch" . 2)
                                              ("defn" . 2))
  "A list of pairs of a symbol and a number that denotes how many
  sexp to skip before inserting a newline. This will be merged
  with `srefactor-lisp-symbol-to-skip'. Symbols in this list
  overrides symbols in `srefactor-lisp-symbol-to-skip'.")

(defun srefactor--appropriate-major-mode (major-mode)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (emacs-lisp-mode))
   ((eq major-mode 'scheme-mode)
    (scheme-mode))
   ((eq major-mode 'common-lisp-mode)
    (common-lisp-mode))
   ((and (fboundp 'clojure-mode)
         (eq major-mode 'clojure-mode))
    (clojure-mode))
   (t (emacs-lisp-mode))))

(defun srefactor--define-skip-list-for-mode (major-mode)
  (cond
   ((and (fboundp 'clojure-mode)
         (eq major-mode 'clojure-mode))
    (remove-duplicates (append srefactor-lisp-symbol-to-skip
                               srefactor-clojure-symbol-to-skip)
                       :test (lambda (a b) (equal (car a) (car b)))))
   (t srefactor-lisp-symbol-to-skip)))

(defun srefactor-lisp-format-buffer ()
  "Format current buffer."
  (interactive)
  (let ((cur-pos (point))
        (buf-content (buffer-substring-no-properties (point-min) (point-max)))
        (cur-major-mode major-mode)
        (orig-skip-list srefactor-lisp-symbol-to-skip)
        (cur-indent-mode indent-tabs-mode))
    (setq buf-content (with-temp-buffer
                        (semantic-default-elisp-setup)
                        (emacs-lisp-mode)
                        (setq indent-tabs-mode cur-indent-mode)
                        (setq srefactor-lisp-symbol-to-skip
                              (srefactor--define-skip-list-for-mode cur-major-mode))
                        (semantic-lex-init)
                        (insert buf-content)
                        (goto-char (point-max))
                        (while (beginning-of-defun-raw)
                          (let ((beg (point))
                                (end (save-excursion
                                       (forward-sexp)
                                       (point))))
                            (srefactor--lisp-format-one-or-multi-lines beg
                                                                       end
                                                                       beg
                                                                       'multi-line
                                                                       nil
                                                                       t)
                            (goto-char beg)))
                        (srefactor--appropriate-major-mode cur-major-mode)
                        (indent-region (point-min) (point-max))
                        (setq srefactor-lisp-symbol-to-skip orig-skip-list)
                        (buffer-substring-no-properties (point-min) (point-max))))
    (kill-region (point-min) (point-max))
    (insert buf-content)
    (goto-char cur-pos)))

(defun srefactor-lisp-format-defun ()
  "Format current defun point is in."
  (interactive)
  (let* ((orig-point (point))
         (beg (save-excursion
                (forward-char 1)
                (beginning-of-defun-raw)
                (point)))
         (end (save-excursion
                (goto-char beg)
                (forward-sexp)
                (point)))
         (orig-skip-list srefactor-lisp-symbol-to-skip)
         (cur-indent-mode indent-tabs-mode)
         (cur-major-mode major-mode)
         (content (buffer-substring-no-properties beg end)))
    (progn
      (setq content (with-temp-buffer
                      (semantic-default-elisp-setup)
                      (emacs-lisp-mode)
                      (setq indent-tabs-mode cur-indent-mode)
                      (setq srefactor-lisp-symbol-to-skip
                            (srefactor--define-skip-list-for-mode cur-major-mode))
                      (semantic-lex-init)
                      (insert content)
                      (srefactor--lisp-format-one-or-multi-lines (point-min)
                                                                 (point-max)
                                                                 (point-min)
                                                                 'multi-line
                                                                 nil
                                                                 t)
                      (srefactor--appropriate-major-mode cur-major-mode)
                      (setq srefactor-lisp-symbol-to-skip orig-skip-list)
                      (indent-region (point-min)
                                     (point-max))
                      (buffer-substring-no-properties
                       (point-min)
                       (point-max))))
      (kill-region beg end)
      (insert content)
      (goto-char orig-point))))

(defun srefactor-lisp-format-sexp ()
  "Transform all sub-sexpressions current sexpression at point
into multiple lines separatedly. If the head symbol belongs to the
list `srefactor-lisp-symbol-to-skip', then the first N next
symbol/sexpressions (where N is the nummber associated with the
head symbol as stated in the list) are skipped before a newline
is inserted."
  (interactive)
  (let* ((orig-point (point))
         (beg (save-excursion
                (unless (looking-at "[({[]")
                  (backward-up-list))
                (point)))
         (end (save-excursion
                (goto-char beg)
                (forward-sexp)
                (point)))
         (orig-skip-list srefactor-lisp-symbol-to-skip)
         (cur-indent-mode indent-tabs-mode)
         (cur-major-mode major-mode)
         (content (buffer-substring-no-properties beg end)))
    (progn
      (setq content (with-temp-buffer
                      (semantic-default-elisp-setup)
                      (emacs-lisp-mode)
                      (setq indent-tabs-mode cur-indent-mode)
                      (setq srefactor-lisp-symbol-to-skip (srefactor--define-skip-list-for-mode cur-major-mode))
                      (semantic-lex-init)
                      (insert content)
                      (srefactor--lisp-format-one-or-multi-lines (point-min)
                                                                 (point-max)
                                                                 (point-min)
                                                                 'multi-line
                                                                 nil
                                                                 t)
                      (srefactor--appropriate-major-mode cur-major-mode)
                      (setq srefactor-lisp-symbol-to-skip orig-skip-list)
                      (buffer-substring-no-properties
                       (point-min)
                       (point-max))))
      (kill-region beg end)
      (insert content)
      (goto-char beg)
      (forward-sexp)
      (setq end (point))
      (indent-region beg end)
      (goto-char orig-point))))

(defun srefactor-lisp-one-line (recursive-p)
  "Transform all sub-sexpressions current sexpression at point
into one line separated each one by a space."
  (interactive "P")
  (let* ((orig-point (point))
         (beg (save-excursion
                (unless (looking-at "[({[]")
                  (backward-up-list))
                (point)))
         (end (save-excursion
                (goto-char beg)
                (forward-sexp)
                (point)))
         (orig-skip-list srefactor-lisp-symbol-to-skip)
         (cur-indent-mode indent-tabs-mode)
         (cur-major-mode major-mode)
         (content (buffer-substring-no-properties beg end)))
    (progn
      (setq content (with-temp-buffer
                      (semantic-default-elisp-setup)
                      (emacs-lisp-mode)
                      (setq indent-tabs-mode cur-indent-mode)
                      (setq srefactor-lisp-symbol-to-skip
                            (srefactor--define-skip-list-for-mode cur-major-mode))
                      (semantic-lex-init)
                      (insert content)
                      (srefactor--lisp-format-one-or-multi-lines (point-min)
                                                                 (point-max)
                                                                 (point-min)
                                                                 'one-line
                                                                 nil
                                                                 recursive-p)
                      (srefactor--appropriate-major-mode cur-major-mode)
                      (setq srefactor-lisp-symbol-to-skip orig-skip-list)
                      (indent-region (point-min)
                                     (point-max))
                      (buffer-substring-no-properties
                       (point-min)
                       (point-max))))
      (kill-region beg end)
      (insert content)
      (goto-char orig-point))))

(defun srefactor--lisp-format-one-or-multi-lines (beg end orig-point format-type &optional newline-betwen-semantic-lists recursive-p)
  "Turn the current sexpression into one line/multi-line depends
on the value of FORMAT-TYPE. If FORMAT-TYPE is 'one-line,
transforms all sub-sexpressions of the same level into one
line. If FORMAT-TYPE is 'multi-line, transforms all
sub-sexpressions of the same level into multiple lines.

Return the position of last closing sexp."
  (let* ((lexemes (semantic-emacs-lisp-lexer beg end 1))
         (first-token (cadr lexemes))
         (first-token-name (buffer-substring-no-properties
                            (semantic-lex-token-start first-token)
                            (semantic-lex-token-end first-token)))
         (second-token (caddr lexemes))
         (tmp-buf (generate-new-buffer (make-temp-name "")))
         (orig-format-type format-type)
         (cur-buf (current-buffer))
         token-str
         ignore-pair
         ignore-num
         token)
    (unwind-protect
        (progn
          (unless (assoc 'semantic-list lexemes)
            (setq format-type 'one-line))
          (if (or (eq (car first-token) 'semantic-list)
                  (assoc first-token-name srefactor-lisp-symbol-to-skip))
              (setq newline-betwen-semantic-lists t))
          (setq ignore-pair (assoc first-token-name srefactor-lisp-symbol-to-skip))
          (setq ignore-num (cdr ignore-pair))

          (while lexemes
            (let* (token-type tok-start tok-end next-token next-token-start next-token-type next-token-str)
              (setq token (srefactor--lisp-forward-token))
              (with-current-buffer tmp-buf
                (insert token-str)
                ;; (srefactor--lisp-comment-formatter)
                (cond
                 ((and (eq token-type 'number)
                       (member next-token-str '("+" "-" "*" "/")))
                  (srefactor--lisp-number-formatter))
                 ((or (eq token-type 'punctuation)
                      (eq token-type 'open-paren)
                      (eq token-type 'close-paren)
                      (eq next-token-type 'close-paren))
                  (srefactor--lisp-punctuation-formatter))
                 ((equal token-str ".")
                  (insert " " next-token-str)
                  (srefactor--lisp-forward-token))
                 ((eq token-type 'symbol)
                  (srefactor--lisp-symbol-formatter))
                 ((eq format-type 'one-line)
                  (srefactor--lisp-oneline-formatter))
                 ((eq format-type 'multi-line)
                  (srefactor--lisp-multiline-formatter))))))
          (save-excursion
            (kill-region beg end)
            (insert (with-current-buffer tmp-buf
                      (buffer-substring-no-properties
                       (point-min)
                       (point-max)))))
          (forward-sexp)
          (setq end (point))

          ;; descend into sub-sexpressions
          (goto-char beg)
          (forward-sexp)
          (setq end (point))
          (setq lexemes (semantic-emacs-lisp-lexer beg end 1))
          (when recursive-p
            (srefactor--lisp-visit-semantic-list-lex (nreverse lexemes))))
      (kill-buffer tmp-buf))))

(defsubst srefactor--lisp-number-formatter ()
  "Make use of dynamic scope of its parent
function `srefactor--lisp-format-one-or-multi-lines'"
  (goto-char (semantic-lex-token-end token))
  (insert next-token-str " ")
  (setq first-token (semantic-lex-token 'symbol
                                        (semantic-lex-token-start token)
                                        (1+ (semantic-lex-token-end token))))
  (setq first-token-name (concat token-str next-token-str))
  (setq second-token (cadr lexemes))
  (when (eq (semantic-lex-token-class second-token) 'semantic-list)
    (insert "\n"))
  (srefactor--lisp-forward-token))

(defsubst srefactor--lisp-punctuation-formatter ()
  "Make use of dynamic scope of its parent
function `srefactor--lisp-format-one-or-multi-lines'"
  (setq first-token (car lexemes))
  (setq second-token (cadr lexemes))
  (let (token token-str)
    (while (srefactor--lisp-token-in-punctuation-p (setq token (srefactor--lisp-forward-token)))
      (setq token-str (with-current-buffer cur-buf
                        (buffer-substring-no-properties (semantic-lex-token-start token)
                                                        (semantic-lex-token-end token))))
      (insert token-str)
      ;; (srefactor--lisp-comment-formatter)
      )
    (when token
      (push token lexemes))))

(defsubst srefactor--lisp-symbol-formatter ()
  " Make use of dynamic scope of its parent
function `srefactor--lisp-format-one-or-multi-lines'"
  (cond
   ((and (not (equal token-str first-token-name))
         (eq orig-format-type 'multi-line)
         (string-match ":.*" token-str))
    (insert " " next-token-str)
    (setq next-token (srefactor--lisp-forward-token))
    (while (member next-token-type '(punctuation open-paren semantic-list))
      (setq next-token (srefactor--lisp-forward-token))
      (insert next-token-str))
    (when (not (eq (semantic-lex-token-class (car lexemes)) 'close-paren))
      (insert "\n")))
   ((eq format-type 'one-line)
    (srefactor--lisp-oneline-formatter))
   ((equal token-str "~@")
    "")
   ((eq format-type 'multi-line)
    (srefactor--lisp-multiline-formatter))))

(defsubst srefactor--lisp-forward-token ()
  (setq token (pop lexemes))
  (when token
    (setq token-type (semantic-lex-token-class token))
    (setq tok-start (semantic-lex-token-start token))
    (setq tok-end (semantic-lex-token-end token))
    (setq token-str (with-current-buffer cur-buf
                      (buffer-substring-no-properties (semantic-lex-token-start token)
                                                      (semantic-lex-token-end token))))
    (setq next-token (car lexemes))
    (setq next-token-type (semantic-lex-token-class next-token))
    (setq next-token-start (semantic-lex-token-start next-token))
    (setq next-token-end (semantic-lex-token-end next-token))
    (setq next-token-str (if next-token
                             (with-current-buffer cur-buf
                               (buffer-substring-no-properties (semantic-lex-token-start next-token)
                                                               (semantic-lex-token-end next-token)))
                           ""))
    token))

(defsubst srefactor--lisp-comment-formatter ()
  (let (comment-token
        comment-start
        comment-end
        comment-content
        next-token-real-line
        token-real-line
        comment-real-line-start
        comment-real-line-end)
    (when (and tok-end next-token-start)
      (setq comment-token (with-current-buffer cur-buf   ;; asdf
                            (condition-case nil
                                (car (semantic-comment-lexer tok-end next-token-start))
                              (error nil))))
      (when comment-token
        (setq comment-content (with-current-buffer cur-buf
                                ;; set values inside the buffer to avoid global variable
                                (setq comment-start (semantic-lex-token-start comment-token))
                                (setq comment-end (semantic-lex-token-end comment-token))
                                (buffer-substring-no-properties comment-start comment-end)))
        (setq token-real-line (line-number-at-pos tok-end))
        (setq next-token-real-line (line-number-at-pos next-token-start))
        (setq comment-real-line-start (line-number-at-pos comment-start))
        (setq comment-real-line-end (line-number-at-pos comment-end))
        (with-current-buffer cur-buf
          (cond
           ;; if comment token is next to a string, chances are it is below the
           ;; docstring. Add a newlien in between.
           ((eq token-type 'string)
            (insert "\n" comment-content))
           ((= token-real-line comment-real-line-start)
            (insert " " comment-content))
           ((not (= token-real-line comment-real-line-start))
            (insert "\n" comment-content))
           (t)))))))

(defsubst srefactor--lisp-oneline-formatter ()
  (unless (srefactor--lisp-token-in-punctuation-p token)
    (insert " ")))

(defsubst srefactor--lisp-multiline-formatter ()
  (cond
   (ignore-num (when (and (equal first-token-name token-str))
                 (when (and ignore-num
                            (= ignore-num 0))
                   (setq ignore-num (1- ignore-num))))
               (while (> ignore-num 0)
                 (insert " ")
                 (setq token (srefactor--lisp-forward-token))
                 (insert token-str)
                 (setq ignore-num (1- ignore-num)))
               (if (srefactor--lisp-token-paren-p (car lexemes))
                   (srefactor--lisp-punctuation-formatter)
                 (insert "\n"))
               (setq ignore-num nil))
   ((and (equal first-token-name token-str)
         (not (eq next-token-type 'semantic-list)))
    (insert " "))
   ((and (eq next-token-type 'semantic-list)
         (eq token-type 'symbol))
    (insert " "))
   ((eq token-type 'semantic-list)
    (unless (srefactor--lisp-token-in-punctuation-p (car lexemes))
      (insert "\n")))
   ((or (null ignore-num)
        (= ignore-num 0))
    (insert "\n"))))

(defsubst srefactor--lisp-token-name-in-skip-list-p (token-name)
  (member token-name srefactor-lisp-symbol-to-skip))

(defsubst srefactor--lisp-token-in-punctuation-p (token-type)
  (member token-type '(open-paren close-paren punctuation)))

(defsubst srefactor--lisp-token-paren-p (token)
  (member (semantic-lex-token-class token) '(open-paren close-paren)))

(defsubst srefactor--lisp-visit-semantic-list-lex (lexemes)
  "Visit and format all sub-sexpressions (semantic list) in LEXEMES."
  (dolist (token lexemes)
    (let ((tok-start (semantic-lex-token-start token))
          (tok-end (semantic-lex-token-end token))
          tok-str)
      (when (and (eq (car token) 'semantic-list)
                 (> (- tok-end tok-start) 2))
        (goto-char (semantic-lex-token-start token))
        (srefactor--lisp-format-one-or-multi-lines tok-start
                                                   tok-end
                                                   tok-start
                                                   format-type (assoc tok-str srefactor-lisp-symbol-to-skip)
                                                   recursive-p)))))

(provide 'srefactor-lisp)
