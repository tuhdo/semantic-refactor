;;; srefactor.el --- A refactoring tool based on Semantic parser framework
;;
;; Filename: srefactor-elisp.el
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

(defcustom srefactor-symbol-to-skip '(("progn" . 1)
                                      ("while" . 1)
                                      ("defun" . 2)
                                      ("lambda" . 2)
                                      ("if" . 1)
                                      ("when" . 1)
                                      ("unless" . 1)
                                      ("with-current-buffer" . 1)
                                      ("let" . 1)
                                      ("let*" . 1))
  "A list of pairs of a symbol and a number that denotes how many
  sexp to be skipped before inserting the first newline. ")

(defun srefactor-one-line ()
  (interactive)
  (srefactor-one-or-multi-lines 'one-line))

(defun srefactor-multi-line ()
  (interactive)
  (srefactor-one-or-multi-lines 'multi-line))

(defun srefactor-one-or-multi-lines (format-type)
  (save-excursion
    (unless (looking-at "(")
      (backward-up-list))
    (let* ((tag-start (point))
           (tag-end (save-excursion
                      (forward-sexp)
                      (point)))
           (lexemes (semantic-emacs-lisp-lexer tag-start tag-end 1))
           (first-symbol (cadr lexemes))
           (first-symbol-name (buffer-substring-no-properties
                               (semantic-lex-token-start first-symbol)
                               (semantic-lex-token-end first-symbol)))
           (tmp-buf (generate-new-buffer "let-buf"))
           start end token-str ignore-pair token ignore-num)
      (setq start (point))
      (while lexemes
        (setq token (pop lexemes))
        (setq token-str (if token
                            (buffer-substring-no-properties
                             (semantic-lex-token-start token)
                             (semantic-lex-token-end token))
                          ""))
        (let* ((token-type (car token))
               (next-token (car lexemes))
               (next-token-type (car next-token)))
          (with-current-buffer tmp-buf
            (insert token-str)
            (cond
             ((or (and (eq token-type 'punctuation)
                       (equal token-str "'"))
                  (eq token-type 'open-paren)
                  (eq token-type 'close-paren)
                  (eq next-token-type 'close-paren))
              "")
             ((eq format-type 'one-line)
              (insert " "))
             ((eq format-type 'multi-line)
              (if (and (eq token-type 'symbol)
                       (string-match ":.*" token-str))
                  (insert " ")
                (insert "\n")))))))
      (setq end (point))
      (kill-region start tag-end)
      (goto-char start)
      (insert (with-current-buffer tmp-buf
                (buffer-substring-no-properties (point-min)
                                                (point-max))))
      (when (eq format-type 'multi-line)
        (when (setq ignore-pair (assoc first-symbol-name srefactor-symbol-to-skip))
          (save-excursion
            (setq ignore-num (cdr ignore-pair))
            (goto-char tag-start)
            (while (> ignore-num 0)
              (forward-line 1)
              (delete-indentation)
              (setq ignore-num (1- ignore-num))))))
      (indent-region tag-start (point)))))

(provide 'srefactor-elisp)
