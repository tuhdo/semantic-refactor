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

(defcustom srefactor-symbol-stand-alone '("progn"
                                          "while"
                                          "save-excursion"
                                          "save-window-excursion"
                                          "unwind-protect")
  "A list of first symbols in a form that must stand alone on its
  own line without any other symbol.")

(defun srefactor-one-line ()
  (interactive)
  (when (looking-at "(")
    (srefactor-one-or-multi-lines 'one-line)))

(defun srefactor-multi-line ()
  (interactive)
  (when (looking-at "(")
    (srefactor-one-or-multi-lines 'multi-line)))

(defun srefactor-one-or-multi-lines (format-type)
  (save-excursion
    (let* ((tag-start (point))
           (tag-end (save-excursion
                      (forward-sexp)
                      (point)))
           (lexemes (semantic-emacs-lisp-lexer tag-start tag-end 1))
           (first-symbol (cadr lexemes))
           (tag-name (buffer-substring-no-properties
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
             ((or (eq token-type 'open-paren)
                  (eq token-type 'close-paren))
              "")
             ((and (eq format-type 'one-line)
                   (not (or (eq token-type 'open-paren)
                            (eq token-type 'close-paren))))
              (insert " "))
             (t
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
        (unless (member tag-name srefactor-symbol-stand-alone)
          (save-excursion
            (goto-char (semantic-lex-token-end first-symbol))
            (delete-indentation 1)))
        (delete-indentation))
      (indent-region tag-start (point)))))

(provide 'srefactor-elisp)
