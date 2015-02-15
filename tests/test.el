;;; srf-src-test.el --- SRefactor tests for comparing to SRecode tag insertion
;;
;; Copyright (C) 2015 Eric Ludlam
;;
;; Author: Eric Ludlam <zappo@ballista>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:
;;
;; SRefactor couldn't use SRecode for inserting new code due to several
;; problems.  This test suite is for comparing the output of the
;; original code generator with an srecode implementaiton.

;;; Code:

(defvar srf-src-test-create-impl-files
  ;;      Starting File         SRecode Version          SRefactor Gold version
  '( ("tests/ColorSolver.hh" "tests/ColorSolver.cpp" "tests/ColorSolverGold.cpp" )
     )
  "List of test source files that SRefactor will be applied to.")

(defun srf-src-run-tests ()
  "Run tests of SRefactor using both the original and SRecode tag inserters."
  (interactive)
  ;; Run the tests for creating an implementation.
  (dolist (TEST srf-src-test-create-impl-files)
    (srf-src-run-create-impl-test (car TEST) (cadr TEST) (caddr TEST))
    )

  )

(defun srf-src-run-create-impl-test (start gen_into_srecode gen_into_gold)
  "Run tests for creating an implementation from a header."
  ;; Find and update the srecode and gold files.
  (srf-src-find-update-src gen_into_srecode)
  (srf-src-find-update-src gen_into_gold)
  ;; Get the starting file updated.
  (srf-src-find-update-src start)

  ;; Run the test.
  (let ((refactor-tag (semantic-current-tag))
        (start_srecode (current-time))
        end_srecode
        start_gold end_gold)

    (setq srefactor-use-srecode-p t)

    (srefactor--refactor-type (srf-src-find gen_into_srecode) refactor-tag)

    (setq end_srecode (current-time)
          start_gold (current-time))

    (setq srefactor-use-srecode-p nil)

    (srf-src-find gen_into_gold)
    (srefactor--refactor-type (srf-src-find gen_into_gold) refactor-tag)

    (setq end_gold (current-time))

    (message "Elapsed Time Srecode: %f" (srf-elapsed-time start_srecode end_srecode))
    (message "Elapsed Time Gold: %f" (srf-elapsed-time start_gold end_gold))

    ))


;; Utilities
(defvar srf-src-test-loadfrom
  (let ((BASEDIR (file-name-directory
                  (or load-file-name (buffer-file-name)))))
    (file-name-directory BASEDIR))
  "Location where this test suite came from.")

(defun srf-src-find (fname)
  "Load in a source filename."
  (let* ((f (expand-file-name fname srf-src-test-loadfrom))
         (buff (find-file-noselect f)))
    (set-buffer buff)))

(defun srf-src-find-update-src (fname)
  "Load in source fname, and update that file to prep for a test."
  (srf-src-find fname)
  (goto-char (point-min))
  (cond
   ((re-search-forward "-!-" nil t)
    ;; We just need to go there.
    nil)

   ((re-search-forward "// ERASE BELOW")
    ;; We want to go there, and delete anything after this.
    (delete-region (point) (point-max))
    (insert "\n"))

   (t
    ;; Unsure what this is about.
    nil)
   )
  )

(defun srf-elapsed-time (start end)
  "Copied from elp.el.  Was elp-elapsed-time.
Argument START and END bound the time being calculated."
  (+ (* (- (car end) (car start)) 65536.0)
     (- (car (cdr end)) (car (cdr start)))
     (/ (- (car (cdr (cdr end))) (car (cdr (cdr start)))) 1000000.0)))

(provide 'srf-src-test)

;;; srf-src-test.el ends here
