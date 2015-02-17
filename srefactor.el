;;; srefactor.el --- A refactoring tool based on Semantic parser framework
;;
;; Filename: srefactor.el
;; Description: A refactoring tool based on Semantic parser framework
;; Author: Tu, Do Hoang <tuhdo1710@gmail.com
;; Maintainer: Tu, Do Hoang
;; Created: Wed Feb 11 21:25:51 2015 (+0700)
;; Version: 0.1
;; Package-Requires: ()
;; Last-Updated: Wed Feb 11 21:25:51 2015 (+0700)
;;           By: Tu, Do Hoang
;;     Update #: 1
;; URL:
;; Doc URL:
;; Keywords: c, languages, tools
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
;; To use this package, user only needs to use this single command:
;; `srefactor-refactor-at-point'
;;
;; This package includes the following features:
;;
;; - Context-sensitive menu: when user runs the command, a menu
;; appears and offer refactoring choices based on current scope of
;; semantic tag. For example, if the cursor is inside a class, the
;; menu lists choices such as generate function implementations for
;; the class, generate class getters/setters... Each menu item also
;; includes its own set of options, such as perform a refactoring
;; option in current file or other file.
;;
;; - Generate class implementation: From the header file, all function
;; prototypes of a class can be generated into corresponding empty
;; function implementation in a source file. The generated function
;; implementations also include all of their (nested) parents as
;; prefix in the names, if any. If the class is a template, then the
;; generated functions also includes all templates declarations and in
;; the parent prefix properly.
;;
;; - Generate function implementation: Since all function
;; implementations can be generated a class, this feature should be
;; present.
;;
;; - Generate function prototype: When the cursor is in a function
;; implementation, a function prototype can be generated and placed in
;; a selected file. When the prototype is moved into, its prefix is
;; stripped.
;;
;; - Convert function to function pointer: Any function can be
;; converted to a function pointer with typedef. The converted
;; function pointer can also be placed as a parameter of a function.
;; In this case, all the parameter names of the function pointer is
;; stripped.
;;
;; - Move semantic units: any meaningful tags recognized by Semantic
;; (class, function, variable, namespace...) can be moved relative to
;; other tags in current file or any other file.
;;
;; - Extract function: select a region and turn it into a function,
;; with relevant variables turned into function parameters and
;; preserve full type information.
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

(with-no-warnings
  (require 'cl))
(require 'semantic)
(require 'hi-lock)
(require 'srefactor-ui)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User options
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom srefactor--getter-prefix "get_"
  "Prefix for inserting getter."
  :group 'srefactor)

(defcustom srefactor--setter-prefix "set_"
  "Prefix for inserting getter."
  :group 'srefactor)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Developer Options
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar srefactor-use-srecode-p nil
  "Use experimental SRecode tag insertion mechanism.")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands - only one currently
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun srefactor-refactor-at-point ()
  "Offer contextual menu with actions based on current tag in scope.

Each menu item added returns a token for what type of refactoring
to perform."
  (interactive)
  (semantic-fetch-tags)
  (let (menu-item-list
        (srefactor--file-options (srefactor-ui--return-option-list 'file))
        (tag (semantic-current-tag))
        (menu (srefactor-ui-menu "menu")))
    (when (srefactor--menu-add-function-implementation-p tag)
      (add-to-list 'menu-item-list `("Generate Function Implementation (Other file)"
                                     gen-func-impl
                                     ,srefactor--file-options)))
    (when (srefactor--menu-add-function-proto-p tag)
      (add-to-list 'menu-item-list `("Generate Function Prototype (Other file)"
                                     gen-func-proto
                                     ,srefactor--file-options)))
    (when (srefactor--menu-add-function-pointer-p tag)
      (add-to-list 'menu-item-list `("Generate Function Pointer (Current file)"
                                     gen-func-ptr
                                     ,srefactor--file-options)))
    (when (srefactor--menu-add-getters-setters-p tag)
      (add-to-list 'menu-item-list `("Generate Getters and Setters (Current file)"
                                     gen-getters-setters
                                     ("(Current file)"))))
    (when (srefactor--menu-add-getter-setter-p tag)
      (add-to-list 'menu-item-list `("Generate Setter (Current file)"
                                     gen-setter
                                     ("(Current file)")))
      (add-to-list 'menu-item-list `("Generate Getter (Current file)"
                                     gen-getter
                                     ("(Current file)")))
      (add-to-list 'menu-item-list `("Generate Getter and Setter (Current file)"
                                     gen-getter-setter
                                     ("(Current file)"))))
    (when (srefactor--local-var-at-point)
      (add-to-list 'menu-item-list `("Rename local variable (Current file)"
                                     rename-local-var
                                     ("(Current file)"))))
    (when (and (semantic-current-tag) (not (region-active-p)))
      (add-to-list 'menu-item-list `("Move (Current file)"
                                     move
                                     ,srefactor--file-options)))
    (when (region-active-p)
      (add-to-list 'menu-item-list `("Extract function (Current file)"
                                     extract-function
                                     nil)))
    (oset menu :items menu-item-list)
    (oset menu :action #'srefactor-ui--refactor-action)
    (oset menu :context tag)
    (oset menu :shortcut-p t)
    (srefactor-ui-create-menu menu)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level functions that select action to make
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun srefactor--refactor-based-on-tag-class (operation &optional file-option)
  "Refractor based on current tag in context.

OPERATION is a refactoring type user selected from the menu.
FILE-OPTION is a file destination associated with OPERATION."
  (senator-copy-tag)
  (let* ((refactor-tag (ring-ref senator-tag-ring 0))
         (class (semantic-tag-class refactor-tag)))
    (cond
     ((eq class 'function)
      (cond
       ((eq operation 'extract-function)
        (srefactor--extract-region 'function))
       ((eq operation 'rename-local-var)
        (let* ((local-var (srefactor--local-var-at-point))
               prompt)
          (unwind-protect
              (condition-case nil
                  (progn
                    (srefactor--highlight-tag local-var 'isearch)
                    (setq prompt (format "Replace (%s) with: " (semantic-tag-name local-var)))
                    (srefactor--rename-local-var local-var
                                                 (semantic-current-tag)
                                                 (read-from-minibuffer prompt))
                    (srefactor--unhighlight-tag local-var))
                (error nil))
            (srefactor--unhighlight-tag local-var))
          (srefactor--unhighlight-tag local-var)
          ))
       (t
        (let ((other-file (srefactor--select-file file-option)))
          (srefactor--refactor-tag (srefactor--contextual-open-file other-file)
                                   refactor-tag
                                   operation
                                   t)))))
     ((eq class 'type)
      (cond
       ((eq operation 'gen-getters-setters)
        (srefactor-insert-class-getters-setters))
       ((eq operation 'move)
        (let ((other-file (srefactor--select-file file-option)))
          (srefactor--refactor-tag (srefactor--contextual-open-file other-file)
                                   refactor-tag
                                   operation
                                   t)))
       (t (srefactor--refactor-type (srefactor--contextual-open-file
                                     (srefactor--select-file file-option))
                                    refactor-tag))))
     ((eq class 'variable)
      (cond
       ((eq operation 'gen-getter)
        (with-current-buffer (semantic-tag-buffer refactor-tag)
          (srefactor--jump-or-insert-public-label (save-excursion
                                                    (goto-char (semantic-tag-start refactor-tag))
                                                    (semantic-current-tag-parent))))
        (srefactor--insert-getter refactor-tag 1 1))
       ((eq operation 'gen-setter)
        (with-current-buffer (semantic-tag-buffer refactor-tag)
          (srefactor--jump-or-insert-public-label (save-excursion
                                                    (goto-char (semantic-tag-start refactor-tag))
                                                    (semantic-current-tag-parent))))
        (srefactor--insert-setter refactor-tag 1 1))
       ((eq operation 'move)
        (let ((other-file (srefactor--select-file file-option)))
          (srefactor--refactor-tag (srefactor--contextual-open-file other-file)
                                   refactor-tag
                                   operation
                                   t)))
       (t
        (with-current-buffer (semantic-tag-buffer refactor-tag)
          (srefactor--jump-or-insert-public-label (save-excursion
                                                    (goto-char (semantic-tag-start refactor-tag))
                                                    (semantic-current-tag-parent))))
        (srefactor--insert-getter refactor-tag 1 1)
        (srefactor--insert-setter refactor-tag 1 1))))
     ((eq class 'package)
      (message "FIXME: 'package refactoring is not yet implemented."))
     ((eq class 'include)
      (message "FIXME: 'include refactoring is not yet implemented."))
     ((eq class 'label)
      (message "FIXME: 'label refactoring is not yet implemented."))
     (t))))

(defun srefactor--select-file (option)
  "Select a file based on OPTION selected by a user."
  (let (other-files file l)
    (cond
     ((string-equal option "(Other file)")
      (when  (featurep 'projectile)
        (setq other-files (projectile-get-other-files (buffer-file-name)
                                                      (projectile-current-project-files)
                                                      nil))
        (setq l (length other-files))
        (setq file (concat (projectile-project-root)
                           (cond ((> l 1)
                                  (completing-read "Select a file: "
                                                   other-files))
                                 ((= l 1)
                                  (car other-files))
                                 (t (projectile-find-file)))))))
     ((string-equal option "(Current file)")
      (setq file (buffer-file-name (current-buffer))))
     ((and (string-equal option "(Project file)") (featurep 'projectile))
      (setq file (concat (projectile-project-root)
                         (completing-read "Select a file: "
                                          (projectile-current-project-files)))))
     ((string-equal option "(File)")
      (setq file (with-current-buffer (call-interactively 'find-file-other-window)
                   (buffer-file-name (current-buffer))))))
    file))

(defun srefactor--refactor-tag (buffer refactor-tag func-type &optional ask-place-p)
  "Refactor a tag.

BUFFER is a buffer from opening selected file chosen from the menu.

REFACTOR-TAG is selected tag to be refactored.

FUNC-TYPE is a refactoring action to be performed.

ASK-PLACE-P, if true, asks user to select a tag in BUFFER to insert next to it."
  (let (dest-tag
        (tag-list (nreverse (srefactor--fetch-candidates))))
    (setq srefactor-ui--func-type func-type)
    (with-current-buffer buffer
      (if (and ask-place-p tag-list)
          (progn
            ;; (setq dest-tag (cdr (assoc (completing-read "Select a place to insert: "
            ;;                                             tag-list)
            ;;                            tag-list)))
            (oset srefactor-ui--current-active-menu :items tag-list)
            (oset srefactor-ui--current-active-menu :action #'srefactor-ui--tag-action)
            (oset srefactor-ui--current-active-menu :shortcut-p nil)
            (srefactor-ui-create-menu srefactor-ui--current-active-menu))
        (srefactor--insert-tag refactor-tag nil func-type)))))

(defun srefactor--refactor-type (dest-buffer refactor-tag)
  "Generate function implementations for all functions in a
class, including functions in nested classes.

DEST-BUFFER is the destination buffer to insert generated code.
REFACTOR-TAG is a Semantic tag that holds information of a C++ class."
  (let* ((members (semantic-tag-type-members refactor-tag))
         (dest-buffer-tags (with-current-buffer dest-buffer
                             (semantic-fetch-tags)))
         ;; (diff (set-difference members
         ;;                       dest-buffer-tags
         ;;                       :test (lambda (t1 t2)
         ;;                               (string-equal (semantic-format-tag-summarize t1 nil nil)
         ;;                                             (semantic-format-tag-summarize t2 nil nil)))))
         )
    (dolist (tag members)
      (cond
       ((and (eq (semantic-tag-class tag) 'function)
             (semantic-tag-prototype-p tag))
        (srefactor--refactor-tag dest-buffer tag 'gen-func-impl))
       ((eq (semantic-tag-class tag) 'type)
        (srefactor--refactor-type dest-buffer tag))
       (t)))))

(defun srefactor--insert-tag (refactor-tag dest-tag insert-type &optional pos)
  "Insert a Semantic TAG to current buffer.

REFACTOR-TAG is selected Semantic tag to be refactored.

DEST-TAG is destination tag for refactored tag to be inserted
next to it. If nil, insert at the end of file.

POS is specific relative position to be inserted. POS is one of
the option \"Before|Inside|After\" that appears when a
destination tag can have its own members, such as a class or a
namespace.
"
  (semantic-fetch-tags)
  (let* ((parent-is-func-p (eq (semantic-tag-class (semantic-tag-calculate-parent dest-tag))
                               'function))
         (class (semantic-tag-class refactor-tag)))

    ;; if  refactor-tag dest-tag is nil, just insert at end of file
    (if dest-tag
        (progn
          (semantic-go-to-tag dest-tag)

          (if parent-is-func-p
              (srefactor--insert-function-as-parameter refactor-tag)

            ;; Handle selected position
            (cond
             ((string-equal pos "(Before)")
              (open-line 1))
             ((string-equal pos "(Inside)")
              (search-forward "{")
              (newline 1))
             (t (goto-char (semantic-tag-end dest-tag))
                (forward-line 1)))

            ;; handle insert type
            (cond
             ((eq insert-type 'gen-func-ptr)
              (srefactor--insert-function-pointer refactor-tag)
              (newline-and-indent)
              (recenter))
             ((eq insert-type 'gen-func-impl)
              (srefactor--insert-function-implementation refactor-tag))
             ((srefactor--tag-pointer refactor-tag)
              (semantic-insert-foreign-tag (srefactor--function-pointer-to-function refactor-tag)))
             ((eq insert-type 'move)
              (with-current-buffer (semantic-tag-buffer refactor-tag)
                (save-excursion
                  (goto-char (semantic-tag-start refactor-tag))
                  (senator-kill-tag)
                  (delete-blank-lines)))
              (if (and (or (srefactor--tag-struct-p dest-tag)
                           (srefactor--tag-struct-p
                            (srefactor--calculate-parent-tag dest-tag)))
                       (eq class 'function)
                       (eq major-mode 'c-mode))
                  (progn
                    (insert (srefactor--function-to-function-pointer refactor-tag))
                    (insert ";"))
                (yank)
                (indent-according-to-mode))
              (newline-and-indent))
             (t (senator-yank-tag)))))
      (goto-char (point-max))
      (cond
       ((eq insert-type 'gen-func-ptr)
        (srefactor--insert-function-pointer refactor-tag))
       ((eq insert-type 'gen-func-impl)
        (srefactor--insert-function-implementation refactor-tag))
       ((semantic-tag-get-attribute refactor-tag :function-pointer)
        (semantic-insert-foreign-tag (srefactor--function-pointer-to-function refactor-tag)))
       (t (senator-yank-tag))))

    ;; indent after inserting refactor-tag
    (indent-according-to-mode)

    ;; post content insertion based on context
    (unless srefactor-use-srecode-p
      (unless parent-is-func-p
        (if (eq insert-type 'gen-func-impl)
            (progn
              (end-of-line)
              (insert " {")
              (newline 1)
              (save-excursion
                (srefactor--insert-initial-content-based-on-return-type
                 (if (or (srefactor--tag-function-constructor refactor-tag)
                         (srefactor--tag-function-destructor refactor-tag))
                     ""
                   (semantic-tag-type refactor-tag)))
                (insert "}")
                (srefactor--maybe-insert-function-end dest-tag insert-type)
                (indent-according-to-mode)
                (srefactor--indent-and-newline 1))
              (goto-char (line-end-position)))
          (srefactor--maybe-insert-function-end dest-tag insert-type))))
    ))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions - IO
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun srefactor--contextual-open-file (other-file)
  "If the current buffer is also the selected file, don't open
the file in another window but use the current buffer and window
instead.

OTHER-FILE is the selected file from the menu."
  (if other-file
      (if (equal other-file (buffer-file-name (current-buffer)))
          (find-file other-file)
        (find-file-other-window other-file))
    ;; use ff-find-other-file if no file is chosen When no file is
    ;; chosen, it means that user selected (Other file) option, but
    ;; does not install Projectile so he cannot use its function to
    ;; return the filename of other file. In this case, he simply gets
    ;; nil, which mean it's the job for `ff-find-other-file'. This needs
    ;; fixing in the future
    (ff-find-other-file t t)

    ;; `ff-find-other-file' does not return a buffer but switching to
    ;; the opened buffer instantly. We must return a buffer from this
    ;; function otherwise things go wrong
    (current-buffer)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that insert actual text or modify text
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; CLASS
;;
(defun srefactor-insert-class-getters-setters ()
  "Select class at point and insert getters and setters if any.
This is a convenient wrapper for srefactor--insert-class-getters-setters."
  (semantic-fetch-tags-fast)
  (let ((tag (semantic-current-tag)))
    (when (eq (semantic-tag-class tag) 'type)
      (srefactor--insert-class-getters-setters tag))))

(defun srefactor--insert-getter (tag &optional newline-before newline-after)
  "Insert getter for TAG.
Add NEWLINE-BEFORE and NEWLINE-AFTER if t."
  (let ((tag-type (srefactor--tag-type-string tag)))
    (when newline-before
      (newline newline-before))
    (when (or (listp (semantic-tag-type tag))
              (semantic-tag-get-attribute tag :pointer))
      (insert "const "))
    (insert tag-type)
    (insert (concat " " srefactor--getter-prefix (semantic-tag-name tag)))
    (insert "() const")
    (insert " {")
    (srefactor--indent-and-newline 1)
    (insert (concat "return"
                    " "
                    (semantic-tag-name tag) ";"))
    (srefactor--indent-and-newline 1)
    (insert "}")
    (indent-according-to-mode)
    (when newline-after
      (newline newline-after))))

(defun srefactor--insert-setter (tag &optional newline-before newline-after)
  "Insert setter for TAG.
Add NEWLINE-BEFORE and NEWLINE-AFTER if t."
  (when newline-before
    (newline newline-before))
  (insert "void")
  (insert (concat " " srefactor--setter-prefix (semantic-tag-name tag)))
  (insert (concat (insert "(")
                  (unless (semantic-tag-variable-constant-p tag)
                    "const ")
                  (srefactor--tag-type-string tag)
                  (when (and (listp (semantic-tag-type tag))
                             ;; (srefactor--tag-reference tag)
                             (not (srefactor--tag-pointer tag)))
                    "&")
                  " "
                  (semantic-tag-name tag)
                  ")"))
  (insert " {")
  (srefactor--indent-and-newline 1)
  (insert (concat "this->" (semantic-tag-name tag) " = " (semantic-tag-name tag) ";"))
  (srefactor--indent-and-newline 1)
  (insert "}")
  (indent-according-to-mode)
  (when newline-after
    (newline newline-after)))

(defun srefactor--jump-or-insert-public-label (tag)
  "Check if TAG is a class or struct.
If so, check if any public label exists, jump to it.
Otherwise, insert one."
  (when (eq (semantic-tag-class tag) 'type)
    (goto-char (semantic-tag-start tag))
    (let* (label-pos
           (members (srefactor--tag-filter 'semantic-tag-class
                                           '(variable label)
                                           (semantic-tag-type-members tag)))
           (public-label (car (srefactor--tag-filter 'semantic-tag-name
                                                     '("public")
                                                     members))))
      (if public-label
          (progn
            (if (semantic-overlay-start (semantic-tag-overlay public-label))
                (progn
                  (goto-char (semantic-tag-end public-label))
                  (setq label-pos (semantic-tag-start public-label)))
              (search-forward "public:")
              (setq label-pos (point))))
        (goto-char (semantic-tag-end tag))
        (search-backward "}")
        (open-line 1)
        (insert "public:")
        (setq label-pos (point)))
      label-pos)))

(defun srefactor--insert-class-getters-setters (tag)
  "Insert getters and setters for class TAG.
Buffer need to be refreshed before TAG is inserted, if the buffer
content changed."
  (when (eq (semantic-tag-class tag) 'type)
    (let* ((members (srefactor--tag-filter 'semantic-tag-class
                                           '(variable label)
                                           (semantic-tag-type-members tag)))
           (variables (srefactor--tag-filter 'semantic-tag-class '(variable) members))
           (label-pos (srefactor--jump-or-insert-public-label tag)))
      (newline 1)
      (save-excursion
        (dolist (tag variables)
          (when (< (semantic-tag-start tag) label-pos)
            (srefactor--insert-getter tag)
            (newline 2)
            (srefactor--insert-setter tag)
            (newline 2)))
        (kill-whole-line))
      (recenter))))

;;
;; FUNCTION
;;

(defun srefactor--insert-function-implementation (func-tag)
  "Insert function implementations for FUNC-TAG at point, a tag that is a function."
  (forward-line 0)
  (open-line 1)
  (forward-line 1)
  (if srefactor-use-srecode-p
      ;; Try using SRecode as the mechanism for inserting a tag.
      (let* ((copy (semantic-tag-copy func-tag))
             ;; (parent (semantic-tag-calculate-parent func-tag))
             ;; TODO - below srefactor fcn should be a part of semantic or srecode.
             (parentstring1 (srefactor--tag-parents-string func-tag))
             (parentstring (substring parentstring1 0 (- (length parentstring1) 2)))
             (endofinsert nil))
        ;; Copied this line from original
        (semantic-tag-put-attribute func-tag :typemodifiers nil)
        (semantic-tag-put-attribute func-tag :parent parentstring)
        ;; Insert the tag
        (require 'srecode/semantic)
        ;; TODO - does it need any special dictionary entries?
        (setq endofinsert
              (srecode-semantic-insert-tag
               func-tag
               nil ;; Style
               (lambda (localtag)
                 (srefactor--insert-initial-content-based-on-return-type
                  (if (or (srefactor--tag-function-constructor copy)
                          (srefactor--tag-function-destructor copy))
                      ""
                    (semantic-tag-type copy)))
                 ) ;; Callbck for function body.
               ;; Dictionary entries go here.
               ))
        (goto-char endofinsert)
        (insert "\n\n")
        )
    (let ((func-tag-name (semantic-tag-name func-tag)))
      (when (srefactor--tag-function-modifiers func-tag)
        (semantic-tag-put-attribute func-tag :typemodifiers nil))
      (insert (srefactor--tag-templates-declaration-string (srefactor--calculate-parent-tag func-tag)))
      (insert (srefactor--tag-function-string func-tag))
      (search-backward func-tag-name)
      (save-excursion
        (when (srefactor--tag-function-destructor func-tag)
          ;; after go to tag start, point is right next to '~' character
          (forward-char -1))
        (insert (srefactor--tag-parents-string func-tag))
        (cond
         ;; TODO: insert ~ in correct place
         ((srefactor--tag-function-destructor func-tag)
          (re-search-backward "void")
          (replace-match ""))
         ((srefactor--tag-function-constructor func-tag)
          (c-beginning-of-statement-1)
          (re-search-forward func-tag-name)
          (replace-match ""))
         (t))))))

(defun srefactor--insert-function-pointer (tag)
  "Insert function pointer definition for TAG."
  (insert (concat "typedef "
                  (srefactor--tag-type-string tag)
                  " "
                  "("
                  (srefactor--tag-parents-string tag)
                  "*"
                  (semantic-tag-name tag)
                  ")"
                  "("))
  (mapc (lambda (tag)
          (let ((ptr-level (srefactor--tag-pointer tag))
                (ref-level (srefactor--tag-reference tag)))
            (insert (concat (srefactor--tag-type-string tag)
                            ", ") )))
        (semantic-tag-function-arguments tag))
  (search-backward ",")
  (replace-match "")
  (insert ");"))

(defun srefactor--insert-function-as-parameter (tag)
  "Insert TAG that is a function as a function parameter.
This means, the function is converted into a function pointer."
  (insert (srefactor--function-to-function-pointer tag))
  (insert ", "))

(defun srefactor--insert-new-function-from-region ()
  "Extract function from region."
  (semantic-force-refresh)
  (push-mark (region-beginning))
  (let ((reg-diff (- (region-end) (region-beginning)))
        (region (buffer-substring (region-beginning) (region-end)))
        (tag (semantic-current-tag))
        (local-vars (semantic-get-all-local-variables))
        l orig p1 p2 name has-error)
    (unwind-protect
        (condition-case e
            (progn
              (setq orig (point))
              (beginning-of-defun-raw)
              (recenter-top-bottom)
              (setq p1 (point))
              (open-line 2)
              (insert (concat "void " "new_function"))
              (insert "()")
              (insert " {")
              (newline 1)
              (insert region)
              (newline 1)
              (insert "}")
              (setq p2 (point))
              ;; must narrow to defun to check existences
              ;; of local variables
              (narrow-to-defun)
              (c-beginning-of-defun)
              (search-forward "(")
              (dolist (v local-vars l)
                (when (srefactor--var-in-region-p v (region-beginning) (region-end))
                  (push v l)))
              (insert (srefactor--tag-function-parameters-string l))
              (save-excursion
                (c-end-of-defun)
                (setq p2 (point)))
              (widen)
              (re-search-backward "new_function" nil t)
              (srefactor--mark-symbol-at-point)
              (setq name (read-from-minibuffer "Enter function name: "))
              (when (re-search-backward "new_function" nil t)
                (replace-match name))
              (indent-region (progn
                               (c-beginning-of-defun)
                               (point))
                             (progn
                               (c-end-of-defun)
                               (point))))
          (error (widen)
                 (setq has-error t)
                 (message "%s" "The selected region is malformed."))))
    (widen)
    (when has-error
      (delete-region p1 p2)
      (kill-line 2)
      (goto-char orig)
      (pop-mark))
    (goto-char (car mark-ring))
    (delete-region (car mark-ring) (+ (car mark-ring) reg-diff))
    (insert name)
    (insert "(")
    (dolist (v l)
      (insert (concat (semantic-tag-name v) ", ")))
    (insert ");")
    (when (re-search-backward ", " nil t)
      (replace-match ""))
    (pop-mark)))

(defun srefactor--insert-initial-content-based-on-return-type (tag-type)
  "Insert initial content of function implementations.

TAG-TYPE is the return type such as int, long, float, double..."
  (cond
   ((listp tag-type)
    (insert (semantic-tag-name tag-type) " b;" )
    (indent-according-to-mode)
    (newline 2)
    (insert "return b;")
    (indent-according-to-mode))
   ((or (string-match "int" tag-type)
        (string-match "short" tag-type)
        (string-match "long" tag-type))
    (insert "return 0;"))
   ((or (string-match "double" tag-type)
        (string-match "float" tag-type))
    (insert "return 0.0;"))
   ((string-match "bool" tag-type)
    (insert "return true;"))
   ((string-match "char" tag-type)
    (insert "return 'a';"))
   (t))
  (srefactor--indent-and-newline 1)
  (forward-line 1))

;; TODO: work on this in next release
(defun srefactor--insert-new-macro-from-region ()
  "Assume region is marked"
  (let ((region (buffer-substring (region-beginning) (region-end)))
        (orig (region-beginning))
        (pend (region-end))
        (longest-line 0)
        (m major-mode)
        p1 p2)
    (goto-char orig)
    (beginning-of-line)
    (while (not (eq (point) (region-end)))
      (setq p1 (point))
      (end-of-line)
      (setq p2 (point))
      (when (> (- p2 p1) longest-line)
        (setq longest-line (- p2 p1)))
      (forward-line 1))
    ;; (setq mark-active nil)
    (setq pend (point))
    (goto-char orig)
    (beginning-of-line)
    (while (< (point) pend)
      (setq p1 (point))
      (end-of-line)
      (setq p2 (point))
      (insert (make-string (- longest-line (- p2 p1)) ?\s))
      (insert " \\")
      (forward-line 1))))

(defun srefactor--maybe-insert-function-end (dest-tag function-insert-type)
  "Insert semicolon depend on the context of DEST-TAG and FUNCTION-INSERT-TYPE."
  ;; handle prototype insertion into a parent class
  (when (and (eq (semantic-tag-class (semantic-tag-calculate-parent dest-tag)) 'type)
             (not (eq function-insert-type 'move))
             (not (eq function-insert-type 'gen-func-ptr)))
    (insert ";")
    (newline-and-indent)))

;;
;; VARIABLE
;;
(defun srefactor--rename-local-var (local-var-tag function-tag new-name)
  "Rename the name of a LOCAL-VAR-TAG in FUNCTION-TAG to NEW-NAME."
  (save-excursion
    (mapc (lambda (l)
            (goto-line l)
            (search-forward-regexp (srefactor--local-var-regexp local-var-tag))
            (replace-match new-name t t nil 1))
          (srefactor--collect-local-var-lines local-var-tag function-tag nil)))
  (message (format "Renamed %s to %s" (semantic-tag-name local-var-tag) new-name)))

;;
;; GENERAL
;;

(defun srefactor--indent-and-newline (&optional number)
  "Indent than insert a NUMBER of newline."
  (indent-according-to-mode)
  (newline (if number number 1)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that operate on a Semantic tag and return information
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro srefactor--tag-filter (predicate tag-classes-or-names tags)
  "Filter TAGS based on PREDICATE that satisfies TAG-CLASSES-OR-NAMES.

TAG-CLASSES-OR-NAMES can be a list of Semantic tag classes, or a
list of Semantic tag names, but not both.

Based on the type of list passed above, either use
`semantic-tag-class' or `semantic-tag-name' as PREDICATE."
  `(let (l)
     (dolist (tag ,tags l)
       (when (member (funcall ,predicate tag) ,tag-classes-or-names)
         (setq l (cons tag l))))))

(defun srefactor--get-all-parents (tag)
  "Return a list of parent tags of a TAG.
The closer to the end of the list, the higher the parents."
  (let* ((tag-buffer (semantic-tag-buffer tag))
         (parent (with-current-buffer (if tag-buffer
                                          tag-buffer
                                        (current-buffer))
                   (save-excursion
                     (goto-char (semantic-tag-start tag))
                     (semantic-current-tag-parent)))))
    (when parent
      (cons parent (srefactor--get-all-parents parent)))))

(defun srefactor--tag-parents-string (tag)
  "Return parent prefix string of a TAG.

It is used for prepending to function or variable name defined
outside of a scope."
  (let ((parents (srefactor--get-all-parents tag)))
    (unless (or (semantic-current-tag-parent)
                (semantic-current-tag)
                (not parents))
      (concat (mapconcat (lambda (T)
                           (concat (semantic-tag-name T)
                                   (srefactor--tag-templates-parameters-string T)))
                         (nreverse parents) "::") "::"))))

(defun srefactor--tag-function-parameters-string (members)
  "Return function parameter string of a function.

MEMBERS is a list of tags that are parameters of a function.  The
parameters are retrieved by the function `semantic-tag-function-arguments'.

The returned string is formatted as \"param1, param2, param3,...\"."
  (mapconcat (lambda (m)
               (concat (srefactor--tag-type-string m)
                       " "
                       (semantic-tag-name m)
                       ))
             members
             ", "))

(defun srefactor--tag-function-string (tag)
  "Return a complete string representation of a TAG that is a function."
  (let ((return-type (srefactor--tag-type-string tag))
        (members (semantic-tag-function-arguments tag)))
    (concat return-type
            " "
            (when (srefactor--tag-function-destructor tag)
              "~")
            (semantic-tag-name tag)
            "("
            (srefactor--tag-function-parameters-string members)
            ")")))

(defun srefactor--tag-template-string-list (tag)
  "Return a list of templates as a list of strings from a TAG."
  (let ((templates (semantic-c-tag-template tag)))
    (unless templates
      (setq templates (semantic-c-tag-template (srefactor--calculate-parent-tag tag))))
    (when templates
      (mapcar #'car templates))))

(defun srefactor--calculate-parent-tag (tag)
  "An alternative version of `semantic-tag-calculate-parent'.

It is the same except does not check if a TAG is in current
buffer.  If such check is performed, even if a TAG has parent, nil
is returned."
  (let ((tag-buffer (semantic-tag-buffer tag)))
    (with-current-buffer (if tag-buffer
                             tag-buffer
                           (current-buffer))
      (save-excursion
        (goto-char (semantic-tag-start tag))
        (semantic-current-tag-parent)))))

(defun srefactor--tag-templates-parameters-string (tag)
  "Return a string with all template parameters from a TAG.

The returned string is formatted as \"<class T1, class T2, ...>\"."
  (let ((tmpl-list (srefactor--tag-template-string-list tag)))
    (if tmpl-list
        (concat "<"
                (mapconcat #'identity tmpl-list ", ")
                ">")
      ""))
  )

(defun srefactor--tag-templates-declaration-string (tag)
  "Return a string with all template declarations from a TAG.

The returned string is formatted as:

\"template <class T1, class T2>\"
\"template <class T3>\"
\"....\"."
  (let ((parent (srefactor--calculate-parent-tag tag))
        (tmpl-list (srefactor--tag-template-string-list tag)))
    (if tmpl-list
        (concat (if parent
                    (srefactor--tag-templates-declaration-string parent)
                  "")
                (concat "template <"
                        (mapconcat (lambda (T)
                                     (concat "class " T))
                                   tmpl-list
                                   ", ")
                        ">"
                        "\n"))
      "")))

(defun srefactor--function-pointer-to-function (tag)
  "Convert a function pointer from a function TAG."
  (let* ((new-tag (semantic-tag-copy tag))
         (args (semantic-tag-function-arguments new-tag))
         (i 1))
    (mapc (lambda (arg)
            (semantic-tag-set-name arg (concat "a" (number-to-string i)))
            (setq i (+ i 1)))
          args)
    (semantic-tag-set-name new-tag (semantic-tag-name new-tag))
    (semantic--tag-put-property new-tag :foreign-flag t)
    (semantic-tag-put-attribute new-tag :function-pointer nil)
    new-tag))

(defun srefactor--function-to-function-pointer (tag)
  "Convert a function to function pointer from a TAG"
  (let* ((type-string (srefactor--tag-type-string tag))
         (tag-name (concat "(*" (semantic-tag-name tag) ")"))
         (args (semantic-tag-function-arguments tag)))
    (concat type-string
            " "
            tag-name
            " "
            "("
            (mapconcat (lambda (arg)
                         (srefactor--tag-type-string arg))
                       args
                       ", ")
            ")")))

(defun srefactor--tag-function-modifiers (tag)
  "Return `:typemodifiers' attribute of a TAG."
  (semantic-tag-get-attribute tag :typemodifiers))

(defun srefactor--tag-function-destructor (tag)
  "Return `:destructor-flag' attribute of a TAG, that is either t or nil."
  (semantic-tag-get-attribute tag :destructor-flag))

(defun srefactor--tag-function-constructor (tag)
  "Return `:constructor-flag' attribute of a TAG, that is either t or nil."
  (semantic-tag-get-attribute tag :constructor-flag))

(defun srefactor--local-var-regexp (tag)
  "Return regexp for seraching local variable TAG."
  (format (concat "\\(\\_\<%s\\)[ ]*\\([^[:alnum:]"
                  (unless (srefactor--tag-lambda-p tag) ")")
                  "]\\)") (semantic-tag-name tag)))

(defun srefactor--tag-pointer (tag)
  "Return `:pointer' attribute of a TAG."
  (semantic-tag-get-attribute tag :pointer))

(defun srefactor--tag-typedef (tag)
  "Return `:typedef' attribute of a TAG."
  (semantic-tag-get-attribute tag :typedef))

(defun srefactor--tag-reference (tag)
  "Return `:reference' attribute of a TAG.

If it does not exist, perform additional check to make sure it
does not, since the actual text in buffer has it but for some
complicated language construct, Semantic cannot retrieve it."
  (let ((reference (semantic-tag-get-attribute tag :reference))
        (tag-buffer (semantic-tag-buffer tag)))
    (if reference
        reference
      (save-excursion
        (with-current-buffer (if tag-buffer
                                 tag-buffer
                               ;; only tag in current buffer does not
                               ;; carry buffer information
                               (current-buffer))
          (goto-char (semantic-tag-start tag))
          (re-search-forward (concat ".*&[ ]+.*" (semantic-tag-name tag))
                             (semantic-tag-end tag)
                             t))))))

(defun srefactor--tag-type-string (tag)
  "Return a complete return type of a TAG as string."
  (let* ((ptr-level (srefactor--tag-pointer tag))
         (ref-level (srefactor--tag-reference tag))
         (tag-type (semantic-tag-type tag))
         (const-p (semantic-tag-variable-constant-p tag))
         (template-specifier (when (semantic-tag-p tag-type)
                               (semantic-c-tag-template-specifier tag-type))))
    (if template-specifier
        (replace-regexp-in-string ",>" ">"
                                  (concat (when (semantic-tag-variable-constant-p tag)
                                            "const ")
                                          (when (srefactor--tag-struct-p tag)
                                            "struct ")
                                          (car (semantic-tag-type tag))
                                          "<"
                                          (srefactor--tag-type-string-inner-template-list template-specifier)
                                          ">"
                                          (cond
                                           (ptr-level
                                            (make-string ptr-level ?\*))
                                           (ref-level
                                            (make-string ref-level ?\&))
                                           (t ""))))
      (if (listp tag-type)
          (concat (when const-p
                    "const ")
                  (when (srefactor--tag-struct-p tag)
                    "struct ")
                  (car tag-type)
                  (when (srefactor--tag-reference tag)
                    "&"))
        tag-type))))

(defun srefactor--tag-type-string-inner-template-list (tmpl-spec-list)
  (mapconcat (lambda (tmpl)
               (let* ((templates (semantic-c-tag-template-specifier tmpl)))
                 (concat (if (listp tmpl)
                             (car tmpl)
                           tmpl)
                         (if (and (not (null templates)) (listp templates))
                             (concat "<"  (srefactor--tag-type-string-inner-template-list templates)) ",")
                         (when templates "> "))))
             tmpl-spec-list
             ""))

(defun srefactor--extract-region (extract-type)
  "Extract region based on type.

EXTRACT-TYPE can be 'function or 'macro."
  (interactive)
  (if (region-active-p)
      (unwind-protect
          (progn
            ;; (narrow-to-region (region-beginning) (region-end))
            ;; (when (semantic-parse-region (region-beginning) (region-end))
            ;;   (error "Please select a region that is not a declaration or an implementation."))
            (save-excursion
              (narrow-to-region (region-beginning) (region-end))
              (c-beginning-of-defun)
              (c-end-of-defun))
            (widen)
            (cond
             ((eq extract-type 'function)
              (srefactor--insert-new-function-from-region))
             ((eq extract-type 'macro)
              (srefactor--insert-new-macro-from-region))
             (t)))
        (widen))
    (error "No active region.")))

(defun srefactor--mark-symbol-at-point ()
  "Activate mark for a symbol at point."
  (interactive)
  (forward-symbol -1)
  (set-mark-command nil)
  (forward-symbol 1)
  (setq deactivate-mark nil))

(defun srefactor--fetch-candidates ()
  "Return a list of candidates in current buffer.

Each candidate is a list '(DISPLAY TAG OPTIONS).  This is a
wrapper for `srefactor--fetch-candidates-helper'.  See
`srefactor--fetch-candidates-helper' for more details."
  (semantic-fetch-tags)
  (srefactor--fetch-candidates-helper (semantic-fetch-tags) 0 nil))

(defun srefactor--fetch-candidates-helper (tags depth &optional class)
  "Return a list of lists '(DISPLAY TAG OPTIONS).

This function is intended to be used with `srefactor-ui-create-menu' to
be displayed as a list of menu items.

DISPLAY is the string to bepresented to user, TAG is a semantic
tag and OPTIONS is a list of possible choices for each menu item.

 TAGS are collection of Semantic tags in current buffer.
 DEPTH is current recursion depth.
 CLASS is the parent class."
  (let ((spaces (make-string (* depth 3) ?\s))
        (srefactor--tag-options (srefactor-ui--return-option-list 'tag))
        (dashes (make-string 1 ?\-))
        (class class)
        cur-type display tag-list)
    (cl-dolist (tag tags)
      (when (listp tag)
        (cl-case (setq cur-type (semantic-tag-class tag))
          ((function type)
           (let ((type-p (eq cur-type 'type)))
             (unless (and (> depth 0) (not type-p))
               (setq class nil))
             (setq display (concat (if (null class)
                                       spaces
                                     (format "%s|%s%s" spaces dashes "►"))
                                   (semantic-format-tag-summarize tag nil t)
                                   (if (eq cur-type 'type)
                                       " (Before)")))
             (and type-p
                  (setq class (car tag)))
             ;; Recurse to children
             (push (list display tag (if (eq cur-type 'type)
                                         srefactor--tag-options
                                       nil)) tag-list)
             (setq tag-list (append (srefactor--fetch-candidates-helper (semantic-tag-components tag)
                                                                        (1+ depth)
                                                                        class)
                                    tag-list))))

          ((package include label variable)
           (let* ((parent-tag (semantic-tag-calculate-parent tag))
                  (display (concat (if parent-tag
                                       (format "%s|%s%s" spaces dashes "►")
                                     spaces)
                                   (semantic-format-tag-summarize tag nil t))))
             (push (list display tag nil) tag-list)))
          ;; Catch-all
          (t))))
    tag-list))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions - Predicates
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun srefactor--menu-add-function-proto-p (tag)
  "Check whether to add generate function prototype menu item for a TAG."
  (let ((class (semantic-tag-class tag)))
    (and (eq class 'function)
         (not (semantic-tag-prototype-p tag))
         (and (not (srefactor--tag-function-constructor tag))
              (not (srefactor--tag-function-destructor tag)))
         (not (region-active-p)))))

(defun srefactor--menu-add-function-implementation-p (tag)
  "Check whether to add generate function implementation menu item for a TAG."
  (let ((class (semantic-tag-class tag)))
    (and (or (eq class 'type)
             (and (eq class 'function)
                  (semantic-tag-prototype-p tag)))
         (not (region-active-p)))))

(defun srefactor--menu-add-function-pointer-p (tag)
  "Check whether to add generate function pointer menu item for a TAG."
  (and (eq (semantic-tag-class tag) 'function)
       (not (semantic-tag-get-attribute tag :pointer))
       (and (not (srefactor--tag-function-constructor tag))
            (not (srefactor--tag-function-destructor tag)))
       (not (region-active-p))))

(defun srefactor--menu-add-getters-setters-p (tag)
  "Check whether to add generate getters and setters menu item for a TAG."
  (and (eq (semantic-tag-class tag) 'type)
       (srefactor--tag-filter 'semantic-tag-class '(variable) (semantic-tag-type-members tag))
       (not (region-active-p))))

(defun srefactor--menu-add-getter-setter-p (tag)
  "Check whether to add generate getter and setter menu item for a TAG."
  (and (eq (semantic-tag-class tag) 'variable)
       (eq (semantic-tag-class (semantic-tag-calculate-parent tag)) 'type)
       (not (region-active-p))))

(defun srefactor--local-var-at-point ()
  "Check whether text at point is a local variable."
  (condition-case nil
      (catch 'exist
        (let (beg end)
          (mapc (lambda (v)
                  (save-excursion
                    (srefactor--mark-symbol-at-point)
                    (setq beg (region-beginning))
                    (setq end (region-end))
                    (setq mark-active nil)
                    (when (srefactor--var-in-region-p v beg end)
                      (throw 'exist v))))
                (semantic-get-all-local-variables)))
        nil)
    (error nil)))

(defun srefactor--activate-region (beg end)
  "Activate a region from BEG to END."
  (interactive)
  (goto-char beg)
  (set-mark-command nil)
  (goto-char end)
  (setq deactivate-mark nil))

(defun srefactor--menu-for-region-p ()
  "Check whether to add exclusive menu item for a region."
  (region-active-p))

(defun srefactor--var-in-region-p (tag beg end)
  "Check if a local variable TAG is in a region from BEG to END."
  (save-excursion
    (goto-char beg)
    (search-forward-regexp (srefactor--local-var-regexp tag) (line-end-position) t)))

(defun srefactor--tag-struct-p (tag)
  "Check if TAG is a C struct."
  (condition-case nil
      (let* ((type-tag (semantic-tag-type tag))
             (typedef-tag (srefactor--tag-typedef tag))
             type-type-tag struct-p)
        (when typedef-tag
          (setq struct-p (semantic-tag-type typedef-tag)))
        (unless struct-p
          (setq type-type-tag (semantic-tag-type type-tag))
          (setq struct-p (and (stringp type-type-tag)
                              (string-equal type-type-tag "struct"))))
        struct-p)
    (error nil)))

(defun srefactor--tag-auto-p (tag)
  "Check whether a TAG is an auto variable."
  (let ((type (semantic-tag-type tag)))
    (and (listp type)
         (string-equal "auto" (car type)))))

(defun srefactor--tag-lambda-p (tag)
  "Check whether TAG is a lambda function."
  (condition-case nil
      (save-excursion
        (goto-char (semantic-tag-start tag))
        (and (srefactor--tag-auto-p tag)
             (search-forward-regexp "=[ ]*\\[.*\\][ ]*(.*)[ ]*" (semantic-tag-end tag) t)))
    (error nil)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions - Utilities
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun srefactor--collect-local-var-lines (local-var-tag function-tag &optional with-content)
  "Return all lines that LOCAL-VAR-TAG occurs in FUNCTION-TAG.
If WITH-CONTENT is nil, returns a list of line numbers.  If
WITH-CONTENT is t, returns a list of pairs, in which each element
is a cons of a line and the content of that line."
  (save-excursion
    (goto-char (semantic-tag-start function-tag))
    (srefactor--collect-lines-regexp (srefactor--local-var-regexp local-var-tag)
                                     (current-buffer)
                                     (semantic-tag-end function-tag)
                                     with-content)))

(defun srefactor--collect-lines-regexp (regexp buffer &optional bound with-content)
  (srefactor--collect-lines (lambda () (re-search-forward regexp bound t)) buffer with-content))

(defun srefactor--collect-lines (predicate buffer &optional with-content)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let (lines)
        (while (funcall predicate)
          (push  (if with-content
                     (cons (line-number-at-pos (point))
                           (buffer-substring-no-properties (line-beginning-position)
                                                           (line-end-position)))
                   (line-number-at-pos (point))) lines))
        lines))))

(defun srefactor--highlight-tag (tag &optional face)
  "Highlight TAG with FACE."
  (highlight-regexp (srefactor--local-var-regexp tag) face))

(defun srefactor--unhighlight-tag (tag)
  "Unhighlight TAG."
  (unhighlight-regexp (srefactor--local-var-regexp tag)))

(provide 'srefactor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; srefactor.el ends here
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
