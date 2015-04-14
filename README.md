<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. What is Semantic Refactor?</a></li>
<li><a href="#sec-2">2. Features</a></li>
<li><a href="#sec-3">3. Installation</a></li>
<li><a href="#sec-4">4. Usage</a></li>
</ul>
</div>
</div>

# What is Semantic Refactor?<a id="sec-1" name="sec-1"></a>

Semantic Refactor is a C/C++ refactoring tool based on Semantic parser
framework.

[Semantic](https://www.gnu.org/software/emacs/manual/html_node/semantic/index.html#Top) is a package that provides a framework for writing parsers.
Parsing is a process of analyzing source code based on programming
language syntax. relies on Semantic for analyzing source code and uses
its results to perform smart code refactoring that based on code
structure of the analyzed language, instead of plain text structure.

**NOTE**: `semantic-refactor` only supports Emacs 24.4 or above at the moment.

# Features<a id="sec-2" name="sec-2"></a>

## C/C++:

-   Context-sensitive menu offers appropriate refactoring actions

-   Generate class implementation (also handles nested class and class template)

-   Generate class getters and setters

-   Generate function implementation (also handles function template)

-   Generate function prototype

-   Convert function to function pointer

-   Convert function to function parameter

-   Move semantic units (class, function, variable)

-   Extract function with proper type information

-   Precise local variable rename

[More info and demos](srefactor-demos/demos.org)

## Lisp:

- Format whole buffer

- Format a defun

- Convert a sexpression into one line precisely

- Convert a sexpression into multiple lines precisely

[More info and demos](srefactor-demos/demos-elisp.org)

# Installation<a id="sec-3" name="sec-3"></a>

This package can be obtained from MELPA:

-   `M-x list-packages`
-   Move to `srefactor` and press <kbd>i</kbd>.
-   Press <kbd>x</kbd> to install.
-   Then, place this configuration in `~/.emacs.d/init.el` or `~/.emacs`:

```elisp
    (require 'srefactor)
    (require 'srefactor-lisp)
    
    ;; OPTIONAL: ADD IT ONLY IF YOU USE C/C++. 
    (semantic-mode 1) ;; -> this is optional for Lisp
    
    (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
    (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
    (global-set-key (kbd "M-RET o") 'srefactor-lisp-one-line)
    (global-set-key (kbd "M-RET m") 'srefactor-lisp-format-sexp)
    (global-set-key (kbd "M-RET d") 'srefactor-lisp-format-defun)
    (global-set-key (kbd "M-RET b") 'srefactor-lisp-format-buffer)
```

**NOTICE**: If you only use Lisp formatting, you don't have to enable `semantic-mode`.

# Usage<a id="sec-4" name="sec-4"></a>

To use this package, a user only needs to use this single command:
`srefactor-refactor-at-point` and `semantic-mode` activated. Based on
the context at point, appropriate menu items are offered. When the
menu opens up, the top line contains the tag at point, which is the
context for offering appropriate refactor actions.

Key bindings of contextual menu:

-   <kbd>1..9</kbd> to quickly execute an action.
-   <kbd>o</kbd> to switch to next option, <kbd>O</kbd> to switch to previous option.
-   <kbd>n</kbd> to go to the next line, <kbd>p</kbd> to got to previous line.
-   <kbd>q</kbd> or <kbd>C-g</kbd> to quit.

You can hide the help message in the menu with by customizing
`srefactor-ui-menu-show-help` and set it to `nil`.

    (setq srefactor-ui-menu-show-help nil)
