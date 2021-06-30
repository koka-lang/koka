;;; koka-mode.el --- A major mode for the Koka programming language -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'rx)

(defgroup koka-mode nil
  "Support for Koka code."
  :link '(url-link "https://koka-lang.github.io/koka/doc/index.html")
  :group 'languages)

;;; Indent:

;; TODO

;;; Syntax Table:

(defconst koka-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Operators
    (dolist (i '(?$ ?% ?& ?+ ?~ ?! ?^ ?# ?= ?. ?: ?- ?? ?< ?> ?|))
      (modify-syntax-entry i "." table))

    ;; Varaible/Constructor can include _, -, ' so treat it as symbol constituent
    ;; even though - can be used as an operator.
    (modify-syntax-entry ?\' "_" table)
    (modify-syntax-entry ?-  "_" table)
    (modify-syntax-entry ?_  "_" table)

    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    ;; String
    (modify-syntax-entry ?\" "\"" table)
    ;; Escape caracter
    (modify-syntax-entry ?\\ "\\" table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124" table)
    (modify-syntax-entry ?*  ". 23b" table)
    (modify-syntax-entry ?\n ">"    table)

    table))

;;; Font Lock:

(defconst koka--fun-name-re
  (rx line-start
      (? (seq (or "public" "private") (+ " ")))
      (? (seq (or "inline" "noinline") (+ " ")))
      "fun"
      (+ " ")
      (group-n 1 lower (* (any alphanumeric ?_ ?-)) (* ?')))
  "Matches a function declartion line and captures function name as group 1.
Does not asccept spaces in the begging of the line to prevent fun
operations matching.")

(defconst koka-font-lock-keywords
  `(;; Keywords
    ;; Exclude too generic keywords which chould be used as variable name(js, c, file)
    ,(regexp-opt '("infix" "infixr" "infixl" "prefix" "type" "struct" "alias" "con" "forall"
                   "exists" "some" "fun" "fn" "val" "var" "extern" "if" "then" "else" "elif"
                   "match" "return" "with" "in" "handle" "handler" "mask" "override" "control"
                   "rcontrol" "effect" "named" "module" "import" "as" "public" "private"
                   "abstract" "pub" "interface" "yield" "qualified" "hiding" "unsafe" "co"
                   "rec" "open" "extend" "behind" "linear" "value" "reference" "inline"
                   "noinline" "include" "import" ) 'symbols)
    ;; Function name
    (,koka--fun-name-re 1 font-lock-function-name-face)))

;;; Mode Map:

(defvar koka-mode-map
  (let ((map (make-sparse-keymap)))
	map)
  "Keymap for Koka major mode.")

;;; Major Mode:

;;;###autoload
(define-derived-mode koka-mode prog-mode "Koka"
  "A major mode for the Koka programming language.

\\{koka-mode-map}"
  :group 'koka-mode
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  ;; (setq-local electric-indent-chars
  ;;             (append koka-electric-indent-chars
  ;;                     (and (boundp 'electric-indent-chars)
  ;;                          electric-indent-chars)))
  ;; (setq-local indent-line-function 'koka-mode-indent-line)
  (setq-local indent-tabs-mode nil)  ; Koka forbids tab characters.
  ;; (setq-local syntax-propertize-function 'koka-syntax-propertize)
  ;; (setq-local imenu-generic-expression koka-imenu-generic-expression)
  (setq font-lock-defaults '(koka-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kk\\'" . koka-mode))

(provide 'koka-mode)

;;; koka-mode.el ends here
