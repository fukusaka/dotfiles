;;; scheme-lock.el --- Configurable font locking for Scheme code

;; Copyright (C) 1998 by Free Software Foundation, Inc.

;; Author: Karl M. Hegbloom <karlheg@bittersweet.inetarena.com>
;; Keywords: faces, languages, matching
;; Version: .001 Debian/Guile

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; Redefine the scheme-font-lock-keywords on the fly, using `custom'.
;; It still needs a lot of work.  Anyone willing to help?

;;; Code:

(require 'regexp-opt)
(require 'font-lock)

(defgroup scheme-lock nil
  "Font lock keywords for Scheme codes."
  :group 'faces)

(defface font-lock-macro-name-face
  '((((class color) (background light)) (:foreground "brown" :bold t)))
  "Face for highlighting Scheme and Lisp macro names in definitions."
  :group 'scheme-lock)

(defvar scheme-lock-function-keywords 
  '("define" "define-public"))

(defvar scheme-lock-macro-keywords
  '("define-syntax" "defmacro"))

(defvar scheme-lock-class-keywords
  '("define-class"))

(defvar scheme-lock-control-structure-keywords
  '("lambda" "begin" "call-with-current-continuation" "call/cc"
    "if" "cond" "=>" "else" "case"
    "do" "for-each"
    "let" "let*"
    "let-syntax" "letrec" "letrec-syntax"
    ;; Hannes Haug <hannes.haug@student.uni-tuebingen.de> wants:
    "and" "or" "delay"
    ;; Stefan Monnier <stefan.monnier@epfl.ch> says don't bother:
    ;;"quasiquote" "quote" "unquote" "unquote-splicing"
    "map" "syntax" "syntax-rules"
    "call-with-input-file" "call-with-output-file"))

(defvar scheme-lock-extra-highlighting-sexps
  '(("David Fox <fox@graphics.cs.nyu.edu> for SOS/STklos class specifiers."
     . ("\\<<\\sw+>\\>" . font-lock-type-face))
    ("Scheme `:' keywords as references."
     . ("\\<:\\sw+\\>" . font-lock-reference-face))
    ("`(define-module (mod1 mod2 ...)\n :use-module (use1 use2 ...)"
     . ("(\\(define-module\\)[ \t]+(\\([^\)]+\\))"
	(1 font-lock-preprocessor-face) (2 font-lock-type-face t)))
    ("Second line of above."
     . (":use-module[ \t]+(\\([^\)]+\\))" (1 font-lock-type-face)))
    ("Bolden the dot in dotted lists"
     . ("\\<\\.\\>" . bold))
    ))

(defun scheme-lock-set-keywords% (var val)
  (set-default var val)
  (setq scheme-font-lock-keywords
	(append (list
		 (when scheme-lock-macro-keywords
		   (list (concat "(\\("
				 (regexp-opt scheme-lock-macro-keywords)
				 "\\)[ \t]+(?\\(\\sw+\\)?")
			 '(1 font-lock-keyword-face)
			 '(2 font-lock-macro-name-face nil t)))
		 ;;
		 (when scheme-lock-class-keywords
		   (list (concat "(\\("
				 (regexp-opt scheme-lock-class-keywords)
				 "\\)[ \t]+(?\\(\\sw+\\)?")
			 '(1 font-lock-keyword-face)
			 '(2 font-lock-type-face nil t)))
		 ;;
		 (when scheme-lock-function-keywords
		   (list (concat "(\\("
				 (regexp-opt scheme-lock-function-keywords)
				 "\\)[ \t]+(?\\(\\sw+\\)?")
			 '(1 font-lock-keyword-face)
			 '(2 font-lock-function-name-face nil t)))
		 ;;
		 (when scheme-lock-control-structure-keywords
		   (cons (concat "(\\("
				 (regexp-opt scheme-lock-control-structure-keywords)
				 "\\)\\>")
			 1))
		 )
		(mapcar #'cdr scheme-lock-extra-highlighting-sexps)
		))
  (loop
    for buf in (buffer-list)
    if (with-current-buffer buf
	 (memq major-mode '(scheme-mode inferior-scheme-mode)))
    do (with-current-buffer buf
	 (when font-lock-mode 
	   (font-lock-mode 0)
	   (font-lock-mode 1)))))

(defcustom scheme-lock-function-keywords
  scheme-lock-function-keywords
  "Keywords that introduce functions.
The next symbol will be highlighted in `font-lock-function-name-face'."
  :type '(repeat (string :tag ""))
  :set #'scheme-lock-set-keywords%
  :group 'scheme-lock)

(defcustom scheme-lock-macro-keywords
  scheme-lock-macro-keywords
  "Keywords that introduce macros.
The next symbol will be highlighted in `font-lock-macro-name-face'."
  :type '(repeat (string :tag ""))
  :set #'scheme-lock-set-keywords%
  :group 'scheme-lock)

(defcustom scheme-lock-class-keywords
  scheme-lock-class-keywords
  "Keywords that introduce classes."
  :type '(repeat (string :tag ""))
  :set #'scheme-lock-set-keywords%
  :group 'scheme-lock)

(defcustom scheme-lock-control-structure-keywords
  scheme-lock-control-structure-keywords
  "Keywords for control structures."
  :type '(repeat (string :tag ""))
  :set #'scheme-lock-set-keywords%
  :group 'scheme-lock)

;; These need to be able to have embedded comments.
(defcustom scheme-lock-extra-highlighting-sexps
  scheme-lock-extra-highlighting-sexps
  "Font locking keywords sexps to do extra highlighting."
  :type '(repeat (cons (string :tag "Comment")
		       (sexp :tag "Sexp")))
  :set #'scheme-lock-set-keywords%
  :group 'scheme-lock)


(provide 'scheme-lock)
;;; scheme-lock.el ends here
