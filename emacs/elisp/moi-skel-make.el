;;; moi-skel-make.el -- minor mode for Emacs Lisp maintainers

;; Copyright (C) 1999 Moimoi(Shoichi Fukusaka)

;; Author: Moimoi(Shoichi Fukusaka) <fukusaka@xa2.so-net.ne.jp>
;; Maintainer: Moimoi(Shoichi Fukusaka) <fukusaka@xa2.so-net.ne.jp>
;; Created: 23 Jun 1999
;; Version: 1.0
;; Keywords: 

;; This file is part of ...

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:
;;
;; (autoload 'moi::find-file "moi-skel-make")
;; (global-set-key "\C-x\C-f" 'moi::find-file)
;;

;;; Code:

(defvar moi::skel-file-dir "~/lib/conf/skel/")

(defvar moi::skel-file-name-alist
  '(
    ("\\.c\\'" . "skel.c")
    ("\\.h\\'" . "skel.h")
    ("\\.el\\'" . "skel.el")
    ("\\.pl\\'" . "skel.pl")
    ("\\.texi\\'" . "skel.texi")
    ("\\.tex\\'" . "skel.tex")
    ("\\.html\\'" . "skel.html")
    ("\\.scm\\'" . "skel.scm")
    ("Makefile.am" . "Makefile.am")
    ))

(provide 'moi-skel-file)
(defun moi::find-file (filename)
  (interactive "FMoi::Find file: ")
  (switch-to-buffer (find-file-noselect filename))
  (let ((fname buffer-file-name))
    (if (and fname (not (file-exists-p fname)))
	(moi::insert-skel-file))))

(defun moi::insert-skel-file ()
  (let* ((pname buffer-file-name)
	 (fname (file-name-nondirectory pname))
	 (dname (file-name-directory pname))
	 (alist moi::skel-file-name-alist)
	 (skel-file nil))
    (while (and (not skel-file) alist)
      (if (string-match (car (car alist)) fname)
	  (setq skel-file (cdr (car alist))))
      (setq alist (cdr alist)))
    (if skel-file
	(if (not (stringp skel-file))
	    (eval skel-file)
	  (let ((skel-file-real (concat moi::skel-file-dir skel-file)))
	    (if (file-readable-p skel-file-real)
		(moi::insert-skel-file-real fname dname skel-file-real)))))
	    
    ))

(defun moi::insert-skel-file-real (file-name dir-name skel-file)
  (insert-file skel-file)
  (font-lock-unfontify-buffer)
  (if (re-search-forward "^@@@@$" nil t)
      (let ((h (match-beginning 0))
	    (vlist nil))
	(while (re-search-forward "^\\([a-zA-Z_][a-zA-Z_0]*\\):" nil t)
	  (let ((v (match-string 1))
		(s (match-end 0)) e r)
	    (end-of-line)
	    (while (looking-at "\n[ \t]")
	      (forward-line) (end-of-line))
	    (setq e (point))
	    (let ((rs (buffer-substring s e)))
	      (setq r (eval (car (read-from-string rs))))
	      (setq vlist (cons (cons v r) vlist)))      
	    ))
	(print vlist)
	(delete-region
	 h (if (re-search-forward "^@@@@$" nil t)
	       (progn
		 (goto-char (match-end 0))
		 (forward-line 1)
		 (point-marker))
	     (point-max)))
	(goto-char (point-min))
	(while (re-search-forward "@@\\([a-zA-Z_][a-zA-Z_0]*\\)@@" nil t)
	  (let* ((var (match-string 1))
		 (vl (assoc var vlist)))
	    (if vl
		(progn
		  (delete-region (match-beginning 0)
				 (match-end 0))
		  (insert (concat (cdr vl)))))))
	(goto-char (point-min))
	))
  (font-lock-fontify-buffer)
  )

(setq moi::license-list
      '(
	(GPL
"This program is free software; you can redistribute it and/or modify"
"it under the terms of the GNU General Public License as published by"
"the Free Software Foundation; either version 2, or (at your option)"
"any later version."
""
"This program is distributed in the hope that it will be useful,"
"but WITHOUT ANY WARRANTY; without even the implied warranty of"
"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
"GNU General Public License for more details."
""
"You should have received a copy of the GNU General Public License"
"along with this program; if not, write to the Free Software"
"Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA"
"02111-1307, USA."
	 )
	(LGPL
"This library is free software; you can redistribute it and/or"
"modify it under the terms of the GNU Library General Public"
"License as published by the Free Software Foundation; either"
"version 2 of the License, or (at your option) any later version."
""
"This library is distributed in the hope that it will be useful,"
"but WITHOUT ANY WARRANTY; without even the implied warranty of"
"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the "
"GNU Library General Public License for more details."
""
"You should have received a copy of the GNU Library General Public"
"License along with this library; if not, write to the"
"Free Software Foundation, Inc., 59 Temple Place - Suite 330,"
"Boston, MA 02111-1307, USA."
	 )
	(BSD
	 )
	))

(defun moi::license-string (type head &optional flag)
  (let ((l-list (assoc type moi::license-list))
	str)
    (if l-list
	(progn
	  (setq l-list (cdr l-list))
	  (while l-list
	    (let* ((a (car l-list))
		   (b (if (not (and flag (string= "" a)))
			  (concat head a ))))
	      (setq str (concat str (if str "\n") b)))
	    (setq l-list (cdr l-list)))
	  str))
    ))
