;;; moi-skel-make.el -- minor mode for Emacs Lisp maintainers

;; Copyright (C) 1999 Moimoi(Shoichi Fukusaka)

;; Author: Moimoi(Shoichi Fukusaka) <fukusaka@xa2.so-net.ne.jp>
;; Maintainer: Moimoi(Shoichi Fukusaka) <fukusaka@xa2.so-net.ne.jp>
;; Created: 23 Jun 1999
;; Version: 1.0
;; Keywords: 

;; This file is part of ...

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; ファイルを開く時
;;     新規のファイルであればスケルトンファイルを読み込む。
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

(defun moi::insert-skel-file-real (file-name dir-name skel-file)
  (insert-file skel-file)
  (font-lock-unfontify-buffer)
  (if (re-search-forward "^@@@@$" nil t)
      (let ((h (match-beginning 0))
	    (vlist nil))
	(while (re-search-forward "^\\([a-zA-Z_][a-zA-Z_0]*\\):" nil t)
	  (let ((v (match-string 1))
		(s (match-end 0)) e r)
	    (while (re-search-forward "^[ \t].*$" nil t)
	      (setq e (match-end 0)))
	    (if (not (integer-or-marker-p e)) (setq e (point-max)))
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

(defun moi::insert-skel-file-0 ()
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

(defun moi::insert-skel-file ()
  (let ((fname buffer-file-name))
    (if (and fname (not (file-exists-p fname)))
	(moi::insert-skel-file-0))))

(add-hook 'find-file-hooks 'moi::insert-skel-file)

(provide 'moi-skel-file)