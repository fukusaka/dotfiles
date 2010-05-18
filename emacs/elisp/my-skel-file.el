;;; my-skel-file.el -- minor mode for Emacs Lisp maintainers

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
;; (autoload 'my-find-file "my-skel-file")
;; (global-set-key "\C-x\C-f" 'my-find-file)
;;

;;; Code:

(defvar my-skel-file-dir (concat my-top-conf-dir "skel/"))

(defvar my-skel-file-name-alist
  '(
    ("\\.c\\'" . "skel.c")
    ("\\.cc\\'" . "skel.cc")
    ("\\.h\\'" . "skel.h")
    ("\\.el\\'" . "skel.el")
    ("\\.pl\\'" . "skel.pl")
    ("\\.texi\\'" . "skel.texi")
    ("\\.tex\\'" . "skel.tex")
    ("\\.html\\'" . "skel.html")
    ("\\.scm\\'" . "skel.scm")
    ("\\`Makefile\\.am\\'" . "Makefile.am")
    ("\\`autogen\\.sh\\'" . "autogen.sh")
    ))

(defun my-find-file (filename)
  (interactive "Fmy-Find file: ")
  (let ((skelflag (not (or (file-exists-p filename)
			   (get-file-buffer filename))))
	buf)
    (setq buf (set-buffer (find-file-noselect filename)))
    (if skelflag (my-insert-skel-file))
    (switch-to-buffer buf)))

(defun my-insert-skel-file ()
  (let ((fname (file-name-nondirectory buffer-file-name))
	(alist my-skel-file-name-alist)
	skel-file)
    (while (and alist (not skel-file))
      (if (string-match (car (car alist)) fname)
	  (setq skel-file (cdr (car alist))))
      (setq alist (cdr alist)))
    (if (and skel-file
	     (stringp skel-file)
	     (file-readable-p (setq skel-file (concat my-skel-file-dir skel-file))))
	(progn
	  (insert-file skel-file)
	  (my-expand-skeleton-buffer))))
  nil)

(if (fboundp 'match-string) t
  (defun match-string (num &optional string)
    (if (match-beginning num)
	(if string
	    (substring string (match-beginning num) (match-end num))
	  (buffer-substring (match-beginning num) (match-end num))))))

(defun my-expand-skeleton-buffer ()
  (if (featurep 'font-lock)
      (font-lock-unfontify-region (point-min) (point-max)))
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
	;; (print vlist)
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
  (if (featurep 'font-lock)
      (font-lock-fontify-buffer))
  )

(defvar my-license-dir (concat my-top-conf-dir "license/"))

(defun my-license-string (type head &optional flag)
  (let ((license-file (concat my-license-dir type ".txt")))
    (when (file-exists-p license-file)
      (with-temp-buffer
	(insert-file-contents license-file)
	(goto-char (point-min))
	(while (re-search-forward "^" nil t)
	  (replace-match head nil nil))
	(buffer-string)))))


(defvar my-ask-license-hist '("None"))

(defun my-ask-license-string (head &optional flag)
  (let ((my-license-list (mapcar '(lambda (e) (substring e 0 -4))
				 (directory-files my-license-dir nil "\\.txt\\'"))))
    (my-license-string
     (completing-read
      "License: "
      (cons "None" my-license-list)
      nil t "None"
      'my-ask-license-hist)
     head flag)))

(defun my-c-include-once-macro (file)
  (let* ((fname (file-name-nondirectory file)) (len (length fname)) (pos 0) str)
    (while (and (< pos len)
		(string-match "[\\.-]" fname pos))
      (setq str (concat str (substring fname pos (match-beginning 0)) "_"))
      (setq pos (match-end 0)))
    (setq str (concat str (substring fname pos)))
    (upcase str)))

(provide 'my-skel-file)
