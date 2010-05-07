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

(setq my-license-list
      '(
	("GPL"
"This program is free software; you can redistribute it and/or modify"
"it under the terms of the GNU General Public License as published by"
"the Free Software Foundation; either version 2 of the License, or"
"(at your option) any later version."
""
"This program is distributed in the hope that it will be useful,"
"but WITHOUT ANY WARRANTY; without even the implied warranty of"
"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
"GNU General Public License for more details."
""
"You should have received a copy of the GNU General Public License"
"along with this program; if not, write to the Free Software"
"Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA"
         )
	("LGPL-2"
"This library is free software; you can redistribute it and/or"
"modify it under the terms of the GNU Library General Public"
"License as published by the Free Software Foundation; either"
"version 2 of the License, or (at your option) any later version."
""
"This library is distributed in the hope that it will be useful,"
"but WITHOUT ANY WARRANTY; without even the implied warranty of"
"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU"
"Library General Public License for more details."
""
"You should have received a copy of the GNU Library General Public"
"License along with this library; if not, write to the Free"
"Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA"
         )
	("LGPL-2.1"
"This library is free software; you can redistribute it and/or"
"modify it under the terms of the GNU Lesser General Public"
"License as published by the Free Software Foundation; either"
"version 2 of the License, or (at your option) any later version."
""
"This library is distributed in the hope that it will be useful,"
"but WITHOUT ANY WARRANTY; without even the implied warranty of"
"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU"
"Lesser General Public License for more details."
""
"You should have received a copy of the GNU Lesser General Public"
"License along with this library; if not, write to the Free Software"
"Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA"
	 )
	("BSD"
"Redistribution and use in source and binary forms, with or without"
"modification, are permitted provided that the following conditions"
"are met:"
"1. Redistributions of source code must retain the above copyright"
"   notice, this list of conditions and the following disclaimer."
"2. Redistributions in binary form must reproduce the above copyright"
"   notice, this list of conditions and the following disclaimer in the"
"   documentation and/or other materials provided with the distribution."
""
"THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND"
"ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE"
"IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE"
"ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE"
"FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL"
"DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS"
"OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)"
"HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT"
"LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY"
"OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF"
"SUCH DAMAGE."
         )
	))

(defun my-license-string (type head &optional flag)
  (let ((l-list (assoc (if (symbolp type) (symbol-name type) type)
		       my-license-list))
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

(defvar my-ask-license-hist '("GPL" "LGPL-2" "LGPL-2.1" "BSD"))

(defun my-ask-license-string (head &optional flag)
  (my-license-string
   (completing-read "License: " my-license-list nil t "BSD"
		    'my-ask-license-hist)
   head flag))

(defun my-c-include-once-macro (file)
  (let* ((fname (file-name-nondirectory file)) (len (length fname)) (pos 0) str)
    (while (and (< pos len)
		(string-match "[\\.-]" fname pos))
      (setq str (concat str (substring fname pos (match-beginning 0)) "_"))
      (setq pos (match-end 0)))
    (setq str (concat str (substring fname pos)))
    (upcase str)))

(provide 'my-skel-file)
