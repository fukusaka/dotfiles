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

(defvar moi::skel-file-dir "~/common/conf/skel/")

(defvar moi::skel-file-name-alist
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

(if (fboundp 'match-string) t
  (defun match-string (num &optional string)
    (if (match-beginning num)
	(if string
	    (substring string (match-beginning num) (match-end num))
	  (buffer-substring (match-beginning num) (match-end num))))))

(provide 'moi-skel-file)
(defun moi::find-file (filename)
  (interactive "FMoi::Find file: ")
  (let* ((skelflag (and (not (file-exists-p filename))
			(null (get-file-buffer filename))))
	 (buf (set-buffer (find-file-noselect filename))))
    (if skelflag (moi::insert-skel-file))
    (switch-to-buffer buf)))

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

(setq moi::license-list
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
"3. Neither the name of the University nor the names of its contributors"
"   may be used to endorse or promote products derived from this software"
"   without specific prior written permission."
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

(defun moi::license-string (type head &optional flag)
  (let ((l-list (assoc (if (symbolp type) (symbol-name type) type)
		       moi::license-list))
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

(defvar moi::ask-license-hist '("GPL" "LGPL-2" "LGPL-2.1" "BSD"))

(defun moi::ask-license-string (head &optional flag)
  (moi::license-string
   (completing-read "License: " moi::license-list nil t "GPL"
		    'moi::ask-license-hist)
   head flag))

