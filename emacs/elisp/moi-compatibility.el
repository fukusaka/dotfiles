;;; moi-compatibility.el --- 

;; Copyright (C) 2007  Free Software Foundation, Inc.

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 互換定義

;;; Code:

(if (not (fboundp 'when))
    (defmacro when (cond &rest body)
      (list 'if cond (cons 'progn body))))

(if (not (fboundp 'unless))
    (defmacro unless (cond &rest body)
      (cons 'if (cons cond (cons nil body)))))

(if (not (fboundp 'push))
    (defmacro push (newelt listname)
      (list 'setq listname
	    (list 'cons newelt listname))))
    
(if (not (fboundp 'caar))
    (defsubst caar (x)
      (car (car x))))

(if (not (fboundp 'cadr))
    (defsubst cadr (x)
      (car (cdr x))))

(if (not (fboundp 'cdar))
    (defsubst cdar (x)
      (cdr (car x))))

(if (not (fboundp 'cddr))
    (defsubst cddr (x)
      (cdr (cdr x))))


(cond
 ((string-match "^20.4" emacs-version)
  (defun char-list-to-string (lst)
    (eval (cons 'concat (mapcar 'char-to-string lst))))
  ))


(provide 'moi-compatibility)
;;; moi-compatibility.el ends here

