;;; my-all-compile-elisp.el --
;; $Id: skel.el 313 2010-04-07 17:38:56Z shoichi $

;; Copyright (C) 2010 Shoichi Fukusaka

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>
;; Maintainer: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>
;; Created: 06 May 2010
;; Version: 1.0
;; Keywords:

;; This file is part of

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp 以下を全部バイトコンパイルして elisp-emacsXX にコピー
(defun my-all-compile-elisp ()
  (interactive)

  (let* ((default-directory my-elisp-dir)
	 (subdirs
	  (sort
	   (remove-if-not 'file-directory-p
			  (directory-files "." nil "^[^.]"))
	   'string<)))

    ;; elisp 直下処理
    (setq load-path (remove my-elisp-dir load-path))
    (add-to-list 'load-path my-compiled-elisp-dir t)
    (add-to-list 'load-path my-elisp-dir t)
    (my-compile-directory ".")

    (dolist (subdir subdirs)
      (let ((subdir-el (expand-file-name (concat my-elisp-dir subdir)))
	    (subdir-elc (expand-file-name (concat my-compiled-elisp-dir subdir))))
	(message "[my-all-compile-elisp] check %s" subdir)
	(add-to-list 'load-path subdir-elc t)
	(add-to-list 'load-path subdir-el t)
	(my-compile-directory subdir)
	(setq load-path (remove subdir-el load-path))
	))
    )

  (message "[my-all-compile-elisp] finish"))

(defun my-compile-directory (directory)
  (let ((files (directory-files directory nil "^[^\\.].*\\.el$" t)))
    (dolist (file files)
      ;;(message "[my-compile-directory] check %s/%s" directory file)
      (my-compile-file
       (concat (file-name-as-directory directory) file)
       my-compiled-elisp-dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-perform-installer-el (addon installer)
  (message "[my-perform-installer-el] %s" addon)
  (let ((installer (concat addon "-installer.el")))
    (if (file-exists-p (concat subdir "-installer.el"))
	(condition-case err
	    (load installer)
	  (error (message "[my-perform-installer-el] can't install %s" addon)))
      (my-compile-directory (concat my-elisp-addons-dir addon)))
    )
  )

(defun my-perform-installer-sh (addon installer)
  (message "[my-perform-installer-sh] %s" addon)
  (let ((process-environment process-environment))
    (setenv "EMACS" (concat invocation-directory invocation-name))
    (setenv "ADDON" addon)
    (setenv "ELCDIR" (concat my-compiled-elisp-dir addon))
    ;;(message "%s" (shell-command-to-string "pwd"))
    (message "%s" (shell-command-to-string (concat my-elisp-addons-dir installer))))
  )

(defun my-all-compile-elisp-addons ()
  (interactive)

  (let* ((default-directory my-elisp-addons-dir)
	 (re "^\\(.*\\)-installer\\.\\(sh\\|el\\)$")
	 (installers (sort (directory-files "." nil re) 'string<))
	 (addons (mapcar '(lambda (x)
			    (string-match re x) (list (match-string 2 x)
						      (match-string 1 x)
						      x))
			 installers)))

    (dolist (addon addons)
      (let ((inst (cond ((string= "sh" (car addon)) 'my-perform-installer-sh)
			((string= "el" (car addon)) 'my-perform-installer-el)
			(t nil))))
	(if inst (apply inst (cdr addon)))))
    )

  (message "[my-all-compile-elisp-addons] finish")
  )

(provide 'my-all-compile-elisp)

;;; my-all-compile-elisp.el ends here
