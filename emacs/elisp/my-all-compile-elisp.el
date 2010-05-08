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
;; elisp 以下を全部バイトコンパイルしてcompiled-emacsXXにコピー
(defun my-all-compile-elisp ()
  (interactive)

  (let* ((default-directory my-elisp-dir)
	 (subdirs (sort (remove-if-not 'file-directory-p
				       (directory-files "." nil "^[^.]"))
			'string<)))

    ;; elisp 直下をコンパイル
    (my-compile-directory ".")

    ;; サブディレクトリ毎に XX-install.elの有無のチェックし、
    ;; あればインストーラーとして起動し、無ければ直下の .el 単にコンパイル
    (dolist (subdir subdirs)
      (message "[my-all-compile-elisp] check %s" subdir)
      (cond
       ((file-exists-p (concat subdir "-installer.el"))
	(condition-case err
	    (load (concat subdir "-installer.el"))
	  (error (message "[my-subdirs-compile] can't install %s" subdir))))
       (t (my-compile-directory subdir))))
    )
  (message "[my-all-compile-elisp] finish")
  )

(defun my-all-compile-elisp-addons ()
  (interactive)

  (let* ((default-directory my-elisp-addons-dir)
	 (installers (sort (directory-files "." nil "-installer\\.sh$")
			   'string<))
	 addon)
    (dolist (installer installers)
      (setq addon (substring installer  0 (- (length "-installer.sh"))))
      (message "[my-all-compile-elisp-addons] check %s" addon)
      (let ((process-environment process-environment))
	(setenv "EMACS" (concat invocation-directory invocation-name))
	(setenv "ADDON" addon)
	(setenv "ELCDIR" (concat my-compiled-elisp-dir addon))
	(message "%s" (shell-command-to-string "pwd"))
	(message "%s" (shell-command-to-string (concat my-elisp-addons-dir installer))))
      )
    )
  (message "[my-all-compile-elisp-addons] finish")
  )

(provide 'my-all-compile-elisp)

;;; my-all-compile-elisp.el ends here
