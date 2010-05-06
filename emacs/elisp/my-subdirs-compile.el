;;; my-subdirs-compile.el --
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
;; 3. Neither the name of the University nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.

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

;;; Change log:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp 以下を全部バイトコンパイルしてcompiled-emacsXXにコピー
(defun my-subdirs-compile ()
  (interactive)

  (let* ((default-directory my-elisp-dir)
	 (subdirs (sort (reduce '(lambda (lst e) (if (file-directory-p e)
						     (cons e lst) lst))
				(directory-files "." nil "^[^.]")
				:initial-value nil)
			'string<)))

    ;; elisp 直下をコンパイル
    (my-compile-directory ".")

    ;; サブディレクトリ毎に XX-install.elの有無のチェックし、
    ;; あればインストーラーとして起動し、無ければ直下の .el 単にコンパイル
    (dolist (subdir subdirs)
      (message "[my-subdirs-compile] check %s" subdir)
      (if (file-exists-p (concat subdir "-installer.el"))
	  (condition-case err
	      (load (concat subdir "-installer.el"))
	    (error (message "[my-subdirs-compile] can't install %s" subdir)))
	(my-compile-directory subdir)))
    )
  )

(provide 'my-subdirs-compile)

;;; my-subdirs-compile.el ends here
