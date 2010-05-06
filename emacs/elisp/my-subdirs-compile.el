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
;; 指定のディレクトリ以下のサブディレクトリを列挙 (遅い、、、)
(defun my-list-subdirs (directory &optional full)
  (let ((dir (expand-file-name directory))
        (lst (list directory))
        subdirs)
    (while lst
      (dolist (file (directory-files (car lst) t "^[^\\.]"))
        (when (file-directory-p file)
          (add-to-list 'lst file t)
          (add-to-list 'subdirs
                       (if full file
                         (substring file (length directory))))
          ))
      (setq lst (cdr lst)))
    (sort subdirs 'string<)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp 以下を全部バイトコンパイルしてcompiled-emacsXXにコピー
(defun my-subdirs-compile ()
  (interactive)
  (let ((default-directory my-elisp-dir))
    (dolist (dir (cons "." (my-list-subdirs my-elisp-dir nil)))
      (let ((files (directory-files dir nil "\\.el$" t)))
        (dolist (file files)
	  (message "[my-elisp-all-compile] check %s/%s" dir file)
          (my-compile-file
           (concat (file-name-as-directory dir) file)
           my-compiled-elisp-dir))))))


(provide 'my-subdirs-compile)

;;; my-subdirs-compile.el ends here
