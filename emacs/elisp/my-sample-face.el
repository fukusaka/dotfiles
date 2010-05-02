;;; my-sample-face.el --
;; $Id: skel.el 313 2010-04-07 17:38:56Z shoichi $

;; Copyright (C) 2010 Shoichi Fukusaka

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>
;; Maintainer: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>
;; Created: 03 May 2010
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

(defvar my-sample-face-str "
_12345678901234567890123456_
_ABCDEFGHIJKLMNOPQRSTUVWXYZ_
_あいうえおかきくけこさしす_
")

(defvar my-sample-face-alist
  '(("Normal" face default)
    ("Bold" face bold)
    ("Italic" face italic)
    ("Bold Italic" face bold-italic)
    ))

(defun my-sample-face ()
  (interactive)
  (let ((buf (generate-new-buffer "*sample-face*"))
	(alist my-sample-face-alist))
    (switch-to-buffer buf)
    (dolist (e alist)
      (let (sta end)
	(setq sta (point))
	(insert (format "### %s ###\n" (car e)))
	(insert my-sample-face-str)
	(insert "\n")
	(setq end (point))
	(add-text-properties sta end (cdr e))
	(set-buffer-modified-p nil)
	))
    (goto-char (point-min))
    ))

(provide 'my-sample-face)
;;; my-sample-face.el ends here
