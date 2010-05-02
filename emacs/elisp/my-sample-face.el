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

(defface my-sample-face-6 '((t :height 60 :inherit default)) "")
(defface my-sample-face-7 '((t :height 70 :inherit default)) "")
(defface my-sample-face-8 '((t :height 80 :inherit default)) "")
(defface my-sample-face-9 '((t :height 90 :inherit default)) "")
(defface my-sample-face-10 '((t :height 100 :inherit default)) "")
(defface my-sample-face-11 '((t :height 110 :inherit default)) "")
(defface my-sample-face-12 '((t :height 120 :inherit default)) "")
(defface my-sample-face-13 '((t :height 130 :inherit default)) "")
(defface my-sample-face-14 '((t :height 140 :inherit default)) "")
(defface my-sample-face-15 '((t :height 150 :inherit default)) "")
(defface my-sample-face-16 '((t :height 160 :inherit default)) "")
(defface my-sample-face-17 '((t :height 170 :inherit default)) "")
(defface my-sample-face-18 '((t :height 180 :inherit default)) "")
(defface my-sample-face-19 '((t :height 190 :inherit default)) "")
(defface my-sample-face-20 '((t :height 200 :inherit default)) "")


(defvar my-sample-face-alist
  '(("height 6" face my-sample-face-6)
    ("height 7" face my-sample-face-7)
    ("height 8" face my-sample-face-8)
    ("height 9" face my-sample-face-9)
    ("height 10" face my-sample-face-10)
    ("height 11" face my-sample-face-11)
    ("height 12" face my-sample-face-12)
    ("height 13" face my-sample-face-13)
    ("height 14" face my-sample-face-14)
    ("height 15" face my-sample-face-15)
    ("height 16" face my-sample-face-16)
    ("height 17" face my-sample-face-17)
    ("height 18" face my-sample-face-18)
    ("height 19" face my-sample-face-19)
    ("height 20" face my-sample-face-20)
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
