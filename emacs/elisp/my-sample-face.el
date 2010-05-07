;;; my-sample-face.el --
;; $Id: skel.el 313 2010-04-07 17:38:56Z shoichi $

;; Copyright (C) 2010 Shoichi Fukusaka

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>
;; Created: 03 May 2010
;; Version: 1.0
;; Keywords:

;;; Commentary:
;;
;; (autoload 'my-sample-face "my-sample-face" "" t)

;;; Change log:

;;; Code:

(defvar my-sample-face-str "
____________________________
_-!\"#$%&'()*+ -./:;<=>?@[\\]_
_^_`{|}~!\"#$%&'()*+ -./:;<=_
_12345678901234567890123456_
_ABCDEFGHIJKLMNOPQRSTUVWXYZ_
_あいうえおかきくけこさしす_
____________________________
")



(defface my-sample-face-6 '((t :height 60 :inherit default)) "")
(defface my-sample-face-7 '((t :height 70 :inherit default)) "")
(defface my-sample-face-8 '((t :height 80 :inherit default)) "")
(defface my-sample-face-9 '((t :height 90 :inherit default)) "")
(defface my-sample-face-10 '((t :height 100 :inherit default)) "")
(defface my-sample-face-10.5 '((t :height 105 :inherit default)) "")
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
  '((6 face my-sample-face-6)
    (7 face my-sample-face-7)
    (8 face my-sample-face-8)
    (9 face my-sample-face-9)
    (10 face my-sample-face-10)
    (10.5 face my-sample-face-10.5)
    (11 face my-sample-face-11)
    (12 face my-sample-face-12)
    (13 face my-sample-face-13)
    (14 face my-sample-face-14)
    (15 face my-sample-face-15)
    (16 face my-sample-face-16)
    (17 face my-sample-face-17)
    (18 face my-sample-face-18)
    (19 face my-sample-face-19)
    (20 face my-sample-face-20)
    ))

(defun my-sample-face ()
  (interactive)
  (let ((buf (generate-new-buffer "*sample-face*"))
	(alist my-sample-face-alist))
    (switch-to-buffer buf)
    (set-buffer-file-coding-system 'utf-8)
    (dolist (e alist)
      (let (sta end)
	(setq sta (point))
	(insert (format "### height %g pt ###\n" (car e)))
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
