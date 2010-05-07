;;; my-sample-ascii.el --
;; $Id$

;; Copyright (C) 1999 Shoichi Fukusaka

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>
;; Created: 22 Nov 1999
;; Version: 1.01
;; Keywords: face sample

;;; Commentary:
;;
;; (autoload 'my-sample-ascii "my-sample-ascii" "" t)

;;; Change log:

;;; Code:

(defvar my-sample-ascii-str
"  ! \" # $ % & ' ( ) * + , - . /
0 1 2 3 4 5 6 7 8 9 : ; < = > ?
@ A B C D E F G H I J K L M N O
P Q R S T U V W X Y Z [ \ ] ^ _
` a b c d e f g h i j k l m n o
p q r s t u v w x y z { | } ~
あいうえお  かきくけこ  さしすせ
朝昼夕晩夜  春秋夏冬暮  右左上下
")

(defvar my-sample-ascii-face-alist
  '(("Normal" face default)
    ("Bold" face bold)
    ("Italic" face italic)
    ("Bold Italic" face bold-italic)
    ))

(defun my-sample-ascii ()
  (interactive)
  (let ((buf (generate-new-buffer "*sample-ascii*"))
	(alist my-sample-ascii-face-alist))
    (switch-to-buffer buf)
    (while alist
      (let (sta end)
	(setq sta (point))
	(insert (format "### %s ###\n" (caar alist)))
	(insert my-sample-ascii-str)
	(insert "\n")
	(setq end (point))
	(add-text-properties sta end (cdar alist))
	)
      (setq alist (cdr alist)))
    (goto-char (point-min))
    ))

(provide 'my-sample-ascii)
;;; my-sample-ascii.el ends here
