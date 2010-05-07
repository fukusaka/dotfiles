;;; my-sample-ascii.el --
;; $Id$

;; Copyright (C) 1999 Shoichi Fukusaka

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>
;; Created: 22 Nov 1999
;; Version: 1.02
;; Keywords: face sample

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS''
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
;; OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;;; Commentary:
;;
;; (autoload 'my-sample-ascii "my-sample-ascii" "" t)
;; (autoload 'my-sample-face-size "my-sample-ascii" "" t)

;;; Code:

(defun my-sample-output (bufname alist msg)
  (let ((buf (generate-new-buffer bufname)))
    (switch-to-buffer buf)
    (dolist (e alist)
      (let (sta end)
	(setq sta (point))
	(insert (format "### %s ###\n" (car e)))
	(insert msg)
	(insert "\n")
	(setq end (point))
	(add-text-properties sta end `(face ,(cadr e)))
	))
    (goto-char (point-min))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  '(("Normal" default)
    ("Bold" bold)
    ("Italic" italic)
    ("Bold Italic" bold-italic)
    ))

(defun my-sample-ascii ()
  (interactive)
  (my-sample-output
   "*sample-ascii*"
   my-sample-ascii-face-alist
   my-sample-ascii-str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-sample-face-str "
____________________________
_-!\"#$%&'()*+ -./:;<=>?@[\\]_
_^_`{|}~!\"#$%&'()*+ -./:;<=_
_12345678901234567890123456_
_ABCDEFGHIJKLMNOPQRSTUVWXYZ_
_あいうえおかきくけこさしす_
____________________________
")

(defvar my-sample-face-size-list
  '(6 7 8 9 10 10.5 11 12 13 14 15 16 17 18 19 20))

(dolist (e my-sample-face-size-list)
  (custom-declare-face
   (intern (format "my-sample-face-%g" e))
   `((t :height ,(floor (* e 10)) :inherit default)) ""))

(defun my-sample-face-size ()
  (interactive)
  (my-sample-output
   "*sample-face*"
   (mapcar
    '(lambda (e) (list (format "height %g pt" e)
		       (intern (format "my-sample-face-%g" e))))
    my-sample-face-size-list)
   my-sample-face-str))

(provide 'my-sample-ascii)
;;; my-sample-ascii.el ends here
