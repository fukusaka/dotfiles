;;; my-sample-ascii.el --
;; $Id$

;; Copyright (C) 1999 Moimoi(Shoichi Fukusaka)

;; Author: Moimoi(Shoichi Fukusaka) <fukusaka@xa2.so-net.ne.jp>
;; Maintainer: Moimoi(Shoichi Fukusaka) <fukusaka@xa2.so-net.ne.jp>
;; Created: 22 Nov 1999
;; Version: 1.0
;; Keywords: 

;; This file is part of 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:

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
      (let (stq end)
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

;;; my-sample-ascii.el ends here
