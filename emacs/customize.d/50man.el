;;; 70man.el --
;; $Id$

;; Copyright (C) 1999 Moimoi(Shoichi Fukusaka)

;; Author: Moimoi(Shoichi Fukusaka) <fukusaka@xa2.so-net.ne.jp>
;; Maintainer: Moimoi(Shoichi Fukusaka) <fukusaka@xa2.so-net.ne.jp>
;; Created: 10 Nov 1999
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

;;; Change log:

;;; Code:


(defun Man-cooked-2-backspace-code ()
  (goto-char (point-min))
  (while (re-search-forward "\\(.\\)\\(\b\b\\1\\)+" nil t)
    (replace-match "\\1")
    (if Man-fontify-manpage-flag
	(put-text-property (1- (point)) (point) 'face Man-overstrike-face)))
  (goto-char (point-min)))
  
(add-hook 'Man-cooked-hook 'Man-cooked-2-backspace-code)


(defadvice man (around man-around)
  (let* ((coding-system 'iso-2022-jp)
	 (coding-system-for-read coding-system))
    ad-do-it))

(ad-activate 'man)

;;; 70man.el ends here
