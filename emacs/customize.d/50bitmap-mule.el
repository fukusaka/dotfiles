;;; 50bitmap-mule.el --
;; $Id$

;; Copyright (C) 2000 Shoichi Fukusaka

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>
;; Maintainer: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>
;; Created: 18 Mar 2000
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

(if window-system
    (let* ((fpar (frame-parameters))
	   (bc (let ((attr (assoc 'background-color fpar)))
		 (if attr (cdr attr) nil)))
	   (fc (let ((attr (assoc 'foreground-color fpar)))
		 (if attr (cdr attr) nil)))
	   )
      (if (and bc fc)
	  (defface smiley-manga-face
	    `((t (:background ,bc :foreground ,fc)))
	    "Face used for displaying smiley manga faces."
	    :group 'faces)
	)))

;;; 50bitmap-mule.el ends here
