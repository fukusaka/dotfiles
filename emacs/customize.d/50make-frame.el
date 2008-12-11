;;; 50make-frame.el --- 

;; Copyright (C) 2007  Free Software Foundation, Inc.

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:


;;
;; 6枚ものフレームを同時生成、同時削除。
;;
(if (featurep 'x-toolkit)
    (progn
      (define-key global-map "\C-z51" 'moi::make-frame-3)
      (define-key global-map "\C-z52" 'moi::make-frame-6)
      (define-key global-map "\C-z50" 'moi::delete-frames)

      (setq moi::desktop-max-x 4)
      (setq moi::desktop-center-x 1)
      (setq moi::desktop-center-y 2)

      (defun moi::make-frame (&optional x y)
	(let* ((fpar (frame-parameters))
	       (left (cdr (assoc 'left fpar)))
	       (top  (cdr (assoc 'top fpar)))
	       (height (cdr (assoc 'height fpar)))
	       (width  (cdr (assoc 'width fpar)))
	       (font (cdr (assoc 'font fpar))))
	  (if (string-match "^20" emacs-version)
	      (let ((bw (cdr (assoc 'border-width fpar))))
		(setq left (+ left bw))
		(setq top (+ top bw))))
	  (if x (setq left (+ left x)))
	  (if y (setq top (+ top y)))
	  (let ((default-frame-alist
		  (append `((height . ,height) (width . ,width)
			    (top . ,top) (left . ,left) (font . ,font))
			  default-frame-alist)))
	    (make-frame))))

      (defun moi::move-frame (frame x y)
	(let ((wid (frame-parameter (or frame (selected-frame)) 'outer-window-id))
	      (desk (int-to-string (+ (* moi::desktop-max-x y) x))))
	(call-process
	 "wmctrl" nil nil nil "-i"
	 "-r" wid "-t" desk)))

      ;; for Virtual Desktop (sawfish etc)
      ;;(defun moi::make-frame2 (x y)
      ;;	(let* ((dh (x-display-pixel-height))
      ;;	       (dw (x-display-pixel-width))
      ;;	       (left (* dw x))
      ;;	       (top (* dh y)))
      ;;   (moi::make-frame left top)))

      (defun moi::make-frame2 (x y)
	(let ((frame (moi::make-frame)))
	  (moi::move-frame frame
			   (+ x moi::desktop-center-x)
			   (+ y moi::desktop-center-y))
	  frame))

      (defvar moi::make-frame-list nil)

      (defun moi::make-frame-3 ()
	(interactive)
	(if (not moi::make-frame-list)
	    (setq moi::make-frame-list
		  (list
		   (moi::make-frame2 1 0)
		   ;;(moi::make-frame2 0 1)
		   (moi::make-frame2 1 1)
		   ))))

      (defun moi::make-frame-6 ()
	(interactive)
	(if (not moi::make-frame-list)
	    (setq moi::make-frame-list
		  (list
		   ;;(moi::make-frame2 1 0)
		   (moi::make-frame2 -1 0)
		   (moi::make-frame2 0 1)
		   (moi::make-frame2 0 -1)
		   (moi::make-frame2 1 1)
		   (moi::make-frame2 -1 1)
		   ;;(moi::make-frame2 1 -1)
		   (moi::make-frame2 -1 -1)
		   ))))

      (defun moi::delete-frames ()
	(interactive)
	(while moi::make-frame-list
	  (delete-frame (car moi::make-frame-list))
	  (setq moi::make-frame-list (cdr moi::make-frame-list))))
      ))

;;; 50make-frame.el ends here
