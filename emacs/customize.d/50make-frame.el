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
      (define-key global-map "\C-z50" 'moi::delete-frame-6)

      (setq moi::desktop-max-x 4)

      (defun moi::make-frame (x y)
	(let* ((fpar (frame-parameters))
	       (bw   
		(if (string-match "^20" emacs-version)
		    (cdr (assoc 'border-width fpar))
		  0))
	       (left (+ x bw (eval (cdr (assoc 'left fpar)))))
	       (top  (+ y bw (eval (cdr (assoc 'top fpar)))))
	       (frame (make-frame)))
	  (sleep-for 0.09)
	  (modify-frame-parameters frame `((top + ,top) (left + ,left) ,(cons 'font "fontset-standard")))
	  (sleep-for 0.09)
	  frame))

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
	(let ((frame (moi::make-frame 0 0)))
	  (moi::move-frame frame (+ x 1) (+ y 2))
	  frame))
	

      (defvar moi::make-frame-6-alist nil)

      (defun moi::make-frame-3 ()
	(interactive)
	(if (not moi::make-frame-6-alist)
	    (setq moi::make-frame-6-alist
		  (list
		   (moi::make-frame2 1 0)
		   ;;(moi::make-frame2 0 1)
		   (moi::make-frame2 1 1)
		   ))))

      (defun moi::make-frame-6 ()
	(interactive)
	(if (not moi::make-frame-6-alist)
	    (setq moi::make-frame-6-alist
		  (list
		   (moi::make-frame2 1 0)
		   (moi::make-frame2 -1 0)
		   ;;(moi::make-frame2 0 1)
		   (moi::make-frame2 0 -1)
		   (moi::make-frame2 1 1)
		   (moi::make-frame2 -1 1)
		   (moi::make-frame2 1 -1)
		   (moi::make-frame2 -1 -1)
		   ))))

      (defun moi::delete-frame-6 ()
	(interactive)
	(while moi::make-frame-6-alist
	  (delete-frame (car moi::make-frame-6-alist))
	  (setq moi::make-frame-6-alist (cdr moi::make-frame-6-alist))))
      ))

;;; 50make-frame.el ends here
