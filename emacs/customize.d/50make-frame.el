;;
;; 6枚ものフレームを同時生成、同時削除。
;;

(when (and (eq window-system 'x) (executable-find "wmctrl"))

  ;; 追加 Key Binding
  (define-key my-prefix-5-map "0" 'moi::delete-frames)
  (define-key my-prefix-5-map "1" 'moi::make-frame-3)
  (define-key my-prefix-5-map "2" 'moi::make-frame-6)
  (define-key my-prefix-5-map "5" 'moi::move-center-frame)

  (setq moi::desktop-max-x 4)
  (setq moi::desktop-center-x 1)
  (setq moi::desktop-center-y 2)

  (defun moi::clone-frame (&optional x y)
    (let* ((fpar (frame-parameters))
	   (left (cdr (assoc 'left fpar)))
	   (top  (cdr (assoc 'top fpar)))
	   (height (cdr (assoc 'height fpar)))
	   (width  (cdr (assoc 'width fpar)))
	   (font (cdr (assoc 'font fpar))))
      (if x (setq left (+ left x)))
      (if y (setq top (+ top y)))
      (make-frame `((height . ,height) (width . ,width)
		    (top . ,top) (left . ,left) (font . ,font)))))

  ;; use wmctrl
  (defun moi::move-frame-wmctrl (frame x y)
    (let ((wid (frame-parameter (or frame (selected-frame)) 'outer-window-id))
	  (desk (int-to-string (+ (* moi::desktop-max-x y) x))))
      (call-process "wmctrl" nil nil nil "-i" "-r" wid "-t" desk)))

  ;; for Virtual Desktop (sawfish etc)
  (defun moi::move-frame-large-desktop (frame x y)
    (let* ((fpar (frame-parameters frame))
	   (left (+ (* (x-display-pixel-width) x) (assoc 'left fpar)))
	   (top (+ (* (x-display-pixel-height) y) (assoc 'top fpar))))
      (set-frame-position frame left top)))

  (defun moi::move-frame (frame x y)
    (moi::move-frame-wmctrl frame x y)
    ;;(moi::move-frame-large-desktop frame x y)
    )

  (defun moi::make-frame-at (x y)
    (let ((frame (moi::clone-frame)))
      (moi::move-frame frame
		       (+ x moi::desktop-center-x)
		       (+ y moi::desktop-center-y))
      frame))

  (defvar moi::make-frame-list nil)

  (defun moi::move-center-frame ()
    (interactive)
    (moi::move-frame nil moi::desktop-center-x moi::desktop-center-y))

  (defun moi::make-frame-3 ()
    (interactive)
    (if (not moi::make-frame-list)
	(setq moi::make-frame-list
	      (list
	       (moi::make-frame-at 1 0)
	       (moi::make-frame-at 0 1)
	       (moi::make-frame-at 1 1)
	       ))))

  (defun moi::make-frame-6 ()
    (interactive)
    (if (not moi::make-frame-list)
	(setq moi::make-frame-list
	      (list
	       ;;(moi::make-frame-at 1 0)
	       (moi::make-frame-at -1 0)
	       (moi::make-frame-at 0 1)
	       (moi::make-frame-at 0 -1)
	       (moi::make-frame-at 1 1)
	       (moi::make-frame-at -1 1)
	       ;;(moi::make-frame-at 1 -1)
	       (moi::make-frame-at -1 -1)
	       ))))

  (defun moi::delete-frames ()
    (interactive)
    (while moi::make-frame-list
      (delete-frame (car moi::make-frame-list))
      (setq moi::make-frame-list (cdr moi::make-frame-list))))

  )
