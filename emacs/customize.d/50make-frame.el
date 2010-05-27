;;
;; 6枚ものフレームを同時生成、同時削除。
;;


(cond
 ((and (eq window-system 'x) (executable-find "wmctrl"))
  ;; use wmctrl
  (defun moi::move-frame-wmctrl (frame x y)
    (let ((wid (frame-parameter (or frame (selected-frame)) 'outer-window-id))
	  (desk (int-to-string (+ (* moi::desktop-max-x y) x))))
      (call-process "wmctrl" nil nil nil "-i" "-r" wid "-t" desk)))

  (setq moi::desktop-max-x 4)

  ;; for Virtual Desktop (fvwm/AfterStep/sawfish etc)
  (defun moi::move-frame-large-desktop (frame x y)
    (let* ((fpar (frame-parameters frame))
	   (left (+ (* (x-display-pixel-width) x) (assoc 'left fpar)))
	   (top (+ (* (x-display-pixel-height) y) (assoc 'top fpar))))
      (set-frame-position frame left top)))

  (defun moi::move-frame (frame x y)
    (moi::move-frame-wmctrl frame x y)
    ;;(moi::move-frame-large-desktop frame x y)
    )
  )
 ((and (eq window-system 'ns) (fboundp 'set-frame-ns-workspace))
  (defun moi::move-frame (frame x y)
    (let ((max-cols (string-to-int
		     (shell-command-to-string
		      "defaults read com.apple.dock workspaces-cols")))
	  (max-rows (string-to-int
		     (shell-command-to-string
		      "defaults read com.apple.dock workspaces-rows"))))
      (setq x (cond ((< x 0) 0) ((> x max-cols) max-cols) (t x)))
      (setq y (cond ((< y 0) 0) ((> y max-rows) max-rows) (t y)))
      (set-frame-ns-workspace (selected-frame) (+ (* max-cols y) x 1))
    ))
  )
 )

(when (fboundp 'moi::move-frame)

  ;; 追加 Key Binding
  (define-key my-prefix-5-map "0" 'moi::delete-frames)
  (define-key my-prefix-5-map "1" 'moi::make-frame-3)
  (define-key my-prefix-5-map "2" 'moi::make-frame-6)
  (define-key my-prefix-5-map "5" 'moi::move-center-frame)

  ;; 初期フレームの位置(手動で合わせておく)
  (setq moi::desktop-center-x 1)
  (setq moi::desktop-center-y 1)

  (defun moi::clone-frame (&optional x y)
    (let* ((fpar (frame-parameters))
	   (left (cdr (assoc 'left fpar)))
	   (top  (cdr (assoc 'top fpar)))
	   (height (cdr (assoc 'height fpar)))
	   (width  (cdr (assoc 'width fpar)))
	   (font (cdr (assoc 'font fpar)))
	   frame)
      (if x (setq left (+ left x)))
      (if y (setq top (+ top y)))
      (setq frame (make-frame `((height . ,height) (width . ,width)
				(top . ,top) (left . ,left) (font . ,font))))
      (if (eq window-system 'ns)
	  (set-frame-height frame height))
      frame
      ))

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
	       (moi::make-frame-at -1 0)
	       (moi::make-frame-at 0 1)
	       (moi::make-frame-at -1 1)
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
