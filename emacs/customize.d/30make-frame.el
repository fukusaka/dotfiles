;;
;; フレームを位置を固定化
;;

(when window-system
  ;; 追加 Key Binding
  (define-key my-prefix-5-map "5" 'my-move-center-frame)

  (defun my-move-center-frame ()
    (interactive)
    (modify-frame-parameters nil default-frame-alist)
    (modify-frame-parameters nil initial-frame-alist)
    (when (fboundp 'my-move-frame)
      (my-move-frame nil my-desktop-center-x my-desktop-center-y t))
    )
  )

;;
;; 複数フレームを別のワークスペースに同時生成、同時削除。
;;

;; ワークスペース間の移動方法
(cond
 ;; For X11
 ((eq window-system 'x)

  ;; for Virtual Desktop (fvwm/AfterStep/sawfish etc)
  (defun my-move-frame-large-desktop (frame x y &optional abs)
    (let* ((fpar (frame-parameters frame))
           (left (+ (* (x-display-pixel-width) x) (assoc 'left fpar)))
           (top (+ (* (x-display-pixel-height) y) (assoc 'top fpar))))
      (set-frame-position frame left top))
    frame)

  ;; use wmctrl
  (when (executable-find "wmctrl")
    (defun my-move-frame-wmctrl (frame x y &optional abs)
      (let* ((wid (string-to-number (frame-parameter (or frame (selected-frame)) 'outer-window-id)))
             (max-desk (x-window-property "_NET_NUMBER_OF_DESKTOPS" nil "CARDINAL" 0 nil t))
             (max-rows
              ;; _NET_WM_ORIENTATION_HORZ/_NET_WM_TOPLEFT 決め打ち
              (elt (x-window-property "_NET_DESKTOP_LAYOUT" nil "CARDINAL" 0 nil t) 2))
             (max-cols (1+ (/ (1- max-desk) max-rows)))
             desk)
        ;; 現在位置からの相対
        (unless abs
          (let* ((now-desk (x-window-property "_NET_WM_DESKTOP" nil "CARDINAL" wid nil t))
                 (now-x (/ now-desk max-rows))
                 (now-y (- now-desk (* now-x max-rows))))
            (setq x (+ now-x x))
            (setq y (+ now-y y))))
        (setq x (cond ((< x 0) 0) ((> x max-cols) max-cols) (t x)))
        (setq y (cond ((< y 0) 0) ((> y max-rows) max-rows) (t y)))
        (setq desk (+ (* max-rows y) x))
        (call-process "wmctrl" nil nil nil "-i" "-r" (int-to-string wid) "-t" (int-to-string desk))
        )
      frame)
    )

  (if (fboundp 'my-move-frame-wmctrl)
      (defalias 'my-move-frame 'my-move-frame-wmctrl))

  ;;(defalias 'my-move-frame 'my-move-frame-large-desktop)
  )

 ;; For Cocoa Emacs / CGS対応改造版
 ((and (eq window-system 'ns) (fboundp 'set-frame-ns-workspace))
  (defun my-move-frame (frame x y &optional abs)
    (let ((max-cols (string-to-number
                     (shell-command-to-string
                      "defaults read com.apple.dock workspaces-cols")))
          (max-rows (string-to-number
                     (shell-command-to-string
                      "defaults read com.apple.dock workspaces-rows"))))
      ;; 現在位置からの相対
      (unless abs
        (let* ((now-desk (1- (string-to-int (frame-ns-workspace))))
               (now-x (/ now-desk max-rows))
               (now-y (- now-desk (* now-x max-rows))))
          (setq x (+ now-x x))
          (setq y (+ now-y y))))
      (setq x (cond ((< x 0) 0) ((> x max-cols) max-cols) (t x)))
      (setq y (cond ((< y 0) 0) ((> y max-rows) max-rows) (t y)))
      (set-frame-ns-workspace (selected-frame) (+ (* max-cols y) x 1))
      )
    frame)
  )
 )

(when (fboundp 'my-move-frame)
  ;; 追加 Key Binding
  (define-key my-prefix-5-map "0" 'my-delete-frames)
  (define-key my-prefix-5-map "1" 'my-make-frame-1)
  (define-key my-prefix-5-map "2" 'my-make-frame-2)
  (define-key my-prefix-5-map "3" 'my-make-frame-3)
  (define-key my-prefix-5-map "4" 'my-make-frame-4)

  (defvar my-make-frame-list nil)

  (defun my-clone-frame (&optional x y)
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
      (push frame my-make-frame-list)
      frame
      ))

  (defun my-delete-frames ()
    (interactive)
    (while my-make-frame-list
      (delete-frame (car my-make-frame-list))
      (setq my-make-frame-list (cdr my-make-frame-list)))
    )

  (defun my-make-frame-at (x y &optional dx dy)
    (my-move-frame (my-clone-frame dx dy) x y))

  (defun my-make-frame-1 ()
    (interactive)
    (unless my-make-frame-list (my-real-make-frame-1)))

  (defun my-make-frame-2 ()
    (interactive)
    (unless my-make-frame-list (my-real-make-frame-2)))

  (defun my-make-frame-3 ()
    (interactive)
    (unless my-make-frame-list (my-real-make-frame-3)))

  (defun my-make-frame-4 ()
    (interactive)
    (unless my-make-frame-list (my-real-make-frame-4)))
  )
