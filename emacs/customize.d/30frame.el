;;
;; フレーム初期位置の設定
;;

(when window-system

  ;; Frame サイズ位置の固定
  (add-to-assoc-list 'default-frame-alist '(width . 80))
  (add-to-assoc-list 'default-frame-alist '(height . 40))

  (cond
   ;; X
   ((eq window-system 'x)
    (when (member ":0.0" (x-display-list))
      (add-to-assoc-list 'default-frame-alist '(width . 100))
      ;;(setq-default line-spacing 0.1)

      (add-to-assoc-list 'initial-frame-alist '(left . 0))
      (let* ((top-panel-size 24)
	     (bottom-panel-size 24)
	     (font-height 14)
	     (view-pixel-height (- (display-pixel-height) (+ top-panel-size bottom-panel-size 2)))
	     (height (/ view-pixel-height font-height)))
	(add-to-assoc-list 'initial-frame-alist `(top . ,(+ top-panel-size 1)))
	(add-to-assoc-list 'initial-frame-alist `(height . ,height)))
      )
    )

   ;; MacOSX
   ((eq system-type 'darwin)
    (add-to-assoc-list 'default-frame-alist '(width . 90))
    (add-to-assoc-list 'default-frame-alist '(alpha . (95 85)))
    (setq frame-alpha-lower-limit 30)
    (setq-default line-spacing 0.1)

    (add-to-assoc-list 'initial-frame-alist '(top . 22))
    (add-to-assoc-list 'initial-frame-alist '(left . 0))
    (if (eq window-system 'ns)
	(add-to-assoc-list 'initial-frame-alist '(height . 56)))
    )

   ;; Windows
   ((eq window-system 'w32)
    ;;(setq-default line-spacing 0.1)

    (add-to-assoc-list 'initial-frame-alist '(top . 0))
    (add-to-assoc-list 'initial-frame-alist '(left . 160))
    )
   )
  )