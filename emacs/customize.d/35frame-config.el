;;
;; フレーム初期位置の設定
;;

(when window-system

  ;; Frame サイズ位置の固定
  (add-to-assoc-list 'default-frame-alist '(width . 80))
  (add-to-assoc-list 'default-frame-alist '(height . 40))

  ;; 好みのワークスペース位置
  (setq my-desktop-center-x 1)
  (setq my-desktop-center-y 1)

  ;; 複数フレーム開く方法 1
  (defun my-real-make-frame-1 ()
    (my-make-frame-at -1 0)
    (my-make-frame-at  0 1)
    (my-make-frame-at -1 1)
    )

  ;; 複数フレーム開く方法 2
  (defun my-real-make-frame-2 ()
    (my-make-frame-at -1 -1)
    (my-make-frame-at  0 -1)
    ;;(my-make-frame-at  1 -1)
    (my-make-frame-at -1  0)
    ;;(my-make-frame-at  1  0)
    (my-make-frame-at -1  1)
    (my-make-frame-at  0  1)
    (my-make-frame-at  1  1)
    )

  (defun my-real-make-frame-3 () )
  (defun my-real-make-frame-4 () )

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
	     (view-pixel-height (- (display-pixel-height)
				   (+ top-panel-size bottom-panel-size 2)))
	     (height (/ view-pixel-height font-height)))
	(add-to-assoc-list 'initial-frame-alist `(top . ,(+ top-panel-size 1)))
	(add-to-assoc-list 'initial-frame-alist `(height . ,height)))
      )
    )

   ;; MacOSX
   ((eq system-type 'darwin)
    (add-to-assoc-list 'default-frame-alist '(width . 90))
    (add-to-assoc-list 'default-frame-alist '(alpha . (95 90)))
    (setq frame-alpha-lower-limit 30)
    (setq-default line-spacing 0.1)

    (add-to-assoc-list 'initial-frame-alist '(top . 22))
    (add-to-assoc-list 'initial-frame-alist '(left . 0))
    )

   ;; Windows
   ((eq window-system 'w32)
    ;;(setq-default line-spacing 0.1)

    (add-to-assoc-list 'initial-frame-alist '(top . 0))
    (add-to-assoc-list 'initial-frame-alist '(left . 160))
    (add-to-assoc-list 'initial-frame-alist '(height . 46))
    )
   )

  ;; 個別設定
  (cond

   ;; Linux on VMWare / MacBookPro 15
   ((string-match "^fuku-umac" (system-name))
    (add-to-assoc-list 'initial-frame-alist '(height . 53))
    )

   ;; Cocoa Emacs / MacBookPro 15
   ((string-match "^kuro-mac" (system-name))
    (add-to-assoc-list 'initial-frame-alist '(height . 56))
    ;; 複数フレーム開く方法 1
    (defun my-real-make-frame-1 ()
      (my-make-frame-at -1 0)
      (my-make-frame-at  0 1)
      ;;(my-make-frame-at -1 1)
      (my-make-frame-at  0 1 675 0)
      (shell-command "make-safari-window-for-develop.applescript")
      )
    )
   )
  )
