;;
;; フォント/フレーム初期位置の設定
;;

(when window-system

  ;; Frame サイズ位置の固定
  (add-to-list 'initial-frame-alist '(width . 80))
  (add-to-list 'initial-frame-alist '(height . 40))
  (add-to-list 'default-frame-alist '(width . 80))
  (add-to-list 'default-frame-alist '(height . 40))

  (cond
   ;; MacOSX
   ((eq window-system 'mac)
    (add-to-list 'initial-frame-alist '(top . 26))
    (add-to-list 'initial-frame-alist '(left . 4))
    (add-to-list 'default-frame-alist '(alpha . (95 90)))
    (setq frame-alpha-lower-limit 30)
    )
   ;; X
   ((eq window-system 'x)
    (add-to-list 'initial-frame-alist '(top . 25))
    (add-to-list 'initial-frame-alist '(left . 0))
    )
   )

  ;; Font 設定
  (cond
   ;; Ver.21 以前
   ((<= emacs-major-version 21)
    (set-default-font "fontset-standard"))

   ;; CarbonEmacs
   ((eq window-system 'mac)
    (require 'carbon-font)
    (setq mac-allow-anti-aliasing t)
    (fixed-width-set-fontset "hirakaku_w3" 12)
    )
   
   ;; Windows
   ((eq window-system 'w32)

    (set-frame-font "Courier New-10")
    ;;(set-frame-font "Inconsolata\-dz-10")
    (let ((fs (frame-parameter nil 'font))
    	  (ff "Meiryo")
    	  ;;(ff "MSGothic")
    	  (sz 12))
      (set-fontset-font fs
			'japanese-jisx0208
			(font-spec :family ff :size sz))
      (set-fontset-font fs
    			'katakana-jisx0201
    			(font-spec :family ff :size sz))
      )

    ;; IPA
    ;;(set-frame-font "IPAGothic-12")
    )

   ;; X / emacs22
   ((and (eq window-system 'x)
	 (= emacs-major-version 22))

    (create-fontset-from-fontset-spec
     "-*-fixed-medium-r-normal--12-*-*-*-*-*-fontset-12")
    (create-fontset-from-fontset-spec
     "-*-fixed-medium-r-normal--14-*-*-*-*-*-fontset-14")
    (create-fontset-from-fontset-spec
     "-*-fixed-medium-r-normal--16-*-*-*-*-*-fontset-16")
    (create-fontset-from-fontset-spec
     "-*-fixed-medium-r-normal--18-*-*-*-*-*-fontset-18")

    (add-to-list 'default-frame-alist '(font . "fontset-14"))
    )

   ;; X / emacs23
   ((and (eq window-system 'x)
	 (= emacs-major-version 23))

    (add-to-list 'default-frame-alist '(font . "VL Gothic-10"))
    )

   ))
