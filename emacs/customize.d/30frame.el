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
   ((eq system-type 'darwin)
    (add-to-list 'initial-frame-alist '(top . 26))
    (add-to-list 'initial-frame-alist '(left . 4))
    (add-to-list 'default-frame-alist '(alpha . (95 90)))
    (setq frame-alpha-lower-limit 30)
    (setq-default line-spacing 0.1)
    )
   ;; X
   ((eq window-system 'x)
    (add-to-list 'initial-frame-alist '(top . 25))
    (add-to-list 'initial-frame-alist '(left . 0))
    )
   ;; Windows
   ((eq window-system 'w32)
    ;;(setq-default line-spacing 0.1)
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

   ;; Cocoa Emacs
   ((eq window-system 'ns)


    (setq mac-allow-anti-aliasing t)
    ;; フォントサイズの微調節
    (setq face-font-rescale-alist
	  '(("^-apple-hiragino.*" . 1.2)
	    (".*osaka-bold.*" . 1.2)
	    (".*osaka-medium.*" . 1.2)
	    (".*courier-bold-.*-mac-roman" . 1.0)
	    (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
	    (".*monaco-bold-.*-mac-roman" . 0.9)
	    ("-cdac$" . 1.3)))

    (set-frame-font "Monaco-12")

    (let ((fs (frame-parameter nil 'font))
	  ;;(ff "Hiragino Maru Gothic Pro")
	  (ff "Hiragino Kaku Gothic Pro")
	  (rg "iso10646-1"))
	  
      (set-fontset-font
       fs 'japanese-jisx0208 `(,ff . ,rg))

      (set-fontset-font
       fs 'katakana-jisx0201 `(,ff . ,rg))

      (set-fontset-font
       fs 'japanese-jisx0212 `(,ff . ,rg))

      ))
   
   ;; Windows
   ((eq window-system 'w32)

    (setq scalable-fonts-allowed t)

    (set-face-attribute 'default nil
			:family "ＭＳ ゴシック"
			:height 120)

    (set-fontset-font "fontset-default"
		      'japanese-jisx0208
		      '("ＭＳ ゴシック*" . "jisx0208-sjis"))

    (set-fontset-font "fontset-default"
		      'katakana-jisx0201
		      '("ＭＳ ゴシック*" . "jisx0201-katakana"))

    (setq w32-enable-synthesized-fonts t)

    (add-to-list 'face-font-rescale-alist
		 `(,(encode-coding-string ".*ＭＳ.*bold.*iso8859.*" 'emacs-mule) . 0.9))
    
    (add-to-list 'face-font-rescale-alist
		 `(,(encode-coding-string ".*ＭＳ.*bold.*jisx02.*" 'emacs-mule) . 0.95))


;;    (setq face-font-rescale-alist
;;	  `((".*Meiryo.*" . 1.4)
;;	    (,(encode-coding-string ".*MS.*" 'emacs-mule) . 1.4)
;;	    ("-cdac$" . 1.3)))
;;    
;;    (set-frame-font "Courier New-10")
;;    ;;(set-frame-font "Inconsolata\-dz-10")
;;    ;;(set-frame-font "IPAGothic-10")
;;
;;    (let ((fs (frame-parameter nil 'font))
;;    	  ;;(ff "Meiryo")
;;    	  ;;(ff "MS Gothic")
;;    	  (ff "VL Gothic")
;;	  (rg "iso10646-1")
;;	  ;;(rg "unicode-bmp")
;;	  )
;;
;;      (set-fontset-font
;;       fs 'japanese-jisx0208 ff nil 'append)
;;
;;      (set-fontset-font
;;       fs 'katakana-jisx0201 `(,ff . ,rg))
;;      )
    )
   ;; X / emacs22
   ((and (eq window-system 'x)
	 (= emacs-major-version 22))

    (dolist (fspec '("-*-fixed-medium-r-normal--12-*-*-*-*-*-fontset-12"
		     "-*-fixed-medium-r-normal--14-*-*-*-*-*-fontset-14"
		     "-*-fixed-medium-r-normal--16-*-*-*-*-*-fontset-16"
		     "-*-fixed-medium-r-normal--18-*-*-*-*-*-fontset-18"))
      (unless (assoc fspec fontset-alias-alist)
	(create-fontset-from-fontset-spec fspec)))
    
    (add-to-list 'default-frame-alist '(font . "fontset-14"))
    )

   ;; X / emacs23 以上
   ((and (eq window-system 'x)
	 (>= emacs-major-version 23))

    (add-to-list 'default-frame-alist '(font . "VL Gothic-10"))
    )

   ))
