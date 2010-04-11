;;
;; フォント/フレーム初期位置の設定
;;

(when window-system

  ;; Frame サイズ位置の固定
  (add-to-assoc-list 'default-frame-alist '(width . 80))
  (add-to-assoc-list 'default-frame-alist '(height . 40))

  (cond
   ;; MacOSX
   ((eq system-type 'darwin)
    (add-to-assoc-list 'initial-frame-alist '(top . 22))
    (add-to-assoc-list 'initial-frame-alist '(left . 0))
    (add-to-assoc-list 'default-frame-alist '(width . 90))
    (add-to-assoc-list 'initial-frame-alist '(height . 54))
    (add-to-assoc-list 'default-frame-alist '(alpha . (95 85)))
    (setq frame-alpha-lower-limit 30)
    (setq-default line-spacing 0.1)
    )
   ;; X
   ((eq window-system 'x)
    (add-to-assoc-list 'initial-frame-alist '(top . 25))
    (add-to-assoc-list 'initial-frame-alist '(left . 0))
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

   ;; X / emacs22
   ((and (eq window-system 'x)
	 (= emacs-major-version 22))

    (dolist (fspec '("-*-fixed-medium-r-normal--12-*-*-*-*-*-fontset-12"
		     "-*-fixed-medium-r-normal--14-*-*-*-*-*-fontset-14"
		     "-*-fixed-medium-r-normal--16-*-*-*-*-*-fontset-16"
		     "-*-fixed-medium-r-normal--18-*-*-*-*-*-fontset-18"))
      (if (not (assoc fspec fontset-alias-alist))
          (create-fontset-from-fontset-spec fspec)))

    (add-to-assoc-list 'default-frame-alist '(font . "fontset-14"))
    )

   ;; X / emacs23 以上
   ((and (eq window-system 'x)
	 (>= emacs-major-version 23))

    (add-to-assoc-list 'default-frame-alist '(font . "VL Gothic-10"))
    )

   ;; CarbonEmacs
   ((eq window-system 'mac)
    (require 'carbon-font)
    (setq mac-allow-anti-aliasing t)
    (fixed-width-set-fontset "hirakaku_w3" 12)
    )

   ;; Cocoa Emacs
   ((eq window-system 'ns)
    (setq mac-allow-anti-aliasing t)

    ;; デフォルトフォント設定
    (set-face-attribute 'default nil
			:family "Monaco"
			:height 120)

    ;; 日本語文字設定にフォントを指定
    (let ((fn (frame-parameter nil 'font))
	  ;;(ff "Hiragino Maru Gothic Pro")
	  (ff "Hiragino Kaku Gothic Pro")
	  (rg "iso10646-1"))
      (set-fontset-font fn 'japanese-jisx0208 `(,ff . ,rg))
      (set-fontset-font fn 'katakana-jisx0201 `(,ff . ,rg))
      (set-fontset-font fn 'japanese-jisx0212 `(,ff . ,rg)))

    ;; フォントサイズの微調節
    (dolist (e '(("^-apple-hiragino.*" . 1.2)
                 (".*osaka-bold.*" . 1.2)
                 (".*osaka-medium.*" . 1.2)
                 (".*courier-bold-.*-mac-roman" . 1.0)
                 (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
                 (".*monaco-bold-.*-mac-roman" . 0.9)
                 ("-cdac$" . 1.3)))
      (add-to-assoc-list 'face-font-rescale-alist e))

    )

   ;; Windows
   ((eq window-system 'w32)
    (setq scalable-fonts-allowed t)
    (setq w32-enable-synthesized-fonts t)

    (let (myfont)
      (setq myfont "ＭＳ ゴシック")
      (if (featurep 'w32-ime)		;; IMEパッチ対応時等幅になる
          (setq myfont "VL ゴシック"))

      ;;(setq myfont "Meiryo")	;; どうも上手くサイズが決まらない

      ;; デフォルトフォント設定
      (set-face-attribute 'default nil
                          :family myfont
                          :height 100)

      ;; 日本語文字設定にフォントを指定
      (let ((fn (frame-parameter nil 'font))
            (ff (concat myfont "*")))
        (set-fontset-font fn 'japanese-jisx0208 `(,ff . "jisx0208-sjis"))
        (set-fontset-font fn 'katakana-jisx0201 `(,ff . "jisx0201-katakana")))
      )

    ;; フォントサイズの微調節
    (dolist (e '((".*ＭＳ.*bold.*iso8859.*"  . 0.9)
                 (".*ＭＳ.*bold.*jisx02.*" . 0.95)))
      (setcar e (encode-coding-string (car e) 'emacs-mule))
      (add-to-assoc-list 'face-font-rescale-alist e))
    )

   ))
