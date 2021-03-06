;;
;; フォントの設定
;;

;; VMware 上の Ubuntu であるか？
(defvar my-x-on-vmware nil)

(if (string-match "^fuku-umac" (system-name))
    (setq my-x-on-vmware t))

(when window-system

  (cond
   ;; Ver.21 以前
   ((<= emacs-major-version 21)
    (with-no-warnings
      (set-default-font "fontset-standard")))

   ;; X / emacs22
   ((and (eq window-system 'x)
         (= emacs-major-version 22))
    (dolist (fspec '("-*-fixed-medium-r-normal--12-*-*-*-*-*-fontset-12"
                     "-*-fixed-medium-r-normal--14-*-*-*-*-*-fontset-14"
                     "-*-fixed-medium-r-normal--16-*-*-*-*-*-fontset-16"
                     "-*-fixed-medium-r-normal--18-*-*-*-*-*-fontset-18"))
      (if (not (assoc fspec fontset-alias-alist))
          (create-fontset-from-fontset-spec fspec)))
    (add-to-list 'default-frame-alist '(font . "fontset-14") t))

   ;; CarbonEmacs
   ((eq window-system 'mac)
    (require 'carbon-font)
    (defvar mac-allow-anti-aliasing t)
    (if (fboundp 'fixed-width-set-fontset)
        (fixed-width-set-fontset "hirakaku_w3" 12)))

   ;; Emacs 23 以上
   ((>= emacs-major-version 23)
    (let (my-font-height my-font my-font-ja my-font-size my-fontset)
      (cond
       ;; for X (debian/ubuntu/fedora)
       ((eq window-system 'x)
        ;;(setq my-font-height 90)
        (setq my-font-height 105)
        ;;(setq my-font-height 120)
        ;;(setq my-font "Monospace")
        (setq my-font "Inconsolata")
        ;;(setq my-font "Takaoゴシック")
        ;;(setq my-font-ja "VL ゴシック")
        ;;(setq my-font-ja "Takaoゴシック")
        (setq my-font-ja "IPAゴシック")

        (setq face-font-rescale-alist
              '(("-cdac$" . 1.3)))

        ;; VMware 上のX11では、800x600 のとき 96 dpi になるように調節されている。
        ;; なので、別のサイズやフルスクリーンにすると、dpi の値が変化する。
        ;; 結果、Emacs Xft では同じ pt に対するpixel値が大きくなってしまう。
        ;; 対処不明。。。取り敢えず直接 pixel サイズで指定して対処？
        (when my-x-on-vmware
          (setq my-font-size 14))
        )

       ;; Cocoa Emacs
       ((eq window-system 'ns)
        (setq mac-allow-anti-aliasing t)
        (setq my-font-height 120)
        ;;(setq my-font "Courier")
        ;;(setq my-font "Courier New")
        ;;(setq my-font "Osaka-Mono")
        ;;(setq my-font "Monaco")       ;; XCode 3.1 で使っているフォント
        (setq my-font "Menlo")        ;; XCode 3.2 で使ってるフォント
        ;;(setq my-font "Consolas")
        (setq my-font-ja "Hiragino Kaku Gothic Pro")
        ;;(setq my-font-ja "Hiragino Maru Gothic Pro")
        ;;(setq my-font-ja "IPAゴシック")

        ;; フォントサイズの微調節 (12ptで合うように)
        (setq face-font-rescale-alist
              '(("^-apple-hiragino.*" . 1.2)
                (".*Hiragino.*" . 1.2)
                (".*osaka-bold.*" . 1.2)
                (".*osaka-medium.*" . 1.2)
                (".*courier-bold-.*-mac-roman" . 1.0)
                (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
                (".*monaco-bold-.*-mac-roman" . 0.9)
                ("-cdac$" . 1.3)))
        )

       ;; NTEmacs
       ((eq window-system 'w32)
        (setq scalable-fonts-allowed t)
        (defvar w32-enable-synthesized-fonts t)
        (setq my-font-height 100)
        ;;(setq my-font "ＭＳ ゴシック")
        ;;(setq my-font "VL ゴシック")
        ;;(setq my-font "IPAゴシック")
        ;;(setq my-font "Takaoゴシック")
        ;;(setq my-font "Inconsolata")
        (setq my-font "Consolas")
        ;;(setq my-font "DejaVu Sans Mono")
        ;;(setq my-font-ja "ＭＳ ゴシック")
        ;;(setq my-font-ja "VL ゴシック")
        (setq my-font-ja "IPAゴシック")
        ;;(setq my-font-ja "Takaoゴシック")
        ;;(setq my-font-ja "メイリオ")
        ;; ime-font の設定がわからん

        ;; フォントサイズの微調節 (10ptで合うように)
        (setq face-font-rescale-alist
              '((".*ＭＳ.*bold.*iso8859.*"  . 0.9)
                (".*ＭＳ.*bold.*jisx02.*" . 0.95)
                (".*DejaVu Sans.*" . 0.9)
                (".*メイリオ.*" . 1.1)
                ("-cdac$" . 1.3)))

        ;;(dolist (e face-font-rescale-alist)
        ;;  (setcar e (encode-coding-string (car e) 'emacs-mule)))
        )
       )

      ;; デフォルトフォント設定
      (cond
       ;; pixel 単位で指定
       ((and my-font-size my-font)
        (setq my-fontset
              (create-fontset-from-ascii-font (format "%s:size=%d" my-font my-font-size))))
       ;; 高さを pt 単位で指定
       (my-font
        (set-face-attribute 'default nil :family my-font :height my-font-height)
        ;;(set-frame-font (format "%s-%d" my-font (/ my-font-height 10)))
        )
       )

      (when my-fontset
        (add-to-list 'default-frame-alist `(font . ,my-fontset) t))

      ;; 日本語文字に別のフォントを指定
      (when my-font-ja
        (let ((fn (or my-fontset (frame-parameter nil 'font)))
              (rg "iso10646-1"))
          (set-fontset-font fn 'katakana-jisx0201 `(,my-font-ja . ,rg))
          (set-fontset-font fn 'japanese-jisx0208 `(,my-font-ja . ,rg))
          (set-fontset-font fn 'japanese-jisx0212 `(,my-font-ja . ,rg)))
        )
      ))
   ))
