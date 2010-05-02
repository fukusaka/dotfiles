;;
;; フォントの設定
;;

(when window-system

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
    (add-to-list 'default-frame-alist '(font . "fontset-14") t))

   ;; CarbonEmacs
   ((eq window-system 'mac)
    (require 'carbon-font)
    (setq mac-allow-anti-aliasing t)
    (fixed-width-set-fontset "hirakaku_w3" 12))

   ;; Emacs 23 以上
   ((>= emacs-major-version 23)
    (let (my-font my-font-ja my-font-height)

      (cond
       ;; for X (debian/ubuntu/fedora)
       ((eq window-system 'x)
	(let ((distrib-id (substring (shell-command-to-string "lsb_release -si") 0 -1)))
	  (cond
	   ;; Ubuntu の Emacs は挙動が不明で分からん
	   ((string= distrib-id "Ubuntu")
	    (setq my-font-height 70) ;; 他の実装に比べて指定の2倍になる？
	    ;;(setq my-font "Monospace")
	    (setq my-font "Inconsolata")
	    ;;(setq my-font "Takaoゴシック")
	    (setq my-font-ja "Takaoゴシック")

	    ;;;; DejaVu Sans Mono は何故か文字幅が大きい？
	    ;;(setq my-font-height 60)
	    ;;(setq my-font "DejaVu Sans Mono")
	    ;;;; フォントサイズの微調節 (DejaVu..のみ必要？)
	    ;;(setq face-font-rescale-alist
	    ;;	  '((".*Takaoゴシック.*" . 1.2)
	    ;;	    ("-cdac$" . 1.3)))

	    )
	   ;; Ubuntu 以外
	   (t
	    (setq my-font-height 100)
	    ;;(setq my-font "Monospace")
	    ;;(setq my-font "Courier")
	    (setq my-font "DejaVu Sans Mono")
	    ;;(setq my-font "VL ゴシック")
	    (setq my-font-ja "VL ゴシック")
	    )
	   )))

       ;; Cocoa Emacs
       ((eq window-system 'ns)
	(setq mac-allow-anti-aliasing t)
	(setq my-font-height 120)
	;;(setq my-font "Courier")
	;;(setq my-font "Courier New")
	;;(setq my-font "Osaka")
	;;(setq my-font "Monaco")	;; XCode 3.1 とかまで使っている
	(setq my-font "Menlo")	;; XCode 3.2 とかで使ってるやつ
	;;(setq my-font "Consolas")
	;;(setq my-font-ja "Hiragino Maru Gothic Pro")
	(setq my-font-ja "Hiragino Kaku Gothic Pro")

	;; フォントサイズの微調節
	(dolist (e '(("^-apple-hiragino.*" . 1.2)
		     (".*osaka-bold.*" . 1.2)
		     (".*osaka-medium.*" . 1.2)
		     (".*courier-bold-.*-mac-roman" . 1.0)
		     (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
		     (".*monaco-bold-.*-mac-roman" . 0.9)))
	  (add-to-list 'face-font-rescale-alist e t))
	)

       ;; NTEmacs
       ((eq window-system 'w32)
	(setq scalable-fonts-allowed t)
	(setq w32-enable-synthesized-fonts t)
	(setq my-font-height 100)
	;;(setq myfont "ＭＳ ゴシック")
	(setq myfont "VL ゴシック")
	;;(setq myfont "Meiryo")	;; どうも上手くサイズが決まらない
	(setq my-font-ja myfont)
	;; ime-font の設定がわからん

	;; フォントサイズの微調節
	(dolist (e '((".*ＭＳ.*bold.*iso8859.*"  . 0.9)
		     (".*ＭＳ.*bold.*jisx02.*" . 0.95)))
	  ;;(setcar e (encode-coding-string (car e) 'emacs-mule))
	  (add-to-list 'face-font-rescale-alist e t))
	)
       )

      ;; デフォルトフォント設定
      (set-face-attribute 'default nil :family my-font :height my-font-height)

      ;; 日本語文字に別のフォントを指定
      (if (or my-font-ja (not (string= my-font my-font-ja)))
	  (let ((fn (frame-parameter nil 'font))
		(rg "iso10646-1"))
	    (set-fontset-font fn 'katakana-jisx0201 `(,my-font-ja . ,rg))
	    (set-fontset-font fn 'japanese-jisx0208 `(,my-font-ja . ,rg))
	    (set-fontset-font fn 'japanese-jisx0212 `(,my-font-ja . ,rg)))
	)
      ))
   ))
