;; standard.el

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

;; 操作/表示の細かい設定
(setq inhibit-startup-message nil)	;; オープニングは大事
(setq next-line-add-newlines nil)	;; カーソルで新しい行を作らない
(setq line-number-mode t)		;; modeline に行番号表示
(setq column-number-mode t)		;; modeline にカラム番号表示
(setq scroll-step 1)			;; スクロールは1行づつ
(setq truncate-lines nil)		;; 継続行は使わない
(setq transient-mark-mode nil)		;; Region に色付けない

;; Xの設定で鳴らさないようにするので、、。
;; ~/.xsession -->
;;   xset b off
;;(setq visible-bell t)


;; 端末ではメニューバーを消す
(if (and (not window-system) (fboundp 'menu-bar-mode))
    (menu-bar-mode 0))

;; scroll-bar は右側
(if (fboundp 'set-scroll-bar-mode)
    (set-scroll-bar-mode 'right))

;; tool-bar は消す
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;; 時計の表示
(setq display-time-day-and-date nil)
(display-time)

;; ヒストリ数を設定する。
;;(setq comint-input-ring-size 200)

;; Emacs内部端末で Password を隠しまほう
(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt)

;; EDITOR=emacsclientで emacs で開く
;;(server-start)

;; Windows系でCygwinのBashを使う
(when (and (or (eq system-type 'windows-nt)
	       (eq system-type 'cygwin))
	   (executable-find "bash"))
  (setq shell-file-name "bash")
  (setenv "SHELL" shell-file-name)
  (setenv "CYGWIN" "binmode nontsec tty")

  (add-hook 'comint-output-filter-functions
	    'comint-strip-ctrl-m)
  )