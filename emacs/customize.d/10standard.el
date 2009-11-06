;; standard.el

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

;;
;; 表示の細かい設定
;;
;;(setq inhibit-startup-message t)
(setq next-line-add-newlines nil)
(setq line-number-mode t)
(setq column-number-mode t)
(setq scroll-step 1)
;;(setq truncate-lines t)
;;(transient-mark-mode t)

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
(setq comint-input-ring-size 200)

;; Emacs内部端末で Password を隠しまほう
(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt)

;; EDITOR=emacsclientで emacs で開く
;;(server-start)

