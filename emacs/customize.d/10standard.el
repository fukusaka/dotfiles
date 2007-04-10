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

;; メニューとツールの背景色
(when (and window-system (fboundp 'facep))
  (if (facep 'scroll-bar)
      (set-face-background 'scroll-bar "AntiqueWhite"))
  (if (facep 'tool-bar)
      (set-face-background 'tool-bar "AntiqueWhite"))
  )

;; 端末ではメニューバーを消す
(if (not window-system)
    (if (fboundp 'menu-bar-mode)
	(menu-bar-mode 0)))

;; scroll-bar は右側
(if (fboundp 'set-scroll-bar-mode)
    (set-scroll-bar-mode 'right))

;; tool-bar は消す
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;;(setq initial-frame-alist '((top . 26) (left . 0) (width . 80) (height . 39)))

;; 

(if (featurep 'mac-carbon)
    (setq default-frame-alist
	  (append (list '(active-alpha . 0.95) ;; active frame
			'(inactive-alpha . 0.95) ;; non active frame
			) default-frame-alist) ))

;; 時計の表示
;;(setq display-time-day-and-date nil)
;;(display-time)

;; ヒストリ数を設定する。
(setq comint-input-ring-size 200)

;; Password を隠しまほう
(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt)

;;
;; EDITOR=emacsclientで emacs で開く
;; PAGER=emacsclientで emacs で開く
;;
;;;(if (featurep 'xemacs)
;;;    nil
;;;  (server-start)
;;;  )

