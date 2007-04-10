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

(if window-system (progn
   (set-face-background 'scroll-bar "AntiqueWhite")
   (set-face-background 'tool-bar "AntiqueWhite")
   ))

(cond
 ((not window-system)
  (menu-bar-mode 0)
  (blink-cursor-mode 0))

 ((featurep 'xemacs)
  nil)

 (t
  (if (fboundp 'set-scroll-bar-mode)
      (set-scroll-bar-mode 'right))
  (tool-bar-mode -1)
  (setq blink-cursor-mode t)))

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
;; 自動識別するモードの設定
;;
(setq auto-mode-alist
      (append
       '(
	 ("\\.h$" . c++-mode)
	 ("\\.pl$" . perl-mode)
	 ("\\.mht$" . html-mode)
	 ("\\.po[tx]?\\'\\|\\.po\\." . po-mode)
	 ("ChangeLog" . change-log-mode)
	 ("patch" . moi-patch-view-mode)
	 ("\\.diff" . moi-patch-view-mode)
	 ("\\.pgc$" . c-mode)
	 ("\\.pgcc$" . c++-mode)
	 ("\\.CPP$" . c++-mode)
	 ("\\.gen_h$" . c++-mode)
	 ("Rakefile" . ruby-mode)
	 )
       auto-mode-alist))

(autoload 'po-mode "po-mode")
(autoload 'moi-patch-view-mode "moi-patch-view")
(autoload 'moi::sample-ascii "moi-sample-ascii" "" t)

(auto-compression-mode)

;;
;; EDITOR=emacsclientで emacs で開く
;; PAGER=emacsclientで emacs で開く
;;
;;;(if (featurep 'xemacs)
;;;    nil
;;;  (server-start)
;;;  )

(cond
 ((string-match "^20.4" emacs-version)
  (defun char-list-to-string (lst)
    (eval (cons 'concat (mapcar 'char-to-string lst))))
  ))

