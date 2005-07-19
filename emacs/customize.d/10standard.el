;; standard.el

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

;;
;; 表示の細かい設定
;;
(setq next-line-add-newlines nil)
(setq line-number-mode t)
(setq column-number-mode t)
(setq scroll-step 1)

;;(setq inhibit-startup-message t)

;;(if window-system
;;    (transient-mark-mode t))

(if (not window-system)
    (menu-bar-mode 0))

;;(setq truncate-lines t)
;; 時計の表示
;;(setq display-time-day-and-date nil)
;;(display-time)

;; Xの設定で鳴らさないようにするので、、。
;; ~/.xsession -->
;;   xset b off
(if (not window-system)
    (setq visible-bell t)
  )

(if (string-match "^21" emacs-version)
    (progn
      (tool-bar-mode -1)
      (blink-cursor-mode 0)
      (setq initial-frame-alist '((top . 26) (left . 0) (width . 80) (height . 39)))
      ))
;;
;; ディフォルトModeの設定
;;
;;(setq default-major-mode 'text-mode)

;;(setq text-mode-hook 'turn-on-auto-fill)

;;
;; コマンド入力の設定 (comint-mode)
;;

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
	 )
       auto-mode-alist))

(autoload 'po-mode "po-mode")
(autoload 'moi-patch-view-mode "moi-patch-view")

;;(which-function-mode)
;;
(setq vc-follow-symlinks t)

;;
;; 圧縮ファイルを自動に展開、圧縮
;;
;; tar-mode と組み合わせるとEUCが化けるんだが、、、。
;; JISだけうまく表示出来る、、、detect-coding-regionのバグ?。
;; ver19 ではコケル、、、

(cond
 ((string-match "^2[01]" emacs-version)
  (auto-compression-mode)

  ;; bzip2, a block-sorting file compressor.  Version 0.9.0, 30-Aug-98.
  (setq jka-compr-compression-info-list 
	(append
	 '(["\\.bz2\\'"
	    "bzip2ing"        "bzip2"         ()
	    "bunzip2ing"      "bzip2"         ("-d")
	    nil t])
	 jka-compr-compression-info-list))
  ))

;;
;; EDITOR=emacsclientで emacs で開く
;; PAGER=emacsclientで emacs で開く
;;
(if (featurep 'xemacs)
    nil
  (server-start)
  )

;;
;; semi-gnus 初期化ファイルの指定
;;

(setq gnus-init-file (concat moi::host-customize-dir "gnus"))

;;
;; compile-mode
;;
(setq compilation-ask-about-save nil)
(setq compilation-window-height 0)

;;
;;(setq ange-ftp-smart-gateway-port "16021")


(cond
 ((string-match "^20.4" emacs-version)
  (defun char-list-to-string (lst)
    (eval (cons 'concat (mapcar 'char-to-string lst))))
  ))

;;
(autoload 'moi::sample-ascii "moi-sample-ascii" "" t)

;; Browser
(setq browse-url-xterm-program "kterm")

(setq browse-url-xterm-args
      '("-fl" "-alias-fixed-medium-r-normal--12-*"
	"-geometry" "119x47+0+0"))

(defun browse-url-w3m-xterm (url &optional new-window)
  (interactive (browse-url-interactive-arg "w3m URL: "))
  (if (= 0 (string-match "file:" url))
      (setq url (substring url 5)))
  (message url)
  (apply #'start-process `(,(concat "w3m" url) nil ,browse-url-xterm-program
             ,@browse-url-xterm-args "-e" "w3m" "-F" ,url)))

(setq browse-url-browser-function 'browse-url-w3m-xterm)
