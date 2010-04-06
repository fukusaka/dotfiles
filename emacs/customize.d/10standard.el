;;
;; 標準設定
;;

;; 操作/表示の細かい設定
(setq inhibit-startup-message nil)		;; オープニングは大事
(setq next-line-add-newlines nil)		;; カーソルで新しい行を作らない
(setq line-number-mode t)			;; modeline に行番号表示
(setq column-number-mode t)			;; modeline にカラム番号表示
(setq size-indication-mode t)			;; modeline にサイズ表示
(setq transient-mark-mode nil)			;; Region に色付けない
(setq scroll-step 1)				;; スクロールは1行づつであればいいなぁ
(setq scroll-margin 4)				;; スクロールのマージン行数 4
(setq scroll-conservatively 2)			;; 最低スクロール行数 1+1
(setq truncate-lines nil)			;; 継続行は使わない

;; 行末スペース削除支援
(setq-default show-trailing-whitespace t)	;; 行末の不要スペースを強調表示
(add-hook 'before-save-hook
          'delete-trailing-whitespace)		;; 保存時に無駄なスペースを削除

;; カーソルは点滅しない
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode 0))

;; Beep 音を鳴らさない
;;(setq visible-bell t)
(setq ring-bell-function '(lambda ()))
;; ~/.xsession -->  xset b off

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

;; MacOSXではMacPortsへパスを通す
(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/opt/local/bin/"))

;; 表示テスト用
(autoload 'my-sample-ascii "my-sample-ascii" "" t)
