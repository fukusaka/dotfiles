;;
;; 標準設定
;;

(eval-when-compile (require 'cl))

;; 操作/表示の細かい設定
(setq inhibit-startup-message nil)      ;; オープニングは大事
(setq next-line-add-newlines nil)       ;; カーソルで新しい行を作らない
(setq line-number-mode t)               ;; modeline に行番号表示
(setq column-number-mode t)             ;; modeline にカラム番号表示
(setq size-indication-mode t)           ;; modeline にサイズ表示
(setq transient-mark-mode nil)          ;; Region に色付けない
(setq scroll-step 1)                    ;; スクロールは1行づつであればいいなぁ
(setq scroll-margin 4)                  ;; スクロールのマージン行数 4
(setq scroll-conservatively 2)          ;; 最低スクロール行数 1+1
(setq truncate-lines nil)               ;; 継続行は使わない

;; カーソルは点滅する
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode 1))

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

;; インデントモードの設定
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; 自動再読み込み
(global-auto-revert-mode 1)

;; ヒストリ数を設定する。
;;(setq comint-input-ring-size 200)

;; history of minibuffer
(savehist-mode 1)
(setq history-length 1000)

;; 最近使ったファイルリストを保持
(add-hook 'after-init-hook 'recentf-mode)
(eval-after-load "recentf"
  '(progn
     (setq recentf-max-saved-items 2000)
     (setq recentf-auto-cleanup 'never) ; default = 'mode
     (run-with-idle-timer 300 t 'recentf-save-list)
     (run-with-idle-timer 600 t 'recentf-cleanup)
     (setq recentf-exclude
           '("^/tmp\\.*" "^/private\\.*" "^/var/folders\\.*" "/TAGS$"))))

;; Emacs内部端末で Password を隠しまほう
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; shellモードで色付け
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; shellモードのプロセスを終了するときに聞かない
(defun my-process-kill-without-query ()
  (let ((process (get-buffer-process (current-buffer))))
    (if process
        (process-kill-without-query process))))

(add-hook 'shell-mode-hook 'my-process-kill-without-query)

;; EDITOR=emacsclientで emacs で開く
;;(server-start)

;; 表示テスト用
(autoload 'my-sample-ascii "my-sample-ascii" "" t)
(autoload 'my-sample-face-size "my-sample-ascii" "" t)

;; Customize 用設定
(if (boundp 'my-emacs-conf-dir)
    (progn
      (setq custom-file (concat my-emacs-conf-dir "custom.el"))
      (if (file-exists-p custom-file)
          (load custom-file))))

;; 簡易インデックスの作成
(setq imenu-auto-rescan t)              ;; imenu 用の自動スキャンをする

;; 現在関数表示モード
(require 'which-func)
(which-func-mode t)
(add-to-list 'which-func-modes 'java-mode)
(add-to-list 'which-func-modes 'javascript-mode)

;; Iswitchb モード
(iswitchb-mode 1)


;; 編集バックアップは一カ所に集める
(defvar my-backup-dir (expand-file-name "~/.emacs.d/backup"))
(unless (file-directory-p my-backup-dir)
  (make-directory my-backup-dir))

(add-to-assoc-list 'backup-directory-alist
                   `("\\.*\\'" . ,my-backup-dir))

;; バックアップファイルはコピーして作成する
(setq backup-by-copying t)

;; 定期的に ~/.emacs.d/backup 内は掃除するべし(cronを使うと良いかも)
;;  find ~/.emacs.d/backup -mtime +30 -exec rm -f {} \;
(defun my-cleanup-backup-directory ()
  (interactive)
  (let* ((now (current-time))
         (files (directory-files my-backup-dir t "\\`[^\\.]"))
         (older (remove-if-not
                 '(lambda (e)
                    (>= (let ((mtime (nth 5 (file-attributes e))))
                          (- (first now) (first mtime)))
                        (* 30 24 60 60)))
                 files)))
    (mapc 'delete-file older)))

;; NTEmacs ではバックアップ(自動バックアップも含む)は行なわない
(when (memq system-type '(cygwin windows-nt))
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  )


;;abbrev
(setq save-abbrevs 'silently)
