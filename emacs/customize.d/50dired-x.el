;;
;; Dired-X を使う設定
;;

;; 追加の省略する拡張子
(setq my-dired-omit-extensions
      '(".o" ".elc" "~" ".bin" ".lbin" ".fasl"
        ".a" ".ln" ".fmt" ".lo" ".flc" ".flh" ))

;; 表示を省略するファイルと拡張子の設定
(cond
 ((eq system-type 'darwin)
  (setq my-dired-omit-files "^#\\|^\\.\\|^Desktop D[BF]$\\|Icon\015"))
 (t
  (setq my-dired-omit-files "^#\\|^\\.")))

;; GNU Emacs に追加バインド
(autoload 'dired-jump "dired-x" nil t nil)
(autoload 'dired-jump-other-window "dired-x" nil t nil)

;; ロード時に設定を行う
(add-hook 'dired-load-hook
          '(lambda ()
             (load "dired-x")

             ;; emacs 21以前への対応
             ;; (dired-omit-toggle => dired-omit-mode)
             (if (and (not (fboundp 'dired-omit-mode))
                      (fboundp 'dired-omit-toggle))
                 (defalias 'dired-omit-mode 'dired-omit-toggle))

             ;;;;; 複数のウィンドウを開かない
             ;;;(put 'dired-find-alternate-file 'disabled nil)
             ;;;
             ;;;(defadvice dired-up-directory
             ;;;  (around kill-up-dired-buffer activate)
             ;;;  (let ((before-buffer (current-buffer)))
             ;;;    ad-do-it
             ;;;    (if (eq major-mode 'dired-mode)
             ;;;        (kill-buffer before-buffer))))
             ;;;
             ;;;(defadvice dired-find-file
             ;;;  (around kill-find-dired-buffer activate)
             ;;;  (let ((before-buffer (current-buffer)))
             ;;;    ad-do-it
             ;;;    (if (eq major-mode 'dired-mode)
             ;;;        (kill-buffer before-buffer))))
             ;;;
             ;;;(defadvice dired-view-file
             ;;;  (around kill-view-dired-buffer activate)
             ;;;  (let ((before-buffer (current-buffer)))
             ;;;    ad-do-it
             ;;;    (if (eq major-mode 'dired-mode)
             ;;;        (kill-buffer before-buffer))))

             ;; モードキーの設定
             (define-key dired-mode-map "\M-o" 'dired-omit-mode)
             (define-key dired-mode-map "f" 'dired-do-shell-command)
             (define-key dired-mode-map "U" 'dired-unmark-all-files-no-query)
             (define-key dired-mode-map "q" 'dired-up-directory)

             ;; wdired (writable dired) があれば設定
             (when (locate-library "wdired")
               (require 'wdired)
               (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))

             ;; Set dired-x variables here.  For example:
             ;;(setq dired-guess-shell-gnutar "tar")
             ;;(setq dired-guess-shell-znew-switches t)
             ;;(setq dired-x-hands-off-my-keys nil)

             (setq dired-omit-files my-dired-omit-files)
             (dolist (ext dired-omit-extensions)
               (add-to-list 'dired-omit-extensions ext t))

             ;; 初期で omit 状態
             (add-hook 'dired-mode-hook '(lambda () (dired-omit-mode)))

             ))

