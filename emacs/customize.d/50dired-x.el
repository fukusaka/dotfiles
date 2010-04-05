;;
;; Dired-X を使う設定
;;

;; 追加の省略する拡張子
(setq moi::dired-omit-extensions
      '(".o" ".elc" "~" ".bin" ".lbin" ".fasl"
	".a" ".ln" ".fmt" ".lo" ".flc" ".flh" ))

;; 表示を省略するファイルと拡張子の設定
(cond
 ((eq system-type 'darwin)
  (setq moi::dired-omit-files "^#\\|^\\.\\|^Desktop D[BF]$\\|Icon\015"))
 (t
  (setq moi::dired-omit-files "^#\\|^\\.")))

;; GNU Emacs に追加バインド
(autoload 'dired-jump "dired-x" nil t nil)
(autoload 'dired-jump-other-window "dired-x" nil t nil)
(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)

;; for emacs 22
(if (>= emacs-major-version 22)
    (defalias 'dired-omit-toggle 'dired-omit-mode))

;; 
(add-hook 'dired-load-hook
	  '(lambda ()
	     (require 'dired-x)
	     (require 'wdired)

	     ;; モードキーの設定
	     (define-key dired-mode-map "\M-o" 'dired-omit-toggle)
	     (define-key dired-mode-map "f" 'dired-do-shell-command)
	     (define-key dired-mode-map "U" 'dired-unmark-all-files-no-query)
	     (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
	     
	     ;; Set dired-x variables here.  For example:
	     ;;(setq dired-guess-shell-gnutar "tar")
	     ;;(setq dired-guess-shell-znew-switches t)
	     ;;(setq dired-x-hands-off-my-keys nil)

	     (setq dired-omit-files moi::dired-omit-files)
	     (setq dired-omit-extensions
		   (append moi::dired-omit-extensions
			   dired-omit-extensions))
	     ))

;; 初期で omit 状態
(add-hook 'dired-mode-hook '(lambda () (dired-omit-toggle)))
