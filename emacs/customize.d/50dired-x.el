;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dired-X を使う設定
;;

(setq moi::dired-omit-extensions
      '(".o" ".elc" "~" ".bin" ".lbin" ".fasl"
	".a" ".ln" ".fmt" ".lo" ".flc" ".flh" ))

;; XEmacs の ls-dired 書式の不具合の対応
(if (featurep 'xemacs)
    (setq dired-use-ls-dired nil))

;; GNU Emacs
(unless (featurep 'xemacs)
  (autoload 'dired-jump "dired-x" nil t nil)
  (autoload 'dired-jump-other-window "dired-x" nil t nil)
  (define-key global-map "\C-x\C-j" 'dired-jump)
  (define-key global-map "\C-x4\C-j" 'dired-jump-other-window))

;; for emacs 22
(if (>= emacs-major-version 22)
    (defalias 'dired-omit-toggle 'dired-omit-mode))

;; 
(add-hook 'dired-load-hook
	  '(lambda ()
	     (unless (featurep 'xemacs) (require 'dired-x))

	     ;; モードキーの設定
	     (define-key dired-mode-map "\M-o" 'dired-omit-toggle)
	     (define-key dired-mode-map "f" 'dired-do-shell-command)
	     (define-key dired-mode-map "U" 'dired-unmark-all-files-no-query)
	     
	     ;; Set dired-x variables here.  For example:
	     (setq dired-guess-shell-gnutar "tar")
	     ;; (setq dired-guess-shell-znew-switches t)
	     ;; (setq dired-x-hands-off-my-keys nil)

	     ;; 表示を省略するファイルと拡張子の設定
	     (if (featurep 'mac-carbon)
		 (setq dired-omit-files "^#\\|^\\.\\|^Desktop D[BF]$\\|Icon\015")
	       (setq dired-omit-files "^#\\|^\\."))

	     (setq dired-omit-extensions
		   (append moi::dired-omit-extensions
			   dired-omit-extensions))
	     ))

;; 初期で omit 状態
(if (featurep 'xemacs)
    (add-hook 'dired-after-readin-hook '(lambda () (dired-omit-toggle)))
  (add-hook 'dired-mode-hook '(lambda () (dired-omit-toggle))))
