;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dired-X を使う設定
;;

(cond
 ;; XEmacs
 ((featurep 'xemacs)

  (add-hook 'dired-load-hook
	    (function
	     (lambda ()
	       (define-key dired-mode-map "\M-o" 'dired-omit-toggle)
	       (define-key dired-mode-map "f" 'dired-do-shell-command)
	       (define-key dired-mode-map "U" 'dired-unmark-all-files-no-query)
	       )))

  (add-hook 'dired-after-readin-hook
	    (function
	     (lambda ()
	       (dired-omit-toggle))))
  )


 ;; GNU Emacs 20
 (t
  (autoload 'dired-jump "dired-x" nil t nil)
  (autoload 'dired-jump-other-window "dired-x" nil t nil)
  (define-key global-map "\C-x\C-j" 'dired-jump)
  (define-key global-map "\C-x4\C-j" 'dired-jump-other-window)

  (add-hook 'dired-load-hook
	    (function
	     (lambda ()
	       (load "dired-x")
	       ;; モードキーの設定
	       (if (fboundp 'dired-omit-toggle)
		   (define-key dired-mode-map "\M-o" 'dired-omit-toggle)
		 (define-key dired-mode-map "\M-o" 'dired-omit-mode))
	       (define-key dired-mode-map "f" 'dired-do-shell-command)
	       (define-key dired-mode-map "U" 'dired-unmark-all-files-no-query)
	       ;; Set dired-x variables here.  For example:
	       (setq dired-guess-shell-gnutar "tar")
	       ;; (setq dired-guess-shell-znew-switches t)
	       ;; (setq dired-x-hands-off-my-keys nil)
	       ;;
	       ;; 表示を省略するファイルと拡張子の設定
	       ;;
	       (setq dired-omit-files "^#\\|^\\.")
	       (setq dired-omit-extensions
		     (append
		      '(".o" ".elc" "~" ".bin" ".lbin" ".fasl"
			".a" ".ln" ".fmt" ".lo" ".flc" ".flh" )
		      dired-omit-extensions)
		     ))))

  (add-hook 'dired-mode-hook
	    (function
	     (lambda ()
	       (setq dired-omit-files-p t))))  

  ))

