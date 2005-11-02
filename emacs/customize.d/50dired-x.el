;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dired-X を使う設定
;;

(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)

(if (featurep 'xemacs) nil
  (autoload 'dired-jump "dired-x" nil t nil)
  (autoload 'dired-jump-other-window "dired-x" nil t nil))

(add-hook 'dired-load-hook
	  (function
	   (lambda ()
	     (if (not (featurep 'xemacs))
		 (load "dired-x"))
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
	     (setq dired-guess-shell-alist-user
		   '(
		     ("\\.ps$" "gv * &")
		     ("\\.eps$" "gv * &")
		     ("\\.dvi.gz$" "zcat * | xdvi-from-stdin &")
		     ( "\\.au$" "aplay -m -u -q")
		     ( "\\.wav$" "aplay -m -w -q")
		     ( "\\.xpm$" "xv * &")
		     ( "\\.jpeg$" "xv * &")
		     ( "\\.png$" "xv * &")
		     ("\\.gif$" "xv * &")
		     ("\\.e?ps.g?z$" "zcat * | gv - &")
		     ))
	     ;;
	     ;; 表示を省略するファイルと拡張子の設定
	     ;;
	     (setq dired-omit-files "^#\\|^\\.")
	     (setq dired-omit-extensions
		   (append
		    '(".o" ".elc" "~" ".bin" ".lbin" ".fasl"
		      ".a" ".ln" ".fmt" ".lo" ".flc" ".flh" )
		    dired-omit-extensions)
		   ))
	   ))

(cond
 ((featurep 'xemacs)
  (add-hook 'dired-after-readin-hook
	    (function
	     (lambda ()
	       (dired-omit-toggle)))))
 (t
  (add-hook 'dired-mode-hook
	    (function
	     (lambda ()
	       (setq dired-omit-files-p t))))))
