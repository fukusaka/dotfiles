;;
;; tramp 設定
;;

(when (<= emacs-major-version 22)
  (autoload 'tramp-compile "tramp-util")
  (define-key global-map "\M-c" 'tramp-compile)
  (define-key global-map "\C-zc" 'tramp-compile))

(cond

 ((eq system-type 'windows-nt)

  (setq tramp-default-user "shoichi")
  ;; minibufferで入力するときユーザ名の最後の１文字が落る
  ;; login 処理に不具合い？かなぁ。
  (setq tramp-debug-buffer t)
  (setq tramp-verbose 10)

  (cond
   ((executable-find "plink")
    (setq tramp-default-method "plink")
    (setq tramp-completion-without-shell-p t)
    (modify-coding-system-alist 'process "plink" 'utf-8-unix)
    ))
  )

 ;; その他
 (t
  (setq tramp-default-method "ssh")))
