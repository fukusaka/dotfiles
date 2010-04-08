;;
;; tramp 設定
;;

(when (<= emacs-major-version 22)
  (autoload 'tramp-compile "tramp-util")
  (define-key global-map "\M-c" 'tramp-compile)
  (define-key global-map "\C-zc" 'tramp-compile))

(setq tramp-default-method "sshx")

;; NTEmacsでCygwinのsshを使う。
;; 必ず、fakecygpty を使うべし
(when (and (eq system-type 'windows-nt)
           (executable-find "f_ssh"))
  (require 'tramp)
  (dolist (methods tramp-methods)
    (if (string-equal (cadr (assq 'tramp-login-program methods)) "ssh")
        (setcdr (assq 'tramp-login-program methods) '("f_ssh")))))
