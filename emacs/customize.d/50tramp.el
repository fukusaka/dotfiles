;;
;; tramp 設定
;;

(when (<= emacs-major-version 22)
  (autoload 'tramp-compile "tramp-util")
  (define-key global-map "\M-c" 'tramp-compile)
  (define-key global-map "\C-zc" 'tramp-compile))

;; NTEmacsでCygwinのsshを使う。
;; 必ず、fakecygpty を使うべし
(eval-after-load "tramp"
  '(when (and (eq system-type 'windows-nt)
              (executable-find "f_ssh"))
     (require 'tramp)
     (dolist (methods tramp-methods)
       (if (string-equal (cadr (assq 'tramp-login-program methods)) "ssh")
           (setcdr (assq 'tramp-login-program methods) '("f_ssh"))))))

;; ローカルアクセスには直接接続する
(add-to-list 'tramp-default-proxies-alist '("localhost" nil nil) t)
(add-to-list 'tramp-default-proxies-alist '((regexp-quote (system-name)) nil nil) t)

;; root アクセスは常に sudo を使う
(add-to-list 'tramp-default-method-alist '(nil "\\`root\\'" "sudo") t)
(add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:") t)

;; 特定のホスト群には途中を経由する
;;(add-to-list 'tramp-default-proxies-alist '("\\`far-host\\'" nil "/ssh:fukusaka@proxy-host:") t)
