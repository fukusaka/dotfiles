;;
;; tramp 設定
;;

(when (<= emacs-major-version 22)
  (autoload 'tramp-compile "tramp-util")
  (define-key global-map "\M-c" 'tramp-compile)
  (define-key global-map "\C-zc" 'tramp-compile))

(eval-after-load "tramp"
  '(progn
     ;; NTEmacsでCygwinのsshを使う。
     ;; 必ず、fakecygpty を使うべし
     (when (and (eq system-type 'windows-nt)
                (executable-find "f_ssh"))
       (dolist (methods tramp-methods)
         (if (string-equal (cadr (assq 'tramp-login-program methods)) "ssh")
             (setcdr (assq 'tramp-login-program methods) '("f_ssh")))))
     (require 'tramp-cmds)
     ))

(require 'tramp)

(setq tramp-verbose 0)

(setq tramp-default-method "ssh")

;; tramp-default-proxies-alist
;; (ホスト ユーザ 経由)
;;tramp-default-method-alist
;; (ホスト ユーザ 方法)

;; リモート接続での root アクセスする場合、マルチ接続に変換して sudo or su を呼び出す
(add-to-list 'tramp-default-proxies-alist `(,tramp-local-host-regexp nil nil))
(add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))

;; root アクセスには常に sudo を使う
(add-to-list 'tramp-default-method-alist '(nil "\\`root\\'" "sudo"))


;; 特定のホスト群には途中を経由する
;;(add-to-list 'tramp-default-proxies-alist '("\\`far-host\\'" nil "/ssh:fukusaka@proxy-host:") t)
