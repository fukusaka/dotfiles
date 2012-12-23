;;; 50php.el --

(defun my-php-mode-init ()

  ;; インデントモードの設定
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode t)

  )

(add-hook 'php-mode-hook 'my-php-mode-init)
