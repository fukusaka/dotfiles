;;
(defun my-python-mode-init ()

  ;; M-x compile でスクリプトを実行
  (make-local-variable 'compile-command)
  (setq compile-command
	(concat "python " (buffer-file-name)))

  )

(add-hook 'python-mode-hook 'my-python-mode-init)
