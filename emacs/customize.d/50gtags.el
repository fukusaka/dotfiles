;; gtags
(autoload 'gtags-mode "gtags" "" t)

(setq gtags-mode-hook
      '(lambda ()
         (define-key gtags-mode-map "\M-." 'gtags-find-tag)
         (define-key gtags-mode-map "\M-*" 'gtags-pop-stack)
         (define-key gtags-mode-map "\M-r" 'gtags-find-rtag)
         (define-key gtags-mode-map "\M-s" 'gtags-find-symbol)
         ))

(add-hook 'c-mode-common-hook
	  '(lambda ()
	     (gtags-mode 1)
	     ))
