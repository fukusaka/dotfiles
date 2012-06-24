;;(when (>= emacs-major-version 23)
;;
;;  (require 'anything-config)
;;  (anything-iswitchb-setup)
;;  (global-set-key "\C-z\C-a" 'anything)
;;  (define-key anything-map "\C-p" 'anything-previous-line)
;;  (define-key anything-map "\C-n" 'anything-next-line)
;;  (define-key anything-map "\C-v" 'anything-next-page)
;;  (define-key anything-map "\M-v" 'anything-previous-page)
;;
;;  (setq anything-sources
;;        '(anything-c-source-buffers
;;          anything-c-source-file-name-history
;;          anything-c-source-imenu
;;          anything-c-source-locate
;;          anything-c-source-man-pages
;;          anything-c-source-occur
;;          anything-c-source-recentf
;;          ))
;;  )
