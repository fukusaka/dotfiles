;;; 50php.el --

(defun my-php-mode-init ()

  ;; インデントモードの設定
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)

  ;; for Indentation of arrays
  ;; http://emacswiki.org/emacs/PhpMode
  ;;(c-set-offset 'arglist-close 0)
  ;;(defun ywb-php-lineup-arglist-intro (langelem)
  ;;  (save-excursion
  ;;    (goto-char (cdr langelem))
  ;;    (vector (+ (current-column) c-basic-offset))))
  ;;(defun ywb-php-lineup-arglist-close (langelem)
  ;;  (save-excursion
  ;;    (goto-char (cdr langelem))
  ;;    (vector (current-column))))
  ;;(c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
  ;;(c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close)

  )

(add-hook 'php-mode-hook 'my-php-mode-init)
