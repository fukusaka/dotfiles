;;
(defun my-python-mode-init ()
  ;;(require 'pycomplete)         ;; pymacs を使わないのでまだ保留
  ;;(require 'lambda-mode)        ;; λはなんか見づらい、、、
  ;;(lambda-mode)
  )

(add-hook 'python-mode-hook 'my-python-mode-init)

(setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))
