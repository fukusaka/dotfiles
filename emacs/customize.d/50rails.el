;;
;; Rails 
;;

(when (>= emacs-major-version 23)

  (defun try-complete-abbrev (old)
    (if (expand-abbrev) t nil))

  (setq hippie-expand-try-functions-list
        '(try-complete-abbrev
          try-complete-file-name
          try-expand-dabbrev))

  (setq rails-ws:default-server-type "webrick")

  (require 'rails nil t)
  )
