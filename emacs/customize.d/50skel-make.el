;;
;; moi-skel-make.el
;;

(cond
 ((string-match "^20" emacs-version)
  (autoload 'moi::find-file "moi-skel-make")
  (global-set-key "\C-x\C-f" 'moi::find-file)
  (global-set-key "\C-z\C-f" 'moi::find-file)
  )
 )

