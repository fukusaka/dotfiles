;;
;; compile-mode
;;
(setq compilation-ask-about-save nil)
(setq compilation-window-height 20)

;;(which-function-mode)

(setq vc-follow-symlinks t)


(add-hook 'c-mode-common-hook
          '(lambda ()
	     (c-set-style "bsd")))

(setq cperl-indent-level 4)

;;;(defadvice cd (around remote-cd (path))
;;;  (progn
;;;    (message path)
;;;    ad-do-it))
;;;
;;;(defadvice compilation-start (around ad-compilation-remote-start-ext activate)
;;;  (progn
;;;    (ad-activate 'cd)
;;;    ad-do-it
;;;    (ad-deactivate 'cd)))
