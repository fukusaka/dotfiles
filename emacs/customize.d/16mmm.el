;;
;; Multiple Major Modes 設定
;;

(setq my-use-mmm nil)

(when my-use-mmm

  (require 'mmm-mode)
  (require 'mmm-sample)
  (require 'mmm-vars)

  (setq mmm-global-mode 'maybe)
  (setq mmm-submode-decoration-level 2)

  (set-face-background 'mmm-declaration-submode-face "lightcyan")
  (set-face-background 'mmm-code-submode-face "oldlace")

;;  (defun save-mmm-c-locals ()
;;      (with-temp-buffer
;;        (php-mode)
;;        (dolist (v (buffer-local-variables))
;;          (when (string-match "\\`c-" (symbol-name (car v)))
;;            (unless (assoc (car v) mmm-save-local-variables)
;;              (message "%s" (car v)))
;;            (add-to-list 'mmm-save-local-variables
;;                         `(,(car v) nil ,mmm-c-derived-modes))))))
;;  (save-mmm-c-locals)


  (mmm-add-mode-ext-class nil "\\.html?\\'" 'embedded-css)
  (mmm-add-mode-ext-class nil "\\.html?\\'" 'html-js)

  (mmm-add-mode-ext-class nil "\\.php\\'" 'embedded-css)
  (mmm-add-mode-ext-class nil "\\.php\\'" 'html-js)
  (mmm-add-mode-ext-class nil "\\.php\\'" 'html-php)

  (mmm-add-classes
   '((html-tt
      :submode tt-mode
      :face mmm-code-submode-face
      :front "\\[%"
      :back "%\\]"
      )))

  (mmm-add-mode-ext-class nil "\\.tt\\'" 'embedded-css)
  (mmm-add-mode-ext-class nil "\\.tt\\'" 'html-js)
  (mmm-add-mode-ext-class nil "\\.tt\\'" 'html-tt)
  )
