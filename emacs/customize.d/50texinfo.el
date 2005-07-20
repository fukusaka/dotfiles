;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Texinfo で日本語を使うようにする。
;;

;; Info がまともな動作するような設定
(setq Info-fontify-maximum-menu-size 50000)
;;(setq Info-default-directory-list
;;      (cons  (expand-file-name "~/info") Info-default-directory-list))

(defun info-file (file)
  (interactive "fInfo file:")
  (info file))

(add-hook 'Info-mode-hook
	  (function (lambda ()
		      (define-key Info-mode-map "\C-c\C-l"
			'Info-reload)
		      )))
(defun Info-reload ()
  (interactive)
  (if (eq major-mode 'Info-mode)
      (let ((file Info-current-file)
	    (node Info-current-node)
	    (lineno (count-lines (point-min) (point))))
	(kill-buffer "*info*")
	(info file)
	(Info-goto-node node)
	(forward-line lineno)
	)))

(add-hook 'texinfo-mode-hook
	  (function (lambda ()
		      (define-key texinfo-mode-map "\C-c\C-v"
			'texinfo-preview-buffer)
	   )))

(defun texinfo-preview-buffer ()
  (interactive)
  (let (filename)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^@setfilename[ \\t]+\\(.*\\)$" nil nil)
	  (setq filename (buffer-substring (match-beginning 1)
					   (match-end 1)))
	(error "Texinfo file needs an `@setfilename FILENAME' line.")
	)
      )
    
    (info (concat (file-name-directory buffer-file-name) filename))
    ))
