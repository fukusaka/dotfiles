;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Texinfo �����ܸ��Ȥ��褦�ˤ��롣
;;

;; Info ���ޤȤ��ư���褦������
(setq Info-fontify-maximum-menu-size 50000)
;;(setq Info-default-directory-list
;;      (cons  (expand-file-name "~/info") Info-default-directory-list))

(defun info-file (file)
  (interactive "fInfo file:")
  (info file))

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

(cond
 ((string-match "^20" emacs-version)
  ;; Info ���˿����դ��Ƥ��ʤ��ʤ��!!!
  ;; ����� font-lock ���Ȥ����ɤ��Ǥʤ��ʤ�ơ�����
  (defface info-node
    '((t (:bold t :italic t)))
    nil)
  
  (defface info-menu-5
    '((t (:underline t)))
    nil)
  
  (defface info-xref
    '((((class color) (background light)) (:foreground "Blue" :bold t))
      (((class color) (background dark)) (:foreground "LightSkyBlue" :bold t))
      (t (:bold t :italic t))
      )
    nil)

  )
 )
