;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mew                                                                ;;
;;   メールリーダー Mew                                               ;;
;;   M-x mew で起動します                                             ;;
;;                                                                    ;;
;; mule2.3(emacs19) --> mew ver 1.70                                  ;;
;; emacs20.3        --> mew ver 1.93                                  ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "mew")

(cond
 ;; Ver.19 の場合
 ((string-match "^1\\.7" mew-version-number)
  (setq load-path
	(append '("/usr/local/share/emacs/site-lisp/mew-1.70")
		load-path))
  (setq exec-path (cons "/usr/local/bin/mh" exec-path))
  (setq exec-path (cons "/usr/local/lib/mh" exec-path))
;; なんだかなぁ。
  (add-hook 'mew-init-hook
	    (function
	     (lambda ()
	       (add-hook 'kill-emacs-hook
			 (function mew-mark-process-all-folders)))))
  )
 
;; From: に余計なモノ(localhost 名等)が付いたりする場合は、
;; xxxx の部分にドメイン名やメールアドレス等を指定します
;(setq mew-mail-domain-list
;      '("xxxx.xxxx.xxxx.xxxx"))
 ((string-match "^1\\.9[0-9]" mew-version-number) 
  (autoload 'mew "mew" nil t)
  (autoload 'mew-send "mew" nil t)

  (setq mew-use-overlay-keymap nil)

  (setq mew-fcc "+outbox")
  (setq mew-ask-subject t)
  (setq mew-demo nil)
  (setq mew-signature-insert-last t)

  (setq mew-use-highlight-cursor-line nil)
  (setq mew-use-highlight-body t)

  (setq mew-cite-prefix "> ")
  (setq mew-cite-fields '("From:" "Subject:" "Date:" "Message-ID:"))
  (setq mew-cite-format "> From: %s\n> Subject: %s\n> Date: %s\n> Message-ID: %s\n\n")

  (add-hook 'mew-summary-mode-hook 
	    (function
	     (lambda ()
	       (define-key mew-summary-mode-map "b" 'mew-summary-auto-refile)
	       )))
  )
 ((string-match "^2" mew-version-number) 
  (if (boundp 'read-mail-command)
      (setq read-mail-command 'mew))
  (autoload 'mew-user-agent-compose "mew" nil t)
  (if (boundp 'mail-user-agent)
      (setq mail-user-agent 'mew-user-agent))
  (if (fboundp 'define-mail-user-agent)
      (define-mail-user-agent
	'mew-user-agent
	'mew-user-agent-compose
	'mew-draft-send-message
	'mew-draft-kill
	'mew-send-hook))
  ;;(setq mew-mailbox-type 'mbox)
  ;;(setq mew-mbox-command "incm")
  ;;(setq mew-mbox-command-arg "-d /var/spool/mail/fukusaka")
  )
 )
;; Ver.1.70 の場合

;; mew で色を付ける設定
(if window-system
    (cond
     ;; mule2.3 の mew-1.70 の場合
     ((string-match "^19" emacs-version)
      (add-hook 'mew-message-hook 'hilit-rehighlight-buffer)
      (hilit-set-mode-patterns 
       '(mew-message-mode)
       '(("^Subject:.*$" nil msg-subject)
	 ("^From:.*$" nil msg-from)
	 ("^X-.*:.*$" nil msg-quote)
	 ("^>.*$" nil msg-quote)
	 ("^[A-Za-z][A-Za-z0-9-]+:" nil msg-header)))
      (add-hook 'mew-draft-mode-hook 'hilit-rehighlight-buffer)
      ;;(defun mew-summary-nkf ()
      ;;  (shell-command-on-region (point-min) (point-max) "nkf -m" t)
      ;;  )
      ;;(add-hook 'mew-summary-mode-hook 'mew-summary-nkf)
      (hilit-set-mode-patterns 
       '(mew-draft-mode)
       '(("^Subject:.*$" nil msg-subject)
	 ("^From:.*$" nil msg-from)
	 ("^>.*$" nil msg-quote)
	 ("^[A-Za-z][A-Za-z0-9-]+:" nil msg-header)))
      (add-hook 'mew-summary-mode-hook 'hilit-rehighlight-buffer)
      (defun hilit-search-mew-summary-name (pend)
	(if (re-search-forward "^[ 0-9][ 0-9][ 0-9][0-9]..[0-9][0-9]/[0-9][0-9] \\(.................\\).*$" nil t nil)
	    (let ((str (buffer-substring (match-beginning 1) (match-end 1)))) 
	      ;; これは17文字
	      ;;  2 Byte 文字が 2 columns であるとした
	      ;; 場合の、17 columns を抜き出す
	      (setq str
		    (code-convert-string
		     (substring
		      (code-convert-string str *internal* *sjis*)
		      0 17)
		     *sjis* *internal*))
	      ;; point は *internal* での長さ
	      (cons (match-beginning 1) (+ (match-beginning 1) (length str))))))
      (hilit-set-mode-patterns
       '(mew-summary-mode)
       '((hilit-search-mew-summary-name nil ForestGreen)
	 ("[- M][0-9][0-9]/[0-9][0-9] " nil Red)
	 ("^[ 0-9][ 0-9][ 0-9][0-9]" nil Blue)
	 ("<<.*$" nil Purple))))

     ;; emacs20.3 の mew-1.93 の場合
     ;; ~/.im/Config -> Form=%+4n %m%d %-17a %S<<%b
     ;; なんかでかすぎ! (mewの色づけ処理とあまり整合性がない)
     (t ;(string-match "^20" emacs-version)
      (require 'moi-util)
      (defvar  mew-summary-number-face 'mew-summary-number-face)
      (defface mew-summary-number-face
	'((((class color) (background light)) (:foreground "Blue"))
	  (((class color) (background dark)) (:foreground "LightSkyBlue"))
	  )
	nil)

      (defvar  mew-summary-date-face 'mew-summary-date-face)
      (defface mew-summary-date-face
	'((((class color) (background light)) (:foreground "Red"))
	  (((class color) (background dark)) (:foreground "Pink"))
	  )
	nil)

      (defvar  mew-summary-from-face 'mew-summary-from-face)
      (defface mew-summary-from-face
	'((((class color) (background light)) (:foreground "ForestGreen"))
	  (((class color) (background dark)) (:foreground "PaleGreen"))
	  )
	nil)

      (defvar  mew-summary-body-face 'mew-summary-body-face)
      (defface mew-summary-body-face
	'((((class color) (background light)) (:foreground "Purple"))
	  (((class color) (background dark)) (:foreground "Cyan"))
	  )
	nil)

      (defvar  mew-summary-mark-face 'mew-summary-mark-face)
      (defface mew-summary-mark-face
	'((((class color) (background light)) (:foreground "ForestGreen"))
	  (((class color) (background dark)) (:foreground "PaleGreen"))
	  )
	nil)

      (defvar  mew-summary-mine-face 'mew-summary-mine-face)
      (defface mew-summary-mine-face
	'((((class color) (background light)) (:foreground "Blue"))
	  (((class color) (background dark)) (:foreground "LightSkyBlue"))
	  )
	nil)

      (defvar mew-summary-number-regexp "^[ 0-9][ 0-9][ 0-9][ 0-9][0-9]")
      (defvar mew-summary-date-regexp   "[- MS][0-9][0-9]/[0-9][0-9]")
      (defvar mew-summary-from-regexp   'mew-summary-from-search)
      (defvar mew-summary-body-regexp   "||.*$")
      (defvar mew-summary-mark-regexp   'mew-summary-mark-search)
      (defvar mew-summary-mine-regexp   "^.    .*$")

      (defun mew-summary-from-search (pend)
	(mew-summary-some-search pend 13 27))

      (defun mew-summary-some-search (pend sta-pos &optional end-pos)
	(if (re-search-forward "^..*$" pend t nil)
	    (save-excursion
	      (let ((str (buffer-substring (match-beginning 0) (match-end 0)))
		    sta end)
		(goto-char (match-beginning 0))
		(forward-char (length (moi-substring-width str sta-pos)))
		(setq sta (point-marker))
		(if end-pos
		    (progn
		      (goto-char (match-beginning 0))
		      (forward-char (length (moi-substring-width str end-pos))))
		  (goto-char (match-end 0)))				      
		(setq end (point-marker))
		(store-match-data (list sta end))
		)
	      (point))
	  nil
	  ))

      (defun mew-summary-mark-search (pend)
	(if (re-search-forward "^[ 0-9][ 0-9][ 0-9][ 0-9][0-9][^ ]" pend t nil)
	    (save-excursion
	      (let (sta end)
		(beginning-of-line)
		(setq sta (point-marker))
		(end-of-line)
		(setq end (point-marker))
		(store-match-data (list sta end))
		)
	      (point))
	  nil
	  ))

      (setq mew-summary-font-lock-keywords
	    (list
	     (cons mew-summary-mark-regexp    'mew-summary-mark-face)
	     (cons mew-summary-number-regexp  'mew-summary-number-face)
	     (cons mew-summary-date-regexp    'mew-summary-date-face)
	     (cons mew-summary-from-regexp    'mew-summary-from-face)
	     (cons mew-summary-body-regexp    'mew-summary-body-face)
	     (cons mew-summary-mine-regexp    'mew-summary-mine-face)
	     )
	    )

      (add-hook 'mew-summary-mode-hook 
		(function
		 (lambda ()
		   (progn
		     (make-local-variable 'font-lock-defaults)
		     (setq font-lock-defaults
			   '(mew-summary-font-lock-keywords t))
		     (turn-on-font-lock)
		     ))))
      (add-hook 'mew-summary-inc-sentinel-hook 'font-lock-fontify-buffer)
      (add-hook 'mew-summary-scan-sentinel-hook 'font-lock-fontify-buffer)
      )
     ))

