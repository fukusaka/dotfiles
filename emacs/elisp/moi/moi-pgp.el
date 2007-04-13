

(defvar moi-pgp-key-id "Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>")

(defconst moi-pgp-msg-enter-pass "Enter pass phrase: ")
(defconst moi-pgp-msg-reenter-pass "Enter pass phrase: ")
(defconst moi-pgp-msg-just-a-moment "Just a moment\\.\\.\\.")

(defconst moi-pgp-prompt-enter-pass   "Enter pass phrase: ")
(defconst moi-pgp-prompt-reenter-pass "Re-enter pass phrase: ")
(defconst moi-pgp-prompt-just-a-moment "Just a moment...")

(defconst moi-pgp-msg-begin "-----BEGIN PGP MESSAGE-----")
(defconst moi-pgp-msg-end "-----END PGP MESSAGE-----")

(defun moi-pgp-encrypt (buffer)
  (interactive "Bbuffer:")
  (message "PGP encrypting ... ")
  (setq moi-pgp-running 'encrypt)
  (let ((buffer2 (generate-new-buffer "temp"))
	process)
    (setq process (start-process "Moi::PGP encrypt"
				 buffer2
				 "pgp"
				 "+language=en"
				 "-fea"
				 "-o" "-"
				 moi-pgp-key-id))
    (set-process-filter process 'moi-pgp-encrypt-filter1)
    (set-process-sentinel process 'moi-pgp-process-sentinel)
    (set-buffer buffer)
    (process-send-region process (point-min) (point-max))
    (process-send-eof process)
    (process-send-eof process)
    (switch-to-buffer buffer2)

    (while moi-pgp-running
      (sit-for 1)
      (discard-input))
    (message "PGP encrypting ... done")
    (insert "\n")
    (insert "Local Variables:\n")
    (insert "mode:moi-pgp\n")
    (insert "End:\n")    
    ))

(defun moi-pgp-encrypt-filter1 (process string)
  (cond
   ((string-match moi-pgp-msg-begin string)
    (moi-pgp-process-filter process string)
    (set-process-filter process 'moi-pgp-process-filter))))


(defun moi-pgp-decrypt (s-buffer d-buffer)
  (message "PGP decrypting ... ")
  (setq moi-pgp-running 'decrypt)
  (setq moi-pgp-wait-count 0)
  (let (process)
    (setq process (start-process "Moi::PGP decrypt"
				 d-buffer
				 "pgp"
				 "+language=en"
				 "-f"
				 "-o" "-"))
    (set-process-filter process 'moi-pgp-decrypt-filter1)
    (set-process-sentinel process 'moi-pgp-process-sentinel)
    (save-excursion
      (set-buffer s-buffer)
      (process-send-region process (point-min) (point-max))
      (process-send-eof process)
      (process-send-eof process))
    
    (while moi-pgp-running
      (sit-for 1)
      (discard-input))

    (message "PGP decrypting ... done")

    (save-excursion
      (set-buffer d-buffer)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (set-auto-mode))
    ))

(defun moi-pgp-process-sentinel (process event)
  (setq moi-pgp-running nil)
  )

(defun moi-pgp-decrypt-filter1 (process string)
  ;; (moi-pgp-process-filter process string)
  (cond
   ((string-match moi-pgp-msg-just-a-moment string)
    (message moi-pgp-prompt-just-a-moment))
   ((string-match moi-pgp-msg-enter-pass string)
    (process-send-string
     process
     (format "%s\n" (moi-input-passwd  moi-pgp-prompt-enter-pass))))
   ((string= "." string)
    (setq moi-pgp-wait-count (+ 1 moi-pgp-wait-count))
    (if (= 3 moi-pgp-wait-count)
	(set-process-filter process 'moi-pgp-process-filter))
    )))

(defun moi-pgp-process-filter (proc string)
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(let (moving)
	  (set-buffer (process-buffer proc))
	  (setq moving (= (point) (process-mark proc)))
	  (save-excursion
	    ;; テキストを挿入し、プロセス・マーカーを移動します。
	    (goto-char (process-mark proc))
	    (insert string)
	    (set-marker (process-mark proc) (point)))
	  (if moving (goto-char (process-mark proc))))
      (set-buffer old-buffer))))


(defun moi-input-passwd (prompt)
  (let ((pass nil)
	(c 0)
	(echo-keystrokes 0)
	(ociea cursor-in-echo-area)
	(inhibit-input-event-recording t))
    (unwind-protect
	(progn
	  (setq cursor-in-echo-area 1)
	  (while (and (/= c ?\r) (/= c ?\n) (/= c ?\e) (/= c 7)) ;; ^G
	    (message "%s%s"
		     prompt
		     (make-string (length pass) ?.))
	    (setq c (read-char-exclusive))
	    (if (char-equal c ?\C-u)
		(setq pass "")
	      (if (and (/= c ?\b) (/= c ?\177))
		  (setq pass (concat pass (char-to-string c)))
		(if (> (length pass) 0)
		    (setq pass (substring pass 0 -1))))))
	  (setq cursor-in-echo-area -1)
	  )
      (setq cursor-in-echo-area ociea)
      nil)
    (message "")
    (sit-for 0)
    (substring pass 0 -1)
    ))

(defun moi-pgp-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Moi-PGP2")
  (setq major-mode 'moi-pgp-mode)
  (let ((temp-buffer (generate-new-buffer "temp")))
    (save-excursion
      (moi-pgp-decrypt (current-buffer) temp-buffer))
    (goto-char (point-max))
    (let ((p (point)))
      (insert-buffer-substring temp-buffer)
      (set-buffer-modified-p nil)
      (kill-buffer temp-buffer)
      (narrow-to-region p (point-max))
      (goto-char (point-min)))
    (set-auto-mode)
    (buffer-read-only)
    ))
