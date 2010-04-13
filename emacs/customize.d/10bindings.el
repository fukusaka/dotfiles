;;
;; キーの設定
;;

;;(setq my-prefix-keys '("\C-z" "\C-d"))
(setq my-prefix-keys '("\C-z"))

(dolist (pkey my-prefix-keys)
  (global-unset-key pkey))

(defun my-define-global-key (key def)
  (dolist (pkey my-prefix-keys)
    (define-key global-map (concat pkey key) def)))

(my-define-global-key "j" 'goto-line)
(my-define-global-key "\C-j" 'goto-line)

;; for find-file
(my-define-global-key "f" 'find-file)
(my-define-global-key "\C-f" 'find-file)

;; for dired
(my-define-global-key "d" 'dired-jump)
(my-define-global-key "\C-d" 'dired-jump)

;; for dired
(my-define-global-key "d" 'dired-jump)
(my-define-global-key "\C-d" 'dired-jump)

;; for vc-dir
(when (>= emacs-major-version 23)
  (my-define-global-key "v" 'vc-dir)
  (my-define-global-key "\C-v" 'vc-dir)
  )

;; for man
(define-key global-map "\M-m" 'man)
(my-define-global-key "m" 'man)

;; for program
(define-key global-map "\M-c" 'compile)
(my-define-global-key "c" 'compile)
;;(my-define-global-key "d" 'gdb)

;; for grep
(my-define-global-key "g" 'grep-buffers)
(my-define-global-key "\C-g" 'grep)
(my-define-global-key "\M-g" 'grep-find)

;; ワンタッチでシェルに行ける
;; トルグにしたいもし
;;
(my-define-global-key "z" 'toggle-shell-default)
(my-define-global-key "\C-z" 'toggle-shell-default)

(my-define-global-key "s" 'toggle-shell)
(my-define-global-key "\C-s" 'toggle-shell)

(my-define-global-key "e" 'toggle-eshell)
(my-define-global-key "\C-e" 'toggle-eshell)

(defun toggle-shell-default ()
  (interactive)
  (toggle-run-mode '(shell)))

(defun toggle-shell ()
  (interactive)
  (toggle-run-mode '(shell) "*shell*"))

(defun toggle-eshell ()
  (interactive)
  (toggle-run-mode '(eshell) "*eshell*"))

(defvar toggle-run-mode-list
  '("*shell*"
    "*eshell*"
    "*tex-shell*"))

;; 明示的に、toggle-run-mode を使わなければ、toggle-run-mode-list を
;; 使う。
(defun toggle-run-mode (run-command &optional toggle-run-mode)
  (if (let ((mode-list (if (stringp toggle-run-mode)
			   (list toggle-run-mode)
			 toggle-run-mode-list)))
	(eval (cons 'or
		    (mapcar (function
			     (lambda (run-mode)
			       (string= (buffer-name) run-mode)))
			    mode-list))))
      (switch-to-buffer (prog1 (other-buffer (current-buffer))
			  (bury-buffer (current-buffer))))
    (eval run-command)))

;;
;; Scratchよ永遠に！
;;
(my-define-global-key "l" 'scratch)
(my-define-global-key "\C-l" 'scratch)

(defun scratch ()
  (interactive)
  (if (get-buffer "*scratch*") nil
    (get-buffer-create "*scratch*")
    (save-excursion
      (set-buffer "*scratch*")
      (if (eq major-mode 'fundamental-mode)
	  (funcall initial-major-mode))
      (and initial-scratch-message
	   (insert initial-scratch-message))
      (set-buffer-modified-p nil))
    )
  (switch-to-buffer (get-buffer "*scratch*")))
