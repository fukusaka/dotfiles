;;
;; キーの設定
;;

(global-unset-key "\C-z")
(define-key global-map "\C-zj" 'goto-line)
(define-key global-map "\C-z\C-j" 'goto-line)

;; for man
(define-key global-map "\M-m" 'man)
(define-key global-map "\C-zm" 'man)

;; for program
(define-key global-map "\M-c" 'compile)
(define-key global-map "\C-zc" 'compile)
(define-key global-map "\C-zd" 'gdb)

;; ワンタッチでシェルに行ける
;; トルグにしたいもし
;;
(define-key global-map "\C-zz" 'toggle-shell-default)
(define-key global-map "\C-z\C-z" 'toggle-shell-default)

(define-key global-map "\C-zs" 'toggle-shell)
(define-key global-map "\C-z\C-s" 'toggle-shell)

(define-key global-map "\C-ze" 'toggle-eshell)
(define-key global-map "\C-z\C-e" 'toggle-eshell)

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
(define-key global-map "\C-zl" 'scratch)
(define-key global-map "\C-z\C-l" 'scratch)

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
