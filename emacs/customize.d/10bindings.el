;; キーの設定

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>
;; $Id$

(global-unset-key "\C-z")
(define-key global-map "\C-zj" 'goto-line)

(when (>= emacs-major-version 21)
  (define-key global-map [home] 'beginning-of-buffer)
  (define-key global-map [end] 'end-of-buffer)
  )

;; for man
(define-key global-map "\M-m" 'man)
(define-key global-map "\C-zm" 'man)

(if (featurep 'xemacs)
    (defalias 'man 'manual-entry))

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

(define-key global-map "\C-zr" 'toggle-scheme)
(define-key global-map "\C-z\C-r" 'toggle-scheme)

(defun toggle-shell-default ()
  (interactive)
  (toggle-run-mode '(shell)))

(defun toggle-shell ()
  (interactive)
  (toggle-run-mode '(shell) "*shell*"))

(defun toggle-scheme ()
  (interactive)
  (toggle-run-mode '(run-scheme "/usr/bin/guile") "*scheme*"))

(defvar toggle-run-mode-list
  '("*shell*"
    "*scheme*"
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
    (eval run-command)
    ))

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
  (switch-to-buffer (get-buffer "*scratch*"))
  )

;; for wheel mouse
(cond
 ;; XEmacs
 ((featurep 'xemacs) nil)

 ;; Emacs
 ((>= emacs-major-version 20)

  (if (fboundp 'mouse-wheel-mode)
      (mouse-wheel-mode)

    (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 5)))
    (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 5)))
    
    (global-set-key [S-mouse-4] '(lambda () (interactive) (scroll-down 1)))
    (global-set-key [S-mouse-5] '(lambda () (interactive) (scroll-up 1)))
    
    (global-set-key [C-mouse-4] '(lambda () (interactive) (scroll-down)))
    (global-set-key [C-mouse-5] '(lambda () (interactive) (scroll-up)))
    )
  ))
