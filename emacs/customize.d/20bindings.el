;; キーの設定

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

(define-key global-map "\C-j" 'goto-line)
(define-key global-map "\M-c" 'compile)
(define-key global-map "\M-m" 'man)
(define-key global-map "\C-xm" 'mew)

(global-unset-key "\C-z")
(define-key global-map "\C-zz" 'toggle-shell-default)
(define-key global-map "\C-z\C-z" 'toggle-shell-default)
(define-key global-map "\C-zs" 'toggle-shell)
(define-key global-map "\C-z\C-s" 'toggle-shell)
(define-key global-map "\C-zr" 'toggle-scheme)
(define-key global-map "\C-z\C-r" 'toggle-scheme)

(define-key global-map "\C-zj" 'goto-line)
(define-key global-map "\C-zc" 'compile)
(define-key global-map "\C-zd" 'gdb)
(define-key global-map "\C-zm" 'man)

;;
;; moi-skel-make.el
;;
(autoload 'moi::find-file "moi-skel-make")

(global-set-key "\C-x\C-f" 'moi::find-file)
(global-set-key "\C-z\C-f" 'moi::find-file)



;; ワンタッチでシェルに行ける
;; トルグにしたいもし
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
    "*tex-shell*"
    ))

;; 明示的に、toggle-run-mode を使わなければ、toggle-run-mode-list を
;; 使う。
(defun toggle-run-mode (run-command &optional toggle-run-mode)
  (if (let ((mode-list
	     (if (stringp toggle-run-mode)
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
;; wheel mouse
;;

(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

(defun up-one () (interactive) (scroll-up 1))
(defun down-one () (interactive) (scroll-down 1))
(global-set-key [S-mouse-4] 'down-one)
(global-set-key [S-mouse-5] 'up-one)

(defun up-a-lot () (interactive) (scroll-up))
(defun down-a-lot () (interactive) (scroll-down))
(global-set-key [C-mouse-4] 'down-a-lot)
(global-set-key [C-mouse-5] 'up-a-lot)

