;; キーの設定

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

;;(define-key global-map "\C-j" 'goto-line)
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

(define-key global-map "\C-z\C-l" 'scratch)
(define-key global-map "\C-zl" 'scratch)


(if (string-match "^21" emacs-version)
    (progn
      (define-key global-map [home] 'beginning-of-buffer)
      (define-key global-map [end] 'end-of-buffer)
      ))
;;
;; 6枚ものフレームを同時生成、同時削除。
;;
(define-key global-map "\C-z51" 'moi::make-frame-3)
(define-key global-map "\C-z52" 'moi::make-frame-6)
(define-key global-map "\C-z50" 'moi::delete-frame-6)

(defun moi::make-frame (x y)
  (let* ((fpar (frame-parameters))
	 (bw   (cdr (assoc 'border-width fpar)))
	 (left (+ x bw (eval (cdr (assoc 'left fpar)))))
	 (top  (+ y bw (eval (cdr (assoc 'top fpar)))))
	 (frame (make-frame)))
    (sleep-for 0.03)
    (modify-frame-parameters frame `((top + ,top) (left + ,left)))
    (sleep-for 0.03)
    frame))

(defvar moi::make-frame-6-alist nil)

(defun moi::make-frame-3 ()
  (interactive)
  (if (not moi::make-frame-6-alist)
      (setq moi::make-frame-6-alist
	    (list
	     (moi::make-frame 1280 0)
	     (moi::make-frame 0 1024)
	     (moi::make-frame 1280 1024)
	     ))))

(defun moi::make-frame-6 ()
  (interactive)
  (if (not moi::make-frame-6-alist)
      (setq moi::make-frame-6-alist
	    (list
	     (moi::make-frame 1280 0)
	     (moi::make-frame -1280 0)
	     (moi::make-frame 0 1024)
	     (moi::make-frame 0 -1024)
	     (moi::make-frame 1280 1024)
	     (moi::make-frame -1280 1024)
	     ;;(moi::make-frame 1024 -768)
	     (moi::make-frame -1280 -1024)
	     ))))

(defun moi::delete-frame-6 ()
  (interactive)
  (while moi::make-frame-6-alist
    (delete-frame (car moi::make-frame-6-alist))
    (setq moi::make-frame-6-alist (cdr moi::make-frame-6-alist))))

;;
;; moi-skel-make.el
;;
(cond
 ((string-match "^20" emacs-version)
  (autoload 'moi::find-file "moi-skel-make")
  (global-set-key "\C-x\C-f" 'moi::find-file)
  (global-set-key "\C-z\C-f" 'moi::find-file)
  )
 )
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
;; Scratchよ永遠に！
;;
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

;;
;; wheel mouse
;;

(if (featurep 'xemacs)
    ;; XEmacs
    nil
  ;; Emacs
  (cond
   ((string-match "^20" emacs-version)
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
    ))
  )
