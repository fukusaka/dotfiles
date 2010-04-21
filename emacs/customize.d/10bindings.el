;;
;; キーの設定
;;

;;(setq my-prefix-keys '("\C-z" "\C-d"))
(setq my-prefix-keys '("\C-z"))

(dolist (pkey my-prefix-keys)
  (global-unset-key pkey))

(defmacro my-define-global-key (key def)
  `(dolist (pkey my-prefix-keys)
     (define-key global-map (concat pkey ,key) ,def)))

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
  (my-define-global-key "\C-v" 'vc-dir))

;; for man
;;(define-key global-map "\M-m" 'man)
(my-define-global-key "m" 'man)

;; for program
(define-key global-map "\M-c" 'compile)
(my-define-global-key "c" 'compile)
;;(my-define-global-key "d" 'gdb)

;; for grep
(my-define-global-key "g" 'grep-buffers)
(my-define-global-key "\C-g" 'grep)
(my-define-global-key "\M-g" 'grep-find)


;; トグル動作
;; 現在バッファ名が run-buffer-name であれば現在バッファを最後尾に下げ、
;; でなければ、run-comanndを実行する。
(defun toggle-run-mode (run-command &optional run-buffer-name)
  (if (let ((toggle-list
             (cond
              ((stringp run-buffer-name) (list run-buffer-name))
              ((consp run-buffer-name) run-buffer-name))))
        (member (buffer-name) toggle-list))
      (switch-to-buffer (prog1 (other-buffer (current-buffer))
			  (bury-buffer (current-buffer))))
    (eval (list run-command))))

(defmacro def-toggle-run (command buffername)
  `(defun ,(intern (concat "toggle-" (symbol-name command))) ()
     (interactive)
     (toggle-run-mode (quote ,command) ,buffername)))

(defvar default-toggle-run-command 'shell)

(defvar default-toggle-run-mode-list
  '("*shell*"
    "*eshell*"
    "*tex-shell*"))

(defun toggle-default ()
  (interactive)
  (toggle-run-mode default-toggle-run-command default-toggle-run-mode-list))

(my-define-global-key "z" 'toggle-default)
(my-define-global-key "\C-z" 'toggle-default)

;; ワンタッチでシェルに行ける
;; トルグにしたいもし
;;
(def-toggle-run shell "*shell*")
(def-toggle-run eshell "*eshell*")

(my-define-global-key "s" 'toggle-shell)
(my-define-global-key "\C-s" 'toggle-shell)

(my-define-global-key "e" 'toggle-eshell)
(my-define-global-key "\C-e" 'toggle-eshell)

;;
;; Scratchよ永遠に！
;;
(defvar scratch-buffer-name "*scratch*")

(defun scratch ()
  (interactive)
  (unless (get-buffer scratch-buffer-name)
    (get-buffer-create scratch-buffer-name)
    (save-excursion
      (set-buffer scratch-buffer-name)
      (if (eq major-mode 'fundamental-mode)
	  (funcall initial-major-mode))
      (and initial-scratch-message
	   (insert initial-scratch-message))
      (set-buffer-modified-p nil)))
  (switch-to-buffer (get-buffer scratch-buffer-name)))

(def-toggle-run scratch scratch-buffer-name)

(my-define-global-key "l" 'toggle-scratch)
(my-define-global-key "\C-l" 'toggle-scratch)
