;;
;; キーの設定
;;

(eval-when-compile (require 'cl))

;;(setq my-prefix-keys '("\C-z" "\C-d"))
(setq my-prefix-keys '("\C-z"))

(defvar my-prefix-map (make-sparse-keymap))

(dolist (pkey my-prefix-keys)
  (global-unset-key pkey)
  (define-key global-map pkey my-prefix-map))

(defmacro my-prefix-set-key (key def)
  `(define-key my-prefix-map ,key ,def))

;; my-prefix + 5
(defvar my-prefix-5-map (make-sparse-keymap))
(my-prefix-set-key "5" my-prefix-5-map)

;; jump
(my-prefix-set-key "j" 'goto-line)
(my-prefix-set-key "\C-j" 'goto-line)

;; for find-file
(my-prefix-set-key "f" 'find-file)
(my-prefix-set-key "\C-f" 'find-file)

;; for dired
(my-prefix-set-key "d" 'dired-jump)
(my-prefix-set-key "\C-d" 'dired-jump)

;; for dired
(my-prefix-set-key "d" 'dired-jump)
(my-prefix-set-key "\C-d" 'dired-jump)
(define-key global-map "\C-x\C-d" 'dired-jump)

;; for vc-dir
(when (>= emacs-major-version 23)
  (my-prefix-set-key "v" 'vc-dir)
  (my-prefix-set-key "\C-v" 'vc-dir))

;; for man
(my-prefix-set-key "m" 'man)

;; for program
(my-prefix-set-key "c" 'compile)
(define-key global-map "\M-c" 'compile)
;;(my-prefix-set-key "d" 'gdb)

;; for grep
(my-prefix-set-key "g" 'grep-buffers)
(my-prefix-set-key "\C-g" 'grep)
(my-prefix-set-key "\M-g" 'grep-find)


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

(my-prefix-set-key "z" 'toggle-default)
(my-prefix-set-key "\C-z" 'toggle-default)

;; ワンタッチでシェルに行ける
;; トルグにしたいもし
;;
(def-toggle-run shell "*shell*")
(def-toggle-run eshell "*eshell*")

(my-prefix-set-key "s" 'toggle-shell)
(my-prefix-set-key "\C-s" 'toggle-shell)

(my-prefix-set-key "e" 'toggle-eshell)
(my-prefix-set-key "\C-e" 'toggle-eshell)

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

(my-prefix-set-key "l" 'toggle-scratch)
(my-prefix-set-key "\C-l" 'toggle-scratch)
