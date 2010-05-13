;; -*- coding: utf-8 -*-

(when (locate-library "flymake")
  (require 'flymake)

  ;;シンタックスチェックは次のコマンドが呼ばれる
  ;;make -s -C . CHK_SOURCES=hoge.cpp SYNTAX_CHECK_MODE=1 check-syntax
  ;;
  ;; Makefile があれば、次のルールを追加
  ;;PHONY: check-syntax
  ;;check-syntax:
  ;;    $(CC) -Wall -Wextra -pedantic -fsyntax-only $(CHK_SOURCES)

  ;; GUIの警告は表示しない
  (setq flymake-gui-warnings-enabled nil)

  ;; 全てのファイルで flymakeを有効化
  (add-hook 'find-file-hook 'flymake-find-file-hook)

  ;; M-p/M-n で警告/エラー行の移動
  (global-set-key "\M-p" 'flymake-goto-prev-error)
  (global-set-key "\M-n" 'flymake-goto-next-error)

  ;;(global-set-key "\M-p" 'flymake-display-err-minibuf)
  ;;(global-set-key "\M-n" 'flymake-display-err-menu-for-current-line)

  (defun my-flymake-popup-menu (menu-data)
    (let* ((menu-title     (nth 0 menu-data))
	   (menu-items     (nth 1 menu-data))
	   (menu-commands  (mapcar (lambda (foo)
				     (cons (nth 0 foo) (nth 1 foo)))
				   menu-items)))
      (popup-menu* menu-items)))

  (defadvice flymake-popup-menu
    (around my-ad-flymake-popup-menu activate)
    (let ((menu-data (ad-get-arg 0)))
      (my-flymake-popup-menu menu-data)))

  (defun my-toggle-flymake-popup-menu-use-popup.el ()
    (interactive)
    (ad-activate-on my-ad-flymake-popup-menu))



;;;; flymake 現在行のエラーをpopup.elのツールチップで表示する
;;(defun flymake-display-err-menu-for-current-line ()
;;  (interactive)
;;  (let* ((line-no             (flymake-current-line-no))
;;         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no))))
;;    (when line-err-info-list
;;      (let* ((count           (length line-err-info-list))
;;             (menu-item-text  nil))
;;        (while (> count 0)
;;          (setq menu-item-text (flymake-ler-text (nth (1- count) line-err-info-list)))
;;          (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
;;                 (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
;;            (if file
;;                (setq menu-item-text (concat menu-item-text " - " file "(" (format "%d" line) ")"))))
;;          (setq count (1- count))
;;          (if (> count 0) (setq menu-item-text (concat menu-item-text "\n")))
;;          )
;;        (popup-tip menu-item-text)))))
;;;(defun flymake-cc-init ()
;;;  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;;;                       'flymake-create-temp-inplace))
;;;         (local-file  (file-relative-name
;;;                       temp-file
;;;                       (file-name-directory buffer-file-name))))
;;;    (list "g++" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))
;;;
;;;(push '("¥¥.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)
;;;


  ;; Invoke ruby with '-c' to get syntax checking
  (defun flymake-ruby-init ()
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
			 'flymake-create-temp-inplace))
	   (local-file  (file-relative-name
			 temp-file
			 (file-name-directory buffer-file-name))))
      (list "ruby" (list "-c" local-file))))

  (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
  (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

  (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

  ;; bash チェック
  (defvar flymake-shell-of-choice
    "/bin/bash"
    "Path of shell.")

  (defvar flymake-shell-arguments
    (list "-n")
    "Shell arguments to invoke syntax checking.")

  (defun flymake-shell-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name))))
      (list flymake-shell-of-choice (append flymake-shell-arguments (list local-file)))))

  (push '(".+\\.sh$" flymake-shell-init) flymake-allowed-file-name-masks)
  (push '("^\\(.+\\): line \\([0-9]+\\): \\(.+\\)$" 1 2 nil 3) flymake-err-line-patterns)

  ;; HTML チェック
  (when (executable-find "tidy")
    (defun flymake-html-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
			 'flymake-create-temp-inplace))
	     (local-file (file-relative-name
			  temp-file
			  (file-name-directory buffer-file-name))))
	(list "tidy" (list local-file))))

    (push '("\\.html$\\|\\.ctp" flymake-html-init) flymake-allowed-file-name-masks)
    (push '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)" nil 1 2 4) flymake-err-line-patterns)
    )

  ;; XSL
  (push '(".+\\.xsl$" flymake-xml-init) flymake-allowed-file-name-masks)
  )
