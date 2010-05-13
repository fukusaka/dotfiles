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

  ;; 警告エラー行の表示
  (global-set-key "\C-cd" 'flymake-display-err-menu-for-current-line)

  (defun my-flymake-popup-menu (menu-data)
    (let* ((menu-title     (nth 0 menu-data))
	   (menu-items     (nth 1 menu-data))
	   (menu-commands  (mapcar (lambda (foo)
				     (nth 0 foo))
				   menu-items)))
      (popup-tip (mapconcat 'identity (cons menu-title menu-commands) "\n")))
    nil)

  (defadvice flymake-popup-menu
    (around my-ad-flymake-popup-menu)
    (let ((menu-data (ad-get-arg 0)))
      (my-flymake-popup-menu menu-data)))

  (defadvice flymake-display-err-menu-for-current-line
    (around my-flymake-display-err-menu-for-current-line activate)
    (ad-activate 'flymake-popup-menu)
    ad-do-it
    (ad-deactivate 'flymake-popup-menu)
    )

  ;; Makefile を使わないC/C++のチェック
  ;;(defun flymake-cc-init ()
  ;;  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
  ;;                       'flymake-create-temp-inplace))
  ;;         (local-file  (file-relative-name
  ;;                       temp-file
  ;;                       (file-name-directory buffer-file-name))))
  ;;    (list "g++" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))
  ;;
  ;;(push '("¥¥.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)
  ;;

  ;; Invoke ruby with '-c' to get syntax checking
  (when (executable-find "ruby")
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
    )

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
