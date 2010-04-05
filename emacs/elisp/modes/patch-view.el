;;; my-patch-view.el --- Coloring for diff/patch file ;; -*-emacs-lisp-*-

;; Author:     Moimoi <fukusaka@xa2.so-net.ne.jp>
;; Version:    0.01

;; $Id$

;; (autoload 'my-patch-view-mode "my-patch-view")


(defvar  my-patch-view-sep-face 'my-patch-view-sep-face)
(defface my-patch-view-sep-face
  '((((class color) (background light)) (:foreground "ForestGreen"))
    (((class color) (background dark)) (:foreground "PaleGreen"))
    )
  nil)

(defvar  my-patch-view-cmd-face 'my-patch-view-cmd-face)
(defface my-patch-view-cmd-face
  '((((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    )
  nil)


(defvar  my-patch-view-new-face 'my-patch-view-new-face)
(defface my-patch-view-new-face
  '((((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "LightSkyBlue"))
    )
  nil)

(defvar  my-patch-view-old-face 'my-patch-view-old-face)
(defface my-patch-view-old-face
  '((((class color) (background light)) (:foreground "Orchid"))
    (((class color) (background dark)) (:foreground "LightSteelBlue"))
    )
  nil)

(defvar  my-patch-view-exc-face 'my-patch-view-exc-face)
(defface my-patch-view-exc-face
  '((((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    )
  nil)

(setq my-patch-view-normal-lock-keywords
      '(
	("^<.*$"       . 'my-patch-view-old-face)
	("^>.*$"       . 'my-patch-view-new-face)
	("^---$"       . 'my-patch-view-sep-face)
	("^[^<>-].*$"  . 'my-patch-view-cmd-face)
	;;("^[0-9]+\\(,[0-9]+\\)?[acd][0-9]+\\(,[0-9]+\\)?$" . 'my-patch-view-cmd-face
	))

(setq my-patch-view-context-lock-keywords
      '(
	("^- .*$"            . 'my-patch-view-old-face)
	("^+ .*$"            . 'my-patch-view-new-face)
	("^! .*$"            . 'my-patch-view-exc-face)
	("^\\*\\*\\* .*$"    . 'my-patch-view-old-face)
	("^---.*$"           . 'my-patch-view-new-face)
	("^\\*\\*\\*\\*.*$" . 'my-patch-view-sep-face)
	)
      )
(setq my-patch-view-unified-lock-keywords
      '(
	("^-[^-].*$"         . 'my-patch-view-old-face)
	("^+[^+].*$"         . 'my-patch-view-new-face)
	("^---.*$"           . 'my-patch-view-old-face)
	("^\\+\\+\\+ .*$"    . 'my-patch-view-new-face)
	("^@@.*@@$"          . 'my-patch-view-sep-face)
	("^diff.*$"          . 'my-patch-view-cmd-face)
	)
      )

(defun my-patch-view-diff-type-chechk ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward
	 (concat "^" (regexp-quote "***************") "$")
	 nil t)
	(setq font-lock-defaults '(my-patch-view-context-lock-keywords t))
      )
    (goto-char (point-min))
    (if (re-search-forward "^@@.*@@$" nil t)
	(setq font-lock-defaults '(my-patch-view-unified-lock-keywords t))
      )
    )
  )

(defun my-patch-view-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'my-patch-view-mode)     ; `describe-mode'が説明
  (setq mode-name "My-Patch-View")          ; モード行に表示されるモード名。
  (lisp-mode-variables nil)                  ; いろいろな変数を定義する。
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults nil)
  (my-patch-view-diff-type-chechk)
  (if font-lock-defaults
      (turn-on-font-lock))
  (run-hooks 'my-patch-view-mode-hook))     ; ユーザがカスタマイズのために定
					     ;   定したフックをここで実行する。

(provide 'my-patch-view-mode)
