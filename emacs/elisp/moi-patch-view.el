;;; moi-patch-view.el --- Coloring for diff/patch file ;; -*-emacs-lisp-*-

;; Author:     Moimoi <fukusaka@xa2.so-net.ne.jp>
;; Version:    0.01

;; $Id$

;; (autoload 'moi-patch-view-mode "moi-patch-view")


(defvar  moi-patch-view-sep-face 'moi-patch-view-sep-face)
(defface moi-patch-view-sep-face
  '((((class color) (background light)) (:foreground "ForestGreen"))
    (((class color) (background dark)) (:foreground "PaleGreen"))
    )
  nil)

(defvar  moi-patch-view-cmd-face 'moi-patch-view-cmd-face)
(defface moi-patch-view-cmd-face
  '((((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    )
  nil)


(defvar  moi-patch-view-new-face 'moi-patch-view-new-face)
(defface moi-patch-view-new-face
  '((((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "LightSkyBlue"))
    )
  nil)

(defvar  moi-patch-view-old-face 'moi-patch-view-old-face)
(defface moi-patch-view-old-face
  '((((class color) (background light)) (:foreground "Orchid"))
    (((class color) (background dark)) (:foreground "LightSteelBlue"))
    )
  nil)

(defvar  moi-patch-view-exc-face 'moi-patch-view-exc-face)
(defface moi-patch-view-exc-face
  '((((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    )
  nil)

(setq moi-patch-view-normal-lock-keywords
      '(
	("^<.*$"       . 'moi-patch-view-old-face)
	("^>.*$"       . 'moi-patch-view-new-face)
	("^---$"       . 'moi-patch-view-sep-face)
	("^[^<>-].*$"  . 'moi-patch-view-cmd-face)
	;;("^[0-9]+\\(,[0-9]+\\)?[acd][0-9]+\\(,[0-9]+\\)?$" . 'moi-patch-view-cmd-face
	))

(setq moi-patch-view-context-lock-keywords
      '(
	("^- .*$"            . 'moi-patch-view-old-face)
	("^+ .*$"            . 'moi-patch-view-new-face)
	("^! .*$"            . 'moi-patch-view-exc-face)
	("^\\*\\*\\* .*$"    . 'moi-patch-view-old-face)
	("^---.*$"           . 'moi-patch-view-new-face)
	("^\\*\\*\\*\\*.*$" . 'moi-patch-view-sep-face)
	)
      )
(setq moi-patch-view-unified-lock-keywords
      '(
	("^-[^-].*$"         . 'moi-patch-view-old-face)
	("^+[^+].*$"         . 'moi-patch-view-new-face)
	("^---.*$"           . 'moi-patch-view-old-face)
	("^\\+\\+\\+ .*$"    . 'moi-patch-view-new-face)
	("^@@.*@@$"          . 'moi-patch-view-sep-face)
	("^diff.*$"          . 'moi-patch-view-cmd-face)
	)
      )

(defun moi-patch-view-diff-type-chechk ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward
	 (concat "^" (regexp-quote "***************") "$")
	 nil t)
	(setq font-lock-defaults '(moi-patch-view-context-lock-keywords t))
      )
    (goto-char (point-min))
    (if (re-search-forward "^@@.*@@$" nil t)
	(setq font-lock-defaults '(moi-patch-view-unified-lock-keywords t))
      )
    )
  )

(defun moi-patch-view-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'moi-patch-view-mode)     ; `describe-mode'が説明
  (setq mode-name "Moi-Patch-View")          ; モード行に表示されるモード名。
  (lisp-mode-variables nil)                  ; いろいろな変数を定義する。
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults nil)
  (moi-patch-view-diff-type-chechk)
  (if font-lock-defaults
      (turn-on-font-lock))
  (run-hooks 'moi-patch-view-mode-hook))     ; ユーザがカスタマイズのために定
					     ;   定したフックをここで実行する。

(provide 'moi-patch-view-mode)
