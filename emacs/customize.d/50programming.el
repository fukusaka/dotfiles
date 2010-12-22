;;
;; compile-mode
;;
(setq compilation-ask-about-save nil)
(setq compilation-window-height 20)

;; 行末の不要スペースを強調表示
;;(set-face-underline 'trailing-whitespace "Red")
(set-face-background 'trailing-whitespace "MistyRose")

;; 行末スペースを色づけ
(setq-default show-trailing-whitespace t)

;; 保存時に無駄なスペースを削除
(add-hook 'before-save-hook
          '(lambda ()
             ;; 他人のソースではスペース削減は行なわない!
             (unless my-others-source-code
               (delete-trailing-whitespace))))

;; 他人のソースをいじる時に指定する
(defun my-current-buffer-others-source-code ()
  (interactive)
  (setq my-others-source-code t)
  (setq show-trailing-whitespace nil)
  (setq require-final-newline nil))

;; 他人のソースは自前で指定する
(make-variable-buffer-local 'my-others-source-code)
(setq-default my-others-source-code nil)

(setq glib-types
      '("gboolean" "gpointer" "gconstpointer"
	"gchar" "guchar" "gint" "guint"
	"gshort" "gushort" "glong" "gulong"
	"gint8" "guint8" "gint16" "guint16"
	"gint32" "guint32" "gint64" "guint64"
	"gfloat" "gdouble" "gsize" "gssize"))

(add-hook
 'c-mode-common-hook
 '(lambda ()
    (setq c-font-lock-extra-types (append c-font-lock-extra-types glib-types))
    (setq c++-font-lock-extra-types (append c++-font-lock-extra-types glib-types))
    ))

(setq c-default-style
      '((java-mode  . "java")
	(awk-mode . "awk")
	(other . "bsd")))

(defun my-c-like-mode-hook ()
  (if (string-match "/usr/src/linux.*/.*\\.[ch]$" (or (buffer-file-name) ""))
      (c-set-style "linux"))

  (c-set-offset 'inextern-lang 0) ;; extern "??" {} 中でインデントしない
  (c-set-offset 'innamespace 0)   ;; namspace {} 中でインデントしない
  )

(add-hook 'c-mode-hook 'my-c-like-mode-hook)
(add-hook 'c++-mode-hook 'my-c-like-mode-hook)
(add-hook 'objc-mode-hook 'my-c-like-mode-hook)

;; インデントモードの設定
;;(setq-default tab-width 4)
;;(setq-default indent-tabs-mode nil)

;; M-x compile でスクリプトを実行
(defvar my-interpreter-program nil)
(defun my-interpreter-mode-init ()
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "#!\\([^\n]+\\)")
      (make-local-variable 'my-interpreter-program)
      (setq my-interpreter-program (match-string 1))
      ))

  (when my-interpreter-program
    (make-local-variable 'compile-command)
    (setq compile-command
	  (concat my-interpreter-program " "
		  (buffer-file-name)))
    )
  )
(add-hook 'find-file-hook 'my-interpreter-mode-init)

;; Emacs 22 以降の対応
(when (and (>= emacs-major-version 22)
           window-system)

  ;; Yasnippet
  ;;(require 'yasnippet-bundle)

  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories
	       (concat my-elisp-dir "auto-complete/dict"))
  (ac-config-default)

  ;; 補完が自動で起動するのを停止
  (setq ac-auto-start nil)
  ;; 起動キーの設定
  (ac-set-trigger-key "<C-tab>")

  ;;(setq ac-auto-show-menu nil)

  ;; Color-moccur
  (when (locate-library "color-moccur")
    (require 'color-moccur))

  )
