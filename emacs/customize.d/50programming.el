;;
;; compile-mode
;;
(setq compilation-ask-about-save nil)
(setq compilation-window-height 20)

;; 行末スペース削除支援
(setq-default show-trailing-whitespace t)	;; 行末の不要スペースを強調表示
;;(add-hook 'before-save-hook
;;          'delete-trailing-whitespace)	;; 保存時に無駄なスペースを削除


(defun my-not-require-final-newline ()
  (interactive)
  (setq require-final-newline nil))

;;(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;(which-function-mode)

(setq vc-follow-symlinks t)

;; gtags
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-t" 'gtags-find-tag)
         (local-set-key "\M-r" 'gtags-find-rtag)
         (local-set-key "\M-s" 'gtags-find-symbol)
         (local-set-key "\C-t" 'gtags-pop-stack)
         ))

(add-hook 'c-mode-common-hook
          '(lambda ()
	     (c-set-style "bsd")))

(setq cperl-indent-level 4)

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
    (gtags-mode 1)
    ))

;; Emacs 22 以降の対応
(when (and (>= emacs-major-version 22)
           window-system)

  ;; Yasnippet
  (require 'yasnippet-bundle)

  (require 'auto-complete-config)
  (ac-config-default)

  ;;  (setq ac-auto-start 3)
  (setq ac-auto-start nil)
  (setq ac-auto-show-menu nil)

  ;; Color-moccur
  (require 'color-moccur)
)
