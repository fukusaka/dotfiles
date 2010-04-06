;;
;; compile-mode
;;
(setq compilation-ask-about-save nil)
(setq compilation-window-height 20)

;; 行末スペース削除支援
(setq-default show-trailing-whitespace t)	;; 行末の不要スペースを強調表示
(add-hook 'before-save-hook
          'delete-trailing-whitespace)		;; 保存時に無駄なスペースを削除


;;(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;(which-function-mode)

(setq vc-follow-symlinks t)

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
    (setq c++-font-lock-extra-types (append c++-font-lock-extra-types glib-types))))

;; Emacs 22 以降の対応
(when (>= emacs-major-version 22)
  ;; Yasnippet
  ;;(require 'yasnippet-bundle)

  (require 'auto-complete-config)
  (ac-config-default)

  ;; Color-moccur
  (require 'color-moccur)
)
