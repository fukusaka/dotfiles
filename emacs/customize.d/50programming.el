;;
;; compile-mode
;;
(setq compilation-ask-about-save nil)
(setq compilation-window-height 20)

;;(which-function-mode)

(setq vc-follow-symlinks t)


(add-hook 'c-mode-common-hook
          '(lambda ()
	     (c-set-style "bsd")))

(setq cperl-indent-level 4)

;;(defadvice cd (around remote-cd (path))
;;  (progn
;;    (message path)
;;    ad-do-it))
;;
;;(defadvice compilation-start (around ad-compilation-remote-start-ext activate)
;;  (progn
;;    (ad-activate 'cd)
;;    ad-do-it
;;    (ad-deactivate 'cd)))

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

;; Yasnippet
;;(require 'yasnippet-bundle)

(require 'auto-complete-config)
(ac-config-default)

;; Color-moccur
(require 'color-moccur)
