;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 色を付ける共通設定
;;
;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>
;;
;; $Id$
;;

(global-font-lock-mode t)

;; jit-lock-mode を使う
(setq font-lock-support-mode 'jit-lock-mode)

(defface moi-string-face
  '((((class color) (background light)) (:foreground "Brown"))
    (((class color) (background dark)) (:foreground "Salmon"))
    (t (:italic t)))
  nil
  )
(setq font-lock-string-face 'moi-string-face)

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
