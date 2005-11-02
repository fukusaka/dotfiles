;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 色を付ける共通設定
;;
;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>
;;
;; $Id$
;;

(cond
 ((featurep 'xemacs)
  (custom-set-variables '(font-lock-mode t t (font-lock)))
  )
 (t
  (global-font-lock-mode t)

  (cond
   ;; emacs20 では lazy-lock-mode を使う
   ((string-match "^20" emacs-version)
    (setq fast-lock-minimum-size 25600)
    (setq lazy-lock-minimum-size 25600)
    (setq lazy-lock-defer-on-scrolling t)
    (setq lazy-lock-defer-contextually t)
    (setq lazy-lock-defer-time 0.10)
    (setq font-lock-support-mode
	  '((t . lazy-lock-mode))))
   ;; emacs21 では jit-lock-mode を使う
   ((string-match "^21" emacs-version)
    (setq font-lock-support-mode 'jit-lock-mode))
   )

  (defface moi-string-face
    '((((class color) (background light)) (:foreground "Brown"))
      (((class color) (background dark)) (:foreground "Salmon"))
      (t (:italic t)))
    nil
    )
  (setq font-lock-string-face 'moi-string-face)
      
  (setq glib-types
	'("gboolean" "gpointer" "gconstpointer"
	  "gchar" "guchar" "gint" "guint" "gshort" "gushort" "glong" "gulong"
	  "gint8" "guint8" "gint16" "guint16" "gint32" "guint32" "gint64" "guint64"
	  "gfloat" "gdouble" "gsize" "gssize"))

  (if (string-match "^20" emacs-version)
      (progn (setq c-font-lock-extra-types (append c-font-lock-extra-types glib-types))
	     (setq c++-font-lock-extra-types (append c++-font-lock-extra-types glib-types))))
  )
 )
