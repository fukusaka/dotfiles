;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 色を付ける共通設定
;;
;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>
;;
;; $Id$
;;

(if window-system
    (cond
     ;; mule2.3 では hilit19 を使う
     ((string-match "^19" emacs-version)
      (setq hilit-mode-enable-list  '(not text-mode)
	    hilit-background-mode   'light
	    hilit-inhibit-hooks     nil
	    hilit-inhibit-rebinding nil
	    hilit-quietly t)
      (require 'hilit19)


      ;; troff で色を付ける設定
      (add-hook 'nroff-mode-hook 
		(function (lambda ()
			    (hilit-translate 	string	  nil)
			    )))
      )
     ((featurep 'xemacs)
      (custom-set-variables '(font-lock-mode t t (font-lock)))
      )
     ;; それ以外(emacs20,xemacs) では font-lock を使う
     ((string-match "^2[01]" emacs-version)
      (global-font-lock-mode t)
      (if (string-match "^21" emacs-version)
	  (setq font-lock-support-mode 'jit-lock-mode)
	(setq font-lock-support-mode
	      '(;;(c-mode . fast-lock-mode)
		;;(c++-mode . fast-lock-mode)
		;;(cc-mode . fast-lock-mode)
		;;(perl-mode . fast-lock-mode)
		;;(cperl-mode . fast-lock-mode)
		(t . lazy-lock-mode)
		)))
      (setq fast-lock-minimum-size 25600)
      (setq lazy-lock-minimum-size 25600)
      (setq lazy-lock-defer-on-scrolling t)
      (setq lazy-lock-defer-contextually t)
      (setq lazy-lock-defer-time 0.10)

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

      (setq c-font-lock-extra-types (append c-font-lock-extra-types glib-types))
      (setq c++-font-lock-extra-types (append c++-font-lock-extra-types glib-types))
      )
     ))
