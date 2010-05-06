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

;; gtags
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-t" 'gtags-find-tag)
         (local-set-key "\M-r" 'gtags-find-rtag)
         (local-set-key "\M-s" 'gtags-find-symbol)
         (local-set-key "\C-t" 'gtags-pop-stack)
         ))

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
    (c-set-style "bsd")
    ))

;; CPerl-mode を使う
(defalias 'perl-mode 'cperl-mode)

;; インデントモードの設定
;;(setq-default tab-width 4)
;;(setq-default indent-tabs-mode nil)

;; Emacs 22 以降の対応
(when (and (>= emacs-major-version 22)
           window-system)

  ;; Yasnippet
  (require 'yasnippet-bundle)

  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories
	       (concat my-elisp-dir "auto-complete-1.2/dict"))
  (ac-config-default)

  ;;(setq ac-auto-start 3)
  ;;(setq ac-auto-start nil)
  ;;(setq ac-auto-show-menu nil)

  ;; Color-moccur
  (require 'color-moccur)

  )
