
(require 'autoinsert)

;; テンプレート展開ツール
(require 'my-skel-make)

;; 自動テンプレート挿入マイナーモード設定
(auto-insert-mode 1)

(setq auto-insert-directory (concat my-top-conf-dir "skel/"))

(setq save-auto-insert-alist auto-insert-alist)

;; テンプレート適用
(dolist (item (nreverse
               '(("\\.h\\'" . "skel.h")
                 ("\\.c\\'" . "skel.c")
                 ("\\.cc\\'" . "skel.cc")
                 ("\\.pl\\'" . "skel.pl")
                 ("\\.el\\'" . "skel.el")
                 ("\\.html\\'" . "skel.html")
                 ("\\.tex\\'" . "skel.tex")
                 ("\\.texi\\'" . "skel.texi"))))
  (setcdr item (vector (cdr item) 'my-expand-skeleton-buffer))
  (add-to-list 'auto-insert-alist item))

;; シンプルファイル
(dolist (item (nreverse '("Makefile.am""autogen.sh")))
  (add-to-list 'auto-insert-alist (cons item item)))
