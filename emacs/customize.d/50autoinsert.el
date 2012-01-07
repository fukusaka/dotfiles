
(require 'autoinsert)

;; テンプレート展開ツール
(require 'my-skel-file)

;; 自動テンプレート挿入マイナーモード設定
(auto-insert-mode 1)

(setq auto-insert-directory (concat my-top-conf-dir "skel/"))

(setq save-auto-insert-alist auto-insert-alist)

;; テンプレート適用
(dolist (e (nreverse
            '(("\\.h\\'" . "skel.h")
              ("\\.c\\'" . "skel.c")
              ("\\.cc\\'" . "skel.cc")
              ("\\.pl\\'" . "skel.pl")
              ("\\.pm\\'" . "skel.pm")
              ("\\.py\\'" . "skel.py")
              ("\\.rb\\'" . "skel.rb")
              ("\\.el\\'" . "skel.el")
              ("\\.scm\\'" . "skel.scm")
              ("\\.html\\'" . "skel.html")
              ("\\.tex\\'" . "skel.tex")
              ("\\.texi\\'" . "skel.texi"))))
  (setcdr e (vector (cdr e) 'my-expand-skeleton-buffer))
  (add-to-list 'auto-insert-alist e))

;; シンプルファイル
(dolist (e '("Makefile.am""autogen.sh"))
  (add-to-list 'auto-insert-alist (cons e e) t))
