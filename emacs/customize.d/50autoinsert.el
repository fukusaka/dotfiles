
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory (concat top-conf-dir "skel/"))

;;(require 'moi-skel-file)

(load "moi-skel-make")
(defadvice insert-file-contents
  (after insert-file-contents-moi::expand-skeleton-buffer)
  (moi::expand-skeleton-buffer))

(defadvice auto-insert
  (around moi::skel-make-process activate)
  (ad-activate-regexp "insert-file-contents-moi::expand-skeleton-buffer")
  ad-do-it
  (ad-deactivate-regexp "insert-file-contents-moi::expand-skeleton-buffer"))

(require 'autoinsert)
(setq save-auto-insert-alist auto-insert-alist)

(setq auto-insert-alist
      (append '(
		("\\.h\\'" . "skel.h")
		("\\.c\\'" . "skel.c")
		("\\.cc\\'" . "skel.cc")
		("\\.pl\\'" . "skel.pl")
		("\\.el\\'" . "skel.el")
		("\\.html\\'" . "skel.html")
		) save-auto-insert-alist))

