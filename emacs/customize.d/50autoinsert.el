
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory (concat my-top-conf-dir "skel/"))

;;(require 'my-skel-file)

(load "my-skel-make")
(defadvice insert-file-contents
  (after insert-file-contents-my-expand-skeleton-buffer)
  (my-expand-skeleton-buffer))

(defadvice auto-insert
  (around my-skel-make-process activate)
  (ad-activate-regexp "insert-file-contents-my-expand-skeleton-buffer")
  ad-do-it
  (ad-deactivate-regexp "insert-file-contents-my-expand-skeleton-buffer"))

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

