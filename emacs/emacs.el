;;; emacs.el  ---  emacsen dummy startup code.

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

(setq my-top-conf-dir (expand-file-name "~/common/conf/"))
(setq my-emacs-conf-dir (concat my-top-conf-dir "emacs/"))

(setq my-place-profile-alist
      '(("\\`black" .	"home")
	("\\`blue" .	"home")
	("\\`red" .	"home")
	("\\`think" .  "home")))

(let ((default-directory my-emacs-conf-dir))
  (load-file "my-compat.el")
  (load-file "my-startup.el"))
