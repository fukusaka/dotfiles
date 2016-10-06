;;; emacs.el  ---  emacsen dummy startup code.

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq my-top-conf-dir (expand-file-name "~/common/"))
(setq my-emacs-conf-dir (concat my-top-conf-dir "emacs/"))

(setq my-place-profile-alist
      '((".*". "home")))

(let ((default-directory my-emacs-conf-dir))
  (if (<= emacs-major-version 20)
      (load-file "my-compat.el"))
  (load-file "my-startup.el"))

;;(setq my-startup-bundling-delay 0)

;; 初期化の実行
(my-startup)

