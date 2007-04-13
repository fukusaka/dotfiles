;;; emacs.el  ---  emacsen dummy startup code.

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

(defvar top-conf-dir
  (expand-file-name "~/common/conf/"))

(load (concat top-conf-dir "emacs/elisp/moi/moi-startup.el"))

(moi::startup)
