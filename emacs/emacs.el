;;; emacs.el  ---  emacsen dummy startup code.

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

(defvar moi::conf-dir "~/lib/conf/emacs/")

(load (concat moi::conf-dir "moi-startup.el"))

(moi::startup)
