;;; emacs.el  ---  emacsen dummy startup code.

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

(defvar this-conf-top-dir
  (expand-file-name "~/lib/conf/emacs/"))

(setq load-path (cons (concat this-conf-top-dir "elisp")
		      load-path))

(setq moi::customize-dir (concat this-conf-top-dir "customize.d/"))

(load "moi-startup")

(moi::startup)
