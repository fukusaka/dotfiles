;;; emacs.el  ---  emacsen dummy startup code.

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

(defvar this-conf-top-dir
  (expand-file-name "~/lib/conf/emacs/"))

(setq moi::elisp-path (concat this-conf-top-dir "elisp/"))
(setq moi::customize-dir (concat this-conf-top-dir "customize.d/"))

(load (concat moi::elisp-path "moi-startup"))

(moi::startup)
