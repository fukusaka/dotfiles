;;; emacs.el  ---  emacsen dummy startup code.

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

(defvar this-conf-top-dir
  (expand-file-name "~/common/conf/emacs/"))

(load (concat this-conf-top-dir "elisp/moi-startup"))

(moi::startup)
