;;; emacs.el  ---  emacsen dummy startup code.

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

(defvar top-conf-dir
  (expand-file-name "~/common/conf/"))

(add-to-list 'load-path (concat top-conf-dir "emacs/elisp/moi"))

(require 'moi-startup)
(moi::startup)
