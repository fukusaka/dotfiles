;;; moi-startup.el  ---  emacsen startup code.

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

(defvar moi::emacs-conf-dir (concat top-conf-dir "emacs/"))

(defvar moi::customize-dir (concat moi::emacs-conf-dir "customize.d/"))

(defvar moi::elisp-path    (concat moi::emacs-conf-dir "elisp/"))

(defvar moi::hostname-nohost "localhost")

(defvar moi::hostname (let ((envhost (shell-command-to-string "echo -n `hostname -s`")))
			(if envhost envhost moi::hostname-nohost)))

(defvar moi::host-customize-dir
  (concat moi::customize-dir "H" moi::hostname "/"))

(defvar moi::emacs-flavor
  (let ((flavor (if (featurep 'xemacs) "xemacs" "emacs")))
    (format "%s%d" flavor emacs-major-version)))

(defvar moi::elc-dir-prefix
  (concat moi::emacs-flavor "/"))

(defvar moi::cluster-file 
  (concat moi::customize-dir "/cluster.el"))

(defvar moi::domain-customize-dir
  (let ((hostname moi::hostname)
	(alist (let ((buf (generate-new-buffer "temp")) data)
		 (save-excursion
		   (set-buffer buf)
		   (insert-file-contents moi::cluster-file)
		   (setq data (read buf)))
		 (kill-buffer buf)
		 data))
	domain)
    (while (and alist (not domain))
      (if (string-match (caar alist) hostname)
	  (setq domain (cdar alist)))
      (setq alist (cdr alist)))
    (if domain (concat moi::customize-dir "D" domain "/"))))

(defun moi::compile-file (file)
  (let* ((el file)
	 (el-dir (file-name-directory el))
	 (elc-dir (concat el-dir moi::elc-dir-prefix))
	 (elc (concat elc-dir (file-name-nondirectory (concat el "c")))))
    (if (not (file-directory-p elc-dir))
	(make-directory elc-dir))
    (if (file-newer-than-file-p el elc)
	(progn
	  (byte-compile-file el)
	  (rename-file (concat el "c") elc t)))
    elc
    ))

(defun moi::load-file (file)
  (load (moi::compile-file file)))

(defun moi::startup-customize ()
  (let ((dir-list (list moi::customize-dir moi::domain-customize-dir moi::host-customize-dir))
	file-list dir files)

    (while dir-list
      (setq dir (car dir-list))
      (setq dir-list (cdr dir-list))
      (when (and dir (file-directory-p dir))
	(setq files (directory-files dir t "^[0-9][0-9].*\\.el$" t))
	(setq file-list (append file-list (sort files 'string<)))))

    (mapcar 'moi::load-file file-list)))

(defun moi::add-to-load-path ()
  (let (files dir subdirs)
    (setq files (directory-files moi::elisp-path nil "^[^\\.]"))
    (while files
      (setq dir (car files))
      (setq files (cdr files))
      (if (and (file-directory-p (concat moi::elisp-path dir))
	       (not (string-match "^x?emacs[0-9]+" dir)))
	  (push dir subdirs)))

    (push moi::emacs-flavor subdirs)
    (push "." subdirs)

    (let ((default-directory (concat top-conf-dir "emacs/elisp")))
      (normal-top-level-add-to-load-path subdirs))))


(defun moi::startup ()
  (moi::add-to-load-path)
  (require 'moi-compatibility)
  (moi::startup-customize))
