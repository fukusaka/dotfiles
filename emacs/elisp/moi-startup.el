;;; moi-startup.el  ---  emacsen startup code.

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

(defvar moi::conf-top-dir "~/lib/conf/emacs/")

(defvar moi::customize-dir (concat this-conf-top-dir "customize.d/"))
(defvar moi::elisp-path    (concat this-conf-top-dir "elisp/"))

(defvar moi::hostname-nohost "localhost")

;;(defvar moi::hostname (let* ((envhost (getenv "HOSTNAME")))
;;			(if envhost envhost moi::hostname-nohost)))
;; (getenv "HOSTNAME") で何故だか nil を返すようになってしまったので、、、。
(defvar moi::hostname (let* ((envhost
			      (shell-command-to-string "echo -n `hostname -s`")))
			(if envhost envhost moi::hostname-nohost)))

(defvar moi::host-customize-dir
  (concat moi::customize-dir "H" moi::hostname "/"))

(defvar moi::elc-dir-prefix
  (if (featurep 'xemacs)
      (cond ((string< emacs-version "21") "xemacs20/")
	    ((string< emacs-version "22") "xemacs21/")
	    (t "xemacsxx/"))
    (cond ((string< emacs-version "19") "emacs18/")
	  ((string< emacs-version "20") "emacs19/")
	  ((string< emacs-version "21") "emacs20/")
	  ((string< emacs-version "22") "emacs21/")
	  ((string< emacs-version "23") "emacs22/")
	  (t "emacsxx/"))))

(defun moi::domain-customize-dir ()
  (let* ((hostname moi::hostname)
	 (alist (let ((buf (generate-new-buffer "temp")) data)
		  (save-excursion
		    (set-buffer buf)
		    (insert-file-contents (concat moi::customize-dir
						  "/cluster.el"))
		    (setq data (read buf)))
		  (kill-buffer buf)
		  data))
	 (domain nil))
    (while (and (not domain) alist)
      (if (string-match (car (car alist)) hostname)
	  (setq domain (cdr (car alist))))
      (setq alist (cdr alist)))
    (if domain (concat moi::customize-dir "D" domain "/"))))

(defun moi::unique-strings (list)
  (if (null list)
      '()
    (if (string= (car list) (car (cdr list)))
        (moi::unique-strings (cdr list))
      (cons (car list) (moi::unique-strings (cdr list))))))

(defun moi::compile-file (file)
  (let* ((base (if (string-match ".el$" file)
		   (replace-match "" t t file)
		 file))
	 (el  (concat base ".el"))
	 (elc-dir (concat (file-name-directory file) moi::elc-dir-prefix))
	 (elc (concat elc-dir (file-name-nondirectory base) ".elc")))
    (if (not (file-directory-p elc-dir))
	(make-directory elc-dir))
    (if (file-newer-than-file-p el elc)
	(progn
	  (byte-compile-file el)
	  (rename-file (concat el "c") elc t) 
	  )
      )
    elc
    ))

(defun moi::load-file (file)
  (load (moi::compile-file file)))

(defun moi::startup-customize ()
  (let* ((files-list
	  (mapcar (lambda (dir)
		    (if (file-directory-p dir)
			(directory-files dir t "^[0-9][0-9].*\\.el$" t)))
		  (let ((domain (moi::domain-customize-dir)))
		    (if domain
			(list moi::customize-dir
			      domain
			      moi::host-customize-dir)
		      (list moi::customize-dir
			    moi::host-customize-dir)))
		  ))
	 (files
	  (let ((files))
	    (while files-list
	      (setq files (append files (car files-list)))
	      (setq files-list (cdr files-list)))
	    files))

	 (s-files
	  (moi::unique-strings
	   (sort
	    (mapcar (lambda (file) (substring file 0 -3))
		    files)
	    'string<)))
	 (d-files
	  (mapcar (lambda (x) (car (cdr x)))
		  (sort
		   (mapcar (lambda (file)
			     (list (file-name-nondirectory file) file))
			   s-files)
		   (lambda (x y) (string< (car x) (car y)))
		   )))
	 )
    (mapcar 'moi::load-file d-files)
    ))

(defun moi::startup ()
  (setq load-path
	(append
	 (list
	  (concat moi::elisp-path
		  (format "%d.%d" emacs-major-version emacs-minor-version))
	  moi::elisp-path)
	 load-path))
  (moi::startup-customize))
