;;; moi-startup.el  ---  emacsen startup code.

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

(defvar moi::customize-dir "~/lib/conf/emacs/customize.d")
(defvar moi::elisp-path "~/lib/conf/emacs/elisp/")

(defvar moi::host-customize-dir
  (concat moi::customize-dir "T" (getenv "HOSTNAME") "/"))

(defvar moi::elc-dir-prefix
  (cond ((string< emacs-version "19") "emacs18/")
	((string< emacs-version "20") "emacs19/")
	((string< emacs-version "21") "emacs20/")
	(t "emacsxx/")))


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
  (let* ((files
	  (append
	   (directory-files moi::customize-dir t "^[0-9][0-9].*\\.el$" t)
	   (if (file-directory-p moi::host-customize-dir)
	       (directory-files moi::host-customize-dir t "^[0-9][0-9].*\\.el$" t))
	   ))
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
