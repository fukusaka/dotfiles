;;; moi-startup.el  ---  emacsen startup code.

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

(defvar moi::conf-dir "~/lib/conf/emacs/")

(defvar moi::host-conf-dir
  (concat moi::conf-dir "T" (getenv "HOSTNAME") "/"))

(defun moi::unique-strings (list)
  (if (null list)
      '()
    (if (string= (car list) (car (cdr list)))
        (moi::unique-strings (cdr list))
      (cons (car list) (moi::unique-strings (cdr list))))))

(defun moi::run-conf (file)
  (let ((el  (concat file ".el"))
	(elc (concat file ".elc")))
    (if (file-newer-than-file-p el elc)
	(byte-compile-file el)
      )
    (load file)
    ))

(defun moi::startup ()
  (let* ((files
	  (append
	   (directory-files moi::conf-dir t "^[0-9][0-9].*\\.elc?$" t)
	   (directory-files moi::host-conf-dir t "^[0-9][0-9].*\\.elc?$" t)
	   ))
	 (s-files
	  (moi::unique-strings
	   (sort
	    (mapcar (lambda (file)
		      (cond ((string-match "\\.el$" file)
			     (substring file 0 -3))
			    ((string-match "\\.elc$" file)
			     (substring file 0 -4))
			    ))
		    files)
	    'string<)))
	 (d-files
	  (mapcar 'cadr
		  (sort
		   (mapcar (lambda (file)
			     (list (file-name-nondirectory file) file))
			   s-files)
		   (lambda (x y) (string< (car x) (car y)))
		   )))
	 )
    (mapcar 'moi::run-conf d-files)
    ))
