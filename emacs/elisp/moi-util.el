;;; moi-util.el  ---  Emacs Lisp utility

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$


(require 'poem)
(defun moi-substring-width (str n)
  (let* ((strl (string-to-char-list str))
	 (wstrl (mapcar 'char-width strl))
	 (w 0) (estrl nil))
    (while (> n w)
      (setq w (+ w (car wstrl))
	    wstrl (cdr wstrl)
	    estrl (cons (car strl) estrl)
	    strl (cdr strl))
      )
    (if (not (= n w))
	(setq estrl (cdr estrl)))
    (char-list-to-string (reverse estrl))
    ))

(require 'poem)
(defun split-string-with-coding (str)
  (let*
	((strJ (encode-coding-string str 'iso-2022-jp))
	 (staJ (string-match "$[B@]" strJ))
	 (endJ (string-match "([BJ]" strJ))
	 he ta)
    (cond
     ((not staJ)
	(setq he strJ
	      ta ""
	      cod 'ascii)
	)
     ((/= staJ 0)
	(setq he (substring strJ 0 staJ)
	      ta (substring strJ staJ)
	      cod 'ascii)
	)
     ((/= endJ 0)
	(setq he (substring strJ 0 (+ 3 endJ))
	      ta (substring strJ (+ 3 endJ))
	      cod 'jis)
	)
     )
    (cons (cons cod he)
	    (if (string= ta "")
		nil
	      (split-string-with-coding ta)))
    ))

(provide 'moi-util)
