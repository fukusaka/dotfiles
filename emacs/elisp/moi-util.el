
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
