;;
;; texinfmtを読む込む時、Hook があれればなぁ、、、
;;

(defun moi::pre-texinfmt ()
  (interactive)
  (load "texinfmt")
  ;;
  ;; @ifset XXX , @ifclear XXX をだます設定。
  ;;
  
  (put 'info 'texinfo-whether-setp 'flag-set)
  (put 'html 'texinfo-whether-setp 'flag-cleared)
  (put 'tex  'texinfo-whether-setp 'flag-cleared)

  ;;
  ;; xref,ref,pxref が二つ以上の引数を持つ時、自動的に.を補完する。
  ;;
  (defun texinfo-format-xref ()
    (let ((args (texinfo-format-parse-args)))
      (texinfo-discard-command)
      (insert "*Note ")
      (let ((fname (or (nth 1 args) (nth 2 args))))
	(if (null (or fname (nth 3 args)))
	    (insert (car args) "::")
	  (insert (or fname (car args)) ": ")
	  (if (nth 3 args)
	      (insert "(" (nth 3 args) ")"))
	  (insert (car args))
	  (if (not (looking-at "[ \n\t]*\\."))
	      (insert "."))
	  ))))
  (defun texinfo-format-pxref () (texinfo-format-xref))

  ;;
  ;; @cindex 項目[よみ] をちょとだけサポート。
  ;;  (あまりかっこよくないかも、、、)
  ;;

  (defun texinfo-format-printindex ()
3    (let ((indexelts (symbol-value
		      (cdr (assoc (texinfo-parse-arg-discard)
				  texinfo-indexvar-alist))))
	  opoint)
      (insert "\n* Menu:\n\n")
      (setq opoint (point))
;;;      (texinfo-print-index nil indexelts)
      (moi::texinfo-print-index nil (moi::texinfo-index-sort indexelts))
      
;;	(if (memq system-type '(vax-vms windows-nt ms-dos))
;;	    (texinfo-sort-region opoint (point))
;;	  (shell-command-on-region opoint (point) "sort -fd" 1))
      ))
      
  (defun moi::texinfo-index-sort (idx)
    (let (s-index)
      (while idx
	(let* (yomi
	       (key (nth 0 (car idx)))
	       (other (nthcdr 1 (car idx))))		
	  (if (string-match "\\[\\([^]]*\\)\\]" key)
	      (progn
		(setq yomi (match-string 1 key))
		(setq key (substring key 0 (match-beginning 0)))
		)
	    )
	  (if (not yomi) (setq yomi key))
	  (setq s-index
		(cons (append (list yomi key) other) s-index))
	  ;;(message (nth 0 (car idx)))
	  )
	(setq idx (cdr idx)))
      (sort s-index '(lambda (x y) (string< (car x) (car y))))
      ))
  
  (defun moi::texinfo-print-index (file indexelts)
    (while indexelts
      (let ((key (nth 1 (car indexelts)))
	    (node (nth 2 (car indexelts))))
	(if (stringp key)
	    (progn
	      (insert "* " key ": " )
;;;	      (indent-to 32)
	      (indent-to 41)
	      (insert
	       (if file (concat "(" file ")") "")
	       node ".")
;;;	      (indent-to 54)
	      (insert
;;;	        (if (nth 3 (car indexelts))
;;;		   (format "  %d." (nth 3 (car indexelts)))
;;;	         "")
	       "\n"))
	  ;; index entries from @include'd file
	  ;; ???
	  (texinfo-print-index (nth 2 (car indexelts))
			       (nth 3 (car indexelts))))
	(while (progn
		 (setq indexelts (cdr indexelts))
		 (and indexelts
		      (string= key  (nth 1 (car indexelts)))
		      (string= node (nth 2 (car indexelts))))))
	)
      ))
  )
  

(moi::pre-texinfmt)
