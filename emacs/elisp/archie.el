;;
;; archie.el v1.0 -- by Brendan Kehoe (brendan@cs.widener.edu)
;;
;; A mock-interface to Archie for Emacs.  This will shave by when you need
;; it.  ARCHIE-SEZ will insert the result of the query in the current buffer,
;; and plain ARCHIE will create a separate buffer .
;;
;; TODO: hack ange-ftp to use the output of archie -l, and let you select
;;       from a magic list which one you want to FTP into an Emacs buffer.
;;

(defvar archie-program "/usr/local/bin/archie"
  "Program that queries archie servers.")

(defvar archie-server "archie.sura.net"
  "Server for \\[archie] searches.

Known archie servers:
	archie.ans.net (USA [NY])
	archie.sura.net (USA [MD])
	archie.mcgill.ca (Canada)
	archie.funet.fi (Finland/Mainland Europe)
	archie.au (Australia)
	archie.doc.ic.ac.uk (Great Britain/Ireland)
")

(defvar archie-search-type "-e"
  "Search type for \\[archie] searches.
Can be one of: -c for substring searches
               -e for exact matches (default)
               -r for a regexp
               -s for a case-insensitive substring search
")

(defun archie-sez (string)
  "Insert the results of an archie query on STRING into the current buffer.
   Uses the function `archie' for its main work."
  (interactive (list (read-string "String: " nil)))
  (archie string nil t))

(defun archie (string &optional type inplace)
  "Look for STRING on an Archie server.
Optional second arg TYPE is the type of search to make -- by default, it's
`archie-search-type'.  Possible values are substring, subcase (case insensitive
substring), and regexp (a regular expression).  Interactively, a prefix arg
will make it prompt for this."
  (interactive (list (read-string "String: " nil)
		     (and current-prefix-arg
			  (read-string "Search type [-c/-e/-r/-s]: "
				       archie-search-type))))
  (let ((buf (or inplace (get-buffer-create "*Archie*"))))
    (if inplace
	(progn
	  (push-mark (point) t)
	  (insert "Archie sez: \n"))
      (progn
	(pop-to-buffer buf)
	(setq buffer-read-only nil
	      mode-line-process (concat ": " string)
	      mode-name "Query for"
	      minor-mode-alist nil)
	(erase-buffer)))
    (call-process archie-program nil buf nil
		  "-h" archie-server
		  (or type archie-search-type)
		  string))

  (if (not inplace)
      (progn
	(setq buffer-read-only t)
	(goto-char (point-min)))))
