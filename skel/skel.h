/* @@FNAME@@ -- 
 * $Id$
 *
 * Copyright (C) @@YEAR@@ @@NAME@@
 *
 * Author: @@NAME@@ <@@ADDRESS@@>
 * Maintainer: @@NAME@@ <@@ADDRESS@@>
 * Created: @@DATE@@
 * Version: 1.0
 * Keywords: 
 *
@@LICENSE@@
 */

#ifndef @@HEADER_DEFINDED@@
#define @@HEADER_DEFINDED@@

#endif /* @@HEADER_DEFINDED@@ */

@@@@
FNAME: (file-name-nondirectory buffer-file-name)
NAME: programmer-name
ADDRESS: programmer-mail-address
YEAR: (format-time-string "%Y")
DATE: (format-time-string "%d %b %Y")
LICENSE: (moi::license-string 'GPL " * ")
HEADER_DEFINDED: (let* ((fname (file-name-nondirectory buffer-file-name))
			(ff (string-to-char-list (concat "_" fname)))
			pp)
		  (while ff
		   (if (= ?. (car ff))
		    (setq pp (cons ?_ pp))
		    (setq pp (cons (car ff) pp)))
		   (setq ff (cdr ff)))
		  (upcase (char-list-to-string (reverse pp))))
@@@@
