/* @@FNAME@@ -- 
 *
 * Copyright (C) @@YEAR@@ @@NAME@@
 *
 * Author: @@NAME@@ <@@ADDRESS@@>
 * Maintainer: @@NAME@@ <@@ADDRESS@@>
 * Created: @@DATE@@
 * Version: 1.0
 * Keywords: 
 *
 * $Id$
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifndef @@HEADER_DEFINDED@@
#define @@HEADER_DEFINDED@@

#endif /* @@HEADER_DEFINDED@@ */

@@@@
FNAME: (file-name-nondirectory buffer-file-name)
NAME: programmer-name
ADDRESS: programmer-mail-address
YEAR: (format-time-string "%Y")
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
