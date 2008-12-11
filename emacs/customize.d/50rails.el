;;; 50rails.el ---

;; Copyright (C) 2007  Free Software Foundation, Inc.

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>
;; Keywords:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

(when (>= emacs-major-version 22)

  (defun try-complete-abbrev (old)
    (if (expand-abbrev) t nil))

  (setq hippie-expand-try-functions-list
        '(try-complete-abbrev
          try-complete-file-name
          try-expand-dabbrev))

  (setq rails-ws:default-server-type "webrick")

  (require 'rails nil t)
  )

;;; 50rails.el ends here
