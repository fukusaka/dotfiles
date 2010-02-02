;;; @@FNAME@@ --
;; $Id$

;; Copyright (C) @@YEAR@@ @@NAME@@

;; Author: @@NAME@@ <@@ADDRESS@@>
;; Maintainer: @@NAME@@ <@@ADDRESS@@>
;; Created: @@DATE@@
;; Version: 1.0
;; Keywords: 

;; This file is part of 

@@LICENSE@@

;;; Commentary:

;;; Change log:

;;; Code:


@@@@
FNAME: (file-name-nondirectory buffer-file-name)
NAME: programmer-name
ADDRESS: programmer-mail-address
YEAR: (format-time-string "%Y")
DATE: (format-time-string "%d %b %Y")
LICENSE: (moi::license-string 'BSD ";; " t)
@@@@

;;; @@FNAME@@ ends here
