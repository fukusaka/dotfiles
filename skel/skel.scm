#!/bin/sh
# -*- scheme -*-
exec guile -s $0 $*
!#
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

;(use-modules (ice-9 debug))
;(use-modules (ice-9 threads))
;(use-modules (ice-9 getopt-long))
;(use-modules (ice-9 string-fun))
;(use-modules (ice-9 format))
;(use-modules (ice-9 regex))
;(use-modules (ice-9 slib))
;(require 'printf)
;(require 'pretty-print)

@@@@
FNAME: (file-name-nondirectory buffer-file-name)
NAME: programmer-name
ADDRESS: programmer-mail-address
YEAR: (format-time-string "%Y")
DATE: (format-time-string "%d %b %Y")
LICENSE: (moi::license-string 'GPL ";; " t)
@@@@

;;; @@FNAME@@ ends here
