#!/usr/bin/env guile -e main -s
# -*- coding: utf-8 -*-
!#
;;; @@FNAME@@ --
;;
;; Copyright (C) @@YEAR@@ @@NAME@@
;;
;; Author: @@NAME@@ <@@ADDRESS@@>
;; Maintainer: @@NAME@@ <@@ADDRESS@@>
;; Created: @@DATE@@
;; Version: 1.0
;; Keywords:

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

(define (main args)
  (display "Hello World")
  (newline))

@@@@
FNAME: (file-name-nondirectory buffer-file-name)
NAME: programmer-name
ADDRESS: programmer-mail-address
YEAR: (format-time-string "%Y")
DATE: (format-time-string "%d %b %Y")
LICENSE: (my-ask-license-string ";; ")
@@@@

;;; @@FNAME@@ ends here
