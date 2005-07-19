;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example-customization.el ---
;; Author           : Manoj Srivastava ( srivasta@tiamat.datasync.com )
;; Created On       : Wed Nov  5 12:56:18 1997
;; Created On Node  : tiamat.datasync.com
;; Last Modified By : Manoj Srivastava
;; Last Modified On : Sat Dec  6 18:08:18 2003
;; Last Machine Used: glaurung.green-gryphon.com
;; Update Count     : 2
;; Status           : Unknown, Use with caution!
;; HISTORY          :
;; Description      :
;; arch-tag: dd83c1e3-1db5-46cd-9589-9659abe9221e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Faces.
;;
(make-face 'sgml-comment-face)
(make-face 'sgml-doctype-face)
(make-face 'sgml-end-tag-face)
(make-face 'sgml-entity-face)
(make-face 'sgml-ignored-face)
(make-face 'sgml-ms-end-face)
(make-face 'sgml-ms-start-face)
(make-face 'sgml-pi-face)
(make-face 'sgml-sgml-face)
(make-face 'sgml-short-ref-face)
(make-face 'sgml-start-tag-face)

(set-face-foreground 'sgml-comment-face "dark green")
(set-face-foreground 'sgml-doctype-face "maroon")
(set-face-foreground 'sgml-end-tag-face "blue2")
(set-face-foreground 'sgml-entity-face "red2")
(set-face-foreground 'sgml-ignored-face "maroon")
(set-face-background 'sgml-ignored-face "gray90")
(set-face-foreground 'sgml-ms-end-face "maroon")
(set-face-foreground 'sgml-ms-start-face "maroon")
(set-face-foreground 'sgml-pi-face "maroon")
(set-face-foreground 'sgml-sgml-face "maroon")
(set-face-foreground 'sgml-short-ref-face "goldenrod")
(set-face-foreground 'sgml-start-tag-face "blue2")

(setq-default sgml-markup-faces
	      '((comment . sgml-comment-face)
		(doctype . sgml-doctype-face)
		(end-tag . sgml-end-tag-face)
		(entity . sgml-entity-face)
		(ignored . sgml-ignored-face)
		(ms-end . sgml-ms-end-face)
		(ms-start . sgml-ms-start-face)
		(pi . sgml-pi-face)
		(sgml . sgml-sgml-face)
		(short-ref . sgml-short-ref-face)
		(start-tag . sgml-start-tag-face)))

;; Turn on the markup based on whether font-lock would be on
(eval-after-load "psgml"
  '(lambda ()
     (if (boundp 'global-font-lock-mode)
         (if global-font-lock-mode
	     (setq-default sgml-set-face t)
           (setq-default sgml-set-face nil))
       (setq-default sgml-set-face (eq 'x  window-system)))

     (when (default-value 'sgml-set-face)
       (require 'font-lock))
     ;; Lots of overlays in a buffer is bad news since they have to
     ;; be relocated on changes, with typically quadratic
     ;; behaviour.
     ))

(add-hook 'html-mode-hook
	  (lambda () (setq font-lock-defaults nil)))

;;;; Alternately, to use font-lock-mode, expand on:
;;(add-hook 'xml-mode-hook
;;	  (lambda () (make-local-variable 'sgml-set-face)
;;	    (setq sgml-set-face nil)))
