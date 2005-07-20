;; Menubar

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

;; change menu-bar action for [rmail] from rmail to mew
(cond
 ((featurep 'xemacs)
  nil)

 ((string-match "^2[01]" emacs-version)
  (define-key menu-bar-tools-menu [rmail] '("Read Mail(mew)" . mew))
  (define-key menu-bar-tools-menu [compose-mail] '("Send Mail(mew)" . mew-send))
  )
 )
