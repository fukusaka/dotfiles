;; Menubar

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

;; change menu-bar action for [rmail] from rmail to mew
(cond ((string-match "^20" emacs-version)
       (define-key menu-bar-tools-menu [rmail] '("Read Mail(mew)" . mew))
       (define-key menu-bar-tools-menu [compose-mail] '("Send Mail(mew)" . mew-send))
       )
      ((string-match "^19.34" emacs-version)
       (define-key menu-bar-tools-menu [rmail] '("Read Mail(mew)" . mew))
       )
      ((string-match "^19.28" emacs-version)
       (define-key menu-bar-file-menu [rmail] '("Read Mail(mew)" . mew))
       )
      )
