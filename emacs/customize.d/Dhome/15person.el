;; person.el

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

;; 改客攫鼠を肋年

;;(setq user-full-name "省轰经办")
(setq user-full-name "Shoichi Fukusaka")
(setq user-mail-address "fukusaka@xa2.so-net.ne.jp")

(setq programmer-name "Shoichi Fukusaka")
(setq programmer-mail-address "fukusaka@xa2.so-net.ne.jp")

(setq mew-from "Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>")
(setq mew-mail-domain-list '("xa2.so-net.ne.jp"))
;;(setq mew-mail-domain-list '("xa2.so-net.ne.jp"))

(setq mew-refile-guess-alist
      '(
	;; Mailing List
	("Resent-From:\\|From:"
	 ("gtk\\(-request\\)?@lists.hypercore.co.jp"
	  . "+ML/gtk-ml")
	 ("pgsql-jp@sra.co.jp"            . "+ML/pgsql-jp")
	 ("JF-admin@linux.or.jp"          . "+ML/jf")
	 )
	("Delivered-To:"
	 ("jf@linux.or.jp"                . "+ML/jf")
	 )
	;; Each User
	("From:"
	 ("@so-net.ne.jp"                 . "+Official/So-net")
	 ("info@jpcert.or.jp"             . "+Official/JPCERT")
	 )
	("Subject:"
	 ("system check"	  . "+security/syslog")
	 ("\\[psdev-ml"			  . "+ML/psdev")
	 )
	))

(setq diary-file "~/Mail/diary")

(setq elmo-imap4-default-server "localhost")
