;;; 15automode.el --- 

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

;;; Code:

;;
;; 自動識別するモードの設定
;;
(setq auto-mode-alist
      (append
       '(
	 ("\\.h$" . c++-mode)
	 ("\\.pl$" . perl-mode)
	 ("\\.mht$" . html-mode)
	 ("\\.po[tx]?\\'\\|\\.po\\." . po-mode)
	 ("ChangeLog" . change-log-mode)
	 ("patch" . moi-patch-view-mode)
	 ("\\.diff" . moi-patch-view-mode)
	 ("\\.pgc$" . c-mode)
	 ("\\.pgcc$" . c++-mode)
	 ("\\.CPP$" . c++-mode)
	 ("\\.gen_h$" . c++-mode)
	 ("Rakefile" . ruby-mode)
	 )
       auto-mode-alist))

(autoload 'po-mode "po-mode")
(autoload 'moi-patch-view-mode "moi-patch-view")
(autoload 'moi::sample-ascii "moi-sample-ascii" "" t)

;;(auto-compression-mode)

;;; 15automode.el ends here
