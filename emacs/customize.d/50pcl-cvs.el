;;; 50pcl-cvs.el --
;; $Id$

;; Copyright (C) 2001 Shoichi Fukusaka

;; Author: Shoichi Fukusaka <fukusaka@indexweb.co.jp>
;; Maintainer: Shoichi Fukusaka <fukusaka@indexweb.co.jp>
;; Created: 07 Aug 2001
;; Version: 1.0
;; Keywords: 

;; This file is part of 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:

;;; Change log:

;;; Code:

(unless (featurep 'psvn)
  (autoload 'svn-status "psvn" nil t))

(setq cvs-diff-flags '("-u"))

(cond

 ((featurep 'mac-carbon)
  (setq svn-status-svn-executable "/usr/bin/svn"))

 ((eq system-type 'windows-nt)
  ;; svn �̃o�C�i���� http://subversion.tigris.org/ �������Ă��A
  ;; Emacs���̃f�t�H���g�� ShiftJIS ���g������A�ǉ��̐ݒ�͖����B
  ;; �Ό��̃��|�W�g���̃��O���b�Z�[�W��UTF-8�ŕۑ������B
  )
 )
;;; 50pcl-cvs.el ends here
