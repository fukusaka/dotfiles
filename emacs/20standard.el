;; standard.el

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

;;
;; ɽ���κ٤�������
;;
(setq next-line-add-newlines nil)
(setq line-number-mode t)
(setq column-number-mode t)
(setq scroll-step 1)

(if (not window-system)
    (menu-bar-mode 0))

;;(setq truncate-lines t)
;; ���פ�ɽ��
;;(setq display-time-day-and-date nil)
;;(display-time)

;; X��������Ĥ餵�ʤ��褦�ˤ���Τǡ�����
;; ~/.xsession -->
;;   xset b off
(if (not window-system)
    (setq visible-bell t)
  )

(setq text-mode-hook
      '(lambda () (auto-fill-mode 1)))

;;
;; ���ޥ�����Ϥ����� (comint-mode)
;;

;; �ҥ��ȥ�������ꤹ�롣
(setq comint-input-ring-size 200)

;; Password �򱣤��ޤۤ�
(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt)

;;
;; ��ư���̤���⡼�ɤ�����
;;
(setq auto-mode-alist
      (append
       '(("\\.pl$" . cperl-mode)
	 ("\\.mht$" . html-mode)
	 ("\\.po[tx]?\\'\\|\\.po\\." . po-mode)
	 )
       auto-mode-alist))

(autoload 'po-mode "po-mode")

;;
;; ���̥ե������ư��Ÿ��������
;;
;; tar-mode ���Ȥ߹�碌���EUC��������������������
;; JIS�������ޤ�ɽ������롢����detect-coding-region/string�ΥХ�?��
;; ver19 �Ǥϥ����롢����

(cond
 ((string-match "^20" emacs-version)
  (auto-compression-mode)

  ;; bzip2, a block-sorting file compressor.  Version 0.9.0, 30-Aug-98.
  (setq jka-compr-compression-info-list 
	(append
	 '(["\\.bz2\\'"
	    "bzip2ing"        "bzip2"         ()
	    "bunzip2ing"      "bzip2"         ("-d")
	    nil t])
	 jka-compr-compression-info-list))
  ))

;;
;; EDITOR=emacsclient�� emacs �ǳ���
;; PAGER=emacsclient�� emacs �ǳ���
;;
(server-start)

;;
;; semi-gnus ������ե�����λ���
;;

(setq gnus-init-file (concat moi::host-conf-dir "gnus"))

(let ((el  (concat gnus-init-file ".el"))
      (elc (concat gnus-init-file ".elc")))
  (if (file-newer-than-file-p el elc)
      (byte-compile-file el)
    )
  )
