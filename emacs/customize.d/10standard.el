;; standard.el

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

;;
;; ɽ���κ٤�������
;;
;;(setq inhibit-startup-message t)
(setq next-line-add-newlines nil)
(setq line-number-mode t)
(setq column-number-mode t)
(setq scroll-step 1)
;;(setq truncate-lines t)
;;(transient-mark-mode t)

;; X��������Ĥ餵�ʤ��褦�ˤ���Τǡ�����
;; ~/.xsession -->
;;   xset b off
;;(setq visible-bell t)

;; ��˥塼�ȥġ�����طʿ�
(when (and window-system (fboundp 'facep))
  (if (facep 'scroll-bar)
      (set-face-background 'scroll-bar "AntiqueWhite"))
  (if (facep 'tool-bar)
      (set-face-background 'tool-bar "AntiqueWhite"))
  )

;; ü���Ǥϥ�˥塼�С���ä�
(if (not window-system)
    (if (fboundp 'menu-bar-mode)
	(menu-bar-mode 0)))

;; scroll-bar �ϱ�¦
(if (fboundp 'set-scroll-bar-mode)
    (set-scroll-bar-mode 'right))

;; tool-bar �Ͼä�
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;;(setq initial-frame-alist '((top . 26) (left . 0) (width . 80) (height . 39)))

;; 

(if (featurep 'mac-carbon)
    (setq default-frame-alist
	  (append (list '(active-alpha . 0.95) ;; active frame
			'(inactive-alpha . 0.95) ;; non active frame
			) default-frame-alist) ))

;; ���פ�ɽ��
;;(setq display-time-day-and-date nil)
;;(display-time)

;; �ҥ��ȥ�������ꤹ�롣
(setq comint-input-ring-size 200)

;; Password �򱣤��ޤۤ�
(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt)

;;
;; EDITOR=emacsclient�� emacs �ǳ���
;; PAGER=emacsclient�� emacs �ǳ���
;;
;;;(if (featurep 'xemacs)
;;;    nil
;;;  (server-start)
;;;  )

