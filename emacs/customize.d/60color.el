;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; �����դ��붦������
;;
;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>
;;
;; $Id$
;;

(if window-system
    (cond
     ;; mule2.3 �Ǥ� hilit19 ��Ȥ�
     ((string-match "^19" emacs-version)
      (setq hilit-mode-enable-list  '(not text-mode)
	    hilit-background-mode   'light
	    hilit-inhibit-hooks     nil
	    hilit-inhibit-rebinding nil
	    hilit-quietly t)
      (require 'hilit19)


      ;; troff �ǿ����դ�������
      (add-hook 'nroff-mode-hook 
		(function (lambda ()
			    (hilit-translate 	string	  nil)
			    )))
      )
     ;; emacs20 �Ǥ� font-lock ��Ȥ�
     ((string-match "^20" emacs-version)
      (global-font-lock-mode 1)
      (setq font-lock-support-mode
	    '((c-mode . fast-lock-mode)
	      (c++-mode . fast-lock-mode)
	      (cc-mode . fast-lock-mode)
	      ))
      (defface moi-string-face
	'((((class color) (background light)) (:foreground "Brown"))
	  (((class color) (background dark)) (:foreground "Salmon"))
	  (t (:italic t)))
	nil
	)
      (setq font-lock-string-face 'moi-string-face)
      )
     ))

