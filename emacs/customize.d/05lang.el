;; $Id$
;;
;; ���ܸ�Ķ���EUC��ᥤ��ˤ�������
;;
;; ��������.emacs �Τߤ� iso-2022-jp (jis code?)�ǽ񤫤ʤ���
;; ʸ���󤬲����롢�����Ȥۡ�
;;
;; ~/.login -->
;;   alias emacs "(setenv XMODIFIERS '@im=none'; exec /usr/bin/emacs )"
(cond
 ;; Ver.19 �ξ��
 ((string-match "^19" emacs-version)
  (if (boundp 'MULE)
      (progn
	(set-primary-environment 'japanese)
	(set-display-coding-system         *euc-japan*)
	(set-keyboard-coding-system        *euc-japan*)
	(set-default-file-coding-system    *euc-japan*)
	(set-default-process-coding-system *euc-japan* *euc-japan*)	
	(define-program-coding-system nil nil (cons *euc-japan* *euc-japan*))
	;; �⡼�ɥ����ɤ�ɽ��������
	;; (setq mc-verbose-code t)
	))
  )
 ;; Ver.20 �ξ��(�Խ�ʬ���ʤ�)
 ((string-match "^20" emacs-version)
  (set-language-environment          'Japanese)
  ;; �ۤȤ�ɤξ�� euc-japan-unix ��Ȥ�
  (set-default-coding-systems       'euc-japan-unix)
  ;; emacs -nw �Ǥϡ���Ǥ��٤����ʤΤǡ�������������
  (set-terminal-coding-system       'euc-japan-unix)
  ;;(setq default-process-coding-system '(euc-jp . euc-jp))
  ;; �ե��������(X�꥽���������ꤹ��Τ��٥���)
  ;; ~/.Xresources -->
  ;;   Emacs.Font: -*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-standard
  ;; (set-default-font "fontset-standard")
  ;; (set-default-font "-*-fixed-*-r-normal-*-16-*-*-*-*-*-fontset-standard")
  ;; scroll bar �򱦤ˤ������ꡣ
  (set-scroll-bar-mode 'right)
  (if nil
  (if (not window-system)
      (progn
	;; Translate `C-h' to <DEL>.
	;;(keyboard-translate ?\C-h ?\C-?)
	
	;; Translate <DEL> to `C-h'.
	;;(keyboard-translate ?\C-? ?\C-h)
       ))
  )
  )
 )