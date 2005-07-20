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
 ;; Ver.19 �ξ�� EUC-JP
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
	)))

 ;; Ver.20 �ξ�� EUC-JP
 ((string-match "^20" emacs-version)
  (defun my-japanese-setup ()
    (if (equal current-language-environment "Japanese")
	(setq default-input-method "japanese-canna")))
  (add-hook 'set-language-environment-hook 'my-japanese-setup)
  (set-language-environment          'Japanese)
  (set-default-coding-systems       'euc-japan-unix)
  (set-terminal-coding-system       'euc-japan-unix)
  (setq default-process-coding-system '(euc-jp . euc-jp))
  (set-default-font "fontset-standard"))

 ;; Ver.21 �ξ�� UTF-8
 ((string-match "^21" emacs-version)
  (defun my-japanese-setup ()
    (if (equal current-language-environment "Japanese")
	(setq default-input-method "japanese-egg-anthy")))
  (add-hook 'set-language-environment-hook 'my-japanese-setup)
  (set-language-environment          'Japanese)  
  (set-default-coding-systems       'utf-8-unix)
  (set-terminal-coding-system       'utf-8-unix)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  (setq default-file-name-coding-system 'utf-8-unix)

  ;; �ե��������(X�꥽���������ꤹ��Τ��٥���)
  (if (featurep 'xemacs) nil
    (set-default-font "fontset-standard")))
 )
