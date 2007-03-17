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
 ;; XEmacs 21
 ((featurep 'xemacs)
  (set-language-environment          'Japanese)  
  (set-default-coding-systems       'utf-8-unix)
  (setq coding-system-for-read      'utf-8-unix))

 ;; Carbon Emacs
 ((featurep 'mac-carbon)
  (set-language-environment 'Japanese)
  (let ((cs 'utf-8-unix))
    (prefer-coding-system cs)
    (set-keyboard-coding-system cs)
    (set-terminal-coding-system cs))
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  (setq default-file-name-coding-system 'utf-8) ;; locale ��̵�ط�������
  (setq mac-allow-anti-aliasing t)
  (require 'carbon-font)
  ;;(set-default-font "fontset-hiraginokaku")
  (set-default-font "fontset-standard")) ;; �ե��������(X�꥽���������ꤹ��Τ��٥���)

 ;; Ver.20 �ξ�� EUC-JP
 ((string-match "^20" emacs-version)
  (set-language-environment          'Japanese)
  (set-input-method "japanese-canna")
  (set-default-coding-systems       'euc-japan-unix)
  (set-terminal-coding-system       'euc-japan-unix)
  (setq default-process-coding-system '(euc-jp . euc-jp))
  (set-default-font "fontset-standard"))

 ;; Ver.21 �ξ�� UTF-8
 ((string-match "^2[12]" emacs-version)
  (if (functionp 'un-define-debian) (un-define-debian)) ;; for Debian
  (set-language-environment 'Japanese)
  (set-input-method "japanese-egg-anthy")
  (let ((cs locale-coding-system))
    (prefer-coding-system cs)
    (set-keyboard-coding-system cs)
    (set-terminal-coding-system cs))
  (setq default-file-name-coding-system 'utf-8) ;; locale ��̵�ط�������
  (set-default-font "fontset-standard")) ;; �ե��������(X�꥽���������ꤹ��Τ��٥���)
 )
