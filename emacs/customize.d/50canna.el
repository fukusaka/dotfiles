;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ����ʤ�Ȥ�����
;;
(cond
 ;; Ver.19 �ξ��
 ((string-match "^19" emacs-version)
  (if (boundp 'MULE)
      (progn
	(if (and (boundp 'CANNA) CANNA) ; �ؤ����/emacs�٤Ǥ��뤳�Ȥ��ǧ����
	    ;; �ؤ����/emacs�� �ξ������ʲ���¹Ԥ��ޤ���
	    (progn
	      (cond ((boundp 'egg-version)
		     (require 'can-n-egg)
		     (can-n-egg))
		    (t
		     (require 'canna)
		     (canna)
		     ;; (global-set-key "\C-o" 'canna-toggle-japanese-mode)
		     ))
	      (setq canna-use-color t)
	      ))
	;;
	;; ���ޤ��Ǥ���­
	;;
	(if (boundp 'egg-version)
	    (progn
	      (setq enable-double-n-syntax t)
	      (its-defrule "dhi" "�Ǥ�" nil nil "roma-kana")
	      (its-defrule "dhu" "�Ǥ�" nil nil "roma-kana")
	      (its-defrule "A" "��" nil nil "roma-kana")
	      (its-defrule "I" "��" nil nil "roma-kana")
	      (its-defrule "U" "��" nil nil "roma-kana")
	      (its-defrule "E" "��" nil nil "roma-kana")
	      (its-defrule "O" "��" nil nil "roma-kana")
	      ))
	)))
 ;; Ver.20 �ξ��(�Խ�ʬ)
 ((string-match "^20.3" emacs-version)
  ;; ��ˡؤ����/emacs�٤ʤΤǡ�������
  (set-input-method "japanese-canna")
  ;; �ȤäƤ��ñ�ˤʤä�����������
  )
 ((string-match "^20.4" emacs-version)
  ;; ����ʤ���ä���Ƥ��졪
  (load "yc")
  )
 )
