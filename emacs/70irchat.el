
;; �������Ѥ��륵����
(setq irchat-server "comicsrv.microsoft.com")

;; IRC�����ФΥꥹ��
(setq irchat-server-alist
      '(("irc.kyoto.wide.ad.jp")
        ("irc.tokyo.wide.ad.jp")
        ("irc.race.u-tokyo.ac.jp")
        ("irc.huie.hokudai.ac.jp")
        ("irc.tohoku.ac.jp")
        ("irc.karrn.ad.jp")
        ("irc.dti.ne.jp")
        ("irc.rcac.tdi.co.jp")))

;; [ɬ��] IRC ��Υ˥å�. �ǥե���Ȥ� whoami �η��
(setq irchat-nickname "Moimoi")

;; /whois ��ɽ�������̾��. �ǥե���Ȥ� GECOS �ե������
(setq irchat-name "�⤤�⤤")

;; CTCP USERINFO ��ɽ����������. �ǥե���Ȥ� nil
(setq irchat-ctcp-userinfo "E-mail: fukusaka@xa2.so-net.ne.jp")

;; �̤���Ȥ��Υ�å�����.  �ǥե���Ȥ� irchat exiting...
;; ��λ���� C-u C-c q �Ȥ�����̤Υ�å������ǽ�λ���뤳�Ȥ��Ǥ���.
(setq irchat-signoff-msg "�Ф�����")

;; �˥å�����ӥ���ͥ�Υ���ץ꡼������
;; ��ʬ���Ȥ� join ���Ƥ������ͥ뤪��ӥ���ͥ�ˤ���ͤ˸¤�
(setq irchat-global-names nil)

;; �����ФȤ���³�����줿���, ��ưŪ�˺���³����
(setq irchat-reconnect-automagic t)

;; ����ͥ뤴�Ȥ˥Хåե�����
(setq irchat-channel-buffer-mode t)

;; ��ȯ�����ɤΥ���ͥ�Ǥʤ��줿��Τ���ɽ��.
;; irchat-channel-buffer-mode �ȵդˤ��Ƥ����Ȥ褤.
(setq irchat-display-channel-always nil)

;; ������֤�, ����ͥ뤴�Ȥ�ȯ�����������뤹��褦�ˤ���
(setq irchat-default-freeze-local nil)

;; \a ��������ȥӡ��פ��Ĥ餹
(setq irchat-beep-on-bells 'always)

;; ��ư���˼�ưjoin�������ͥ�
(setq irchat-startup-channel-list
	'("#GOLDENBEARS"))

;; ����ͥ��ư�Υ����Х����
;; ���������, C-1 : #����ͤ�, C-2 : #����ͤ�:*.jp, C-3: #channel
;; �������꤬�ʤ����, C-(����) �� irchat ��Ŭ���˳�꿶��
;; �ʤ�, #����ͤ�:*.jp �Ȥ�������ͥ��, %����ͤ� �Ƚ񤯤��Ȥ��Ǥ��ޤ�.
(setq irchat-default-channel-binding
	'("#GOLDENBEARS"))

;; ��λ���ˤ��٤ƤΥХåե���������
(add-hook 'irchat-Exit-hook
	    '(lambda ()
	       (mapcar '(lambda (item)
			  (if (or (string-match "^IRC:" (buffer-name item))
				  (string= "*IRC*" (buffer-name item)))
			      (kill-buffer item)))
		       (buffer-list))
	       (setq irchat-Private-buffer (format irchat-buffer-format " Private"))))
