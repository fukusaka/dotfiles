;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GNUS                                                               ;;
;;   NetNews �꡼���� GNUS                                            ;;
;;   M-x gnus �ǵ�ư���ޤ�                                            ;;
;;                                                                    ;;
;; emacs20.3        --> semi-gnus 6.8.x + semi + apel + flim          ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(load "mime-setup")

;;(setq gnus-local-domain "so-net.ne.jp")
;;(setq gnus-local-organization "from So-net")

;; NNTP �����С������ɤ�
(setq gnus-select-method '(nntp "news01.so-net.ne.jp"))
;;(setq gnus-select-method '(nntp "localhost"))

;;(setq nnmail-spool-file nil)
;;(setq gnus-secondary-select-methods
;;      '((nnmh "private")))

;;;	(nntp "forums.macromedia.com")))

(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)
(setq gnus-group-line-format
      "%M\%S\%p\%P\%5y: %(%-40,40g%) %6,6~(cut 2)d\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ���ɤ˴ؤ�������

;; active �ե�������ɤ߹��ߤ�����
;;   ���ɤ��Ƥ���Τ��������å�(=>������INN�Ǥʤ����٤������)
(setq gnus-read-active-file 'some)
;; ������NG�Υ����å�������
(setq gnus-check-new-news nil)
;(setq gnus-check-new-news 'ask-server)
;(setq gnus-save-killed-list nil)
;; �˥塼�������Фˤʤ���(bogus)NG������å����ʤ�
(setq gnus-check-bogus-newsgroups nil)

;; �������˥塼�����롼�פ��Ǥ��������н�
(setq gnus-subscribe-newsgroup-method 'gnus-subscribe-zombies)

;; ���ɥ��롼�פ�����
;;(setq gnus-options-subscribe
;;      "^nnfolder\\|^nnmh" )
;;(setq gnus-options-not-subscribe "^alt\\|^rec")

;; ̤�ɿ����礭������ HEADER �������ɤ�
(setq gnus-large-newsgroup 400)
;;(setq gnus-large-newsgroup 100)

;; `SPACE' ���ޥ�ɤǥ��롼�פ����ä��Ȥ��ˡ�
;; ��ưŪ�˵��������򤹤뤫�ɤ���
(setq gnus-auto-select-first nil)

;;���롼�ץ�٥�
;;�����Ф��䤤��碌��٥�
(setq gnus-activate-level 5)

;; ��Ʊ���˵������ɤߤޤ�(���ɤ�)
(setq gnus-asynchronous nil)
;;(setq gnus-use-article-prefetch 15)
;; ���롼�פ�ȴ����ޤǲ����Ƥ���
(setq gnus-prefetched-article-deletion-strategy '(exit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ɽ���˴ؤ�������

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; ɽ��������ν��
(setq gnus-thread-sort-functions 
      '(gnus-thread-sort-by-date
	;;gnus-thread-sort-by-subject
	;;gnus-thread-sort-by-score
	))

(setq gnus-use-full-window t)
(setq gnus-window-configuration
      '((SelectNewsgroup (0 1 0))
	(ExitNewsgroup   (1 0 0))
	(SelectArticle   (0 6 10))
	(ExpandSubject   (0 1 0))))

;; �����ƥ�����ɽ���Ǥθ�����إå�������
;;(setq gnus-show-all-headers t)
;;(setq gnus-visible-headers "^From:\\|^Subject:\\|^Date:")
(setq gnus-ignored-headers "^References:\\|^Xref:")
;;(add-hook 'gnus-article-display-hook 'gnus-article-hide-boring-headers)
;; �إå��ν��
(setq gnus-sorted-header-list '("^Subject:" "^From:" "^Date:"))

(setq gnus-boring-article-headers
      '(empty followup-to reply-to long-to many-to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ��������¸���뻨¿������
;; 1�����ƤΥإå�����¸ 2��PlainFile ����¸
(setq gnus-save-all-headers t)
(setq gnus-use-long-file-name t)
(setq gnus-default-article-saver 'gnus-summary-save-in-file)
(setq gnus-article-save-directory "~/Doc/News/")

(add-hook 'gnus-summary-mode-hook
	  '(lambda ()
	     (setq gnus-summary-expunge-below -500)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ��Ƥ˴ؤ�������

;;(setq gnus-use-generic-from nil)

;; Message-ID ���դ��ʤ�������Ƥ��뤫�ʤ���
(setq message-required-news-headers
      '(From Newsgroups Subject Date ;; Message-ID
	     (optional . Organization) Lines
	     (optional . X-Newsreader)))

;;(gnus-agentize)
