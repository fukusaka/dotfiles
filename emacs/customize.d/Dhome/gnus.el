;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GNUS                                                               ;;
;;   NetNews リーダー GNUS                                            ;;
;;   M-x gnus で起動します                                            ;;
;;                                                                    ;;
;; emacs20.3        --> semi-gnus 6.8.x + semi + apel + flim          ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(load "mime-setup")

;;(setq gnus-local-domain "so-net.ne.jp")
;;(setq gnus-local-organization "from So-net")

;; NNTP サーバーから読む
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
;; 購読に関する設定

;; active ファイルの読み込みを制御
;;   購読しているのだけチェック(=>新しいINNでないと遅いんだと)
(setq gnus-read-active-file 'some)
;; 新しいNGのチェックを抑制
(setq gnus-check-new-news nil)
;(setq gnus-check-new-news 'ask-server)
;(setq gnus-save-killed-list nil)
;; ニュースサーバにない偽(bogus)NGをチェックしない
(setq gnus-check-bogus-newsgroups nil)

;; 新しいニュースグループができた時の対処
(setq gnus-subscribe-newsgroup-method 'gnus-subscribe-zombies)

;; 購読グループの弁別
;;(setq gnus-options-subscribe
;;      "^nnfolder\\|^nnmh" )
;;(setq gnus-options-not-subscribe "^alt\\|^rec")

;; 未読数が大きい場合も HEADER を全部読む
(setq gnus-large-newsgroup 400)
;;(setq gnus-large-newsgroup 100)

;; `SPACE' コマンドでグループに入ったときに、
;; 自動的に記事を選択するかどうか
(setq gnus-auto-select-first nil)

;;グループレベル
;;サーバに問い合わせレベル
(setq gnus-activate-level 5)

;; 非同期に記事を読みます(先読み)
(setq gnus-asynchronous nil)
;;(setq gnus-use-article-prefetch 15)
;; グループを抜けるまで憶えておく
(setq gnus-prefetched-article-deletion-strategy '(exit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 表示に関する設定

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; 表示する時の順序
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

;; アーティクル表示での見せるヘッダの選択
;;(setq gnus-show-all-headers t)
;;(setq gnus-visible-headers "^From:\\|^Subject:\\|^Date:")
(setq gnus-ignored-headers "^References:\\|^Xref:")
;;(add-hook 'gnus-article-display-hook 'gnus-article-hide-boring-headers)
;; ヘッダの順序
(setq gnus-sorted-header-list '("^Subject:" "^From:" "^Date:"))

(setq gnus-boring-article-headers
      '(empty followup-to reply-to long-to many-to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 記事を保存する雑多な設定
;; 1．全てのヘッダを保存 2．PlainFile で保存
(setq gnus-save-all-headers t)
(setq gnus-use-long-file-name t)
(setq gnus-default-article-saver 'gnus-summary-save-in-file)
(setq gnus-article-save-directory "~/Doc/News/")

(add-hook 'gnus-summary-mode-hook
	  '(lambda ()
	     (setq gnus-summary-expunge-below -500)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 投稿に関する設定

;;(setq gnus-use-generic-from nil)

;; Message-ID を付けない。出来ているかなぁ？
(setq message-required-news-headers
      '(From Newsgroups Subject Date ;; Message-ID
	     (optional . Organization) Lines
	     (optional . X-Newsreader)))

;;(gnus-agentize)
