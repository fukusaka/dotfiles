
;; 普段利用するサーバ
(setq irchat-server "comicsrv.microsoft.com")

;; IRCサーバのリスト
(setq irchat-server-alist
      '(("irc.kyoto.wide.ad.jp")
        ("irc.tokyo.wide.ad.jp")
        ("irc.race.u-tokyo.ac.jp")
        ("irc.huie.hokudai.ac.jp")
        ("irc.tohoku.ac.jp")
        ("irc.karrn.ad.jp")
        ("irc.dti.ne.jp")
        ("irc.rcac.tdi.co.jp")))

;; [必要] IRC 上のニック. デフォルトは whoami の結果
(setq irchat-nickname "Moimoi")

;; /whois で表示される名前. デフォルトは GECOS フィールド
(setq irchat-name "もいもい")

;; CTCP USERINFO で表示される情報. デフォルトは nil
(setq irchat-ctcp-userinfo "E-mail: fukusaka@xa2.so-net.ne.jp")

;; ぬけるときのメッセージ.  デフォルトは irchat exiting...
;; 終了時に C-u C-c q とすれば別のメッセージで終了することができる.
(setq irchat-signoff-msg "ばぁ〜い")

;; ニックおよびチャネルのコンプリーションを
;; 自分自身が join しているチャネルおよびチャネルにいる人に限る
(setq irchat-global-names nil)

;; サーバとの接続がきれた場合, 自動的に再接続する
(setq irchat-reconnect-automagic t)

;; チャネルごとにバッファを作る
(setq irchat-channel-buffer-mode t)

;; 各発言がどのチャネルでなされたものかの表示.
;; irchat-channel-buffer-mode と逆にしておくとよい.
(setq irchat-display-channel-always nil)

;; 初期状態で, チャネルごとの発言がスクロールするようにする
(setq irchat-default-freeze-local nil)

;; \a を受け取るとビープを鳴らす
(setq irchat-beep-on-bells 'always)

;; 起動時に自動joinするチャネル
(setq irchat-startup-channel-list
	'("#GOLDENBEARS"))

;; チャネル移動のキーバインド
;; 下の例だと, C-1 : #ちゃねる, C-2 : #ちゃねる:*.jp, C-3: #channel
;; この設定がなければ, C-(数字) は irchat が適当に割り振る
;; なお, #ちゃねる:*.jp というチャネルは, %ちゃねる と書くことができます.
(setq irchat-default-channel-binding
	'("#GOLDENBEARS"))

;; 終了時にすべてのバッファを削除する
(add-hook 'irchat-Exit-hook
	    '(lambda ()
	       (mapcar '(lambda (item)
			  (if (or (string-match "^IRC:" (buffer-name item))
				  (string= "*IRC*" (buffer-name item)))
			      (kill-buffer item)))
		       (buffer-list))
	       (setq irchat-Private-buffer (format irchat-buffer-format " Private"))))
