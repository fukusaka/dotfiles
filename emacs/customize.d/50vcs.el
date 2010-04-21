;;
;; VCS (PCL-CVS/PSVN/etc) の設定
;;

;; VCは自動的にリンクを辿って、かわりに本当のファイルを訪問
(setq vc-follow-symlinks t)

;; VC-DIR を PCL-CVS -status に近づける
(when (>= emacs-major-version 23)

  (add-hook 'vc-dir-mode-hook
            '(lambda ()
               (define-key vc-dir-mode-map "a" 'vc-register)
               (define-key vc-dir-mode-map "c" 'vc-next-action)
               )))

;; for CVS
(setq cvs-diff-flags '("-u"))

;; for Subversion
(when (locate-library "psvn")
  (autoload 'svn-status "psvn" nil t))

(setq svn-status-hide-unmodified t)

;; for Git / git-coreのcontribにある
(when (locate-library "git")
  (autoload 'git-status "git" nil t)
  (autoload 'git-blame-mode "git-blame" nil t))

;; for MaGit
(when (locate-library "magit")
  (autoload 'magit-status "magit" nil t))


;; for Subversion
;; サーバ内部ではログメッセージの文字コードはUTF-8になってる。
;; なので、Windowsでクライアント等のASCII/UTF-8の文字コード以外の場合、
;; Subversionに文字コードを教える必要(コマンドスイッチ)がある。
;; なぜかコマンド引数のエンコードは、coding-system-for-write
;; または default-process-coding-system の decoding で変換される。
(when (eq system-type 'windows-nt)
  (cond

   ;; SJISロケール
   ((not my-prefer-utf8-for-cygwin)

    ;; vc-svn.el用
    (setq vc-svn-checkin-switches '("--encoding" "Shift_JIS"))

    ;; psvn.el 用
    (setq svn-status-default-commit-arguments '("--encoding" "SJIS"))
    (setq svn-status-svn-process-coding-system 'shift_jis)
    (setq svn-status-svn-file-coding-system 'shift_jis)
    )

   ;; UTF8
   (t
    ;; vc-svn.el 用
    (defadvice process-file
      (around my-process-file)
      (let ((coding-system-for-write 'shift_jis)) ;; ここが一番分からん
        ad-do-it))

    (defadvice vc-svn-checkin
      (around my-vc-svn-checkin activate)
      (ad-activate-regexp "my-process-file")
      ad-do-it
      (ad-deactivate-regexp "my-process-file"))
    ;; 今現在の環境(Cygwn1.7.4/LANG=ja_JP.UTF-8)では、
    ;; なぜかこれだけが上手くいくなぜ？
    ;; だれが SJIS->UTF8への変換を行っているのか？
    ;;

    ;; psvn.el 用
    (setq svn-status-svn-process-coding-system 'utf-8)
    (setq svn-status-svn-file-coding-system 'utf-8)
    )
   )

  ;; vc-git.el 用 (vc-git-checkin自体が小さいので置き換えた、、、)
  (defadvice vc-git-checkin
    (around my-vc-git-checkin activate)
    (let ((files (ad-get-arg 0))
          (comment (ad-get-arg 2))
          (fname (make-temp-file "vc-git-")))
      (let ((buffer-file-coding-system 'utf-8-unix))
        (with-temp-file fname (insert comment)))
      (vc-git-command nil 0 files "commit" "-F" fname "--only" "--")
      (delete-file fname)
      ))
  )
;; もう少し整理できそうなんだが、、、
