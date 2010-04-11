;;
;; VCS (PCL-CVS/PSVN/etc) の設定
;;

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
(when (and (not my-prefer-utf8-locale-for-cygwin)
           (eq system-type 'windows-nt))

  ;; vc-svn.el用
  (setq vc-svn-checkin-switches '("--encoding" "Shift_JIS"))

  ;; psvn.el 用
  (setq svn-status-svn-process-coding-system 'shift_jis)
  (setq svn-status-default-commit-arguments '("--encoding" "SJIS"))

  ;; propedit の時使われる？、、、svn:ignoreに日本語ファイル名に登録するとか？
  (setq svn-status-svn-file-coding-system 'shift_jis)
  )

;; vc-svn.el用
(setq vc-svn-checkin-switches '("--encoding" "UTF-8"))
;;(setq vc-svn-checkin-switches nil)

