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


;; Windows クライアントでもコメントなどの処理は UTF-8にする
;; サーバ側のコメントはUTF-8で書く運用
(when (eq system-type 'windows-nt)

  ;; svn コマンドのI/OはUTF8で行う
  ;;(modify-coding-system-alist 'process "svn" 'sjis)

  ;; vc-svn.el用
  (setq vc-svn-checkin-switches '("--encoding" "Shift_JIS"))

  ;; psvn.el 用
  (setq svn-status-svn-file-coding-system 'utf-8)
  (setq svn-status-svn-process-coding-system 'utf-8)
  )
