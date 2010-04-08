;;
;; VCS (PCL-CVS/PSVN/etc) の設定
;;

;; for CVS
(setq cvs-diff-flags '("-u"))

;; for Subversion
(when (locate-library "psvn")
  (autoload 'svn-status "psvn" nil t)

  (when (eq system-type 'windows-nt)
    ;; サーバ側のコメントはUTF-8で書く運用
    (setq svn-status-svn-file-coding-system 'utf-8)
    (setq svn-status-svn-process-coding-system 'utf-8)
    (modify-coding-system-alist 'process "svn" 'utf-8) ;; svn-diff 文字化け対応
    )
  )

;; for Git / git-coreのcontribにある
(when (locate-library "git")
  (autoload 'git-status "git" nil t)
  (autoload 'git-blame-mode "git-blame" nil t))

;; for MaGit
(when (locate-library "magit")
  (autoload 'magit-status "magit" nil t))
