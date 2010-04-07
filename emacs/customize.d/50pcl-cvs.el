;;
;; PCL-CVS/PSVN の設定
;;

;; for CVS
(setq cvs-diff-flags '("-u"))

;; for Subversion
(setq svn-status-svn-file-coding-system 'utf-8)         ;; サーバ側はUTF-8にする
(setq svn-status-svn-process-coding-system 'utf-8)      ;;
(modify-coding-system-alist 'process "svn" 'utf-8-unix) ;; svn-diff 文字化け対応
(autoload 'svn-status "psvn" nil t)

;; for Git
(autoload 'git-status "git" nil t)
(autoload 'git-blame-mode "git-blame" nil t)

;; for MaGit
(autoload 'magit-status "magit" nil t)
