;;
;; PCL-CVS/PSVN の設定
;;

;; for CVS
(setq cvs-diff-flags '("-u"))

;; for Subversion
(autoload 'svn-status "psvn" nil t)

;; for Git
(autoload 'git-status "git" nil t)
(autoload 'git-blame-mode "git-blame" nil t)

;; for MaGit
(autoload 'magit-status "magit" nil t)
