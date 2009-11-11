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

(require 'vc-git)
(add-to-list 'vc-handled-backends 'GIT)

;; for MaGit
(autoload 'magit-status "magit" nil t)

(cond

 ;; MacPorts の Subversion 1.5系を使う
 ((featurep 'mac-carbon)
  (setq svn-status-svn-executable "/opt/local/bin/svn"))

 ((eq system-type 'windows-nt)
  ;; svn のバイナリを http://subversion.tigris.org/ から取ってき、
  ;; Emacs内のデフォルトを ShiftJIS を使う限り、追加の設定は無い。
  ;; 対向のレポジトリのログメッセージはUTF-8で保存される。
  )
 )
