;;
;; PCL-CVS/PSVN の設定
;;

(autoload 'svn-status "psvn" nil t)

(setq cvs-diff-flags '("-u"))

(cond

 ((featurep 'mac-carbon)
  (setq svn-status-svn-executable "/usr/bin/svn"))

 ((eq system-type 'windows-nt)
  ;; svn のバイナリを http://subversion.tigris.org/ から取ってき、
  ;; Emacs内のデフォルトを ShiftJIS を使う限り、追加の設定は無い。
  ;; 対向のレポジトリのログメッセージはUTF-8で保存される。
  )
 )
