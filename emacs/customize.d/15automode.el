;;
;; 自動識別するモードの設定
;;

(setq auto-mode-alist
      (append
       '(
	 ("\\.h\\'" . c++-mode)
	 ("\\.pl\\'" . perl-mode)
	 ("\\.CPP\\'" . c++-mode)
	 ("patch" . moi-patch-view-mode)
	 ("\\.diff" . moi-patch-view-mode)
	 ("\\.pgc\\'" . c-mode)
	 ("\\.pgcc\\'" . c++-mode)
	 )
       auto-mode-alist))

(autoload 'moi-patch-view-mode "moi-patch-view")
(autoload 'moi::sample-ascii "moi-sample-ascii" "" t)
