;;
;; 自動識別するモードの設定
;;

(setq auto-mode-alist
      (append
       '(
	 ("\\.h\\'" . c++-mode)
	 ("\\.pl\\'" . perl-mode)
	 ("\\.CPP\\'" . c++-mode)
	 ("patch" . my-patch-view-mode)
	 ("\\.diff" . my-patch-view-mode)
	 ("\\.pgc\\'" . c-mode)
	 ("\\.pgcc\\'" . c++-mode)
	 ("\\.y\\'" . bison-mode)
	 ("\\.l\\'" . flex-mode)
	 )
       auto-mode-alist))

(autoload 'my-patch-view-mode "my-patch-view")
(autoload 'my-sample-ascii "my-sample-ascii" "" t)

(autoload 'bison-mode "bison-mode" "bison" t)
(autoload 'flex-mode "flex-mode" "flex" t)
