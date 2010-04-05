;;
;; 自動識別するモードの設定
;;

(setq auto-mode-alist
      (append
       '(
	 ("\\.h\\'" . c++-mode)
	 ("\\.pl\\'" . perl-mode)
	 ("\\.y\\'" . bison-mode)
	 ("\\.l\\'" . flex-mode)
	 ("\\.applescript$" . applescript-mode)

	 ;; DOS/Windows 系だと大文字になってる場合がある
	 ("\\.CPP\\'" . c++-mode)

	 ;; 差分に色づけする
	 ("patch" . my-patch-view-mode)
	 ("\\.diff" . my-patch-view-mode)

	 ;; epcg ? 用
	 ("\\.pgc\\'" . c-mode)
	 ("\\.pgcc\\'" . c++-mode)
	 )
       auto-mode-alist))

(autoload 'bison-mode "bison-mode" "bison" t)
(autoload 'flex-mode "flex-mode" "flex" t)
(autoload 'applescript-mode "applescript-mode" "major mode for editing AppleScript source." t)

(autoload 'my-patch-view-mode "my-patch-view")
