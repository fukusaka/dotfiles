;;
;; 自動識別するモードの設定
;;

(dolist (e (nreverse
            '(("\\.h\\'" . c++-mode)
              ("\\.pl\\'" . perl-mode)
              ("\\.y\\'" . bison-mode)
              ("\\.l\\'" . flex-mode)
              ("\\.applescript$" . applescript-mode)

              ("\\.mm\\'" . objc-mode)

	      ;;("\\.js$" . js2-mode)

              ;; DOS/Windows 系だと大文字になってる場合がある
              ("\\.CPP\\'" . c++-mode)

              ;; 差分に色づけする
              ("patch" . my-patch-view-mode)
              ("\\.diff" . my-patch-view-mode)

              ;; epcg ? 用
              ("\\.pgc\\'" . c-mode)
              ("\\.pgcc\\'" . c++-mode))))
  (add-to-assoc-list 'auto-mode-alist e))

(autoload 'bison-mode "bison-mode"
  "bison" t)

(autoload 'flex-mode "flex-mode"
  "flex" t)

(autoload 'applescript-mode "applescript-mode"
  "major mode for editing AppleScript source." t)

(autoload 'my-patch-view-mode "my-patch-view")

(defalias 'js-mode 'js2-mode)
(autoload 'js2-mode "js2" nil t)
