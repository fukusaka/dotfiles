;;
;; 自動識別するモードの設定
;;

(dolist (e (nreverse
            '(
	      ("\\.h\\'" . c++-mode)
              ("\\.pl\\'" . perl-mode)
              ("\\.mm\\'" . objc-mode)
              ("\\.CPP\\'" . c++-mode) ;; DOS/Windows 系だと大文字になってる場合がある

	      ("\\.y\\'" . bison-mode)
	      ("\\.l\\'" . flex-mode)
	      ("\\.applescript\\'" . applescript-mode)
	      ("\\.as\\'" . actionscript-mode)
	      ("\\.cs\\'" . csharp-mode)
	      ("\\.php\\'" . php-mode)

              ;; epcg ? 用
              ("\\.pgc\\'" . c-mode)
              ("\\.pgcc\\'" . c++-mode)
	      )))
  (add-to-assoc-list 'auto-mode-alist e))

(autoload 'bison-mode "bison-mode"
  "Major mode for editing bison/yacc files" t)

(autoload 'flex-mode "flex-mode"
  "Major mode for editing flex files" t)

(autoload 'applescript-mode "applescript-mode"
  "major mode for editing AppleScript source." t)

(autoload 'actionscript-mode "actionscript-mode"
  "Major mode for editing Actionscript files." t)

(autoload 'csharp-mode "csharp-mode"
  "Major mode for editing C# code." t)

(autoload 'php-mode "php-mode"
  "Major mode for editing PHP code." t)

;;(defalias 'js-mode 'js2-mode)
(autoload 'js2-mode "js2"
  "Major mode for editing JavaScript code." t)

(require 'mmm-mode)
(require 'mmm-sample)

(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 1)

(mmm-add-mode-ext-class nil "\\.html\\'" 'embedded-css)
(mmm-add-mode-ext-class nil "\\.html\\'" 'html-js)

(mmm-add-mode-ext-class nil "\\.php\\'" 'embedded-css)
(mmm-add-mode-ext-class nil "\\.php\\'" 'html-js)
(mmm-add-mode-ext-class nil "\\.php\\'" 'html-php)
