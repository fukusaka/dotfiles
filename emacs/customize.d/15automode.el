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
              ("\\.tpl\\'" . smarty-mode)

              ("\\.htaccess\\'"   . apache-mode)
              ("httpd\\(-.*\\)?\\.conf"  . apache-mode)
              ("srm\\.conf\\'"    . apache-mode)
              ("access\\.conf\\'" . apache-mode)
              ("sites-\\(available\\|enabled\\)/" . apache-mode)
              ("https?d/conf/" . apache-mode)

              ;; epcg ? 用
              ("\\.pgc\\'" . c-mode)
              ("\\.pgcc\\'" . c++-mode)
              )))
  (add-to-assoc-list 'auto-mode-alist e t))

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

(autoload 'smarty-mode "smarty-mode"
  "Smarty Mode" t)

(autoload 'python-mode "python-mode"
  "Python editing mode." t)

;;(defalias 'js-mode 'js2-mode)
(autoload 'js2-mode "js2"
  "Major mode for editing JavaScript code." t)

(autoload 'apache-mode "apache-mode"
  "Apache Conf mode." t)


(require 'go-mode-load)