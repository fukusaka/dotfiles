;;
;; 自動識別するモードの設定
;;

(dolist (e (nreverse
            `(
              ("\\.h\\'" . c++-mode)
              ("\\.mm\\'" . objc-mode)
              ("\\.CPP\\'" . c++-mode) ;; DOS/Windows 系だと大文字になってる場合がある

              ("\\.y\\'" . bison-mode)
              ("\\.l\\'" . flex-mode)
              ("\\.applescript\\'" . applescript-mode)
              ("\\.as\\'" . actionscript-mode)
              ("\\.cs\\'" . csharp-mode)
              ("\\.tpl\\'" . smarty-mode)

              ;; perl
              ("\\.pl\\'" . perl-mode)
              ("\\.psgi\\'" . perl-mode)
              ("\\.t\\'" . perl-mode)

              ;; use MMM-mode
              ("\\.php\\'" . php-mode)
              ("\\.tt\\'" . tt-mode)
              ("\\.ctp\\'" . php-mode)

              ;; apache conf
              ("\\.htaccess\\'"   . apache-mode)
              ("httpd\\(-.*\\)?\\.conf"  . apache-mode)
              ("srm\\.conf\\'"    . apache-mode)
              ("access\\.conf\\'" . apache-mode)
              ("sites-\\(available\\|enabled\\)/" . apache-mode)
              ("https?d/conf/" . apache-mode)

              ("\\.json\\'" . js-mode)

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

(autoload 'tt-mode "tt-mode"
  "Template Toolkit mode." t)

;;(load (concat my-emacs-conf-dir "nxhtml/autostart"))

(autoload 'tt-html-mumamo-mode "mumamo-fun")
