;;; 09package.el --

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-install 'apache-mode)
(package-install 'applescript-mode)
(package-install 'go-mode)
(package-install 'js2-mode)
(package-install 'php-mode)
(package-install 'pretty-lambdada)
(package-install 'python-mode)
(package-install 'tt-mode)
(package-install 'yaml-mode)
(package-install 'mode-compile)
(package-install 'csharp-mode)
(package-install 'psvn)
(package-install 'flymake)
(package-install 'gtags)
(package-install 'auto-complete)
(package-install 'sr-speedbar)
(package-install 'yasnippet-bundle)
(package-install 'popwin) ;; use version 20150315.600
(package-install 'ahg)
;;(package-install 'moccur-edit)
(package-install 'magit)
(package-install 'magit-gitflow)
(package-install 'magit-find-file)
(package-install 'magit-svn)
(package-install 'magit-tramp)
(package-install 'htmlize)
(package-install 'muse)
(package-install 'git)
(package-install 'git-blame)
(package-install 'mmm-mode)
(package-install 'anything)
