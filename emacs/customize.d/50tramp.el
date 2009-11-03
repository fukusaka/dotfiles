;;; 50tramp.el --- 

;; Copyright (C) 2009  Shoichi Fukusaka

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>
;; Keywords: 

(autoload 'tramp-compile "tramp-util")

(define-key global-map "\M-c" 'tramp-compile)
(define-key global-map "\C-zc" 'tramp-compile)

(setq tramp-default-method "sshx")



;;; 50tramp.el ends here
