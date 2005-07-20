;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs/W3 Configuration
;;
(setq w3-default-homepage "http://fuku.moimoitei.jp/private/pukiwiki/")
;;(setq url-proxy-services '(("http"     . "localhost:3128")
;;			     ))
;;(setq url-be-asynchronous t)
;;(setq w3-delay-image-loads t)
;;(setq url-xterm-command "kterm -title %s -ut -e %s %s %s")

;; Browser
(setq browse-url-xterm-program "kterm")

(setq browse-url-xterm-args
      '("-fl" "-alias-fixed-medium-r-normal--12-*"
	"-geometry" "119x47+0+0"))

(defun browse-url-w3m-xterm (url &optional new-window)
  (interactive (browse-url-interactive-arg "w3m URL: "))
  (if (= 0 (string-match "file:" url))
      (setq url (substring url 5)))
  (message url)
  (apply #'start-process `(,(concat "w3m" url) nil ,browse-url-xterm-program
             ,@browse-url-xterm-args "-e" "w3m" "-F" ,url)))

(setq browse-url-browser-function 'browse-url-w3m-xterm)
