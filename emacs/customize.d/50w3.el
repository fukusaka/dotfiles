;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs/W3 Configuration
;;
(if (featurep 'w3-auto)
    (cond
     ((string-match "^20" emacs-version)
      (require 'w3-auto "w3-auto")
      (setq w3-default-homepage "http://sheep-poe/~fukusaka")
      ;;(setq url-proxy-services '(("http"     . "localhost:3128")
      ;;			     ))
      ;;(setq url-be-asynchronous t)
      ;;(setq w3-delay-image-loads t)
      ;;(setq url-xterm-command "kterm -title %s -ut -e %s %s %s")
      )))

