
(require 'muse-mode)     ; load authoring mode
(require 'muse-html)     ; load publishing styles I use
(require 'muse-latex)

(require 'muse-project)  ; publish files in projects

(setq muse-project-alist
      '(("Muse"
         ("~/Documents/Muse/Pages" :default "index")
         (:base "html" :path "~/Documents/Muse/public_html"))))
