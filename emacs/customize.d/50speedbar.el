;; sr-speedbar
(require 'sr-speedbar)

(setq sr-speedbar-right-side nil)
(setq sr-speedbar-width-x 45)

(my-prefix-set-key "\C-s" 'sr-speedbar-toggle)
(my-prefix-set-key "s" 'sr-speedbar-refresh-toggle)
