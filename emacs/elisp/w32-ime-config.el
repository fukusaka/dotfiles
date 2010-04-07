;; ごく簡単な対応

(require 'cl)
(require 'w32-ime)

;; IMEパッチで必要らしい。
(set-keyboard-coding-system 'japanese-shift-jis-dos)

;; W32-IME の初期化
(setq-default w32-ime-mode-line-state-indicator "[--]")
(setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))

(dolist (args '((y-or-n-p nil nil) (yes-or-no-p nil nil)
                (universal-argument t nil) (read-string nil nil)
                (read-from-minibuffer nil nil) (read-key-sequence nil nil)
                (map-y-or-n-p nil nil) (read-passwd t t)))
  (apply 'wrap-function-to-control-ime args))

(setq w32-ime-on-cursor-color "blue")
(setq w32-ime-off-cursor-color "black")

(add-hook 'w32-ime-on-hook
          '(lambda () (set-cursor-color w32-ime-on-cursor-color)))

(add-hook 'w32-ime-off-hook
          '(lambda () (set-cursor-color w32-ime-off-cursor-color)))

(add-hook 'minibuffer-setup-hook
          '(lambda ()
             (if (ime-get-mode)
                 (set-cursor-color w32-ime-on-cursor-color)
               (set-cursor-color w32-ime-off-cursor-color))))

(w32-ime-initialize)

(provide 'w32-ime-config)
