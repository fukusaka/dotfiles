;; CocoaEmacs 対応
(when (eq window-system 'ns)
  (require 'ns-win)

  ;; JISキーボード円記号/バックスラッシュ対応
  (mac-translate-from-yen-to-backslash)

  ;; Option/Command キー を Super/Meta に割当
  (setq ns-command-modifier 'meta)
  (setq ns-alternate-modifier 'super)

  ;; システムへ修飾キーを渡さない設定
  (setq mac-pass-control-to-system nil)
  (setq mac-pass-command-to-system nil)
  (setq mac-pass-option-to-system nil)

  (define-key global-map [ns-drag-file] 'ns-find-file)

  )

