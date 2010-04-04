;;
;; key bindings fixup
;;

;; ver.21 以降 HOME/END キー対策
(when (>= emacs-major-version 21)
  (define-key global-map [home] 'beginning-of-buffer)
  (define-key global-map [end] 'end-of-buffer))


;; MacOSX対応
(when (eq system-type 'darwin)

  ;; Cocoa のフレーム時
  (when (eq window-system 'ns)
    (require 'ns-win)

    ;; Option/Command キー を Super/Meta に割当
    (setq ns-command-modifier 'meta)
    (setq ns-alternate-modifier 'super)

    ;; バックスラッシュ対応
    (mac-translate-from-yen-to-backslash)
    )


  ;; 端末時のMacOSXのDELキー対策
;;  (when (not window-system)
;;    (global-set-key "\C-h" 'delete-backward-char)
;;    (global-set-key "\e[3~" 'delete-char))

  ;; フレーム時のMacOSXのIM呼び出し抑制
  (when window-system
    (global-unset-key "\C-\\"))
  )
