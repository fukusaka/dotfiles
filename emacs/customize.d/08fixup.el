;;
;; system fixup
;;

;; ver.20 以前のdired対応
(when (<= emacs-major-version 20)
  (setenv "LC_TIME" "C"))

;;
;; key bindings fixup
;;

;; ver.21 以降 HOME/END キー対策
(when (>= emacs-major-version 21)
  (define-key global-map [home] 'beginning-of-buffer)
  (define-key global-map [end] 'end-of-buffer))

;; 端末時のDELキー対策
(when (not window-system)
  (global-set-key "\C-h" 'delete-backward-char)
  (global-set-key "\e[3~" 'delete-char)
  )

;; MacOSX対応
(when (eq system-type 'darwin)

  ;; MacOSXではMacPortsへパスを通す
  (add-to-list 'exec-path "/opt/local/bin/")
  (setenv "MANPATH" "/opt/local/man")

  ;; フレーム時のMacOSXのIM呼び出し抑制
  (when window-system
    (global-unset-key "\C-\\"))

  ;; Cocoa のフレーム時
  (when (eq window-system 'ns)
    (require 'ns-win)

    ;; JISキーボード円記号/バックスラッシュ対応
    (mac-translate-from-yen-to-backslash)

    ;; Option/Command キー を Super/Meta に割当
    (setq ns-command-modifier 'meta)
    (setq ns-alternate-modifier 'super)
    )
  )

;; AppleKeyboard(Eisu/Kanji) の挙動を定義
(when (eq window-system 'x)
  ;; X11.app
  (define-key global-map [kanji]
    '(lambda () (interactive) (activate-input-method default-input-method)))
  (define-key global-map [eisu-shift]
    '(lambda () (interactive) (activate-input-method nil)))

  ;; X11 on VMWare Fusion
  (define-key global-map [hiragana-katakana]
    '(lambda () (interactive) (activate-input-method default-input-method)))
  (define-key global-map [eisu-toggle]
    '(lambda () (interactive) (activate-input-method nil)))
  )
