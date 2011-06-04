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
  (add-to-list 'exec-path "/usr/local/bin/")
  (add-to-list 'exec-path "/opt/local/bin/")
  (let ((path (split-string (getenv "PATH") path-separator))
        (manpath (if (getenv "MANPATH")
                     (split-string (getenv "MANPATH") path-separator))))
    (add-to-list 'path "/usr/local/bin")
    (add-to-list 'path "/opt/local/bin")
    (add-to-list 'path (concat (getenv "HOME") "/common/bin"))
    (add-to-list 'path (concat (getenv "HOME") "/bin"))
    (add-to-list 'manpath "/usr/local/man")
    (add-to-list 'manpath "/opt/local/man")
    (add-to-list 'manpath "")

    (setenv "PATH" (mapconcat 'identity path path-separator))
    (setenv "MANPATH" (mapconcat 'identity manpath path-separator))
    )

  ;; フレーム時のMacOSXのIM呼び出し抑制
  (when window-system
    (global-unset-key "\C-\\"))
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

(defun my-sample-popup ()
  (interactive)
  (x-popup-dialog
   t
   '("Sample Popup"
     ("OK" . t))
   t))
