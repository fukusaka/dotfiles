;; CocoaEmacs 対応
(when (eq window-system 'ns)
  (require 'ns-win)

  ;; JISキーボード円記号/バックスラッシュ対応
  ;;(if (fboundp 'mac-translate-from-yen-to-backslash)
  ;;    (mac-translate-from-yen-to-backslash))

  (define-key global-map [165] nil)
  (define-key global-map [2213] nil)
  (define-key global-map [3420] nil)
  (define-key global-map [67109029] nil)
  (define-key global-map [67111077] nil)
  (define-key global-map [8388773] nil)
  (define-key global-map [134219941] nil)
  (define-key global-map [75497596] nil)
  (define-key global-map [201328805] nil)
  (define-key function-key-map [165] [?\\])
  (define-key function-key-map [2213] [?\\]) ;; for Intel
  (define-key function-key-map [3420] [?\\]) ;; for PowerPC
  (define-key function-key-map [67109029] [?\C-\\])
  (define-key function-key-map [67111077] [?\C-\\])
  (define-key function-key-map [8388773] [?\M-\\])
  (define-key function-key-map [134219941] [?\M-\\])
  (define-key function-key-map [75497596] [?\C-\M-\\])
  (define-key function-key-map [201328805] [?\C-\M-\\])

  ;; Option/Command キー を Super/Meta に割当
  (setq ns-command-modifier 'meta)
  (setq ns-alternate-modifier 'super)

  ;; システムへ修飾キーを渡さない設定
  (setq mac-pass-control-to-system nil)
  (setq mac-pass-command-to-system nil)
  (setq mac-pass-option-to-system nil)

  (define-key global-map [ns-drag-file] 'ns-find-file)

  (setq select-enable-clipboard nil)
  (setq select-enable-primary t)
  (setq select-active-regions nil)

  ;; see http://stackoverflow.com/questions/13517910/yank-does-not-paste-text-when-using-emacs-over-ssh
  (defun copy-from-osx ()
    "Handle copy/paste intelligently on osx."
    (let ((pbpaste (purecopy "/usr/bin/pbpaste")))
      (if (and (eq system-type 'darwin)
               (file-exists-p pbpaste))
          (let ((tramp-mode nil)
                (default-directory "~"))
            (shell-command-to-string pbpaste)))))

  (defun paste-to-osx (text &optional push)
    "Handle copy/paste intelligently on osx.
TEXT gets put into the Macosx clipboard.

The PUSH argument is ignored."
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx)
  )
