;;
;; 日本語環境で UTF-8 をメインにする設定
;;

;; 日本語環境
(set-language-environment 'Japanese)

;; UTF8サポートありならば、優先してUTF-8を使う
(if (memq 'utf-8 coding-system-list)
    (prefer-coding-system 'utf-8-unix))

;; 但し、Windows系のみファイル名はロケール(ShiftJIS)を使う
(if (or (eq system-type 'windows-nt)
        (eq system-type 'cygwin))
    (setq-default file-name-coding-system 'japanese-shift-jis-dos))

;; Windows系は processI/O が utf-8 になるので必要に応じて変更すること

;; プロセスの環境変数設定
(unless (or (eq system-type 'windows-nt)
            (eq system-type 'cygwin)
            (not (memq 'utf-8 coding-system-list)))
  (setenv "LANG" "ja_JP.UTF-8"))

;; GNU/Linux で egg-anthy を使う設定
(if (eq system-type 'gnu/linux)
    (set-language-info "Japanese" 'input-method "japanese-anthy"))

;; Cocoa inline_path 対応
(if (featurep 'ns)
    (set-language-info "Japanese" 'input-method "MacOSX"))

;; NTEmacs IME 対応
(when (featurep 'w32-ime)
  (require 'w32-ime-config)
  (set-language-info "Japanese" 'input-method "W32-IME"))
