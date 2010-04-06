;;
;; 日本語環境で UTF-8 をメインにする設定
;;

;; 日本語環境
(set-language-environment 'Japanese)

;; UTF8サポートありならば、優先してUTF-8のファイルを使う
(if (memq 'utf-8 coding-system-list)
    (prefer-coding-system 'utf-8-unix))

;; Windows系は除外
(unless (or (eq system-type 'windows-nt)
            (eq system-type 'cygwin)
            (not (memq 'utf-8 coding-system-list)))

  ;; locale に無関係にファイル名は UTF8 として扱う
  (setq default-file-name-coding-system
        (if (eq system-type 'darwin) 'utf-8-nfd 'utf-8))

  ;; プロセスの環境変数設定
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
