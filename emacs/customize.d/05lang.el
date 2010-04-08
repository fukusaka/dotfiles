;;
;; 日本語環境で UTF-8 をメインにする設定
;;

;; 日本語環境
(set-language-environment 'Japanese)

;; Windows系はロケール(ShiftJIS)を使い、
;; UTF8サポートありならば、優先してUTF-8を使う
(unless (or (eq system-type 'windows-nt)
            (eq system-type 'cygwin)
            (not (memq 'utf-8 coding-system-list)))
  (prefer-coding-system 'utf-8-unix))

;; Windows系でも、utf-8-unix を使いたい場合は個別に指定する
(when (or (eq system-type 'windows-nt)
          (eq system-type 'cygwin))

  ;; 新規ファイル
  (setq-default buffer-file-coding-system 'utf-8-unix)
  )

;; プロセスの環境変数設定
(unless (or (eq system-type 'windows-nt)
            (eq system-type 'cygwin)
            (not (memq 'utf-8 coding-system-list)))
  (setenv "LANG" "ja_JP.UTF-8"))
