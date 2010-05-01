;;
;; 日本語環境で UTF-8 をメインにする設定
;;

;; 日本語環境
(set-language-environment 'Japanese)

(cond
 ;; Windows系で対応
 ((memq system-type '(cygwin windows-nt))

  (prefer-coding-system 'shift_jis-dos)
  ;;(prefer-coding-system 'utf-8-unix)

  ;; NTEmacs内部ではWin32のA系の呼び出しが使われるっぽいので、
  ;; 現在の言語環境でのコードページが固定で使われるのかぁ？
  ;; locale-coding-system 以外では文字化けは避けられないと思う。
  (when locale-coding-system
    ;; ファイル名はロケール依存
    (setq default-file-name-coding-system locale-coding-system)
    ;; IMEパッチで必要らしい。
    (set-keyboard-coding-system locale-coding-system))
  )

 ;; UTF8サポートありならば、優先してUTF-8を使う
 ((memq 'utf-8 coding-system-list)

  (prefer-coding-system 'utf-8-unix)
  (setenv "LANG" "ja_JP.UTF-8")
  )
 )

;; 常に新規ファイルは utf-8-unix を使いたい
(if (memq 'utf-8 coding-system-list)
    (setq-default buffer-file-coding-system 'utf-8-unix))

;;
