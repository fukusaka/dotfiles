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
