;;
;; 日本語環境で UTF-8 をメインにする設定
;;

;; 日本語環境
(set-language-environment 'Japanese)

(cond
 ;; Windows系はロケール(ShiftJIS)を使い、
 ((or (eq system-type 'windows-nt)
      (eq system-type 'cygwin))

  (prefer-coding-system 'shift_jis-dos)
  (setenv "LANG" "ja_JP.SJIS"))

 ;; UTF8サポートありならば、優先してUTF-8を使う
 ((memq 'utf-8 coding-system-list)

  (prefer-coding-system 'utf-8-unix)
  (setenv "LANG" "ja_JP.UTF-8"))
 )


;; Windows系でも、utf-8-unix を使いたい場合は個別に指定する
(when (or (eq system-type 'windows-nt)
          (eq system-type 'cygwin))

  ;; 新規ファイル
  (setq-default buffer-file-coding-system 'utf-8-unix)
  )
