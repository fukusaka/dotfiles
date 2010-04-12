;;
;; 日本語環境で UTF-8 をメインにする設定
;;

;; 日本語環境
(set-language-environment 'Japanese)

(cond
 ;; Windows系で対応
 ((or (eq system-type 'windows-nt)
      (eq system-type 'cygwin))

  ;; Cygwin 1.7.x以降 UTF-8ロケールをサポートしてるので
  ;; このUnix互換コマンドを UTF8ロケールで使うかどうか?
  (defvar my-prefer-utf8-locale-for-cygwin
    (>= cygwin-major-version 7))

  ;; 1.7.4 でもまだ対応レベルが低いようなのでまだ使わない
  (setq my-prefer-utf8-locale-for-cygwin nil)

  (prefer-coding-system 'shift_jis-dos)
  ;;(prefer-coding-system 'utf-8-unix)
  )

 ;; UTF8サポートありならば、優先してUTF-8を使う
 ((memq 'utf-8 coding-system-list)

  (prefer-coding-system 'utf-8-unix)
  (setenv "LANG" "ja_JP.UTF-8"))
 )

;; 常に新規ファイルは utf-8-unix を使いたい
(if (memq 'utf-8 coding-system-list)
    (setq-default buffer-file-coding-system 'utf-8-unix))

;;
