;;
;; 日本語環境で UTF-8 をメインにする設定
;;

;; 日本語環境
(set-language-environment 'Japanese)

;; Cygwin 1.7.x以降 UTF-8ロケールをサポートしてるので
;; このUnix互換コマンドを UTF8ロケールで使うかどうか?
(setq my-prefer-utf8-locale-for-cygwin t)

(cond
 ;; Windows系でロケール(ShiftJIS)を使う場合
 ((and (or (eq system-type 'windows-nt)
           (eq system-type 'cygwin))
       (not my-prefer-utf8-locale-for-cygwin))

  (prefer-coding-system 'shift_jis-dos)
  (setenv "LANG" "ja_JP.SJIS")
  (setenv "CYGWIN" "binmode nontsec tty nodosfilewarning")

  ;; Windows系でも、新規ファイルはutf-8-unix を使いたい
  (setq-default buffer-file-coding-system 'utf-8-unix)
  )

 ;; Windows系でUTF-8を使う場合
 ((and (or (eq system-type 'windows-nt)
           (eq system-type 'cygwin))
       my-prefer-utf8-locale-for-cygwin)

  (prefer-coding-system 'utf-8-unix)
  (setenv "LANG" "ja_JP.UTF-8")
  (setenv "CYGWIN" "binmode nontsec tty nodosfilewarning codepage:utf8")

  ;; w32-fns.el で適当？に設定されているの修正
  (setq default-process-coding-system '(utf-8-dos . utf-8-unix))
  )

 ;; UTF8サポートありならば、優先してUTF-8を使う
 ((memq 'utf-8 coding-system-list)

  (prefer-coding-system 'utf-8-unix)
  (setenv "LANG" "ja_JP.UTF-8"))
 )

;; TEST