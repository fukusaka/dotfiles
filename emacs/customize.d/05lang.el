;;
;; 日本語環境で UTF-8 をメインにする設定
;;

;; 日本語環境
(set-language-environment 'Japanese)

(cond
 ;; Windows系で対応
 ((or (eq system-type 'windows-nt)
      (eq system-type 'cygwin))

  ;; cygwin のバージョンチェック
  (let* ((text (shell-command-to-string "cygcheck.exe -cd cygwin"))
         (version
          (if (and text
                   (string-match ".*cygwin +\\\([0-9.]+\\\).*" text))
              (match-string 1 text)))
         (vers (if version (split-string version "\\\.")))
         (major (if (listp vers) (nth-value 1 vers)))
         (minor (if (listp vers) (nth-value 2 vers))))

    (defvar cygwin-version version)
    (defvar cygwin-major-version (string-to-number major))
    (defvar cygwin-minor-version (string-to-number minor))
    )

  ;; Cygwin 1.7.x以降 UTF-8ロケールをサポートしてるので
  ;; このUnix互換コマンドを UTF8ロケールで使うかどうか?
  (defvar my-prefer-utf8-locale-for-cygwin
    (>= cygwin-major-version 7))

  ;; 1.7.4 でもまだ対応レベルが低いようなのでまだ使わない
  ;;(setq my-prefer-utf8-locale-for-cygwin nil)

  (cond
   ;; Windows系でロケール(ShiftJIS)を使う場合
   ((not my-prefer-utf8-locale-for-cygwin)

    (prefer-coding-system 'shift_jis-dos)
    (setenv "LANG" "ja_JP.SJIS")
    (setenv "CYGWIN" "binmode nontsec tty nodosfilewarning codepage:932")

    ;; Windows系でも、新規ファイルはutf-8-unix を使いたい
    (setq-default buffer-file-coding-system 'utf-8-unix)
    )

   ;; Windows系でUTF-8を使う場合
   (t
    (prefer-coding-system 'utf-8-unix)
    (setenv "LANG" "ja_JP.UTF-8")
    (setenv "CYGWIN" "binmode nontsec tty nodosfilewarning codepage:utf8")

    ;; w32-fns.el で適当？に設定されているの修正？
    ;;(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

    ;; ファイル名はSJISで返される？
    (setq file-name-coding-system locale-coding-system)


    ;; ...まだ使えん
    )
   )
  )

 ;; UTF8サポートありならば、優先してUTF-8を使う
 ((memq 'utf-8 coding-system-list)

  (prefer-coding-system 'utf-8-unix)
  (setenv "LANG" "ja_JP.UTF-8"))
 )

;;
