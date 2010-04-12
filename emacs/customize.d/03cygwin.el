;;
;; Windows系でCygwin対応コード
;;

(when (or (eq system-type 'windows-nt)
          (eq system-type 'cygwin))

  ;; cygwin のバージョン取得
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
    (defvar cygwin-minor-version (string-to-number minor)))

  (defun setenv-cygwin (value)
    (let* ((func '(lambda (val)
                    (string-match "\\\(no\\\)?\\\([^:]*\\\)\\\(:.*\\\)?" val)
                    (match-string 2 val)))
           (env (getenv "CYGWIN"))
           (envs (if env (split-string env " ")))
           replaced)
      (setq envs
            (mapcar '(lambda (val)
                       (if (not (string=
                                 (funcall func val)
                                 (funcall func value)))
                           val
                         (setq replaced t) value))
                    envs))
      (unless replaced (setq envs (append envs `(,value))))
      (setenv "CYGWIN" (mapconcat 'identity envs " "))))

  (defadvice prefer-coding-system
    (after my-prefer-coding-system activate)
    (let ((coding (coding-system-base (ad-get-arg 0))))
      (cond
       ;; Windows系でロケール(ShiftJIS)を使う場合
       ((coding-system-equal 'shift_jis coding)
        (setenv "LANG" "ja_JP.SJIS")
        (setenv-cygwin "codepage:932")
        )
       ;; Windows系でUTF-8を使う場合
       ((coding-system-equal 'utf-8 coding)
        (setenv "LANG" "ja_JP.UTF-8")
        (setenv-cygwin "codepage:utf8")

        ;; w32-fns.el で適当？に設定されているの修正？
        ;;(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

        )
       )
      )
    ;; ファイル名はCygwin経由では取得しないので常にロケールなのか？
    (if locale-coding-system
        (setq default-file-name-coding-system locale-coding-system))
    )
  )
