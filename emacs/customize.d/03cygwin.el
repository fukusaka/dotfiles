;;
;; Windows系でCygwin対応コード
;;

(when (memq system-type '(cygwin windows-nt))

  ;; cygwin のパスを確認する
  (defvar cygwin-top-directory "C:/cygwin/")

  ;; cygwin のパスを追加
  (add-to-list
   'exec-path
   (expand-file-name (concat cygwin-top-directory "bin"))
   t)

  ;; cygwin のバージョン取得
  (let* ((text (shell-command-to-string "cygcheck.exe -cd cygwin"))
         (version
          (if (and text
                   (string-match ".*cygwin +\\([0-9.]+\\).*" text))
              (match-string 1 text)))
         (vers (if version (split-string version "\\.")))
         (major (if (listp vers) (nth-value 1 vers)))
         (minor (if (listp vers) (nth-value 2 vers))))

    (defvar cygwin-version version)
    (defvar cygwin-major-version (string-to-number major))
    (defvar cygwin-minor-version (string-to-number minor)))

  ;; cygwin-mount.el
  (when (locate-library "cygwin-mount")
    (require 'cygwin-mount)
    (cygwin-mount-activate))

  (defun setenv-cygwin (value)
    (let* ((func '(lambda (val)
                    (string-match "\\(no\\)?\\([^:]*\\)\\(:.*\\)?" val)
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

  ;; CYGWINの設定
  (setenv-cygwin "nodosfilewarning")
  (setenv-cygwin "tty")

  ;; Shell としてCygwinのBashを使う
  (when (executable-find "bash")
    (setq shell-file-name "bash")
    (setenv "SHELL" shell-file-name)

    ;; shellモードで色付け
    (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

  ;; Cygwin の Info 追加
  (add-to-list
   'Info-default-directory-list
   (expand-file-name (concat cygwin-top-directory "usr/share/info")) t)

  ;; cygwin-1.7のman(groff)は日本語が使えない。。。
  (setq manual-program "LANG=C man")

  ;; cygwin のfindを明示的に指定
  (setq find-program (concat cygwin-top-directory "bin/find"))

  (defadvice prefer-coding-system
    (after my-prefer-coding-system activate)
    (let ((coding (coding-system-base (ad-get-arg 0))))
      (cond
       ;; Windows系でロケール(ShiftJIS)を使う場合
       ((coding-system-equal 'shift_jis coding)
        (setenv "LANG" "ja_JP.SJIS")

        (setq my-prefer-utf8-for-cygwin nil)
        )
       ;; Windows系でUTF-8を使う場合
       ((coding-system-equal 'utf-8 coding)
        (setenv "LANG" "ja_JP.UTF-8")

        (setq my-prefer-utf8-for-cygwin t)
        )
       )
      )
    )

;;; Follow Cygwin symlinks.
;;; Handles old-style (text file) symlinks and new-style (.lnk file) symlinks.
;;; (Non-Cygwin-symlink .lnk files, such as desktop shortcuts, are still loaded as such.)
  (defun follow-cygwin-symlink ()
    "Follow Cygwin symlinks.
Handles old-style (text file) and new-style (.lnk file) symlinks.
\(Non-Cygwin-symlink .lnk files, such as desktop shortcuts, are still
loaded as such.)"
    (save-excursion
      (goto-char 0)
      (cond
       ((looking-at
         "L\x000\x000\x000\x001\x014\x002\x000\x000\x000\x000\x000\x0C0\x000\x000\x000\x000\x000\x000\x046\x00C")
        (re-search-forward
         "\x000\\([-A-Za-z0-9_\\.\\\\\\$%@(){}~!#^'`][-A-Za-z0-9_\\.\\\\\\$%@(){}~!#^'`]+\\)")
        (find-alternate-file (match-string 1))) ;; ?
       ((looking-at "!<symlink>\377\376")
        (re-search-forward "!<symlink>\\(\377\376.*\\)")
        (find-alternate-file
         (decode-coding-string (match-string 1) 'utf-16)))
       ((looking-at "!<symlink>")
        (re-search-forward "!<symlink>\\(.*\\)\0")
        (find-alternate-file (match-string 1)))
       )))

  (add-hook 'find-file-hooks 'follow-cygwin-symlink)
  )
