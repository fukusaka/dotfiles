;;
;; Windows系でCygwin対応コード
;;

(when (memq system-type '(cygwin windows-nt))

  ;; cygwin のパスを確認する
  (defvar cygwin-top-directory "C:/App/cygwin/")

  ;; cygwin のパスを追加
  (defvar cygwin-bin-directory
    (expand-file-name (concat cygwin-top-directory "bin")))

  ;; 先頭にCygwinのパスに移動する
  (setq exec-path
	(cons cygwin-bin-directory
	      (remove cygwin-bin-directory exec-path)))

  (let ((path (split-string (getenv "PATH") path-separator)))
    (setq path (remove-if '(lambda (e) (string-equal "C:\\App\\cygwin\\bin" e))
			  path))
    (add-to-list 'path cygwin-bin-directory)
    (setenv "PATH" (mapconcat 'identity path path-separator)))

  ;; cygwin のバージョン取得
  (defvar cygwin-version nil)
  (defvar cygwin-major-version nil)
  (defvar cygwin-minor-version nil)

  (defvar cygwin-api-version nil)
  (defvar cygwin-api-major-version nil)
  (defvar cygwin-api-minor-version nil)

  (defvar cygwin-shared-data-version nil)
  (defvar cygwin-registry-version nil)

  (let* ((text (substring (shell-command-to-string "uname -r") 0 -1))
	 (version
	  (if (and text
		   (string-match "\\([0-9.]+\\)(\\([0-9.]+\\)/\\([0-9.]+\\)/\\([0-9.]+\\))" text))
	      (mapcar '(lambda (c) (match-string c text)) '(1 2 3 4)))))
    (when version
      (setq cygwin-version (nth-value 0 version))
      (setq cygwin-api-version (nth-value 1 version))
      (setq cygwin-shared-data-version
	    (string-to-number (nth-value 2 version)))
      (setq cygwin-registry-version
	    (string-to-number (nth-value 3 version)))

      (let ((vers (split-string (nth-value 0 version) "\\.")))
	(setq cygwin-major-version (string-to-number (nth-value 1 vers)))
	(setq cygwin-minor-version (string-to-number (nth-value 2 vers))))

      (let ((vers (split-string (nth-value 1 version) "\\.")))
	(setq cygwin-api-major-version (string-to-number (nth-value 0 vers)))
	(setq cygwin-api-minor-version (string-to-number (nth-value 1 vers))))
      ))

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

  ;; 環境CYGWINの設定
  (setenv-cygwin "nodosfilewarning")
  (setenv-cygwin "tty")

  ;; cygwin-mount.el
  (when (locate-library "cygwin-mount")
    (require 'cygwin-mount)
    (cygwin-mount-activate))

  ;; Shell としてCygwinのBashを使う
  (when (executable-find "bash")
    (setq shell-file-name "bash")
    (setenv "SHELL" shell-file-name))

  ;; Cygwin の Info 追加
  (add-to-list
   'Info-default-directory-list
   (expand-file-name (concat cygwin-top-directory "usr/share/info")) t)

  ;; cygwin-1.7のman(groff)は日本語が使えない。。。
  (setq manual-program "LANG=C man")

  ;; Prevent issues with the Windows null device (NUL)
  ;; when using cygwin find with rgrep.
  (defadvice grep-compute-defaults
    (around grep-compute-defaults-advice-null-device activate)
    "Use cygwin's /dev/null as the null-device."
    (let ((null-device "/dev/null"))
      ad-do-it))

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
