

(defvar moi-pgp-key-id "Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>")

(defun moi-pgp-encode (file)
  (let ((moi (find-file file))
	(moi2 (generate-new-buffer "*MoiTemp2*"))
	(moi3 (generate-new-buffer "*MoiTemp3*")))
    (set-buffer moi)
    (shell-command-on-region
     (point-min) (point-max)
     (format "pgp -fea \'%s\'" moi-pgp-key-id)
     moi2 nil moi3)
    ))

(defun moi-pgp-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'moi-pgp-mode)     ; `describe-mode'が説明
  (setq mode-name "Moi-Pgp")          ; モード行に表示されるモード名。
  (lisp-mode-variables nil)              ; いろいろな変数を定義する。
  (run-hooks 'moi-pgp-mode-hook))     ; ユーザがカスタマイズのために定
					;   定したフックをここで実行する。
