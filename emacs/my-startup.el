;;; my-startup.el  ---  emacsen startup code.

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

(defvar my-emacs-flavor
  (let ((flavor (if (featurep 'xemacs) "xemacs" "emacs")))
    (format "%s%d" flavor emacs-major-version)))

(defvar my-top-conf-dir       (expand-file-name "~/common/conf/"))
(defvar my-emacs-conf-dir     (concat my-top-conf-dir "emacs/"))
(defvar my-elisp-path         (concat my-emacs-conf-dir "elisp/"))

(defvar my-customize-dir      (concat my-emacs-conf-dir "customize.d/"))

(defvar my-hostname-nohost "localhost")

(defvar my-hostname (let ((envhost (shell-command-to-string "echo -n `hostname -s`")))
                        (if envhost envhost my-hostname-nohost)))

(defvar my-place-profile-alist nil)

(defvar my-place-customize-dir
  (let ((alist my-place-profile-alist) place)
    (while (and alist (not place))
      (if (string-match (caar alist) my-hostname)
	  (setq place (cdar alist)))
      (setq alist (cdr alist)))
    (if place (concat my-customize-dir place "/"))))

(defun my-compile-file (file)
  (let* ((el file)
         (el-dir (file-name-directory el))
         (elc-dir (concat el-dir my-emacs-flavor "/"))
         (elc (concat elc-dir (file-name-nondirectory (concat el "c"))))
         (ret el))
    (if (not (file-directory-p elc-dir))
        (make-directory elc-dir))
    (if (file-newer-than-file-p el elc)
        (when (byte-compile-file el)
          (rename-file (concat el "c") elc t)
          (setq ret elc)))
    ret
    ))

(defvar my-ignore-compile-file nil)

(defun my-customize-load (dir)
  (when (and dir (file-directory-p dir))
    (let (file files)
      (setq files (directory-files dir t "^[0-9][0-9].*\\.el$" t))
      (setq files (sort files 'string<))
      (while files
	(setq file (car files))
	(if my-ignore-compile-file
	    (load file)
	  (load (my-compile-file file)))
	(setq files (cdr files))))))
      
(defun my-startup ()

  ;; my-elisp-path以下のディレクトリを全て load-path に追加
  (let ((default-directory my-elisp-path))
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path))

  ;; カスタマイズ設定の読み出し
  (my-customize-load my-customize-dir)

  ;; 場所指定のカスタマイズ設定の読み出し
  (if my-place-customize-dir
      (my-customize-load my-place-customize-dir)))

;; 実際に実行
(my-startup)

(provide 'my-startup)
;;; my-startup.el ends here
