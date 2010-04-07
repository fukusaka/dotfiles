;;; my-startup.el  ---  emacsen startup code.

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

;; カスタマイズ設定がし易いように cl パケージは読み
(require 'cl)

(defvar my-emacs-flavor
  (let ((flavor (if (featurep 'xemacs) "xemacs" "emacs")))
    (format "%s%d" flavor emacs-major-version)))

(defvar my-top-conf-dir       (expand-file-name "~/common/conf/"))

(defvar my-emacs-conf-dir     (file-name-as-directory
                               (concat my-top-conf-dir "emacs")))

(defvar my-elisp-path         (file-name-as-directory
                               (concat my-emacs-conf-dir "elisp")))

(defvar my-customize-dir      (file-name-as-directory
                               (concat my-emacs-conf-dir "customize.d")))

(defvar my-customize-bundle    (concat my-emacs-conf-dir
                                 (format ".my-startup-bundle-%s.el" my-emacs-flavor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 場所指定のカスタマイズ。。。便利か?

(defvar my-place-profile-alist nil)

(defvar my-hostname-nohost "localhost")

(defvar my-hostname
  (let ((envhost (shell-command-to-string "echo -n `hostname -s`")))
    (if envhost envhost my-hostname-nohost)))

(defvar my-place
  (let ((alist my-place-profile-alist) place)
    (while (and alist (not place))
      (if (string-match (caar alist) my-hostname)
	  (setq place (cdar alist)))
      (setq alist (cdr alist)))
    place))

(defvar my-place-customize-dir
  (if my-place (concat my-customize-dir my-place)))

(defvar my-place-customize-bundle
  (concat my-emacs-conf-dir
          (format ".my-startup-bundle=%s-%s.el" my-place my-emacs-flavor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; よく使う連想リストの追加用
(defun add-to-assoc-list (list-var element)
  (let ((list (assoc (car element) (symbol-value list-var))))
    (if list
        (setcdr list (cdr element))
      (set list-var (append (symbol-value list-var) (list element)))))
  (symbol-value list-var))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 必要に応じてバイトコンパイルしてその名前を返す
(defun my-compile-file (file)
  (let* ((base (file-name-sans-extension (file-name-nondirectory file)))
         (el-dir (file-name-directory file))
         (elc-dir (file-name-as-directory (concat el-dir my-emacs-flavor)))
         (el file)
         (elc (concat elc-dir base ".elc"))
         (ret file))

    ;; emacs-flavor毎にbytecode格納ディレクトリ作成
    (if (not (file-directory-p elc-dir))
        (make-directory elc-dir))

    ;; bytecodeが無いか古い場合再生成
    (if (or (not (file-exists-p elc))
            (file-newer-than-file-p el elc))
        (if (byte-compile-file el)
            (rename-file (concat el "c") elc t)))

    ;; 何らかの問題が無ければバイトコードが出来てるはず
    (if (file-exists-p elc)
        (setq ret elc))

    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 指定のファイルリストをバンドル化
(defun my-make-bundle (bundle-file-name file-list)
  (with-temp-file bundle-file-name
    (mapcar 'insert-file-contents file-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 指定ディレクトリの頭二文字が数字のファイルを順次 load する
(defun my-customize-load (dir &optional bundle)
  (when (and dir (file-directory-p dir))
    (let (files)
      (setq files (directory-files dir t "^[0-9][0-9].*\\.el$" t))
      (setq files (sort files 'string<))
      (cond
       (my-startup-avoid-compile
        (mapcar 'load files))

       ((or my-startup-avoid-bundled
            (not bundle))
        (setq files (mapcar 'my-compile-file files))
        (mapcar 'load files))

       (t
        (let ((rebuild nil))
          (dolist (file files)
            (if (file-newer-than-file-p file bundle)
                (setq rebuild t)))
          (when rebuild
            (with-temp-file bundle
              (erase-buffer)
              (mapcar '(lambda (file)
                         (insert-file-contents file)
                         (goto-char (point-max)))
                      files)))
          (if (file-newer-than-file-p bundle (concat bundle "c"))
              (byte-compile-file bundle))
          (load bundle))
        )
       ))))

(defvar my-startup-avoid-compile nil)
(defvar my-startup-avoid-bundled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 初期化本体
(defun my-startup ()

  ;; my-elisp-path以下のディレクトリを全て load-path に追加
  (let ((default-directory (directory-file-name my-elisp-path)))
    (setq load-path (append load-path (list default-directory)))
    (normal-top-level-add-subdirs-to-load-path))

  ;; カスタマイズ設定の読み出し
  (my-customize-load my-customize-dir my-customize-bundle)

  ;; 場所指定のカスタマイズ設定の読み出し
  (if my-place-customize-dir
      (my-customize-load my-place-customize-dir my-place-customize-bundle)))

(provide 'my-startup)
;;; my-startup.el ends here
