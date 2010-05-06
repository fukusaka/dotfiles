;;; my-startup.el  ---  emacsen startup code.

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

(require 'cl)

(defvar my-emacs-flavor
  (let ((flavor (if (featurep 'xemacs) "xemacs" "emacs")))
    (format "%s%d" flavor emacs-major-version))
  "Emacs のフレーバ。
バージョン毎にbytecodeが異なるときの場合わけの区別の基準になる")

(defvar my-top-conf-dir
  (expand-file-name "~/common/conf/")
  "設定ファイル群を置く基準ディレクトリ")

(defvar my-emacs-conf-dir
  (concat (file-name-as-directory my-top-conf-dir)
	  "emacs/")
  "emacs用設定ファイルの基準ディレクトリ")

(defvar my-customize-dir
  (concat (file-name-as-directory my-emacs-conf-dir)
	  "customize.d/")
  "emacs設定を分割したファイルを置く場所")

(defvar my-customize-bundle
  (concat (file-name-as-directory my-customize-dir)
	  "all-bundle.el")
  "emacs設定を全部入りにするファイル名")

(defvar my-elisp-dir
  (concat (file-name-as-directory my-emacs-conf-dir)
	  "elisp/")
  "elispを入れて置く場所")

(defvar my-compiled-elisp-dir
  (concat (file-name-as-directory my-emacs-conf-dir)
	  (format "elisp-%s/" my-emacs-flavor))
  "elispをbyteコンパイルしたファイルの置き場所")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 場所指定のカスタマイズ。。。便利か?

(defvar my-place-profile-alist nil)

(defvar my-hostname-nohost "localhost")

(defvar my-hostname (if system-name system-name my-hostname-nohost))

(defvar my-place
  (let ((alist my-place-profile-alist) place)
    (while (and alist (not place))
      (if (string-match (caar alist) my-hostname)
	  (setq place (cdar alist)))
      (setq alist (cdr alist)))
    place))

(defvar my-place-customize-dir
  (if my-place (file-name-as-directory (concat my-customize-dir my-place))))

(defvar my-place-customize-bundle
  (concat my-customize-dir
          (format "all-bundle-%s.el" my-place)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-compile-file (file &optional elc-topdir)
  "ファイルの更新時間を比較して byte-compile してそのファイル名前を返す。
作成される bytecode ファイルの置き場所は、fileがあるディレクトリに下に
 emacs-flavor 毎のサブディレクトリになる。

elc-topdirを指定した場合は、elc-topdirを基準にした相対パス file に指定した
パス名になる。従って、.el ファイルはdefalut-directoryからの相対パスで指定しなくては行けない。"
  (let* ((base (file-name-nondirectory file))
         (el-dir (file-name-directory file))
         (elc-dir (file-name-as-directory
                   (if elc-topdir
                       (concat (file-name-as-directory elc-topdir) el-dir)
                     (concat el-dir my-emacs-flavor))))
         (el file)
         (elc (concat elc-dir base "c")))

    ;; emacs-flavor毎にbytecode格納ディレクトリ作成
    (if (not (file-directory-p elc-dir))
        (make-directory elc-dir t))

    ;; bytecodeが無いか古い場合再生成
    (if (or (not (file-exists-p elc))
            (file-newer-than-file-p el elc))
        (condition-case err
            (if (and (byte-compile-file el)
                     (file-exists-p (concat el "c")))
                (rename-file (concat el "c") elc t))
          (error (message "[my-compile-file] %s" err))))

    ;; 何らかの問題が無ければバイトコードが出来てるはず
    (if (file-exists-p elc)
        (setq file elc))

    file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ディレクトリ内の elisp を全部バイトコンパイルしてcompiled-emacsXXにコピー
(defun my-compile-directory (directory)
  (let ((files (directory-files directory nil "^[^\\.].*\\.el$" t)))
    (dolist (file files)
      ;;(message "[my-compile-directory] check %s/%s" directory file)
      (my-compile-file
       (concat (file-name-as-directory directory) file)
       my-compiled-elisp-dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-customize-bundling-one-file (bundle dir)
  "指定の設定ファイルを一つのファイルに固める。"
  (let ((files
         (cond ((listp dir) dir)
               (t (sort
                   (directory-files dir t "^[0-9][0-9].*\\.el$" t)
                   'string<)))))
    (with-temp-file bundle
      (erase-buffer)
      (mapcar '(lambda (file)
                 (insert-file-contents file)
                 (goto-char (point-max)))
              files))
    bundle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-when-eval-safe (loadlib)
  "安全な load。読み込みに失敗しても、評価に失敗してもそこで止まらない。"
  (condition-case err
      (load loadlib)
    (error (message "[load-when-eval-safe] %s" err))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 指定ディレクトリの頭二文字が数字のファイルを順次 load する
(defun my-customize-load (dir &optional bundle)
  (when (and dir (file-directory-p dir))
    (let (files newer)
      (setq files (directory-files dir t "^[0-9][0-9].*\\.el$" t))
      (setq files (sort files 'string<))

      ;; 最後に修正された設定素を検索
      (setq newer (car files))
      (dolist (file files)
        (if (file-newer-than-file-p file newer)
            (setq newer file)))

      (cond
       ;; .el を読み込む
       (my-startup-avoid-compile
        (mapcar 'load-when-eval-safe files))

       ;; byte-compile 後 .elc を読み込む
       ;; 設定片の最終更新時間が比較的に新しいときもこっち
       ((or my-startup-avoid-bundled
            (not bundle)
            (<= (let ((now (current-time))
                      (mtime (nth-value 5 (file-attributes newer))))
                  (+ (* (- (first now) (first mtime)) 65356)
                     (- (second now) (second mtime))))
                my-startup-bundling-delay))
        (setq files (mapcar 'my-compile-file files))
        (mapcar 'load-when-eval-safe files))

       ;; 一つに固めて bundle を読み込む
       (t
        (when (file-newer-than-file-p newer bundle)
          (my-customize-bundling-one-file bundle dir))
        (setq bundle (my-compile-file bundle))
        (load-when-eval-safe bundle))
       )
      )))

(defvar my-startup-avoid-compile nil)
(defvar my-startup-avoid-bundled nil)

;; 設定変更して１２時間はバンドル化しない
(defvar my-startup-bundling-delay (* 12 60 60))

;; elisp以下のコンパイル関数
(autoload 'my-subdirs-compile "my-subdirs-compile" "" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 初期化本体
(defun my-startup ()

  ;; elispのパスを通す(subdirを含めない)
  (add-to-list 'load-path my-elisp-dir t)

  ;; elisp直下に修正があればコンパイル
  (let ((default-directory my-elisp-dir))
	(my-compile-directory "."))

  ;; コンパイル済みのelispのパスを通す(subdirを含む)
  (let ((default-directory my-compiled-elisp-dir))
    (when (file-directory-p default-directory)
      (add-to-list 'load-path default-directory t)
      (normal-top-level-add-subdirs-to-load-path)))

  ;; カスタマイズ設定の読み出し
  (my-customize-load my-customize-dir my-customize-bundle)

  ;; 場所指定のカスタマイズ設定の読み出し
  (if my-place-customize-dir
      (my-customize-load my-place-customize-dir my-place-customize-bundle)))

(provide 'my-startup)

;;; my-startup.el ends here
