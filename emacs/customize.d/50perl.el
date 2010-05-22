;; CPerl-mode を使う
(defalias 'perl-mode 'cperl-mode)

(defun my-cperl-mode-init ()

  ;; M-x compile でスクリプトを実行
  (make-local-variable 'compile-command)
  (setq compile-command
	(concat "perl -T " (buffer-file-name)))

  ;; PerlStyle (perldoc perlstyle ベースの設定)
  (cperl-set-style "PerlStyle")

  ;; Tabスペースは使わない
  (setq indent-tabs-mode nil)

  ;; perlplus
  ;; Perlのシンボルを補完できるようにする
  ;; http://www.gentei.org/~yuuji/software/perlplus.el
  (require 'perlplus)
  (local-set-key "\M-\t" 'perlplus-complete-symbol)
  (perlplus-setup)

  ;; パスの追加?
  ;;(require 'set-perl5lib)
  ;;(set-perl5lib)

  ;; 関数ヘルプで、Perldocの呼び出し
  (local-set-key "\C-hf" 'cperl-perldoc)

  ;; Eldoc on CPerl
  (set (make-local-variable 'eldoc-documentation-function)
       'my-cperl-eldoc-documentation-function)

  (turn-on-eldoc-mode)
  )

(add-hook 'cperl-mode-hook 'my-cperl-mode-init)

;; emacs上でリージョンを選択して実行する
(defun perl-eval (beg end)
  "Run selected region as Perl code"
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "perl -T"))
  )

;; flymakeでの文法チェック時に taint mode を有効にする
(defadvice flymake-perl-init
  (after flymake-perl-init-taint-mode activate)
  (let ((ret ad-return-value))
    (setcar (cadr ret) (concat (caadr ret) "-T "))))

(defun my-cperl-eldoc-documentation-function ()
  "Return meaningful doc string for `eldoc-mode'."
  (car
   (let ((cperl-message-on-help-error nil))
     (cperl-get-help))))

;; array/hash のfaceを変更
(set-face-attribute 'cperl-array-face nil :background nil)
(set-face-attribute 'cperl-hash-face nil :background nil)
