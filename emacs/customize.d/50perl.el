;; CPerl-mode を使う
(defalias 'perl-mode 'cperl-mode)

(add-hook 'cperl-mode-hook
          '(lambda ()

	     ;; M-x compile でスクリプトを実行
             (make-local-variable 'compile-command)
             (setq compile-command
                   (concat "perl -T " (buffer-file-name)))

	     ;; perlplus
	     ;; Perlのシンボルを補完できるようにする
	     ;; http://www.gentei.org/~yuuji/software/perlplus.el
	     (require 'perlplus)
	     (local-set-key "\M-\t" 'perlplus-complete-symbol)
	     (perlplus-setup)

	     ;; パスの追加?
	     ;;(require 'set-perl5lib)
	     ;;(set-perl5lib)
	     ))

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
