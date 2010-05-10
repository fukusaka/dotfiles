;; CPerl-mode を使う
(defalias 'perl-mode 'cperl-mode)

(require 'set-perl5lib)

;; Perl用設定
;; http://unknownplace.org/memo/2007/12/21#e001
(defvar flymake-perl-err-line-patterns
  '(("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)))

(defconst flymake-allowed-perl-file-name-masks
  '(("\\.pl$" flymake-perl-init)
    ("\\.pm$" flymake-perl-init)
    ("\\.t$" flymake-perl-init)))

(defun flymake-perl-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "perl" (list "-wc" local-file))))

(defun flymake-perl-load ()
  (interactive)
  (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-perl-file-name-masks))
  (setq flymake-err-line-patterns flymake-perl-err-line-patterns)
  (set-perl5lib)
  (flymake-mode t))

(add-hook 'cperl-mode-hook 'flymake-perl-load)

;; C-c C-cでスクリプトを実行もしくはsyntax check
;; http://d.hatena.ne.jp/hirose31/20060823/1156331805
(add-hook 'cperl-mode-hook
          '(lambda ()
             (make-local-variable 'compile-command)
             (setq compile-command
                   (concat "perl -T " (buffer-file-name)))
             (cperl-define-key "\C-c\C-c" 'compile)))

;; emacs上でリージョンを選択して実行する
(defun perl-eval (beg end)
  "Run selected region as Perl code"
  (interactive "r")
  (save-excursion
  (shell-command-on-region beg end "perl"))
)

;; perlplus
;; Perlのシンボルを補完できるようにする
;; http://www.gentei.org/~yuuji/software/perlplus.el
(add-hook 'cperl-mode-hook
          (lambda ()
            (require 'perlplus)
            (local-set-key "\M-\t" 'perlplus-complete-symbol)
            (perlplus-setup)))
