;; cperl-mode を使う
(defalias 'perl-mode 'cperl-mode)

(setq-default perl5-command "perl")

(defun my-cperl-mode-init ()

  ;; PerlStyle (perldoc perlstyle ベースの設定)
  (cperl-set-style "PerlStyle")
  (setq cperl-indent-level 4                         ;インデント幅を４にする
        cperl-continued-statement-offset 4           ;連続する文のオフセット
        cperl-brace-offset -4                        ;ブレースのオフセット
        cperl-close-paren-offset -4                  ;閉じる括弧のオフセット
        cperl-label-offset -4                        ;labelのオフセット
        cperl-indent-parens-as-block t               ;括弧もブロックとしてインデント
        cperl-tab-always-indent t                    ;TABをインデントにする
        cperl-highlight-variables-indiscriminately t ;スカラを常にハイライト
        )

  (defun cperl-backward-to-start-of-continued-exp (lim)
    (goto-char containing-sexp)
    (let ((sexp-start (following-char)))
      (forward-char)
      (skip-chars-forward " \t\n")
      (if (memq sexp-start (append "([" nil)) ; this hack is for continued statements inside parentheses
          (backward-char cperl-continued-statement-offset))))

  ;; Tabスペースは使わない
  (setq indent-tabs-mode nil)

  ;; array/hash のfaceを変更
  (set-face-attribute 'cperl-array-face nil :background nil)
  (set-face-attribute 'cperl-hash-face nil :background nil)

  ;; perlplus
  ;; Perlのシンボルを補完できるようにする
  ;; http://www.gentei.org/~yuuji/software/perlplus.el
  ;;(require 'perlplus)
  ;;(local-set-key "\M-\t" 'perlplus-complete-symbol)
  ;;(perlplus-setup)

  ;; スクリプト位置からディレクトリを遡って site.pl を探し、
  ;; 内部の @INC を抽出して、文法チェック呼び出しの -I オプションに引き渡す
  (set-perl5site)

  ;; 関数ヘルプで、Perldocの呼び出し
  (local-set-key "\C-hf" 'cperl-perldoc)

  ;; Eldoc on CPerl
  ;;(set (make-local-variable 'eldoc-documentation-function)
  ;;     'my-cperl-eldoc-documentation-function)
  ;;
  ;;(turn-on-eldoc-mode)

  (require 'perl-completion)
  (perl-completion-mode t)

  (auto-complete-mode t)
  (make-variable-buffer-local 'ac-sources)
  (setq ac-sources
        '(ac-source-perl-completion))
  )

(add-hook 'cperl-mode-hook 'my-cperl-mode-init)

;; emacs上でリージョンを選択して実行する
(defun perl-eval (beg end)
  "Run selected region as Perl code"
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "perl"))
  )

;; flymakeでの文法チェック時に
;; perl5lib-list があれば、-Ixx で追加する
(defadvice flymake-perl-init
  (after my-flymake-perl-init-mode activate)
  (let ((ret ad-return-value))
    (setcar (cdr ret)
            (append (mapcar '(lambda (x) (format "-I%s" x))
                            perl5lib-list)
                    (cadr ret)))
    (setcar ret perl5-command)
    ret))

(defun my-cperl-eldoc-documentation-function ()
  "Return meaningful doc string for `eldoc-mode'."
  (car
   (let ((cperl-message-on-help-error nil))
     (cperl-get-help))))

;; PERL5LIB のパスリスト
(setq perl5lib-list nil)

;; 下記条件で perl5 の検索パスを探します
;;
;; 1. ファイルのパスに "lib" というディレクトリが含まれていたら、
;;    そこまでのパスを候補パスに加える。
;; 2. ファイルのパスに"bin"というディレクトリが含まれており、
;;    その階層に "lib" というディレクトリがあれば、候補パスに加える。
;; 3. 候補パスの下層に "perl5" が含まれていれば、そのパスを検索パスとする。
;; 4. 候補パスの下層に追加した検索パスが無ければ、それ自身を検索パスとする

(defun perllib-guess (pathname)
  (let (cand-list dir base)
    (while
        (progn
          (setq dir (file-name-directory pathname))
          (setq base (file-name-nondirectory pathname))
          (not (string= dir pathname)))
      (cond
       ((string= base "lib")
        (add-to-list 'cand-list pathname))
       ((string= base "bin")
        (let ((lib-path (concat dir "lib")))
          (if (file-exists-p lib-path)
              (add-to-list 'cand-list lib-path))))
       )
      (setq pathname (directory-file-name dir))
      )
    (mapcar (lambda (x)
              (cond
               ((file-readable-p (concat x "/perl5"))
                (concat x "/perl5"))
               (t x)))
            cand-list)))

(defun set-perl5lib-list ()
  (interactive)
  (let* ((current-perl5lib-list (split-string (or (getenv "PERL5LIB") "") ":"))
         (path-list (remove-if (lambda (x) (member x current-perl5lib-list))
                               (perllib-guess (buffer-file-name)))))
    (setq perl5lib-list path-list)))

(defun set-perl5site ()
  "Set path into PERL5LIB from site.pl"
  (interactive)
  (make-local-variable 'perl5lib-list)
  (make-local-variable 'perl5-command)
  (set-perl5lib-list))
