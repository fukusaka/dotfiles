;; CPerl-mode を使う
(defalias 'perl-mode 'cperl-mode)

(defun my-cperl-mode-init ()

  ;; PerlStyle (perldoc perlstyle ベースの設定)
  (cperl-set-style "PerlStyle")

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

  ;; スクリプトパスから lib を抽出してPERL5LIBに追加
  ;;(require 'set-perl5lib)
  ;;(set-perl5lib)

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
    (shell-command-on-region beg end "perl -T"))
  )

;; flymakeでの文法チェック時に taint mode を有効にする
;; perl5lib-list があれば、-Ixx で追加する
(defadvice flymake-perl-init
  (after flymake-perl-init-taint-mode activate)
  (let ((ret ad-return-value))
    (setcar (cdr ret)
            (append (mapcar '(lambda (x) (format "-I%s" x))
                            perl5lib-list)
                    (cons "-T " (cadr ret))))
    ret))

(defun my-cperl-eldoc-documentation-function ()
  "Return meaningful doc string for `eldoc-mode'."
  (car
   (let ((cperl-message-on-help-error nil))
     (cperl-get-help))))


;; PERL5LIB のパスリスト
(setq perl5lib-list nil)

;; file 位置からディレクトリを遡って site.pl を探す
(defun search-perl5site (file)
  (let (sitefile dir lst)
    (while (progn
             (setq dir (file-name-directory file))
             (setq sitefile (concat dir "site.pl"))
             (not (or (string= dir file) (file-exists-p sitefile))))
      (setq lst (cons sitefile lst))
      (setq file (directory-file-name dir))
      )
    (if (file-exists-p sitefile)
        sitefile)))

;; site.pl が単純と仮定して、追加の@INCを抽出
;;
;; ex)
;; use lib qw(path1 path2);
;; 1;
(defun perl5lib-perl5site (sitefile)
  (split-string
   (with-output-to-string
     (with-current-buffer
         standard-output
       (call-process "perl" nil t nil "-e" (format "@OINC=@INC; require \"%s\"; print join \" \",@INC[0..($#INC-$#OINC-1)];" sitefile))
       ))))
;; (あまり実装が良くない)

;;
(defun set-perl5site ()
  "Set path into PERL5LIB from site.pl"
  (interactive)
  (let ((sitefile (search-perl5site buffer-file-name)))
    (make-local-variable 'perl5lib-list)
    (setq perl5lib-list (if sitefile (perl5lib-perl5site sitefile)))))
