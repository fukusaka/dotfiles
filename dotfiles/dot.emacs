;; -*-emacs-lisp-*-
;; $Id$

;; ニュースに流れていたソースでラップ。
;;; Wrapper to make .emacs self-compiling.
;;; (To recompile .emacs interactively, just reload it.)
;;; Author: Bill Brodie <wbrodie@panix.com>   Last Rev: Fri Apr 7 10:19 1995
;;; Changed by Kazushi Jam Marukawa <jam@pobox.com>

(defvar init-top-level t)
(defvar init-complete nil)

(if init-top-level
    (let ((init-top-level nil)
	  (elc-filename
	   (cond ((string< emacs-version "19") "~/.emacs18.elc")
		 ((string< emacs-version "20") "~/.emacs19.elc")
		 ((string< emacs-version "21") "~/.emacs20.elc")
		 (t "~/.emacsxx.elc"))))
      (if (file-newer-than-file-p "~/.emacs" elc-filename)
          (progn
            (or init-complete (load "~/.emacs" nil t t))
            (byte-compile-file "~/.emacs")
	    (if (file-exists-p "~/.emacsc")
		(rename-file "~/.emacsc" elc-filename t))
	    (if (file-exists-p "~/.emacs.elc")
		(rename-file "~/.emacs.elc" elc-filename t)))
        (or init-complete (load elc-filename nil t t))))
(progn

;;;     <Your current .emacs file here, unchanged>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 日本語環境でEUCをメインにする設定
;;
;; しかし、.emacs のみは iso-2022-jp (jis code?)で書かないと
;; 化ける、、、とほ。
;;
;; ~/.login -->
;;   alias emacs "(setenv XMODIFIERS '@im=none'; exec /usr/bin/emacs )"
(cond
 ;; Ver.19 の場合
 ((string-match "^19" emacs-version)
  (if (boundp 'MULE)
      (progn
	(set-primary-environment 'japanese)
	(set-display-coding-system         *euc-japan*)
	(set-keyboard-coding-system        *euc-japan*)
	(set-default-file-coding-system    *euc-japan*)
	(set-default-process-coding-system *euc-japan* *euc-japan*)	
	(define-program-coding-system nil nil (cons *euc-japan* *euc-japan*))
	;; モードコードの表示の設定
	;; (setq mc-verbose-code t)
	))
  )
 ;; Ver.20 の場合(不十分かなぁ)
 ((string-match "^20" emacs-version)
  (set-language-environment          'Japanese)
  ;; ほとんどの場合 euc-japan-unix を使う
  (set-default-coding-systems       'euc-japan-unix)
  ;; emacs -nw では、上では遅すぎなので、、、本当か？
  (set-terminal-coding-system       'euc-japan-unix)
  ;; フォント設定(Xリソースで設定するのがベスト)
  ;; ~/.Xresources -->
  ;;   Emacs.Font: -*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-standard
  ;; (set-default-font "fontset-standard")
  ;; (set-default-font "-*-fixed-*-r-normal-*-16-*-*-*-*-*-fontset-standard")
  ;; scroll bar を右にする設定。
  (set-scroll-bar-mode 'right)
  (if nil
  (if (not window-system)
      (progn
	;; Translate `C-h' to <DEL>.
	(keyboard-translate ?\C-h ?\C-?)
       
	;; Translate <DEL> to `C-h'.
	(keyboard-translate ?\C-? ?\C-h)
       ))
  )
  )
 )

(setq text-mode-hook
      '(lambda () (auto-fill-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 個人情報を設定
;; 
(setq user-full-name "福坂将一")
(setq user-mail-address "fukusaka@xa2.so-net.ne.jp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 表示の細かい設定
;;
(setq next-line-add-newlines nil)
(setq line-number-mode t)
(setq column-number-mode t)
(setq scroll-step 1)

(if (not window-system)
    (menu-bar-mode 0))

;;(setq truncate-lines t)
;; 時計の表示
;;(setq display-time-day-and-date nil)
;;(display-time)

;; Xの設定で鳴らさないようにするので、、。
;; ~/.xsession -->
;;   xset b off
(if (not window-system)
    (setq visible-bell t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; キーの設定
;;
(define-key global-map "\C-j" 'goto-line)
(define-key global-map "\M-c" 'compile)
(define-key global-map "\M-m" 'man)
(define-key global-map "\C-xm" 'mew)

(define-key global-map [mouse-4] 'previous-line)
(define-key global-map [mouse-5] 'next-line)

(global-unset-key "\C-z")
(define-key global-map "\C-zz" 'toggle-shell-default)
(define-key global-map "\C-z\C-z" 'toggle-shell-default)
(define-key global-map "\C-zs" 'toggle-shell)
(define-key global-map "\C-z\C-s" 'toggle-shell)
(define-key global-map "\C-zr" 'toggle-scheme)
(define-key global-map "\C-z\C-r" 'toggle-scheme)

;; ワンタッチでシェルに行ける
;; トルグにしたいもし
(defun toggle-shell-default ()
  (interactive)
  (toggle-run-mode '(shell)))

(defun toggle-shell ()
  (interactive)
  (toggle-run-mode '(shell) "*shell*"))

(defun toggle-scheme ()
  (interactive)
  (toggle-run-mode '(run-scheme "/usr/bin/guile") "*scheme*"))

(defvar toggle-run-mode-list
  '("*shell*"
    "*scheme*"
    "*tex-shell*"
    ))

;; 明示的に、toggle-run-mode を使わなければ、toggle-run-mode-list を
;; 使う。
(defun toggle-run-mode (run-command &optional toggle-run-mode)
  (if (let ((mode-list
	     (if (stringp toggle-run-mode)
		 (list toggle-run-mode)
	       toggle-run-mode-list)))
	(eval (cons 'or 
		    (mapcar (function
			     (lambda (run-mode)
			       (string= (buffer-name) run-mode)))
			    mode-list))))
      (switch-to-buffer (prog1 (other-buffer (current-buffer))
			  (bury-buffer (current-buffer))))
    (eval run-command)
    ))

;;
;; Guile をつかう設定
(setq scheme-program-name "/usr/bin/guile")


;; change menu-bar action for [rmail] from rmail to mew
(cond ((string-match "^20" emacs-version)
       (define-key menu-bar-tools-menu [rmail] '("Read Mail(mew)" . mew))
       (define-key menu-bar-tools-menu [compose-mail] '("Send Mail(mew)" . mew-send))
       )
      ((string-match "^19.34" emacs-version)
       (define-key menu-bar-tools-menu [rmail] '("Read Mail(mew)" . mew))
       )
      ((string-match "^19.28" emacs-version)
       (define-key menu-bar-file-menu [rmail] '("Read Mail(mew)" . mew))
       )
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; コマンド入力の設定 (comint-mode)
;;

;; ヒストリ数を設定する。
(setq comint-input-ring-size 200)

;; Password を隠しまほう
(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 自動識別するモードの設定
;;
(setq auto-mode-alist
      (append
       '(("\\.pl$" . cperl-mode)
	 ("\\.mht$" . html-mode)
	 ("\\.po[tx]?\\'\\|\\.po\\." . po-mode)
	 )
       auto-mode-alist))

(autoload 'po-mode "po-mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; かんなを使う設定
;;
(cond
 ;; Ver.19 の場合
 ((string-match "^19" emacs-version)
  (if (boundp 'MULE)
      (progn
	(if (and (boundp 'CANNA) CANNA) ; 『かんな/emacs』であることを確認して
	    ;; 『かんな/emacs』 の場合だけ以下を実行します。
	    (progn
	      (cond ((boundp 'egg-version)
		     (require 'can-n-egg)
		     (can-n-egg))
		    (t
		     (require 'canna)
		     (canna)
		     ;; (global-set-key "\C-o" 'canna-toggle-japanese-mode)
		     ))
	      (setq canna-use-color t)
	      ))
	;;
	;; たまごでの補足
	;;
	(if (boundp 'egg-version)
	    (progn
	      (setq enable-double-n-syntax t)
	      (its-defrule "dhi" "でぃ" nil nil "roma-kana")
	      (its-defrule "dhu" "でゅ" nil nil "roma-kana")
	      (its-defrule "A" "ぁ" nil nil "roma-kana")
	      (its-defrule "I" "ぃ" nil nil "roma-kana")
	      (its-defrule "U" "ぅ" nil nil "roma-kana")
	      (its-defrule "E" "ぇ" nil nil "roma-kana")
	      (its-defrule "O" "ぉ" nil nil "roma-kana")
	      ))
	)))
 ;; Ver.20 の場合(不十分)
 ((string-match "^20" emacs-version)
  ;; 常に『かんな/emacs』なので、、、。
  (require 'canna)
  (canna)
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 色を付ける共通設定
;;
(if window-system
    (cond
     ;; mule2.3 では hilit19 を使う
     ((string-match "^19" emacs-version)
      (setq hilit-mode-enable-list  '(not text-mode)
	    hilit-background-mode   'light
	    hilit-inhibit-hooks     nil
	    hilit-inhibit-rebinding nil
	    hilit-quietly t)
      (require 'hilit19)


      ;; troff で色を付ける設定
      (add-hook 'nroff-mode-hook 
		(function (lambda ()
			    (hilit-translate 	string	  nil)
			    )))
      )
     ;; emacs20 では font-lock を使う
     ((string-match "^20" emacs-version)
      (global-font-lock-mode 1)
      (setq font-lock-support-mode
	    '((c-mode . fast-lock-mode)
	      (c++-mode . fast-lock-mode)
	      (cc-mode . fast-lock-mode)
	      ))
      (defface moi-string-face
	'((((class color) (background light)) (:foreground "Brown"))
	  (((class color) (background dark)) (:foreground "Salmon"))
	  (t (:italic t)))
	nil
	)
      (setq font-lock-string-face 'moi-string-face)
      )
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dired-X を使う設定
;;

(cond
 ((string-match "^19" emacs-version)
  ;; 何でだろう？
  (setq dired-chown-program "chown")
  ))

(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)

(autoload 'dired-jump "dired-x" nil t nil)
(autoload 'dired-jump-other-window "dired-x" nil t nil)

(add-hook 'dired-load-hook
	  (function
	   (lambda ()
	     (load "dired-x")
	     ;; モードキーの設定
	     (define-key dired-mode-map "f" 'dired-do-shell-command)
	     (define-key dired-mode-map "U" 'dired-unmark-all-files-no-query)
	     ;; Set dired-x variables here.  For example:
	     (setq dired-guess-shell-gnutar "tar")
	     ;; (setq dired-guess-shell-znew-switches t)
	     ;; (setq dired-x-hands-off-my-keys nil)
	     (setq dired-guess-shell-alist-user
		   (list
		    (list "\\.ps$" "gv")
		    (list "\\.eps$" "gv")
		    (list "\\.dvi.gz$" "zcat * | xdvi -")
		    ))
	     ;;
	     ;; 表示を省略するファイルと拡張子の設定
	     ;;
	     (setq dired-omit-files "^#\\|^\\.")
	     (setq dired-omit-extensions
		   (append 
		    '(".o" ".elc" "~" ".bin" ".lbin" ".fasl"
		      ".a" ".ln" ".fmt" ".lo" ".flc" ".flh" )
		    dired-tex-unclean-extensions
		    dired-latex-unclean-extensions
		    dired-bibtex-unclean-extensions
		    dired-texinfo-unclean-extensions
		    ))
	     )))

(add-hook 'dired-mode-hook
	  (function
	   (lambda ()
	     (setq dired-omit-files-p t)
	     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mew                                                                ;;
;;   メールリーダー Mew                                               ;;
;;   M-x mew で起動します                                             ;;
;;                                                                    ;;
;; mule2.3(emacs19) --> mew ver 1.70                                  ;;
;; emacs20.3        --> mew ver 1.93                                  ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond
 ;; Ver.19 の場合
 ((string-match "^19" emacs-version)
  (setq load-path
	(append '("/usr/local/share/emacs/site-lisp/mew-1.70")
		load-path))
  (setq exec-path (cons "/usr/local/bin/mh" exec-path))
  (setq exec-path (cons "/usr/local/lib/mh" exec-path))
  )
 ) 
;; From: に余計なモノ(localhost 名等)が付いたりする場合は、
;; xxxx の部分にドメイン名やメールアドレス等を指定します
;(setq mew-mail-domain-list
;      '("xxxx.xxxx.xxxx.xxxx"))

(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)

(setq mew-use-overlay-keymap nil)

(setq mew-from "もいもい <fukusaka@xa2.so-net.ne.jp>")
(setq mew-mail-domain-list '("xa2.so-net.ne.jp"))
(setq mew-fcc "+send")
(setq mew-reply-to nil)
(setq mew-ask-subject t)
(setq mew-demo nil)
(setq mew-signature-insert-last t)

(setq mew-use-highlight-cursor-line nil)
(setq mew-use-highlight-body t)

(setq mew-cite-prefix "> ")
(setq mew-cite-fields '("From:" "Subject:" "Date:" "Message-ID:"))
(setq mew-cite-format "> From: %s\n> Subject: %s\n> Date: %s\n> Message-ID: %s\n\n")

(add-hook 'mew-summary-mode-hook 
	  (function
	   (lambda ()
	     (define-key mew-summary-mode-map "b" 'mew-summary-auto-refile)
	     )))

;; Ver.1.70 の場合
;; なんだかなぁ。
(if (string-match "^19" emacs-version)    
    (add-hook 'mew-init-hook
	      (function
	       (lambda ()
		 (add-hook 'kill-emacs-hook
			   (function mew-mark-process-all-folders)))))
  )

;; mew で色を付ける設定
(if window-system
    (cond
     ;; mule2.3 の mew-1.70 の場合
     ((string-match "^19" emacs-version)
      (add-hook 'mew-message-hook 'hilit-rehighlight-buffer)
      (hilit-set-mode-patterns 
       '(mew-message-mode)
       '(("^Subject:.*$" nil msg-subject)
	 ("^From:.*$" nil msg-from)
	 ("^X-.*:.*$" nil msg-quote)
	 ("^>.*$" nil msg-quote)
	 ("^[A-Za-z][A-Za-z0-9-]+:" nil msg-header)))
      (add-hook 'mew-draft-mode-hook 'hilit-rehighlight-buffer)
      (hilit-set-mode-patterns 
       '(mew-draft-mode)
       '(("^Subject:.*$" nil msg-subject)
	 ("^From:.*$" nil msg-from)
	 ("^>.*$" nil msg-quote)
	 ("^[A-Za-z][A-Za-z0-9-]+:" nil msg-header)))
      (add-hook 'mew-summary-mode-hook 'hilit-rehighlight-buffer)
      (defun hilit-search-mew-summary-name (pend)
	(if (re-search-forward "^[ 0-9][ 0-9][ 0-9][0-9]..[0-9][0-9]/[0-9][0-9] \\(.................\\).*$" nil t nil)
	    (let ((str (buffer-substring (match-beginning 1) (match-end 1)))) 
	      ;; これは17文字
	      ;;  2 Byte 文字が 2 columns であるとした
	      ;; 場合の、17 columns を抜き出す
	      (setq str
		    (code-convert-string
		     (substring
		      (code-convert-string str *internal* *sjis*)
		      0 17)
		     *sjis* *internal*))
	      ;; point は *internal* での長さ
	      (cons (match-beginning 1) (+ (match-beginning 1) (length str))))))
      (hilit-set-mode-patterns
       '(mew-summary-mode)
       '((hilit-search-mew-summary-name nil ForestGreen)
	 ("[- M][0-9][0-9]/[0-9][0-9] " nil Red)
	 ("^[ 0-9][ 0-9][ 0-9][0-9]" nil Blue)
	 ("<<.*$" nil Purple))))

     ;; emacs20.3 の mew-1.93 の場合
     ;; ~/.im/Config -> Form=%+4n %m%d %-17a %S<<%b
     ;; なんかでかすぎ! (mewの色づけ処理とあまり整合性がない)
     ((string-match "^20" emacs-version)
      (defun moi-substring-width (str n) ; 要APELかなぁ
	(let ((strl (string-to-char-list str)) estrl)
	  (while (> n (eval (cons '+ (mapcar 'char-width estrl))))
	    (setq estrl (append estrl (list (car strl))))
	    (setq strl  (cdr strl))
	    )
	  (char-list-to-string estrl)
	  ))

      (defvar  mew-summary-number-face 'mew-summary-number-face)
      (defface mew-summary-number-face
	'((((class color) (background light)) (:foreground "Blue"))
	  (((class color) (background dark)) (:foreground "LightSkyBlue"))
	  )
	nil)

      (defvar  mew-summary-date-face 'mew-summary-date-face)
      (defface mew-summary-date-face
	'((((class color) (background light)) (:foreground "Red"))
	  (((class color) (background dark)) (:foreground "Pink"))
	  )
	nil)

      (defvar  mew-summary-from-face 'mew-summary-from-face)
      (defface mew-summary-from-face
	'((((class color) (background light)) (:foreground "ForestGreen"))
	  (((class color) (background dark)) (:foreground "PaleGreen"))
	  )
	nil)

      (defvar  mew-summary-body-face 'mew-summary-body-face)
      (defface mew-summary-body-face
	'((((class color) (background light)) (:foreground "Purple"))
	  (((class color) (background dark)) (:foreground "Cyan"))
	  )
	nil)

      (defvar  mew-summary-mark-face 'mew-summary-mark-face)
      (defface mew-summary-mark-face
	'((((class color) (background light)) (:foreground "ForestGreen"))
	  (((class color) (background dark)) (:foreground "PaleGreen"))
	  )
	nil)

      (defvar  mew-summary-mine-face 'mew-summary-mine-face)
      (defface mew-summary-mine-face
	'((((class color) (background light)) (:foreground "Blue"))
	  (((class color) (background dark)) (:foreground "LightSkyBlue"))
	  )
	nil)

      (defvar mew-summary-number-regexp "^[ 0-9][ 0-9][ 0-9][0-9]")
      (defvar mew-summary-date-regexp   "[- MS][0-9][0-9]/[0-9][0-9]")
      (defvar mew-summary-from-regexp   'mew-summary-from-search)
      (defvar mew-summary-body-regexp   "<<.*$")
      (defvar mew-summary-mark-regexp   'mew-summary-mark-search)
      (defvar mew-summary-mine-regexp   "^.    .*$")

      (defun mew-summary-from-search (pend)
	(if (re-search-forward "^[ 0-9][ 0-9][ 0-9][0-9]..[0-9][0-9]/[0-9][0-9] \\(.*\\)$" pend t nil)
	    (save-excursion
	      (let ((str (buffer-substring (match-beginning 1) (match-end 1)))
		    sta end)
		(goto-char (match-beginning 1))
		(setq sta (point-marker))
		(forward-char (length (moi-substring-width str 17)))
		(setq end (point-marker))
		(store-match-data (list sta end))
		)
	      (point))
	  nil
	  ))

      (defun mew-summary-mark-search (pend)
	(if (re-search-forward "^[ 0-9][ 0-9][ 0-9][0-9][^ ]" pend t nil)
	    (save-excursion
	      (let (sta end)
		(beginning-of-line)
		(setq sta (point-marker))
		(end-of-line)
		(setq end (point-marker))
		(store-match-data (list sta end))
		)
	      (point))
	  nil
	  ))

      (setq mew-summary-font-lock-keywords
	    (list
	     (cons mew-summary-mark-regexp    'mew-summary-mark-face)
	     (cons mew-summary-number-regexp  'mew-summary-number-face)
	     (cons mew-summary-date-regexp    'mew-summary-date-face)
	     (cons mew-summary-from-regexp    'mew-summary-from-face)
	     (cons mew-summary-body-regexp    'mew-summary-body-face)
	     (cons mew-summary-mine-regexp    'mew-summary-mine-face)
	     )
	    )

      (add-hook 'mew-summary-mode-hook 
		(function
		 (lambda ()
		   (progn
		     (make-local-variable 'font-lock-defaults)
		     (setq font-lock-defaults
			   '(mew-summary-font-lock-keywords t))
		     (turn-on-font-lock)
		     ))))
      (add-hook 'mew-summary-inc-sentinel-hook 'font-lock-fontify-buffer)
      (add-hook 'mew-summary-scan-sentinel-hook 'font-lock-fontify-buffer)
      )
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ついでに mh-e を快適に使う設定                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; もうmh-e は使わない
;;;; mime を使う設定。
;;;; (load "mime-setup")
;;(setq exec-path (cons "/usr/local/bin/mh" exec-path))
;;(setq exec-path (cons "/usr/local/lib/mh" exec-path))
;;;;
;;;; mh-e で色を付ける設定 mule 2.3 のみ
;;;;
;;(if window-system
;;    (progn
;;      (add-hook 'mh-folder-mode-hook 'hilit-rehighlight-buffer-quietly)
;;      (add-hook 'mh-inc-folder-hook 'hilit-rehighlight-buffer-quietly)
;;      (defun hilit-search-mh-folder-name (pend)
;;        (if (re-search-forward "^[ 0-9][ 0-9][ 0-9][0-9]..[0-9][0-9]/[0-9][0-9] \\(.................\\).*$" nil t nil)
;;            (let ((str (buffer-substring (match-beginning 1) (match-end 1))))
;;					; これは17文字
;;                                       ;  2 Byte 文字が 2 columns であるとした
;;                                        ;場合の、17 columns を抜き出す
;;              (setq str
;;                    (code-convert-string
;;                     (substring (code-convert-string str *internal* *sjis*) 0 17)
;;                     *sjis* *internal*))
;;                                        ; point は *internal* での長さ
;;              (cons (match-beginning 1) (+ (match-beginning 1) (length str))))))
;;     (hilit-set-mode-patterns
;;       'mh-folder-mode
;;       '((hilit-search-mh-folder-name nil ForestGreen)
;;         ("[- M][0-9][0-9]/[0-9][0-9] " nil Red)
;;         ("^[ 0-9][ 0-9][ 0-9][0-9]" nil Blue)
;;         ("<<.*$" nil Purple)))

;;      (require 'mh-e)
;;      (define-key mh-folder-mode-map "\er"
;;        '(lambda ()
;;           (interactive)
;;           (mh-rescan-folder)
;;           (hilit-rehighlight-buffer-quietly)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Latex を使う設定
;;
(setq latex-run-command "platex *")
(setq tex-dvi-view-command "xdvi -geometry +0+0 *")
(setq tex-dvi-print-command "dvi2ps * | lpr -Pgs360")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Texinfo で日本語を使うようにする。
;;
(setq texinfo-tex-comand "jtex")

;; Info がまともな動作するような設定
(setq Info-fontify-maximum-menu-size 50000)
;;(setq Info-default-directory-list
;;      (cons  (expand-file-name "~/info") Info-default-directory-list))

(defun info-file (file)
  (interactive "fInfo file:")
  (info file))

(add-hook 'texinfo-mode-hook
	  (function (lambda ()
		      (define-key texinfo-mode-map "\C-c\C-v"
			'texinfo-preview-buffer)
	   )))

(defun texinfo-preview-buffer ()
  (interactive)
  (let (filename)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^@setfilename[ \\t]+\\(.*\\)$" nil nil)
	  (setq filename (buffer-substring (match-beginning 1)
					   (match-end 1)))
	(error "Texinfo file needs an `@setfilename FILENAME' line.")
	)
      )
    
    (info (concat (file-name-directory buffer-file-name) filename))
    ))

(cond
 ((string-match "^20" emacs-version)
  ;; Info 君に色が付いていないなんて!!!
  ;; それも font-lock 君との仲良しでないなんて、、、
  (defface info-node
    '((t (:bold t :italic t)))
    nil)
  
  (defface info-menu-5
    '((t (:underline t)))
    nil)
  
  (defface info-xref
    '((((class color) (background light)) (:foreground "Blue" :bold t))
      (((class color) (background dark)) (:foreground "LightSkyBlue" :bold t))
      (t (:bold t :italic t))
      )
    nil)

  )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 電子辞書関係
;;
;;EJ-dic,JE-dic
;;(global-set-key "\C-x#" 'lookup-edic)
;;(global-set-key "\C-x$" 'lookup-jdic) 
;;(global-set-key "\M-\\" 'lookup-edic)
;;(autoload 'lookup-edic "eldic" nil t)
;;(autoload 'lookup-jdic "eldic" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs/W3 Configuration
;;
(cond
 ((string-match "^22" emacs-version)
  (require 'w3-auto "w3-auto")
  (setq w3-default-homepage "http://sheep-poe.ouchi")
  (setq url-proxy-services '(("http"     . "localhost:3128")
			     ))
  ;;(setq url-be-asynchronous t)
  ;;(setq w3-delay-image-loads t)
  ;;(setq url-xterm-command "kterm -title %s -ut -e %s %s %s")
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 圧縮ファイルを自動に展開、圧縮
;;
;; tar-mode と組み合わせるとEUCが化けるんだが、、、。
;; JISだけうまく表示出来る、、、そらそうだ。
;; ver19 ではコケル、、、

(cond
 ((string-match "^20" emacs-version)
  (auto-compression-mode)

  ;; bzip2, a block-sorting file compressor.  Version 0.9.0, 30-Aug-98.
  (setq jka-compr-compression-info-list 
	(append
	 '(["\\.bz2\\'"
	    "bzip2ing"        "bzip2"         ()
	    "bunzip2ing"      "bzip2"         ("-d")
	    nil t])
	 jka-compr-compression-info-list))
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SGML を使うぜ!!
;;
(setq sgml-public-map '("%S" "/usr/lib/sgml/%o/%c/%d"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EDITOR=emacsclientで emacs で開く
;; PAGER=emacsclientで emacs で開く
;;
(server-start)

;;; Matches IF from top of file
))
