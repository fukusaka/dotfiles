;; $Id$
;;
;; 日本語環境でEUCをメインにする設定
;;
;; しかし、.emacs のみは iso-2022-jp (jis code?)で書かないと
;; 文字列が化ける、、、とほ。
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
 ((string-match "^2[01]" emacs-version)
  (defun my-japanese-setup ()
    (if (equal current-language-environment "Japanese")
	(setq default-input-method "japanese-egg-canna")))
  (add-hook 'set-language-environment-hook 'my-japanese-setup)
  (set-language-environment          'Japanese)

  ;; ほとんどの場合 euc-japan-unix を使う
  (set-default-coding-systems       'euc-japan-unix)
  ;; emacs -nw では、上では遅すぎなので、、、本当か？
  (set-terminal-coding-system       'euc-japan-unix)
  ;;(setq default-process-coding-system '(euc-jp . euc-jp))
  ;; フォント設定(Xリソースで設定するのがベスト)
  ;; ~/.Xresources -->
  ;;   Emacs.Font: -*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-standard
  (set-default-font "fontset-standard")
  ;; (set-default-font "-*-fixed-*-r-normal-*-16-*-*-*-*-*-fontset-standard")
  ;; scroll bar を右にする設定。
  (if (fboundp 'set-scroll-bar-mode)
      (set-scroll-bar-mode 'right))
  (if nil
  (if (not window-system)
      (progn
	;; Translate `C-h' to <DEL>.
	;;(keyboard-translate ?\C-h ?\C-?)
	
	;; Translate <DEL> to `C-h'.
	;;(keyboard-translate ?\C-? ?\C-h)
       ))
  )
  )
 )
