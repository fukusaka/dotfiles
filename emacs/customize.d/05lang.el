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
 ;; Ver.19 の場合 EUC-JP
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
	)))

 ;; Ver.20 の場合 EUC-JP
 ((string-match "^20" emacs-version)
  (defun my-japanese-setup ()
    (if (equal current-language-environment "Japanese")
	(setq default-input-method "japanese-canna")))
  (add-hook 'set-language-environment-hook 'my-japanese-setup)
  (set-language-environment          'Japanese)
  (set-default-coding-systems       'euc-japan-unix)
  (set-terminal-coding-system       'euc-japan-unix)
  (setq default-process-coding-system '(euc-jp . euc-jp))
  (set-default-font "fontset-standard"))

 ;; Ver.21 の場合 UTF-8
 ((string-match "^21" emacs-version)
  (defun my-japanese-setup ()
    (if (equal current-language-environment "Japanese")
	(setq default-input-method "japanese-egg-anthy")))
  (add-hook 'set-language-environment-hook 'my-japanese-setup)
  (set-language-environment          'Japanese)  
  (set-default-coding-systems       'utf-8-unix)
  (set-terminal-coding-system       'utf-8-unix)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  (setq default-file-name-coding-system 'utf-8-unix)

  ;; フォント設定(Xリソースで設定するのがベスト)
  (if (featurep 'xemacs) nil
    (set-default-font "fontset-standard")))
 )
