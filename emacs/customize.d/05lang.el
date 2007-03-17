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
 ;; XEmacs 21
 ((featurep 'xemacs)
  (set-language-environment          'Japanese)  
  (set-default-coding-systems       'utf-8-unix)
  (setq coding-system-for-read      'utf-8-unix))

 ;; Carbon Emacs
 ((featurep 'mac-carbon)
  (set-language-environment 'Japanese)
  (let ((cs 'utf-8-unix))
    (prefer-coding-system cs)
    (set-keyboard-coding-system cs)
    (set-terminal-coding-system cs))
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  (setq default-file-name-coding-system 'utf-8) ;; locale に無関係に設定
  (setq mac-allow-anti-aliasing t)
  (require 'carbon-font)
  ;;(set-default-font "fontset-hiraginokaku")
  (set-default-font "fontset-standard")) ;; フォント設定(Xリソースで設定するのがベスト)

 ;; Ver.20 の場合 EUC-JP
 ((string-match "^20" emacs-version)
  (set-language-environment          'Japanese)
  (set-input-method "japanese-canna")
  (set-default-coding-systems       'euc-japan-unix)
  (set-terminal-coding-system       'euc-japan-unix)
  (setq default-process-coding-system '(euc-jp . euc-jp))
  (set-default-font "fontset-standard"))

 ;; Ver.21 の場合 UTF-8
 ((string-match "^2[12]" emacs-version)
  (if (functionp 'un-define-debian) (un-define-debian)) ;; for Debian
  (set-language-environment 'Japanese)
  (set-input-method "japanese-egg-anthy")
  (let ((cs locale-coding-system))
    (prefer-coding-system cs)
    (set-keyboard-coding-system cs)
    (set-terminal-coding-system cs))
  (setq default-file-name-coding-system 'utf-8) ;; locale に無関係に設定
  (set-default-font "fontset-standard")) ;; フォント設定(Xリソースで設定するのがベスト)
 )
