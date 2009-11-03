;; $Id$
;;
;; 日本語環境で UTF-8 をメインにする設定
;;

(cond
 ;; XEmacs 21 UTF-8
 ((featurep 'xemacs)
  (if (functionp 'un-define-debian) (un-define-debian)) ;; for Debian
  (custom-set-variables '(load-home-init-file t t))
  (custom-set-faces)
  (set-language-environment          'Japanese)
  (set-default-coding-systems       'utf-8-unix)
  (setq coding-system-for-read      'utf-8-unix))

 ;; Carbon Emacs UTF-8
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
 ((= emacs-major-version 20)
  (set-language-info "Japanese" 'input-method "japanese")
  (set-language-environment          'Japanese)
  (set-default-coding-systems       'euc-japan-unix)
  (set-terminal-coding-system       'euc-japan-unix)
  (setq default-process-coding-system '(euc-jp . euc-jp))
  (set-default-font "fontset-standard"))

 ;; Ver.21 の場合 UTF-8
 ((= emacs-major-version 21)
  (if (functionp 'un-define-debian) (un-define-debian)) ;; for Debian
  (set-language-info "Japanese" 'input-method "japanese-egg-anthy")
  (set-language-environment 'Japanese)
  (let ((cs locale-coding-system))
    (prefer-coding-system cs)
    (set-keyboard-coding-system cs)
    (set-terminal-coding-system cs))
  (setq default-file-name-coding-system 'utf-8) ;; locale に無関係に設定
  (set-default-font "fontset-standard"))

 ;; Ver.22 の場合 UTF-8
 ((>= emacs-major-version 22)
  (if (functionp 'un-define-debian) (un-define-debian)) ;; for Debian
  (set-language-info "Japanese" 'input-method "japanese-anthy")
  (set-language-environment 'Japanese)
  (let ((cs locale-coding-system))
    (prefer-coding-system cs)
    (set-keyboard-coding-system cs)
    (set-terminal-coding-system cs))
  (setq default-file-name-coding-system 'utf-8) ;; locale に無関係に設定
  ;;(set-default-font "fontset-12")
  )
 )
