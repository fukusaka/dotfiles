;; $Id$
;;
;; 日本語環境で UTF-8 をメインにする設定
;;

;; 日本語環境
(set-language-environment 'Japanese)

(when (not (featurep 'w32-win))
  ;; UTF-8を設定
  (prefer-coding-system 'utf-8)

  ;; locale に無関係に UTF8 を設定
  (setq default-file-name-coding-system 'utf-8)
  
  ;; プロセスの環境変数設定
  (setenv "LANG" "ja_JP.UTF-8"))

(cond
 ;; MacOSX/CarbonEmacs
 ((featurep 'mac-carbon))
 ((<= emacs-major-version 21)
  (set-language-info "Japanese" 'input-method "japanese-egg-anthy"))
 ((= emacs-major-version 22)
  (set-language-info "Japanese" 'input-method "japanese-anthy"))
 )
