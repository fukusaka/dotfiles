;;
;; 日本語環境で UTF-8 をメインにする設定
;;

;; 日本語環境
(set-language-environment 'Japanese)

;; Windows系はロケールに従う
(unless (or (eq system-type 'windows-nt)
	    (eq system-type 'cygwin))

  ;; UTF-8を設定
  (prefer-coding-system 'utf-8)

  ;; locale に無関係に UTF8 を設定
  (setq default-file-name-coding-system 'utf-8)
  
  ;; プロセスの環境変数設定
  (setenv "LANG" "ja_JP.UTF-8"))

;; GNU/Linux で egg-anthy を使う設定
(if (eq system-type 'gnu/linux)
    (set-language-info "Japanese" 'input-method "japanese-anthy"))
