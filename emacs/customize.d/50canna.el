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
 ((string-match "^20.3" emacs-version)
  ;; 常に『かんな/emacs』なので、、、。
  (set-input-method "japanese-canna")
  ;; とっても簡単になったもんだ〜ぁ。
  )
 ((string-match "^20.4" emacs-version)
  ;; かんなよ戻って来てくれ！
  (load "yc")
  )
 )
