;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ついでに mh-e を快適に使う設定                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; もうmh-e は使わない
;; mime を使う設定。
;; (load "mime-setup")
(setq exec-path (cons "/usr/local/bin/mh" exec-path))
(setq exec-path (cons "/usr/local/lib/mh" exec-path))
;;
;; mh-e で色を付ける設定 mule 2.3 のみ
;;
(if window-system
    (progn
      (add-hook 'mh-folder-mode-hook 'hilit-rehighlight-buffer-quietly)
      (add-hook 'mh-inc-folder-hook 'hilit-rehighlight-buffer-quietly)
      (defun hilit-search-mh-folder-name (pend)
        (if (re-search-forward "^[ 0-9][ 0-9][ 0-9][0-9]..[0-9][0-9]/[0-9][0-9] \\(.................\\).*$" nil t nil)
            (let ((str (buffer-substring (match-beginning 1) (match-end 1))))
					; これは17文字
					;  2 Byte 文字が 2 columns であるとした
                                        ;場合の、17 columns を抜き出す
              (setq str
                    (code-convert-string
                     (substring (code-convert-string str *internal* *sjis*) 0 17)
                     *sjis* *internal*))
                                        ; point は *internal* での長さ
              (cons (match-beginning 1) (+ (match-beginning 1) (length str))))))
     (hilit-set-mode-patterns
       'mh-folder-mode
       '((hilit-search-mh-folder-name nil ForestGreen)
         ("[- M][0-9][0-9]/[0-9][0-9] " nil Red)
         ("^[ 0-9][ 0-9][ 0-9][0-9]" nil Blue)
         ("<<.*$" nil Purple)))

      (require 'mh-e)
      (define-key mh-folder-mode-map "\er"
        '(lambda ()
           (interactive)
           (mh-rescan-folder)
           (hilit-rehighlight-buffer-quietly)))))

