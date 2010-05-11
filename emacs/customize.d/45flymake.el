(require 'flymake)

;;; エラーをミニバッファに表示
;;;; http://d.hatena.ne.jp/xcezx/20080314/1205475020
;;(defun flymake-display-err-minibuf ()
;;  "Displays the error/warning for the current line in the minibuffer"
;;  (interactive)
;;  (let* ((line-no             (flymake-current-line-no))
;;         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
;;         (count               (length line-err-info-list)))
;;    (while (> count 0)
;;      (when line-err-info-list
;;        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
;;               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
;;               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
;;               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
;;          (message "[%s] %s" line text)))
;;      (setq count (1- count)))))

(add-hook 'find-file-hook 'flymake-find-file-hook)

(global-set-key [f3] 'flymake-display-err-menu-for-current-line)
(global-set-key [f4] 'flymake-goto-next-error)


;; Makefile があれば、次のルールを追加
;;PHONY: check-syntax
;;check-syntax:
;;    $(CXX) -Wall -Wextra -pedantic -fsyntax-only $(CHK_SOURCES)

;;シンタックスチェックは次のコマンドが呼ばれる
;;make -s -C . CHK_SOURCES=hoge.cpp SYNTAX_CHECK_MODE=1 check-syntax