;;
;; 色を付ける共通設定
;;

(global-font-lock-mode t)

;; jit-lock-mode を使う
(setq font-lock-support-mode 'jit-lock-mode)

(defface moi-string-face
  '((((class color) (background light)) (:foreground "Brown"))
    (((class color) (background dark)) (:foreground "Salmon"))
    (t (:italic t)))
  nil
  )
(setq font-lock-string-face 'moi-string-face)

