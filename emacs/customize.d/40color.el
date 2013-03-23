;;
;; 色を付ける共通設定
;;
(global-font-lock-mode t)

;; jit-lock-mode を使う
(setq font-lock-support-mode 'jit-lock-mode)

;; クォート文字列の色付け

(custom-set-faces
 '(font-lock-string-face
   ((((class color) (background light)) (:foreground "Brown"))
    (((class color) (background dark)) (:foreground "Salmon"))
    (t (:italic t)))))


;; 行末の不要スペースを強調表示
;;(set-face-underline 'trailing-whitespace "Red")
(set-face-background 'trailing-whitespace "MistyRose")

;; 行末スペースを色づけ
(setq-default show-trailing-whitespace t)

;; タブと改行と全角空白の色付け
(defface my-face-b-1 '((t (:background "gray80"))) nil)
(defface my-face-b-2 '((t (:background "gray90"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)

(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("\t" 0 my-face-b-2 append)
     ("　" 0 my-face-b-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

;; mmm-mode のサブモードの背景色

(custom-set-faces
 '(mmm-default-submode-face
   ((t (:background "gray95")))))

;; nxhtml-mode
(custom-set-faces
 '(mumamo-background-chunk-major
   ((t (:background "white")))))

(custom-set-faces
 '(mumamo-background-chunk-submode1
   ((t (:background "lightcyan")))))
