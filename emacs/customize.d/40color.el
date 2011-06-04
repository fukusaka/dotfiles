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

;; 差分表示(diff-mode)のとき色付け

(custom-set-faces
 '(diff-header
   ((((class color) (min-colors 88) (background light))
     :background "grey80" :foreground "ForestGreen" :weight bold)
    (((class color) (min-colors 88) (background dark))
     :background "grey45" :foreground "PaleGreen" :weight bold)
    (((class color) (background light)) :foreground "ForestGreen" :weight bold)
    (((class color) (background dark))  :foreground "PaleGreen" :weight bold)
    (t :weight bold)))
 '(diff-file-header
   ((((class color) (min-colors 88) (background light))
     :background "grey75" :foreground "ForestGreen" :weight bold)
    (((class color) (min-colors 88) (background dark))
     :background "grey60" :foreground "PaleGreen" :weight bold)
    (((class color) (background light)) :foreground "ForestGreen" :weight bold)
    (((class color) (background dark))  :foreground "PaleGreen" :weight bold)
    (t :weight bold)))
 '(diff-index       ((t :inherit diff-file-header)))
 '(diff-nonexistent ((t :inherit diff-file-header)))
 '(diff-hunk-header ((t :inherit diff-header)))
 '(diff-function    ((t :inherit diff-header)))

 '(diff-removed
   ((((class color) (background light)) :foreground "Orchid")
    (((class color) (background dark))  :foreground "LightSteelBlue")))
 '(diff-added
   ((((class color) (background light)) :foreground "Blue")
    (((class color) (background dark))  :foreground "LightSkyBlue")))
 '(diff-changed
   ((((class color) (background light)) :foreground "DarkMagenta")
    (((class color) (background dark))  :foreground "yellow")))
 '(diff-indicator-removed ((t :inherit diff-removed)))
 '(diff-indicator-added   ((t :inherit diff-added)))
 '(diff-indicator-changed ((t :inherit diff-changed)))

 '(diff-context ((((class color grayscale) (min-colors 88)) :inherit shadow)))
 '(diff-refine-change
   ((((class color) (min-colors 88) (background light))     :background "grey85")
    (((class color) (min-colors 88) (background dark))      :background "grey60")
    (((class color) (background light)) :background "yellow")
    (((class color) (background dark))  :background "green")
    (t :weight bold)))
 )

;; mmm-mode のサブモードの背景色

(custom-set-faces
 '(mmm-default-submode-face
   ((t (:background "gray95")))))
