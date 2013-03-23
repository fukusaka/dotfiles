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

(cond
 ((< emacs-major-version 23)

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
     ;;;("[ ]+$" 0 my-face-u-1 append)
       )))
  (ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
  (ad-activate 'font-lock-mode)
  )

 ((>= emacs-major-version 23)
  (require 'whitespace)

  (setq whitespace-style
        '(face
          tabs spaces trailing
          tab-mark space-mark
          ;;newline-mark
          ))
;;        '(face
;;          tabs spaces trailing lines space-before-tab newline
;;          indentation empty space-after-tab
;;          space-mark tab-mark newline-mark))

  (setq whitespace-space-regexp "\\(\u3000+\\)")
  (setq whitespace-display-mappings
        '(
          (space-mark   ?\u3000 [?\u25a1])
          (tab-mark     ?\t     [?\u00BB ?\t] [?\\ ?\t])
          (newline-mark ?\n     [?| ?\n])
          ;;(newline-mark ?\n     [?$ ?\n])
          ;;(newline-mark ?\n     [?\u21B5 ?\n] [?$ ?\n])
          ;;(newline-mark ?\n     [?\u00B6 ?\n] [?$ ?\n])
          ;;(newline-mark ?\n     [?\u00AF ?\n] [?$ ?\n])
          ;;(newline-mark ?\n     [?\u00AC ?\n] [?$ ?\n])
          ;;(newline-mark ?\n     [?\u00B0 ?\n] [?$ ?\n])
          ))
;;	ddd
  (set-face-foreground 'whitespace-space "lightgray")
  (set-face-background 'whitespace-space nil)
  (set-face-foreground 'whitespace-tab "lightgray")
  (set-face-background 'whitespace-tab nil)
  (set-face-foreground 'whitespace-newline "gray95")
  (set-face-background 'whitespace-newline nil)
  (set-face-background 'whitespace-trailing "MistyRose")

  (global-whitespace-mode 1)
  (global-whitespace-newline-mode 1)
  ))

;; タブと改行と全角空白の色付け

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
