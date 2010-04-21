;; カスタマイズを簡素化するマクロ関数群
;; my-startup.el をダイエットする為に移動

;; カスタマイズ設定がし易いように cl パケージは読み
(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; よく使う連想リストの追加用
(defun add-to-assoc-list (list-var element &optional append)
  "連想リスト版の add-to-list。既にキーが存在するとき上書きする"
  (let ((list (assoc (car element) (symbol-value list-var))))
    (if list
        (setcdr list (cdr element))
      (set list-var (append (symbol-value list-var) (list element)))))
  (symbol-value list-var))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.sodan.org/~knagano/emacs/dotemacs.html
;; より拝借

(defmacro exec-if-bound (sexplist)
  "関数が存在する時だけ実行する。（car の fboundp を調べるだけ）"
  `(if (fboundp (car ',sexplist))
       ,sexplist))

(defmacro defun-add-hook (hookname &rest sexplist)
  "add-hook のエイリアス。引数を関数にパックして hook に追加する。"
  `(add-hook ,hookname
             (function (lambda () ,@sexplist))))

(defun load-safe (loadlib)
  "安全な load。読み込みに失敗してもそこで止まらない。"
  ;; missing-ok で読んでみて、ダメならこっそり message でも出しておく
  (let ((load-status (load loadlib t)))
    (or load-status
        (message (format "[load-safe] failed %s" loadlib)))
    load-status))

(defmacro eval-safe (&rest body)
  "安全な評価。評価に失敗してもそこで止まらない。"
  `(condition-case err
       (progn ,@body)
     (error (message "[eval-safe] %s" err))))

