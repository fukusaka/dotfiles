;;; 44mode-compile.el --

;; mode-compile
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)

(global-set-key "\C-cc" 'mode-compile)
(global-set-key "\C-ck" 'mode-compile-kill)

;; 全てバッファを自動的にセーブする
(setq mode-compile-always-save-buffer-p t)
;; コマンドをいちいち確認しない
(setq mode-compile-never-edit-command-p t)
;; メッセージ出力を抑制
(setq mode-compile-expert-p t)
;; メッセージを読み終わるまで待つ時間
(setq mode-compile-reading-time 0)

;;; 44mode-compile.el ends here
