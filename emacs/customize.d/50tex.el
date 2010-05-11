;;
;; Latex を使う設定
;;

(setq tex-run-command "ptex *")
(setq latex-run-command "platex *")
(setq slitex-run-command "platex *")
(setq texinfo-tex-comand "ptex")

(setq tex-offer-save nil)

(when (memq window-system '(ns mac))
  (setq tex-dvi-print-command "opendvi *")
  (setq tex-dvi-view-command "opendvi *"))

