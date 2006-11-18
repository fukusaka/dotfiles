;;
;; Latex を使う設定
;;
(setq tex-run-command "ptex *")
(setq latex-run-command "platex *")
(setq slitex-run-command "platex *")

(setq tex-dvi-view-command "xdvi -geometry +0+0 *")
(setq tex-dvi-print-command "dvips -Plw230d * ")

(setq texinfo-tex-comand "ptex")