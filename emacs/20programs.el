;; プログラム

;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; $Id$

;;
;; Guile をつかう設定
(setq scheme-program-name "/usr/bin/guile")

;;
;; Latex を使う設定
;;
(setq latex-run-command "platex *")
(setq tex-dvi-view-command "xdvi -geometry +0+0 *")
(setq tex-dvi-print-command "dvi2ps * | lpr -Pgs360")

(setq texinfo-tex-comand "ptex")
