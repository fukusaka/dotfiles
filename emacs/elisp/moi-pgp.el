

(defvar moi-pgp-key-id "Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>")

(defun moi-pgp-encode (file)
  (let ((moi (find-file file))
	(moi2 (generate-new-buffer "*MoiTemp2*"))
	(moi3 (generate-new-buffer "*MoiTemp3*")))
    (set-buffer moi)
    (shell-command-on-region
     (point-min) (point-max)
     (format "pgp -fea \'%s\'" moi-pgp-key-id)
     moi2 nil moi3)
    ))

(defun moi-pgp-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'moi-pgp-mode)     ; `describe-mode'������
  (setq mode-name "Moi-Pgp")          ; �⡼�ɹԤ�ɽ�������⡼��̾��
  (lisp-mode-variables nil)              ; ��������ѿ���������롣
  (run-hooks 'moi-pgp-mode-hook))     ; �桼�����������ޥ����Τ������
					;   �ꤷ���եå��򤳤��Ǽ¹Ԥ��롣
