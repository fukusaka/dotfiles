;; �������ޥ�������ǲ�����ޥ���ؿ���
;; my-startup.el ��������åȤ���٤˰�ư

;; �������ޥ������꤬���פ��褦�� cl �ѥ��������ɤ�
(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; �褯�Ȥ�Ϣ�ۥꥹ�Ȥ��ɲ���
(defun add-to-assoc-list (list-var element)
  "Ϣ�ۥꥹ���Ǥ� add-to-list�����˥�����¸�ߤ���Ȥ���񤭤���"
  (let ((list (assoc (car element) (symbol-value list-var))))
    (if list
        (setcdr list (cdr element))
      (set list-var (append (symbol-value list-var) (list element)))))
  (symbol-value list-var))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.sodan.org/~knagano/emacs/dotemacs.html
;; ����Ҽ�

(defmacro exec-if-bound (sexplist)
  "�ؿ���¸�ߤ���������¹Ԥ��롣��car �� fboundp ��Ĵ�٤������"
  `(if (fboundp (car ',sexplist))
       ,sexplist))

(defmacro defun-add-hook (hookname &rest sexplist)
  "add-hook �Υ����ꥢ����������ؿ��˥ѥå����� hook ���ɲä��롣"
  `(add-hook ,hookname
             (function (lambda () ,@sexplist))))

(defun load-safe (loadlib)
  "������ load���ɤ߹��ߤ˼��Ԥ��Ƥ⤽���ǻߤޤ�ʤ���"
  ;; missing-ok ���ɤ�Ǥߤơ�����ʤ餳�ä��� message �Ǥ�Ф��Ƥ���
  (let ((load-status (load loadlib t)))
    (or load-status
        (message (format "[load-safe] failed %s" loadlib)))
    load-status))

(defmacro eval-safe (&rest body)
  "������ɾ����ɾ���˼��Ԥ��Ƥ⤽���ǻߤޤ�ʤ���"
  `(condition-case err
       (progn ,@body)
     (error (message "[eval-safe] %s" err))))

