;;; my-compat.el ---
;;
;; Author: Shoichi Fukusaka <fukusaka@xa2.so-net.ne.jp>

;; for Compatibility

;; for ancient emacs ... ?
(if (not (fboundp 'when))
    (defmacro when (cond &rest body)
      (list 'if cond (cons 'progn body))))

(if (not (fboundp 'unless))
    (defmacro unless (cond &rest body)
      (cons 'if (cons cond (cons nil body)))))

(if (not (fboundp 'push))
    (defmacro push (newelt listname)
      (list 'setq listname
	    (list 'cons newelt listname))))

(if (not (fboundp 'caar))
    (defsubst caar (x)
      (car (car x))))

(if (not (fboundp 'cadr))
    (defsubst cadr (x)
      (car (cdr x))))

(if (not (fboundp 'cdar))
    (defsubst cdar (x)
      (cdr (car x))))

(if (not (fboundp 'cddr))
    (defsubst cddr (x)
      (cdr (cdr x))))

;; for Emacs20
(when (and (<= emacs-major-version 20)
           (not (fboundp 'executable-find)))
  (defvar executable-binary-suffixes
    (if (memq system-type '(ms-dos windows-nt))
        '(".exe" ".com" ".bat" ".cmd" ".btm" "")
      '("")))
  (defun executable-find (command)
    (let ((list exec-path) file)
      (while list
        (setq list
              (if (and (setq file (expand-file-name command (car list)))
                       (let ((suffixes executable-binary-suffixes)
                             candidate)
                         (while suffixes
                           (setq candidate (concat file (car suffixes)))
                           (if (and (file-executable-p candidate)
                                    (not (file-directory-p candidate)))
                               (setq suffixes nil)
                             (setq suffixes (cdr suffixes))
                             (setq candidate nil)))
                         (setq file candidate)))
                  nil
                (setq file nil)
                (cdr list))))
      file))
  )

;; for Emacs20
(when (<= emacs-major-version 20)
  (defun add-to-list (list-var element &optional append compare-fn)
    (if (cond
         ((null compare-fn)
          (member element (symbol-value list-var)))
         ((eq compare-fn 'eq)
          (memq element (symbol-value list-var)))
         ((eq compare-fn 'eql)
          (memql element (symbol-value list-var)))
         (t
          (let ((lst (symbol-value list-var)))
            (while (and lst
                        (not (funcall compare-fn element (car lst))))
              (setq lst (cdr lst)))
            lst)))
        (symbol-value list-var)
      (set list-var
           (if append
               (append (symbol-value list-var) (list element))
             (cons element (symbol-value list-var)))))))

(provide 'my-compat)
;;; my-compat.el ends here
