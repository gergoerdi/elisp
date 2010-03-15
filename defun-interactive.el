;; (C) 2009 Gergo ERDI http://gergo.erdi.hu/
;; Get the latest version of this file at git://gergo.erdi.hu/elisp/defun-interactive.el

(require 'cl)

(defmacro* defun/interactive (name (&rest args) doc &body body)
  ;; TODO: Support for special initializers not actually supported by (interactive), such as line and col number
  (flet ((interactive-code (init-spec)
           (if (stringp init-spec)
               (interactive-code `(:string ,init-spec))
               (destructuring-bind (init-op &rest init-args) (if (consp init-spec) init-spec (list init-spec))
                 (flet ((prompt (directive)
                          (concat directive (apply #'concat init-args) ": ")))
                   (ecase init-op
                     ((:string)
                      (prompt "s"))                     
                     ((:number)
                      (prompt "n"))                     
                     ((:dir)
                      (prompt "D"))                     
                     ((:file)
                      (prompt "f"))                     
                     ((:newfile)
                      (prompt "F"))
                     ((:point)
                      "d")
                     ((:region)
                      "r"))))))         
         (collect-argument (arg-spec)
           (cond ((atom arg-spec) (list arg-spec))
                 ((consp (car arg-spec)) (car arg-spec))
                 (t (list (car arg-spec))))))

    (let ((args/names (loop for arg in args
                            append (collect-argument arg)))
          (args/interactive (mapconcat (lambda (arg)
                                         (interactive-code (if (consp arg) (cadr arg) (symbol-name arg))))
                                       args "\n")))
      `(defun ,name ,args/names
         ,@(when (stringp doc) (list doc))
         ,(if args/interactive
              `(interactive ,args/interactive)
              '(interactive))
         ,(unless (stringp doc) doc)
         ,@body))))
