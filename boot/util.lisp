(in-package :cl-user)

(export '(without-protection-errors
          without-redefinition-warnings
          ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-gensyms (symbols &body body)
    `(let ,(mapcar #'(lambda (symbol)
                       `(,symbol (gensym)))
             symbols)
       ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro allf (val &rest args)
    (with-gensyms (gval) 
      (list 'let (list (list gval val)) 
            (cons 'setf 
                  (mapcan 
                   #'(lambda (a)
                       (list a gval))
                   args))))))

(defmacro nilf (&rest args) `(allf nil ,@args))


(defmacro when-bind ((var form &rest predicates) &body body)
   `(let ((,var ,form))
       ,(if (consp predicates)
           `(if (and ,var (funcall (fint ,@predicates) ,var))
               (progn ,@body)
               NIL)
           `(if ,var 
               (progn ,@body)
               NIL)
           )))

(defmacro if-bind ((var form &rest predicates) form1 form2)
   `(let ((,var ,form))
       ,(if (consp predicates)
           `(if (and ,var (funcall (fint ,@predicates) ,var))
               ,form1
               ,form2)
           `(if ,var 
               ,form1
               ,form2))))


(defun fint (fn &rest fns)
   "function intersection. E.g. (funcall (fint #'integerp #'plusp #'oddp) 1) returns T"
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        (lambda (x) 
          (and (funcall fn x) (funcall chain x))))))


(defmacro ifn (test f1 &optional (f2 T))
  `(if ,test ,f2  ,f1))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro without-redefinition-warnings (&body body)
    #+:lispworks
    `(let ((dspec:*redefinition-action* nil))
       ,@body)
    #-:lispworks
    `(progn ,@body)))


(defmacro without-protection-errors (&body body)
  `(without-redefinition-warnings
     #+:lispworks
     (let ((*handle-warn-on-redefinition* nil))
       ,@body)
     #-:lispworks
     ,@body))


(defmacro msg (&rest arguments)
  `(let ((stream *standard-output*))
     (funcall #'format STREAM "~&__~{ ~a~}" (list ,@arguments))
     (ignore-errors (force-output stream))
     (values)))

(defmacro msgc (control-string &rest arguments)
  `(let ((stream *standard-output*))
     (apply #'format stream ,(concatenate 'string
                                          "~&__ "
                                          control-string)
            (list ,@arguments))
     (ignore-errors (force-output stream))
     (values)))

(defun filter (fn lst)
   "returns the items for which FN returns T , in same order"
   (loop for x in lst
     when (funcall fn x)
     collect x))