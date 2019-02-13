;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: dialogue -*-

;;;; Dialogue Version 1.0
;;;; An abstract layer over SWT controls and layout capabilities.
;;;; Copyright (C), Francis Leboutte, see license.txt for details
;;;; Homepage of Dialogue: http://www.algo.be/cl/dialogue/index.htm

(in-package :dialogue)


;;; ensure that var content is t or nil
;;; returns the t or nil (the boolean value of var)
(defmacro ensure-boolean-value (var)
  `(if ,var
       (setf ,var t)
     nil))

(defmacro do-j-vector ((var vec &key
                            (start 0)
                            (end nil)
                            (key nil)
                            ;(index (gensym))
                            )
                       &body body)
  (with-gensyms (index)
    `(loop for ,index from ,start below (if ,end
                                            ,end
                                          (vlength ,vec))
           do
           (let ((,var (if ,key
                           (funcall ,key (vref ,vec ,index))
                         (vref ,vec ,index))))
             ,@body))))

;;; - Items length limited to call-arguments-limit
;;; - foil make-new-vector function also uses apply
;;; (make-j-vector (list 1 3)) or (make-j-vector (list 1 3) :type :int)
;;; (make-j-vector (list T)) or (make-j-vector (list T) :type :boolean)
(defun make-j-vector (&optional (items nil) &key (type string. type?) key)
  (when (and items (not type?))
    (let* ((first (first items))
           (type%
            (typecase first
              (bignum :long)
              (integer :int)
              (double-float :double)
              (number :float)
              (character :char)
              (t (when (or (eq first t)
                           (eq first nil))
                   :boolean)))))
      (when type% (setf type type%))))          
  (apply #'make-new-vector
         type
         (length items)
         (if key
             (loop for i in items collect (funcall key i))
           items)))


(defun j-vector-to-list (vec1)
  (let ((list nil))
    (dotimes (i (vlength vec1) list)
      (push (vref vec1 i) list))
    (nreverse list)))
 
;;; ****************************************************************************

;;; to define a "generic" function corresponding to java member methods
;;; For example, (def-java-generic "SETTEXT") defines a SETTEXT function to be applied
;;; to any SWT widget.
;;; Allow to use settext instead of button.settext, label.settext, group.settext, etc.
;;; Arguments:
;;; Java-member-designator : for example :settext, settext or "SETTEXT"
;;; Function-name: the "generic" function name, defaults to
;;;  the java-member-designator argument
;;; Action-if-null-object is one of: :error :return-T :return-nil
;;;  Specify what to do if the object given to the generic function is NIL
;;; Def-java-generic exports the "generic" function name
;;; Note : see apply-j-method
(defmacro def-java-generic (java-member-designator &optional
                                                   function-name
                                                   (action-if-null-object :error))
  (let* ((java-string (if (symbolp java-member-designator)
                          (symbol-name java-member-designator)
                        java-member-designator))
         (java-name (if (symbolp java-member-designator)
                        java-member-designator
                      (intern java-member-designator)))
         (function-name (if function-name
                            function-name
                          java-name))
         (error-format-string-no-def
          (concatenate 'string
                       "can't find function definition of "
                       (string-downcase function-name)
                       " for ~s object"))
         (action (case action-if-null-object
                   (:error `(error
                             ,(concatenate 'string
                                           (string-downcase function-name)
                                           " doesn't accept nil as first argument")))
                   (:return-T t)
                   (:return-nil nil)
                   (t (error "In def-java-generic, action-if-null-object value is unknown: ~s" action-if-null-object)))))
    `(progn
       (export ',function-name)
       (defun ,function-name (object &rest rest)
         (ensure-typed-ref object)
         (if object
             (let* ((fun-name (concatenate 'string
                                           (symbol-name (type-of object))
                                           ,java-string))
                    (fun (find-symbol fun-name)))
               (unless (and fun (fboundp fun))
                 (error ,error-format-string-no-def object))
               (apply fun object rest))
           ,action)))))

;;; ****************************************************************************

;;; bug! Plante si fref est obsolète
(defun valid-fref? (fref &optional (fvm *fvm*))
  (and fref
       (let ((fref-fvm (fref-vm fref)))
         (ignore-errors (equals fvm fref-fvm)))))