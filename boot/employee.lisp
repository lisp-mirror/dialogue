(in-package :dialogue)


;;; ****************************************************************************
;;;

(defgeneric class-slot-value (class slot-name)
  (:documentation "class : class name or class object"))

(defmethod class-slot-value ((class standard-class) slot-name)
  (unless (clos:class-finalized-p class)
    (clos:finalize-inheritance class))
  (slot-value (clos:class-prototype class) slot-name))

(defmethod class-slot-value ((class symbol) slot-name)
  (class-slot-value (find-class class) slot-name))

(defmethod (setf class-slot-value) (new (class symbol) slot-name)
  (setf (class-slot-value (find-class class) slot-name)
    new))

(defmethod (setf class-slot-value) (new (class standard-class) slot-name)
  (unless (clos:class-finalized-p class)
    (clos:finalize-inheritance class))
  (setf (slot-value (clos:class-prototype class) slot-name)
    new))

;;; ****************************************************************************
;;;

(defparameter *employees* NIL)

(defvar *departments* '(:shipping :accounting :r&d))

(defclass employee ()
  ((name :type string :initarg :name 
         :accessor name)
   (email :type string :initarg :email
          :accessor email)
   (full-time? :type boolean :initarg :full-time?
               :initform T :accessor full-time?)
   (department :type keyword :initarg :department
               :initform :shipping :accessor department)
   (salary :type integer :initarg :salary
           :initform 2000 :accessor salary)
   (text1 :type string :accessor text1 :initform "tTT111")
   (text2 :type string :accessor text2 :initform "t2-abc
t2")
   ))
   
          


(defmethod initialize-instance :after ((object employee) &key name email)
  (setf (email object)
        (concatenate 'string (if email email name) "@" "algo.be"))
  (pushnew object *employees* :key #'name :test #'string-equal))


;;; spy salary slot
(defmethod salary :before ((object employee))
   (format t "~&Reading ~A salary" (name object)))
(defmethod (setf salary) :before (new-value (object employee))
   (format t "~&Setting ~A salary to ~S" (name object) new-value))


(defvar *employee-1*
  (make-instance 'employee :name "Noé"
                 :full-time? nil
                 :salary 1000
                 :department :r&d
                 :email "nono"
                 ))

(defvar *employee-2*
  (make-instance 'employee :name "Jules"
                 :department :accounting))

(make-instance 'employee :name "Albert")
(make-instance 'employee :name "Anne")
(make-instance 'employee :name "Maude")
(make-instance 'employee :name "Henri")

(defun department.items (data)
  (declare (ignore data))
  (let ((array (make-new-array string. (length *departments*))))
    (loop for d in *departments
          as i = 0 then (1+ i)
          do
          (m i )
          (setf (jref array i) (symbol-name d)))
    array))

;(dotimes (i 10)(make-instance 'employee :name (string (gensym))))

