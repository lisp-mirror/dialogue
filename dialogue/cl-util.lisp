;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: dialogue -*-

;;;; Dialogue Version 1.0
;;;; An abstract layer over SWT controls and layout capabilities.
;;;; Copyright (C), Francis Leboutte, see license.txt for details
;;;; Homepage of Dialogue: http://www.algo.be/cl/dialogue/index.htm

(in-package :dialogue)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (proclaim (cons 'special
                  '(ignore1 ignore2 ignore))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-gensyms (symbols &body body)
    `(let ,(mapcar #'(lambda (symbol)
                       `(,symbol (gensym)))
             symbols)
       ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro m (&rest arguments)
    #-dev
    (declare (ignore arguments))
    (progn)
    #+dev
    (with-gensyms (stream)
      `(let ((,stream *standard-output*))
         (funcall #'format ,STREAM "~&__* ~{ ~S~}~&" (list ,@arguments))
         (force-output ,stream)
         (values)))))

(defmacro when-bind ((var form) &body body)
  `(let ((,var ,form))
     (if ,var
         (progn ,@body)
       NIL)))

(defmacro if-bind ((var form) form1 form2)
  `(let ((,var ,form))
     (if ,var
         ,form1
       ,form2)))

(defmacro while (test &body body)
  (let ((end-test (gensym))
	(loop (gensym)))
    `(block nil
       (tagbody (go ,end-test)
		,loop
		,@body
		,end-test
		(unless (not ,test) (go ,loop))
		(return)))))

(defmacro with-package (package &body body)
   `(let ((*package* (if (packagep ,package)
                        ,package
                        (find-package ,package))))
       (progn ,@body)))

(defmacro neq (x y)
   `(not (eq ,x ,y)))

(defmacro memberq (x list)
  `(member ,x ,list :test #'eq))

(defmacro deleteq (x list)
  `(delete ,x ,list :test #'eq))

(defmacro xor (&rest predicates)
  "True only if exactly one predicate is true. Short circutes when it finds
   a second one is true. Returns the true predicate"
  (let ((result (gensym))
        (temp (gensym))
        (block-name (gensym)))
    `(block ,block-name
       (let ((,result ,(car predicates))
             ,temp)
         ,@(let (code-result)
             (dolist (pred (cdr predicates))
               (push `(cond
                       ((and (setq ,temp ,pred)
                             ,result)
                        (return-from ,block-name nil))
                       (,temp
                        (setq ,result ,temp)))
                     code-result))
             (nreverse code-result))
         ,result))))


(defun symbol-macro? (symbol)
  #+:lispworks
  (find 'system::pc-symbol-macro-definition
         (symbol-plist symbol)
         :test #'eq)
  #-:lispworks
  (error "symbol-macro? function is not defined in this Lisp"))

;;; ****************************************************************************
;;;

(defmacro i<= (a b)
  (declare (type fixnum a b))
  `(<=  ,a ,b))
(defmacro i+ (a b)
  `(the fixnum
        (+  (the fixnum ,a) (the fixnum ,b))))

;;; ****************************************************************************
;;; DEBUG

(defmacro with-error- ((tag
                        &key
                        detail
                        message
                        (end-message nil)
                        ;; a value to return on error (if no on-error-fun )
                        (on-error nil)
                        ;; a zero arg fun to execute on error
                        (on-error-fun nil)
                        (silent nil)
                        )
                       &body body)
  `(handler-case
       (progn ,@body)
     (error (condition)
            (when ,silent (m condition))
            (unless ,silent
              (handler-case
                  (error-dialog condition
                                :message ,message
                                :tag ,tag
                                :detail ,detail
                                :end-message ,end-message)
                (error (c)
                       (m :error-in-error-dialog c))))
            (if ,on-error-fun
                (funcall ,on-error-fun)
              ,on-error))))

(defmacro with-error-- ((tag
                         &key
                         detail
                         message
                         (end-message nil)
                         ;; a value to return on error (if no on-error-fun )
                         (on-error nil)
                         ;; a zero arg fun to execute on error
                         (on-error-fun nil)
                         (silent nil)
                         )
                        &body body)
  (declare (ignore tag message detail end-message on-error on-error-fun silent))
  `(progn
     ,@body))

(defun error-dialog (condition
                     &key
                     message
                     tag
                     detail
                     end-message
                     (parent *main-swt-shell*)
                     (title ""))
  (let (text)
    (setf message (if message message "Sorry, the following error has occured:")
          text (if text text (concatenate
                              'string
                              (if tag (format nil "~%tag: ~a~%" tag) "")
                              (if detail (format nil "~% ~a~%" detail) "")
                              (if condition (format nil "~%~a~%" condition) "")))
          end-message (if end-message (format nil "~a" end-message) ""))
    (dialogue 
     `(
       (:initial-value ,message
        :prompt nil
        :type :text
        :style (:read_only)
        )
       (:initial-value ,text
        :prompt nil
        :type :text
        :style (:multi) )
       (:initial-value ,end-message
        :prompt nil
        :type :text
        :style (:read_only)
        )      )
     ;; :edit :special-variable
     :title title
     :name :error-dialog
     :parent parent
     :exit-controls (list `(:name :ok
                            :type :button
                            :style :push
                            :initial-value "OK"   ; or :methods (:settext "OK")
                            :value-accepter nil
                            :event (:selection
                                    ,(lambda (event cd)  ; cd: control-data
                                       (declare (ignore event))
                                       (shell.close (dd-dialog (cd-dialog-data cd)))))))
     )))

;;; ****************************************************************************

; (filter-key&value-pairs '(:a 2 :x T 3 4 :b 3) '(:x :b))
; > (:a 2 3 4) 
; (filter-key&value-pairs (list :a 1 :b 2 :c 3) '( :b))
; > (:a 1 :c 3)
(defun filter-key&value-pairs (list keys-to-filter)
  (loop with result 
      for sub on list by #'cddr
      as k  = (first sub)
      finally (return (nreverse result))
      unless (member k keys-to-filter :test #'eq)
      do
      (push k result)
      (push (second sub) result)))

(defun prop-present? (plist key)
   (neq (getf plist key :%no%) :%no%))

;;; like getf, but accept a malformed property list: look in plist while it is
;;; well formed.
(defun getf-secure (plist key &optional default)
  (loop for sublist on plist by #'cddr
        as k = (first sublist)
        unless (keywordp k) do (return-from getf-secure default)
        when (eq k key) do (return-from getf-secure (second sublist))
        finally (return default)))

(defun getf+ (plist key)
   (let* ((val (getf plist key :%no%))
          (present? (neq val :%no%)))
      (values (if present? val NIL)(if present? T NIL))))


(defun rotate (list)
   (if list
      (append (rest list) (list (first list)))
      list))

;;; kv-pairs-and-list is a list like ({k v}* {item}*)
;;; k : keyword    v : value
;;; split-kv-pairs-and-list returns 2 values ({k v}*) and ({item}*)
(defun split-kv-pairs-and-things (kv-pairs-and-lists)
  (loop with kv-list = nil
        for sublist on kv-pairs-and-lists by #'cddr
        as keyword  = (first sublist)
        finally (return (values kv-list sublist))
        if (keywordp keyword)
        do
        (push (second sublist) kv-list)
        (push keyword kv-list)
        else do
        (return (values kv-list sublist))))


;;; list-items-and-kv-pairs : (i1 i2 ... :k1 v1 :k2 :v2 ...)
(defun split-list-items-and-kv-pairs (list-items-and-kv-pairs)
  (loop with list-items = nil
        for sublist on list-items-and-kv-pairs
        as thing = (first sublist)
        finally (return (values (nreverse list-items)
                                sublist))
        if (keywordp thing)
        do
        (return (values (nreverse list-items)
                                sublist))
        else do
        (push thing list-items) ))


(defun symbolp+ (item)
   (and item (not (eq item t)) (symbolp item)))


;;; return the type of the slot or NIL if the type was not defined or defined as T
(defun slot-definition-type+ (slot-name instance)
   (ignore-errors
     (let ((type (clos::slot-definition-type
                   (find slot-name (clos::class-slots (class-of instance))   
                     :key #'clos::slot-definition-name))))
        (if (eq type t)
           NIL
           type))))

(defmacro ignore-errors+ (form-on-error
                          &body body)
  `(handler-case
       (progn ,@body)
     (error (condition)
            condition
            ,form-on-error)))

(defmacro with-nice-error-msg (error-msg
                               &body body)
  `(handler-case
       (progn ,@body)
     (error (condition)
            condition
            (ok-di ,error-msg :icon :error))))

;;; ****************************************************************************

(without-protection-errors
  (defmacro :dtest (&body body)
    `(without-protection-errors
       (defun :test ()
         ,@body))))


;;; ****************************************************************************
