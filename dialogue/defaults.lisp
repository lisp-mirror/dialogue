;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: dialogue -*-

;;;; Dialogue Version 1.0
;;;; An abstract layer over SWT controls and layout capabilities.
;;;; Copyright (C), Francis Leboutte, see license.txt for details
;;;; Homepage of Dialogue: http://www.algo.be/cl/dialogue/index.htm

(in-package :dialogue)

;;; ****************************************************************************

;; the default edit-specifier which may be:
;; :clos , :sv (or :special-variable) , :plist, NIL , :struct (or :structure - not implemented)
(defvar *edit-specifier* :clos)

;;; if T make control-data for composites and labels too
(defvar *make-any-control-data?* t)
(defvar *make-composite-control-data?* t)

(defun ok-control (&key after-hook)
  `(:name :ok
    :type :button
    :style :push
    :initial-value "OK"   ; or :methods (:settext "OK")
    :value-accepter nil
    :event (:selection
            ,(lambda (event cd)  ; cd: control-data
               (declare (ignore event))
               (let ((dialog-data (cd-dialog-data cd)))  ; or *dialog-data
                 (update-data dialog-data :after-hook after-hook)
                 (setf (getf (dd-result dialog-data) :ok) t)
                 (shell.close (dd-dialog dialog-data)))))))

(defun cancel-control (&optional (text "Cancel"))
  `(:name :cancel
    :type :button
    :style :push
    :value-accepter nil
    :event (:selection
            ,(lambda (event cd)   ; cd: control-data
               (declare (ignore event))
               (let ((dialog-data (cd-dialog-data cd)))  ; or *dialog-data
                 (setf (getf (dd-result dialog-data) :cancel) t)
                 (shell.close (dd-dialog dialog-data)))))
    :methods (:settext ,text)))



#|
Example:
(def-swt-test
  (dialogue
   `(:grid
     (
      ((:type :label :initial-value "first in first line")
       (:group :text "Group"
        :layout (:verticalSpan 2)
        (:column ())))
      ((:type :label :initial-value "first in 2nd line")
       :span)
      (,(make-exit-controls-composite-spec))
      ))
   :exit-controls NIL ;; !
   :options `(:setbackground ,(system-color :yellow))))
|#
;;; define 2 ok and cancel controls in a composite control.
;;; to add exit-controls-specs in the compound argument without using the :exit-controls
;;; argument (this one has to be set to nil).
(defun make-exit-controls-composite-spec (&key
                                          (exit-controls-spec
                                           (list (ok-control)
                                                 (cancel-control)))
                                          ;; default layout data of the composite to be used
                                          ;; in :GRID layout
                                          (layout-data
                                           `(;; 4 ou plus, peu importe si il y a moins de cols
                                             :horizontalspan 4
                                             :horizontalalignment ,*swt.fill*
                                             ))
                                          (margintop 20))
  (list
   :composite :layout-data layout-data
   (list :row
         :justify t :pack nil :margintop margintop :marginbottom 0
         (if (= (display.getdismissalalignment *display*)
                *swt.left*)
             exit-controls-spec
           (rotate exit-controls-spec)))))


;;; ****************************************************************************
;;; 

(defvar *dialogue-default-font* nil)
(defvar *dialogue-default-trimmings* nil)

(defvar *prompt-alignemnt* *swt.right*)
  
(defvar *float-tolerance* .0001)

(defun value-equal (x y)
  (cond
   ((and (typep x 'string) (typep y 'string))
    (string= x y))
   ((and (typep x 'rational)(typep y 'rational))
    (= x y))
   ((and (typep x 'number)(typep y 'number))
    (<= (abs (- x y))
        (min *float-tolerance*
             ;; for small numbers, *float-tolerance* is treated as
             ;; a relative value of tolerance
             (* (max (abs x) (abs y))
                *float-tolerance*))))
   (t (eql x y))))