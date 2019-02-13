;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: dialogue -*-

;;;; An abstract layer over SWT controls and layout capabilities.
;;;; Copyright (C), Francis Leboutte, see license.txt for details
;;;; Homepage of Dialogue: http://www.algo.be/cl/dialogue/index.htm
;;;;
;;;; If needed, load employee.lisp file first
;;;;

(in-package :dialogue)


;;; ****************************************************************************

(run-swt-ui ()
  (dialogue 
   `(name
     department
     full-time?
     email
     salary)
   :main t
   :data *employee-1*
   :title "Minimal"))

(run-swt-ui ()
  (dialogue 
   `(name
     (:accessor department
      :type :combo
      :style :read_only
      :range ,*departments*)
     full-time?
     email
     (:accessor salary
      :value-accepter ,(lambda (new control-data)
                         (and (integerp new) (<= 0 new 5000)))))
   :main t
   :data *employee-1*
   :title "Adequate"))

;;; replace the check box by 2 radio buttons
(run-swt-ui ()
  (dialogue 
   `(name
     (:accessor department
      :type :combo
      :style :read_only
      :range ,*departments*)
     email
     (:accessor salary
      :value-accepter ,(lambda (new control-data)
                         (and (integerp new) (plusp new))))
     (:group :prompt "Employment time" 
      (:column
       ((:accessor full-time?
         :type :button :style :radio
         :methods (:settext "Full"))
        (:type :button :style :radio
         :reader ,(lambda (data)
                   (not (full-time? data)))
         :methods (:settext "Partial"))))))
   :main t
   :data *employee-1*
   :title "Radio buttons"))

;;; Tabfolder
(run-swt-ui ()
  (dialogue 
   `(:tabfolder
     ("Employee 1"
      (:composite ; :setbackground ,(system-color :yellow)
       (name
        (:accessor department
         :type :combo
         :style :read_only
         :range ,*departments*)
        full-time?
        email
        (:accessor salary
         :value-accepter ,(lambda (new control-data)
                            (and (integerp new) (plusp new)))))))
     ("Employee 2"
      (:composite
       ((:accessor name
         :data ,*employee-2*)
        (:accessor department
         :data ,*employee-2*
         :type :combo
         :style :read_only
         :range ,*departments*)
        (:accessor full-time?
         :type :button :style :check
         :data ,*employee-2*)
        (:accessor email
         :data ,*employee-2*)
        (:accessor salary
         :data ,*employee-2*
         :value-accepter ,(lambda (new control-data)
                            (and (integerp new) (plusp new)))))))
     ("report"
      (:composite ;  :background ,(system-color :yellow)
       (:column :fill t :pack t
        ((:type :button :style (:push)
          :name :show
          :initial-value "Show control names"
          :layout-data (:width 300)
          :event (:selection
                  ,(lambda (event control-data)
                     (declare (ignore event))
                     (let ((text-control (find-control :report control-data)))
                       (do-controls (cd control-data)
                         (text.append text-control
                                      (format nil "~s~%" (cd-name cd))))
                       (text.append
                        text-control
                        (format nil "  ~d controls"
                                (/ (length
                                    (dd-control-data-list
                                     (find-dialog-data control-data)))
                                   2)))))))
         (:type :button :style (:push)
          :name :clear
          :initial-value "Clear"
          :event (:selection
                  ,(lambda (event control-data)
                     (declare (ignore event))
                     (let ((text-control (find-control :report control-data)))
                       (settext text-control "")))))
         (:type :text :style (:multi :border :v_scroll)
          :name :report
          :layout-data (:height 100)
          :methods
          (:setbackground ,(system-color :cyan)))
         )))))
   :main t
   :data *employee-1*
   :title "Tabfolder, controls report"))


(defvar *number-var* 1)
(defvar *string-var* "test")

(run-swt-ui ()
  (dialogue 
   `((:item *string-var*
      :prompt "&String variable"
      :layout-data (:width 100))
     *number-var*)
   :main t
   :edit :special-variable
   :data *employee-1*
   :title "Variables"))


(run-swt-ui ()
  (labels ((nested-tabfolders  (employees)
             (if (null employees)
                 ()
               (let ((employee (first employees)))
                 `(:tabfolder
                   (,(string-capitalize (name employee))
                    (:composite
                     ((:accessor salary :data ,employee)
                      (:accessor email :data ,employee)
                      (:accessor full-time? :data ,employee
                       :type :button :style :check)
                      (:accessor department :data ,employee
                       :type :combo :style :read_only
                       :range ,*departments*))))
                   ("next"
                    ,(nested-tabfolders (rest employees))))))))
    (dialogue
     (nested-tabfolders *employees*)
     :main t
     :title "Nested tabfolders")))



(run-swt-ui ()
  (flet ((hide-salary?(employee)
           (member (name employee) '("Noé" "Anne")
                   :test #'string-equal)))
    (dialogue
     `(:grid
       :horizontalSpacing 15
       :verticalSpacing 10
       ,(cons
         '((:type :label :initial-value "Name")
           (:type :label :initial-value "Salary")
           (:type :label :initial-value "Email")
           (:type :label :initial-value "FT")
           (:type :label :initial-value "Departement"))
         (mapcar (lambda (employee)
                   `((:accessor name :data ,employee
                      :layout-data (:horizontalalignment ,*swt.fill*))
                     ,(if (hide-salary? employee)
                          ()
                        `(:accessor salary :data ,employee
                          :layout-data (:horizontalalignment ,*swt.fill*)))
                     (:accessor email :data ,employee
                      :layout-data (:horizontalalignment ,*swt.fill*))
                     (:accessor full-time? :data ,employee
                      :type :button :style :check)
                     (:accessor department :data ,employee
                      :type :combo :style :read_only
                      :range ,*departments*)))
                 *employees*)))
     :main t
     :title "Grid")))


(run-swt-ui ()
  (dialogue 
   `(name
     (:accessor department
      :type :combo
      :range ,*departments*
      ; :read-converter ,(lambda (value cd) (symbol-name value))
      )
     full-time?
     email
     salary)
   :main t
   :data *employee-1*
   :title "Editable combo"
   :exit-controls
   (list (ok-control
          :after-hook 
          (lambda (dialog-data)
            ;; instead of this code, you could just get the value of the
            ;; department of *employee-1* and pushnew it in *departments*
            #+method-1
            (let* ((dept-cd (find-control-data :department dialog-data))
                   (ctrl-val (combo.gettext (cd-control dept-cd)))
                   (value (funcall (cd-write-converter dept-cd)
                                   ctrl-val dept-cd)))
              (pushnew value *departments*))
            (let ((dept-cd (find-control-data :department dialog-data)))
              (when (data-written-back? dept-cd)
                (pushnew (cd-new-lisp-value dept-cd) *departments*)))))
         (cancel-control))))